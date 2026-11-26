struct CircularReference
   objref::UInt64
end


mutable struct SharedObject
   obj::Any
   refcount::UInt64
end

function SharedObject(obj)
   SharedObject(obj, UInt32(1))
end


# Encapsulate the io stream and a lock to prevent concurrent writing
# by multiple Tasks because this would mess up the messages
# Making the struct mutable makes it easier to share the object with R,
# as it always has a fixed address
mutable struct CommunicatoR{T}
   # the iostream for communicating with R
   io::T

   # for synchronizing the communication
   # (stderr and stdout are communicated asynchronously to evaluation)
   io_lock::ReentrantLock

   # used to detect circular references
   objrefs::Set{UInt64}

   # for sharing references between Julia and R
   sharedheap::Dict{UInt64, SharedObject}

   # make sharedheap thread safe (needed due to asynchronicity of finalization)
   sharedheap_lock::ReentrantLock

   # whether the next translation is forced to be a full translation
   # in contrast to a mere passing of a reference
   full_translation::Bool

   # Contains identifiers of finalized callback functions.
   # These can be communicated to R to be removed there as well.
   finalized_callbacks::Vector{String}

   # contains callback functions and their corresponding identifiers
   # for the communication with R
   registered_callbacks::Dict{Function, String}

   # make changing finalized_callbacks and registered_callbacks thread safe
   # (needed due to asynchronicity of finalization)
   callbacks_lock::ReentrantLock

   # The message being written, sent to R in one piece by `send_buffered`.
   # Each write to the socket waits for the event loop, so writing the parts
   # of a message one by one dominated the time of calls with small results.
   # Guarded by io_lock, like the messages themselves.
   outbuf::IOBuffer

   # the tasks sending stdout and stderr to R (see redirect_outputstream)
   forwarders::Vector{Any}
end

function CommunicatoR(io)
   CommunicatoR(
      io,
      ReentrantLock(),
      Set{UInt64}(),
      Dict{UInt64, SharedObject}(),
      ReentrantLock(),
      false,
      Vector{String}(),
      Dict{Function, String}(),
      ReentrantLock(),
      IOBuffer(),
      Any[]
   )
end


# Arrays of at least this many bytes are written to the socket directly
# instead of being copied into the message buffer first
const DIRECT_WRITE_MIN = 65536

function write_bin(c::CommunicatoR, x)
   write(c.outbuf, x)
end

function write_bin(c::CommunicatoR, x::Array)
   if sizeof(x) < DIRECT_WRITE_MIN
      write(c.outbuf, x)
   else
      # the part of the message before the array goes first
      send_buffered(c)
      write(c.io, x)
   end
end

# Sends what has been buffered of the current message to R
function send_buffered(c::CommunicatoR)
   if position(c.outbuf) > 0
      write(c.io, take!(c.outbuf))
   end
   nothing
end

function begin_message(c::CommunicatoR)
   lock(c.io_lock)
   # (a message that failed while it was written must not be sent in part)
   truncate(c.outbuf, 0)
end

function end_message(c::CommunicatoR)
   try
      send_buffered(c)
   finally
      unlock(c.io_lock)
   end
end


function read_bin(c::CommunicatoR, n::Int)
   ret = read(c.io, n)
   while length(ret) < n
      # A short read means the stream ended before n bytes were available.
      chunk = read(c.io, n - length(ret))
      if isempty(chunk)
         error("Connection lost while reading message")
      end
      append!(ret, chunk)
   end
   ret
end


function read_byte(c::CommunicatoR)
   read(c.io, UInt8)
end


# GLOBAL variable nclients to count the number of clients that are connected
const nclients = Ref{Int}(0)
const nclients_lock = ReentrantLock()

function inc_nclients()
   lock(nclients_lock)
   nclients.x += 1
   unlock(nclients_lock)
end

# decrement number of clients. Stop server if it is reduced to 0.
function dec_nclients()
   lock(nclients_lock)
   nclients.x -= 1
   if nclients.x <= 0
      exit(0)
   end
   unlock(nclients_lock)
end


"""
    serve(port_hint; keeprunnning = false, portfile = "")
Starts the JuliaConnectoR server, which will accept connections.

It is attempted to start the server at the port given via `port_hint`.
The actual port may be different. If the argument `portfile` is specified,
the real port is written to the file with the given name
as decimal number in form of a string.

If the argument `multiclient` is `false`,
the server accepts only one client and stops after the client disconnects.
If the argument `multiclient` is set to `true`,
the server will accept multiple clients and keeps running
after a client disconnects.
Note that multiple clients share the same global workspace.
"""
function serve(port_hint::Int;
      multiclient::Bool = false,
      portfile::String = "")

   realport, server = listenany(port_hint)
   println("Julia is listening on port $realport.")
   # write real port to portfile
   if (portfile != "")
      open(portfile, "w") do f
         write(f, string(Int(realport)))
         write(f, '\n')
      end
   end

   # Prevents crash when run as script and interrupted
   ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 0)

   if multiclient
      while true
         sock = accept(server)
         disable_nagle(sock)
         @async serve_repl(sock)
         inc_nclients()
      end
   else
      sock = accept(server)
      # close(server) # don't listen for other clients
      # (not used because then listenany will re-use the same port)
      disable_nagle(sock)
      serve_repl(sock)
   end
end


# Disable Nagle's algorithm to avoid delays in the
# request-response communication with R.
# (Sockets.nagle requires at least Julia 1.3.)
function disable_nagle(sock)
   if isdefined(Sockets, :nagle)
      try
         Sockets.nagle(sock, false)
      catch
      end
   end
end


function serve_repl(sock)
   communicator = CommunicatoR(sock)
   capture_outputstreams(communicator)

   while isopen(sock)
      call = Call()
      try
         firstbyte = read_bin(communicator, 1)
         if length(firstbyte) == 1 && firstbyte[1] == CALL_INDICATOR
            # Parse incoming function call
            call = read_call(communicator)
         else
            if !(length(firstbyte) == 0 || firstbyte[1] == BYEBYE)
               close(sock)
               error("Unexpected leading character $(firstbyte[1])")
            else
               close(sock)
               dec_nclients()
               return
            end
         end
      catch ex
         @warn "Unexpected parsing error. Stopping listening. " *
               "Original error: $ex"
         dec_nclients()
         return
      end

      result = evaluate_checked!(call, communicator)
      write_message(communicator, result)
   end

   dec_nclients()
end


function callbackfun(callbackid::String, communicator::CommunicatoR)

   callbackfinalizer = CallbackFinalizer(callbackid, communicator)

   ret = (args...; kwargs...) -> begin
      stayalivewithme(callbackfinalizer)

      posargs = isempty(args) ? Vector{Any}() : collect(Any, args)
      callbackargs = ElementList(posargs, Dict{Symbol, Any}(kwargs))
      write_callback_message(communicator, callbackid, callbackargs)

      # read, parse and return answer
      while true
         firstbyte = read_bin(communicator, 1)[1]
         if firstbyte == RESULT_INDICATOR
            answer = read_element(communicator)
            parsingcheck(answer)
            return evaluate!(answer, communicator)
         elseif firstbyte == CALL_INDICATOR
            call = read_call(communicator)
            result = evaluate_checked!(call, communicator)
            write_message(communicator, result)
         elseif firstbyte == FAIL_INDICATOR
            r_errormsg = read_string(communicator)
            error("Error in R callback: " * r_errormsg)
         else
            error("Unexpected leading byte: " * string(firstbyte))
         end
      end
   end

   register_callback(communicator, ret, callbackid)
   ret
end


function callanonymous(communicator::CommunicatoR, functionref::Vector{UInt8},
      args...; kwargs...)

   ref = parseheapref(functionref)
   objref::ImmutableObjectReference = sharedheapget(communicator, ref)
   f::Function = objref.obj
   ret = f(args...; kwargs...)
   ret
end


# Upper bound on the rounds that output pending in a pipe is waited for,
# in case a task keeps writing to it
const MAX_OUTPUT_ROUNDS = 100

function start_result_message(c::CommunicatoR)
   # Ensure that output is transferred before the result:
   # Let the tasks redirecting the outputstreams take over
   # (see function redirect_outputstream).
   # Each round waits for the event loop twice and costs more than a small
   # call itself, so it is skipped when no output is pending.
   for _ in 1:MAX_OUTPUT_ROUNDS
      pending = output_pending(c)
      if pending === false
         break
      end

      flush(stdout)
      yield()
      flush(stderr)
      yield()

      if pending === missing
         # can't tell whether output is left: one round, as ever
         break
      end
   end

   # after the output is handled,
   # take control of the outputstream to send the result
   begin_message(c)
end

start_fail_message = start_result_message
start_callback_message = start_result_message


function end_result_message(c::CommunicatoR)
   # end of circular reference detection
   empty!(c.objrefs)

   # release lock of outputstream for next call
   end_message(c)
end

end_fail_message = end_result_message
end_callback_message = end_result_message


function start_output_message(c::CommunicatoR)
   begin_message(c)
end


function end_output_message(c::CommunicatoR)
   end_message(c)
end


function capture_outputstreams(communicator::CommunicatoR)
   redirect_outputstream(communicator, redirect_stdout()[1], STDOUT_INDICATOR)
   rd, wr = redirect_stderr()

   # Also redirect the logger.
   global_logger(SimpleLogger(wr))
   # For debugging, comment out the line above, use @debug lines and
   # set the environment variable "JULIA_DEBUG" to "all", e. g. in PowerShell
   # $env:JULIA_DEBUG = "all".
   # Start Julia and execute:
   # `import Tables; include("inst/Julia/RConnector.jl"); RConnector.serve(11980)`
   # Set the environment variable JULIACONNECTOR_SERVER=localhost:11980
   # for the R session. Then start R and load the JuliaConnectoR.
   # If you execute commands, debugging info is displayed on the command line.

   redirect_outputstream(communicator, rd, STDERR_INDICATOR)
end


mutable struct OutputForwarder
   # the read end of the pipe that the output stream is redirected to
   reader::Any

   # its file descriptor, or -1 if it can't be polled (see pipe_readable)
   fd::Cint

   # whether output that has been read from the pipe is being sent to R
   sending::Bool
end

function OutputForwarder(reader)
   fd = Cint(-1)
   @static if !Sys.iswindows()
      try
         fd = Base.cconvert(Cint, Base._fd(reader))
      catch
      end
   end
   OutputForwarder(reader, fd, false)
end


# Starts an asynchronous task that sends the output to R
function redirect_outputstream(communicator::CommunicatoR, reader,
      output_type_indicator::UInt8)

   forwarder = OutputForwarder(reader)
   push!(communicator.forwarders, forwarder)

   @async while isopen(communicator.io) && isopen(reader)
      iobuffer = readavailable(reader)
      forwarder.sending = true
      try
         write_output_message(communicator, iobuffer, output_type_indicator)
      finally
         forwarder.sending = false
      end
   end
end


# Whether output may still have to be sent to R before a result:
# `true` or `false`, or `missing` if that can't be told.
function output_pending(c::CommunicatoR)
   ret = false
   for forwarder in c.forwarders
      if forwarder.sending || bytesavailable(forwarder.reader) > 0
         return true
      end
      readable = pipe_readable(forwarder)
      if readable === true
         return true
      elseif readable === missing
         ret = missing
      end
   end
   ret
end


# The ioctl request for the number of bytes waiting in a pipe, or 0 if unknown.
# (poll() would also tell, but costs about 10 us on macOS.)
# On Linux, the value differs on some architectures (e.g. powerpc, mips).
const FIONREAD = Sys.isapple() || Sys.isbsd() ? Culong(0x4004667f) :
      Sys.islinux() && Sys.ARCH in (:x86_64, :i686, :aarch64, :armv7l, :armv6l,
         :riscv64) ? Culong(0x541B) : Culong(0)

# Whether the pipe holds data that has not been read yet
# (data that the event loop has not handed to the reader),
# or `missing` if that can't be told
function pipe_readable(forwarder::OutputForwarder)
   if forwarder.fd < 0 || FIONREAD == 0
      return missing
   end

   nbytes = Ref{Cint}(0)
   # (ioctl is variadic, which matters for the calling convention on arm64 macOS)
   if ccall(:ioctl, Cint, (Cint, Culong, Ptr{Cint}...),
         forwarder.fd, FIONREAD, nbytes) != 0
      return missing
   end
   nbytes[] > 0
end


function write_output_message(communicator::CommunicatoR, msg::Vector{UInt8},
      output_type_indicator::UInt8)

   start_output_message(communicator)
   write_bin(communicator, output_type_indicator)
   write_bin(communicator, Int32(length(msg)))
   write_bin(communicator, msg)
   end_output_message(communicator)
end


const DEFAULT_DISPLAY_LINES = 24
const DEFAULT_DISPLAY_COLUMNS = 80

function showobj(x, width::Int)
   io = IOBuffer()
   displaysize = (DEFAULT_DISPLAY_LINES, width)
   context = IOContext(io, :limit => true, :displaysize => displaysize)
   show(context, "text/plain", x)
   String(take!(io))
end

function showobj(x, width)
   intwidth = try
      convert(Int, width)
   catch
      DEFAULT_DISPLAY_COLUMNS
   end
   showobj(x, intwidth)
end
