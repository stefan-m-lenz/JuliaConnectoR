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
   # for synchronizing the communication
   lock::ReentrantLock

   # the iostream for communicating with R
   io::T

   # used to detect circular references
   objrefs::Set{UInt64}

   # for sharing references between Julia and R
   sharedheap::Dict{UInt64, SharedObject}

   # whether the next translation is forced to be a full translation
   # in contrast to a mere passing of a reference
   full_translation::Bool

   # Contains identifiers of finalized callback functions.
   # These can be communicated to R to be removed there as well.
   finalized_callbacks::Vector{String}

   # contains callback functions and their corresponding identifiers
   # for the communication with R
   registered_callbacks::Dict{Function, String}
end

function CommunicatoR(io)
   CommunicatoR(
      ReentrantLock(),
      io,
      Set{UInt64}(),
      Dict{UInt64, SharedObject}(),
      false,
      Vector{String}(),
      Dict{Function, String}()
   )
end


function write_bin(c::CommunicatoR, x)
   write(c.io, x)
end


function read_bin(c::CommunicatoR, n::Int)
   ret = read(c.io, n)
   while length(ret) < n
      ret = [ret; read(c.io, length(ret) - n)]
   end
   ret
end


function read_byte(c::CommunicatoR)
   read(c.io, UInt8)
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
         @async serve_repl(sock)
      end
   else
      sock = accept(server)
      serve_repl(sock)
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
               return
            end
         end
      catch ex
         error("Unexpected parsing error. Stopping listening. " *
               "Original error: $ex")
      end

      result = evaluate_checked!(call, communicator)
      write_message(communicator, result)
   end
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

   communicator.registered_callbacks[ret] = callbackid
   ret
end


function callanonymous(communicator::CommunicatoR, functionref::Vector{UInt8},
      args...; kwargs...)

   objref::ImmutableObjectReference =
         communicator.sharedheap[parseheapref(functionref)].obj
   f::Function = objref.obj
   ret = f(args...; kwargs...)
   ret
end


function start_result_message(c::CommunicatoR)
   # Ensure that output is transferred before the result:
   # Let the tasks redirecting the outputstreams take over
   # (see function redirect_outputstream)
   flush(c.io)
   flush(stdout)
   yield()
   flush(stderr)
   yield()

   # after the output is handled,
   # take control of the outputstream to send the result
   lock(c.lock)
end

start_fail_message = start_result_message
start_callback_message = start_result_message


function end_result_message(c::CommunicatoR)
   # end of circular reference detection
   empty!(c.objrefs)

   # release lock of outputstream for next call
   unlock(c.lock)
end

end_fail_message = end_result_message
end_callback_message = end_result_message


function start_output_message(c::CommunicatoR)
   lock(c.lock)
end


function end_output_message(c::CommunicatoR)
   unlock(c.lock)
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


# Starts an asynchronous task that sends the output to R
function redirect_outputstream(communicator::CommunicatoR, reader,
      output_type_indicator::UInt8)

   @async while isopen(communicator.io) && isopen(reader)
      iobuffer = readavailable(reader)
      write_output_message(communicator, iobuffer, output_type_indicator)
   end
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
   try
      intwidth = convert(Int, width)
   catch ex
      intwidth = DEFAULT_DISPLAY_COLUMNS
   end
   showobj(x, width)
end