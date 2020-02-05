mutable struct SharedObject
   obj::Any
   refcount::UInt64
end

function SharedObject(obj)
   SharedObject(obj, UInt32(1))
end

mutable struct AnonymousFunctionReference
   f::Function
end

# global variables
const sharedheap = Dict{UInt64, SharedObject}()
const fetch_mode = Ref(false) # TODO implement

function fetchmode!(mode::Bool)
   fetch_mode.x = mode
end

function sharedheapref!(obj)
   ref = UInt64(pointer_from_objref(obj))
   sharedheap[ref] = SharedObject(obj)
   ref
end


mutable struct ImmutableObjectReferece
   obj::Any
end


function shareobject!(obj)
   if isimmutable(obj)
      refobj = ImmutableObjectReferece(obj)
   else
      refobj = obj
   end
   ref = UInt64(pointer_from_objref(refobj))
   if haskey(sharedheap, ref)
      sharedheap[ref].refcount += 1
   else
      sharedheap[ref] = SharedObject(refobj)
   end
   ref
end


function parseheapref(ref::Vector{UInt8})
   UInt64(reinterpret(UInt64, ref)[1])
end


function decrefcount(ref::UInt64)
   newrefcount = sharedheap[ref].refcount - 1
   if newrefcount == 0
      delete!(sharedheap, ref)
   else
      sharedheap[ref].refcount = newrefcount
   end
end

function decrefcounts(refs::Vector{UInt8})
   for ref in reinterpret(UInt64, refs)
      decrefcount(ref)
   end
end


function sharedfinalizer(newobj, oldobjref::UInt64)
   # If a new Julia heap object is recreated via backtranslation from R,
   # it must reference the original object such that this one lives on.
   # It must be prevented that the old object is garbage collected because
   # this might finalize resources that are needed for the copy to work.
   if haskey(sharedheap, oldobjref)
      sharedheap[oldobjref].refcount += 1
      finalizer(obj -> decrefcount(oldobjref), newobj)
   else
      @warn "Please be sure that the revived objects " *
            "do not contain external references."
   end
end


struct CircularReference
   objref::UInt64
end


# Encapsulate the io stream and a lock to prevent concurrent writing
# by multiple Tasks because this would mess up the messages
struct CommunicatoR{T}
   lock::ReentrantLock   # for synchronizing the communication
   io::T                 # the iostream for communicating with R
   objrefs::Set{UInt64}  # used to detect circular references
end

function CommunicatoR(io)
   CommunicatoR(ReentrantLock(), io, Set{UInt64}())
end


function write_bin(c::CommunicatoR, x)
   write(c.io, x)
end


function read_bin(c::CommunicatoR, x)
   read(c.io, x)
end


function shareref!(communicator::CommunicatoR, obj)
   ref = sharedheapref!(obj)
   refexists = ref in communicator.objrefs
   push!(communicator.objrefs, ref)
   ref, refexists
end


function serve(port_hint::Int; multiclient::Bool = false,
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
      callbacks = Vector{Function}()
      try
         firstbyte = read_bin(communicator, 1)
         if length(firstbyte) == 1 && firstbyte[1] == CALL_INDICATOR
            # Parse incoming function call
            call = read_call(communicator, callbacks)
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

      result = evaluate!(call)
      write_message(communicator, result, callbacks)
   end
end


function callbackfun(callbackid::Int, communicator::CommunicatoR)
   (args...; kwargs...) -> begin
      posargs = isempty(args) ? Vector{Any}() : collect(Any, args)
      callbackargs = ElementList(posargs, Dict{Symbol, Any}(kwargs))
      write_callback_message(communicator, callbackid, callbackargs)

      # read, parse and return answer
      while true
         firstbyte = read_bin(communicator, 1)[1]
         if firstbyte == RESULT_INDICATOR
            callbacks = Vector{Function}()
            answer = read_element(communicator, callbacks)
            fails = collectfails(answer)
            if isempty(fails)
               ret =  evaluate!(answer)
               return ret
            else
               return Fail("Parsing failed. Reason: " * string(fails))
            end
         elseif firstbyte == CALL_INDICATOR
            callbacks = Vector{Function}()
            call = read_call(communicator, callbacks)
            result = evaluate!(call)
            write_message(communicator, result, callbacks)
         elseif firstbyte == FAIL_INDICATOR
            r_errormsg = read_string(communicator)
            throw(ErrorException("Error in R callback: "* r_errormsg))
         else
            error("Unexpected leading byte: " * string(firstbyte))
         end
      end
   end
end


function callanonymous(functionref::Vector{UInt8}, args...; kwargs...)
   anofunref::AnonymousFunctionReference =
         sharedheap[parseheapref(functionref)].obj
   ret = anofunref.f(args...; kwargs...)
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
   # For debugging, comment the following line out, use @debug lines and
   # set the environment variable "JULIA_DEBUG" to "all", e. g. in PowerShell
   # $env:JULIA_DEBUG = "all".
   global_logger(SimpleLogger(wr))

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
