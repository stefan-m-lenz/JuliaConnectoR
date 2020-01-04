struct SharedObject
   obj::Any
   refcount::UInt32
end

const sharedheap = Dict{UInt64, SharedObject}()

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
   ref = UInt64(pointer_from_objref(obj))
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
