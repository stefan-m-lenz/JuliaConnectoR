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

"""
    serve(port_hint; keeprunnning = false, portfile = "")
Starts the JuliaConnectoR server, which will accept connections.

It is attempted to start the server at the port given via `port_hint`.
The actual port may be different. If the argument `portfile` is specified,
the real port is written to the file with the given name
as decimal number in form of a string.

If the argument `keeprunning` is set to `true`, the server will keep running
after a disconnect from the client. This is useful for debugging.
"""
function serve(port_hint::Int; keeprunning::Bool = false,
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

   if keeprunning
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

      result = evaluate_checked!(call)
      write_message(communicator, result)
   end
end


function callbackfun(callbackid::String, communicator::CommunicatoR)

   callbackfinalizer = CallbackFinalizer(callbackid)

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
            return evaluate!(answer)
         elseif firstbyte == CALL_INDICATOR
            call = read_call(communicator)
            result = evaluate_checked!(call)
            write_message(communicator, result)
         elseif firstbyte == FAIL_INDICATOR
            r_errormsg = read_string(communicator)
            error("Error in R callback: " * r_errormsg)
         else
            error("Unexpected leading byte: " * string(firstbyte))
         end
      end
   end

   registered_callbacks[ret] = callbackid
   ret
end


function callanonymous(functionref::Vector{UInt8}, args...; kwargs...)
   objref::ImmutableObjectReference = sharedheap[parseheapref(functionref)].obj
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
