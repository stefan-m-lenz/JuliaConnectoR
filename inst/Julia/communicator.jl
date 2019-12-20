# Encapsulate the io stream and a lock to prevent concurrent writing
# by multiple Tasks because this would mess up the messages
struct CommunicatoR{T}
   lock::ReentrantLock
   io::T
end

function CommunicatoR(io)
   CommunicatoR(ReentrantLock(), io)
end


function write_bin(c::CommunicatoR, x)
   write(c.io, x)
end


function read_bin(c::CommunicatoR, x)
   read(c.io, x)
end


function start_result_message(c::CommunicatoR)
   # ensure that output is transferred before the result
   flush(c.io)
   # let task redirecting the outputstream take over
   # (see function redirect_outputstream)
   yield()
   # lock the outputstream
   lock(c.lock)
end

start_fail_message = start_result_message
start_callback_message = start_result_message


function end_result_message(c::CommunicatoR)
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
