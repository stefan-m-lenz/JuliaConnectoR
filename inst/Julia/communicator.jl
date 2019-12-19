# Encapsulate the io stream and a lock to prevent concurrent writing
# by multiple Tasks because this would mess up the messages
struct CommunicatoR{T}
   lock::ReentrantLock
   io::T
end

function CommunicatoR(io)
   CommunicatoR(ReentrantLock(), io)
end

# Redirect to members
write_bin(c::CommunicatoR, x) = write(c.io, x)
read_bin(c::CommunicatoR, x) = read(c.io, x)
message_start(c::CommunicatoR) = lock(c.lock)
message_end(c::CommunicatoR) = unlock(c.lock)


function redirect_outputstream(communicator::CommunicatoR, reader,
      output_type_indicator::UInt8)

   @async while isopen(communicator.io) && isopen(reader)
      iobuffer = readavailable(reader)
      write_output_message(communicator, iobuffer, output_type_indicator)
   end
end


function write_output_message(communicator::CommunicatoR, msg::Vector{UInt8},
      output_type_indicator::UInt8)

   lock(communicator)
   write(communicator, output_type_indicator)
   write(communicator, Int32(length(msg)))
   write(communicator, msg)
   unlock(communicator)
end
