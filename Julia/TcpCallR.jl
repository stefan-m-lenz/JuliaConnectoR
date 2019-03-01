module TcpCallR

using Sockets

const TYPE_ID_NOTHING = 0x00
const TYPE_ID_FLOAT64 = 0x01
const TYPE_ID_INT = 0x02
const TYPE_ID_BOOL = 0x03
const TYPE_ID_STRING = 0x04
const TYPE_ID_LIST = 0x05
const TYPE_ID_FAIL = 0xff

const CALL_INDICATOR = 0x01
const RESULT_INDICATOR = 0x00
const BYEBYE = 0xbb

include("reading.jl")
include("evaluating.jl")
include("writing.jl")

function serve(port::Int)
   server = listen(port)
   sock = accept(server)
   println("Julia is listening on port $port.")
   while isopen(sock)
      functioncall = FunctionCall()
      try
         firstbyte = read(sock, 1)[1]
         if firstbyte == CALL_INDICATOR
            # Parse incoming function call
            functioncall = read_call(sock)
         elseif firstbyte == BYEBYE
            close(sock)
            return
         end
      catch ex
         error("Unexpected parsing error. Stopping listening. " *
               "Original error: $ex")
      end

      fails = collectfails(functioncall)
      if isempty(fails)
         # evaluate function only if the parsing was totally successful
         result = evaluate!(functioncall)
      else
         result = Fail("Parsing failed. Reason: " * string(fails))
      end

      write(sock, RESULT_INDICATOR)
      writeElement(sock, result)
   end
end

end