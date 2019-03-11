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
   if port < 0
      return # argument just to trigger precompilation
   end
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

""" Lists the content of a package for import in R"""
function pkgContentList(pkgname::AbstractString)
   themodule = TcpCallR.maineval(pkgname)::Module

   allsyms = names(themodule, all = true)
   filter!(sym -> string(sym)[1] != '#', allsyms)

   exportedsyms = names(themodule, all = false)

   exportedfunctions = Vector{String}(undef, 0)
   sizehint!(exportedfunctions, length(exportedsyms))

   internalfunctions = Vector{String}(undef, 0)
   sizehint!(internalfunctions, length(exportedsyms))

   for sym in allsyms
      field = themodule.eval(sym)
      if field isa Function || field isa DataType
         if sym in exportedsyms
            push!(exportedfunctions, string(sym))
         else
            push!(internalfunctions, string(sym))
         end
      end
   end

   ElementList(Vector{Any}(), [:exportedFunctions, :internalFunctions],
         Dict{Symbol, Any}(
               :exportedFunctions => exportedfunctions, 
               :internalFunctions => internalfunctions))
end

end