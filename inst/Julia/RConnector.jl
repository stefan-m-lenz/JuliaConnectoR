module RConnector

using Sockets

const TYPE_ID_NOTHING = 0x00
const TYPE_ID_FLOAT64 = 0x01
const TYPE_ID_COMPLEX = 0x02
const TYPE_ID_RAW = 0x03
const TYPE_ID_INT = 0x04
const TYPE_ID_BOOL = 0x05
const TYPE_ID_STRING = 0x06
const TYPE_ID_LIST = 0x07
const TYPE_ID_EXPRESSION = 0xee
const TYPE_ID_CALLBACK = 0xcb

const CALL_INDICATOR = 0x01
const RESULT_INDICATOR = 0x00
const FAIL_INDICATOR = 0xff
const BYEBYE = 0xbb

include("reading.jl")
include("evaluating.jl")
include("writing.jl")

function serve(port::Int)
   server = listen(port)
   sock = accept(server)
   println("Julia is listening on port $port.")
   while isopen(sock)
      call = Call()
      callbacks = Vector{Function}()

      try
         firstbyte = read(sock, 1)
         if length(firstbyte) == 1 && firstbyte[1] == CALL_INDICATOR
            # Parse incoming function call
            call = read_call(sock, callbacks)
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
      write_message(sock, result, callbacks)
   end
end


""" Lists the content of a package for import in R"""
function moduleinfo(pkgname::AbstractString; all::Bool = false)
   themodule = RConnector.maineval(pkgname)::Module

   function symbolisa(sym::Symbol, type)
      if isdefined(themodule, sym)
         field = themodule.eval(sym)
         return field isa type
      else
         return false
      end
   end

   isafunction(sym) = symbolisa(sym, Function)
   isadatatype(sym) = symbolisa(sym, DataType)

   function bangfuns_removed(funs::AbstractVector{String})
      if isempty(funs)
         return funs
      end
      ret = Vector{String}()
      sizehint!(ret, length(funs))
      push!(ret, funs[1])
      for i in 2:length(funs)
         # assumes that funs is a sorted vector of strings
         if !(funs[i][end] == '!' && funs[i-1] == funs[i][1:(end-1)])
            push!(ret, funs[i])
         end
      end
      ret
   end

   exportedsyms = names(themodule, all = false)
   exportedfunctions = map(string, filter(isafunction, exportedsyms))
   exportedfunctions = bangfuns_removed(exportedfunctions)
   exporteddatatypes = map(string, filter(isadatatype, exportedsyms))

   if all
      allsyms = names(themodule, all = true)
      filter!(sym -> string(sym)[1] != '#', allsyms)
      exportedSymSet = Set(exportedsyms)
      internalsyms = filter(sym -> !(sym in exportedSymSet), allsyms)
      internalfunctions = map(string, filter(isafunction, internalsyms))
      internaldatatypes = map(string, filter(isadatatype, internalsyms))
      internalfunctions = bangfuns_removed(internalfunctions)
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedDataTypes => exporteddatatypes,
                  :internalFunctions => internalfunctions,
                  :internalDataTypes => internaldatatypes))
   else
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedDataTypes => exporteddatatypes))
   end
end


function callbackfun(callbackid::Int, io)
   (args...; kwargs...) -> begin
      posargs = isempty(args) ? Vector{Any}() : collect(Any, args)
      callbackargs = ElementList(posargs, Dict{Symbol, Any}(kwargs))
      write_callback_message(io, callbackid, callbackargs)

      # read, parse and return answer
      while true
         firstbyte = read(io, 1)[1]
         if firstbyte == RESULT_INDICATOR
            callbacks = Vector{Function}()
            answer = read_element(io, callbacks)
            fails = collectfails(answer)
            if isempty(fails)
               ret =  evaluate!(answer)
               return ret
            else
               return Fail("Parsing failed. Reason: " * string(fails))
            end
         elseif firstbyte == CALL_INDICATOR
            callbacks = Vector{Function}()
            call = read_call(io, callbacks)
            result = evaluate!(call)
            write_message(io, result, callbacks)
         elseif firstbyte == FAIL_INDICATOR
            return Fail(read_string(io))
         else
            error("Unexpected leading byte: " * string(firstbyte))
         end
      end
   end
end


end