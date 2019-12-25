module RConnector

using Sockets
using Logging

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
const TYPE_ID_FUNCTION = 0xfc

const CALL_INDICATOR = 0x01
const RESULT_INDICATOR = 0x00
const STDOUT_INDICATOR = 0x50
const STDERR_INDICATOR = 0x5e
const FAIL_INDICATOR = 0xff
const BYEBYE = 0xbb

const NO_ATTRIBUTES = Base.ImmutableDict{String, Any}()

const SEND_AS_RAW_TYPES = Union{UInt64, Int128, UInt128}
const SEND_AS_INT32 = Union{Int8, Int16, UInt16, Char}
const SEND_AS_DOUBLE = Union{Float16, Float32, UInt32}

include("communicator.jl")
include("handling_undefined.jl")
include("reading.jl")
include("evaluating.jl")
include("writing.jl")

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


""" Lists the content of a package for import in R"""
function moduleinfo(modulename::AbstractString; all::Bool = false)
   themodule = RConnector.maineval(modulename)::Module

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


end