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

   exportedsyms = names(themodule, all = false)
   exportedfunctions = map(string, filter(isafunction, exportedsyms))
   exporteddatatypes = map(string, filter(isadatatype, exportedsyms))

   if all
      allsyms = names(themodule, all = true)
      filter!(sym -> string(sym)[1] != '#', allsyms)
      exportedSymSet = Set(exportedsyms)
      internalsyms = filter(sym -> !(sym in exportedSymSet), allsyms)
      internalfunctions = map(string, filter(isafunction, internalsyms))
      internaldatatypes = map(string, filter(isadatatype, internalsyms))
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
