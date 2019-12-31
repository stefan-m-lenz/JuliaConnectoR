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
   isatype(sym) = symbolisa(sym, Type)

   exportedsyms = names(themodule, all = false)
   exportedfunctions = map(string, filter(isafunction, exportedsyms))
   exportedtypes = map(string, filter(isatype, exportedsyms))

   if all
      allsyms = names(themodule, all = true)
      filter!(sym -> string(sym)[1] != '#', allsyms)
      exportedSymSet = Set(exportedsyms)
      internalsyms = filter(sym -> !(sym in exportedSymSet), allsyms)
      internalfunctions = map(string, filter(isafunction, internalsyms))
      internaltypes = map(string, filter(isatype, internalsyms))
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedTypes => exportedtypes,
                  :internalFunctions => internalfunctions,
                  :internalTypes => internaltypes))
   else
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedTypes => exportedtypes))
   end
end
