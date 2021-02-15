""" Escape latex symbols in given string """
function escapelatexsymbols(str::String)
   if isascii(str)
      return str
   else
      # create a new string where each symbol in the original string
      # is replaced with its latex representation if it has one
      ret = Vector{String}(undef, length(str))
      retindex = 1
      for i in eachindex(str)
         if isascii(str[i])
            ret[retindex] = string(str[i])
         else
            latexname = REPL.symbol_latex(string(str[i]))
            if latexname != ""
               ret[retindex] = "<" * latexname[2:end] * ">"
               if isempty(ret[retindex])
                  ret[retindex] = string(str[i])
               end
            else
               ret[retindex] = string(str[i])
            end
         end
         retindex += 1
      end
      return join(ret)
   end
end


"""
Returns an object that translates to an R list with keys
"original" and "escaped".
Both entries have as values character vectors.
"original" contains all names that have a different version that
contains latex for describing symbols.
"escaped" contains the names with the latex sequences in the same order.
"""
function escapednames(originalnames::Vector{String})
   escaped = escapelatexsymbols.(originalnames)
   needsescaping = (escaped .!= originalnames)
   original = originalnames[needsescaping]
   escaped = escaped[needsescaping]
   ElementList(Vector{Any}(), Dict{Symbol, Any}(
         :original => original, :escaped => escaped))
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
      escapedfunctions = escapednames(vcat(exportedfunctions, internalfunctions))
      escapedtypes = escapednames(vcat(exportedtypes, internaltypes))
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedTypes => exportedtypes,
                  :internalFunctions => internalfunctions,
                  :internalTypes => internaltypes,
                  :escapedFunctions => escapedfunctions,
                  :escapedTypes => escapedtypes))
   else
      escapedfunctions = escapednames(exportedfunctions)
      escapedtypes = escapednames(exportedtypes)
      return ElementList(Vector{Any}(), Dict{Symbol, Any}(
                  :exportedFunctions => exportedfunctions,
                  :exportedTypes => exportedtypes,
                  :escapedFunctions => escapedfunctions,
                  :escapedTypes => escapedtypes))
   end
end
