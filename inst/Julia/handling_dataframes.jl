struct RDataFrame <: AbstractColumns
   ellist::ElementList
end

istable(::Type{RDataFrame}) = true
columnaccess(::Type{RDataFrame}) = true
columns(df::RDataFrame) = df
columnnames(df::RDataFrame) = getfield(getfield(df, :ellist), :names)

function getcolumn(df::RDataFrame, nm::Symbol)
   getfield(getfield(df, :ellist), :namedelements)[nm]
end

function getcolumn(df::RDataFrame, i::Int)
   getfield(getfield(df, :ellist), :namedelements)[getfield(df, :names)[i]]
end

function getcolumn(df::RDataFrame, ::Type{T}, col::Int, nm::Symbol) where {T}
   getcolumn(df, nm)
end


function get_df(obj)
   coldict = Dict{Symbol, Any}()
   attributes = Dict{String, Any}()
   cols = columns(obj)
   colnames = columnnames(cols)
   for col in columnnames(cols)
      coldict[col] = getcolumn(cols, col)
   end
   colnamesarr = collect(colnames)
   
   if length(colnamesarr) > 0
      allsame(x) = all(y -> y == first(x), x)
      if !allsame(map(length, values(coldict)))
         error("Lengths of columns not equal")
      end
   end
   
   RDataFrame(ElementList([], colnamesarr, coldict, attributes))
end

