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


function r_compatible_type(t::Type{Char})
   Int32
end

function r_compatible_type(t::Type{T}) where T <: AbstractString
   String
end

function r_compatible_type(t::Type{Int32})
   Int32
end

function r_compatible_type(t::Type{Bool})
   Bool
end

function r_compatible_type(t::Type{T}) where {T2, T <: Complex{T2}}
   Complex{T2}
end

function r_compatible_type(t::Type{T}) where T <: Number
   Float64
end

function r_compatible_type(t::Type{Missing})
   Missing
end

function r_compatible_type(t::Type{Union{Missing, T}}) where T
   Union{Missing, r_compatible_type(T)}
end

function convert_to_r_compatible_type(x::AbstractArray{T}) where T
   convert(Array{r_compatible_type(T)}, x)
end

"""
Convert an object implementing the Tables.jl interface to an RDataFrame object
"""
function get_df(obj)
   coldict = Dict{Symbol, Any}()
   attributes = Dict{String, Any}()
   cols = columns(obj)
   colnames = columnnames(cols)
   for col in columnnames(cols)
      coldict[Symbol(col)] = convert_to_r_compatible_type(getcolumn(cols, col))
   end
   colnamesarr = Symbol.(collect(colnames))

   if length(colnamesarr) > 0
      allsame(x) = all(y -> y == first(x), x)
      if !allsame(map(length, values(coldict)))
         error("Lengths of columns not equal")
      end
   end

   RDataFrame(ElementList([], colnamesarr, coldict, attributes))
end

