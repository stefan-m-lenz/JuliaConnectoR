function getprop(value, name)
   getproperty(value, Symbol(name))
end

# "d$key" will result in a lookup instead of a access of fields
function getprop(d::AbstractDict{K, V}, key::K) where {K, V}
   d[key]
end


function setprop!(value, name, x)
   setproperty!(value, Symbol(name), x)
   return nothing
end

function setprop!(d::AbstractDict{K, V}, key::K, x) where {K, V}
   d[key] = x
   return nothing
end


"""
    dimensionidxs(x)
Returns a proper index for one dimension when indexing multiple elements in
a multidimensional array.
This transformation of the indexes makes the R style single indexing possible
to always return a list or a vector of elements.
"""
function dimensionidxs(x)
   x
end

function dimensionidxs(x::Number)
   [Int(x)]
end

function dimensionidxs(x::AbstractArray{Float64})
   convert(Vector{Int}, x)
end


"""
    getidx(collection, key)
Extract a single element from a collection.
Called by `collection[[key]]` in R.
"""
function getidx(collection, key)
   getindex(collection, key)
end

# get one element in a multidimensional array
function getidx(collection, key::Vararg{<:Number})
   intkey = map(Int, key)
   getindex(collection, intkey...)
end


"""
    getidxs(collection, idx)
Access multiple elements in a collection.
Called via "collection[idx]" in R.
Expected to return a vector or a vector-like object.
The exact return type may depend on the implementation of `getindex`
of the specific collection.
"""
function getidxs(collection, idx::Number)
   getindex(collection, [idx])
end

# indexing with e.g. v[1] in R
function getidxs(collection, idx::Float64)
   getindex(collection, [Int(idx)])
end

# indexing with e.g. v[1:4] in R
function getidxs(collection, idxs::Vector{Float64})
   getindex(collection, map(Int, idxs))
end

# indexing with e.g. v[v > 4]
function getidxs(collection, idxs::Union{AbstractVector, AbstractRange})
   getindex(collection, idxs)
end

# multidimensional indexing
function getidxs(collection,
      keys::Vararg{<:Union{AbstractArray, AbstractRange, Float64, Int}})
   dimidxs = map(dimensionidxs, keys)
   getindex(collection, dimidxs...)
end

# multidimensional indexing using a colon in one dimenstion
function getidxs(collection,
   keys::Vararg{<:Union{AbstractArray, AbstractRange, Float64, Int, Colon}})

   # same as implementation without colons, except ...
   dimidxs = map(dimensionidxs, keys)
   ret = getindex(collection, dimidxs...)
   # ... drop dimension of single values
   dropdims(ret; dims = tuple(findall(x -> x isa Number, keys)...))
end

# indexing dictionary with e.g. d["hi"]
function getidxs(d::AbstractDict{K, V}, key::K) where {K, V}
   [d[key]]
end

# indexing dictionary with e.g. d[c("hi", "you")]
function getidxs(d::AbstractDict{K, V}, keys::Vararg{K}) where {K, V}
   ret = Vector{V}(undef, length(keys))
   for i in eachindex(keys)
      ret[i] = d[keys[i]]
   end
   ret
end


"""
    setidx!(collection, value, key)
Set a single element in a collection.
Called by `collection[[key]] <- value` in R.
"""
function setidx!(collection, value, key)
   setindex!(collection, value, key)
   return nothing
end

# replace single element in multidimensional array
function setidx!(collection, value, key::Vararg{<:Number})
   intkey = map(Int, key)
   setindex!(collection, value, intkey...)
   return nothing
end


function check_lengths_for_assignment(keys, values)
   if length(values) != length(keys)
      error("Number of items to replace is not equal to replacement length")
   end
end


"""
    setidxs!(collection, values, keys)
Set multiple elements in a collection.
Called by `collection[key] <- value` in R.
"""
function setidxs!(collection, value,
      keys::Vararg{<:Union{AbstractArray, AbstractRange, Float64, Int}})

   dimidxs = map(dimensionidxs, keys)
   collection[dimidxs...] .= value
   return nothing
end

# replace single element in multidimensional array
function setidxs!(collection, value, key::Vararg{<:Union{Int, Float64}})
   intkey = map(Int, key)
   setindex!(collection, value, intkey...)
   return nothing
end

# replace multiple elements in a multidimensional array with multiple values
function setidxs!(collection::AbstractArray{T}, value::AbstractArray{T},
      keys::Vararg{<:Union{AbstractArray, AbstractRange, Float64, Int}}) where T

   dimidxs = map(dimensionidxs, keys)
   collection[dimidxs...] .= value
   return nothing
end

# replace multiple elements in a multidimensional array with a single value
function setidxs!(collection::AbstractArray{T}, value::T,
      keys::Vararg{<:Union{AbstractArray, AbstractRange, Float64, Int}}) where T

   dimidxs = map(dimensionidxs, keys)
   collection[dimidxs...] .= repeat([value], length.(keys)...)
   return nothing
end

# replace single element in dictionary with e.g. d["hi"]
function setidxs!(d::AbstractDict{K, V}, value, key::K) where {K, V}
   d[key] = value
   return nothing
end

# replace multiple elements in dictionary with e.g. d[c("hi", "you")]
function setidxs!(d::AbstractDict{K, V}, values::Vector, keys::Vararg{K}) where {K, V}
   check_lengths_for_assignment(keys, values)
   map(i -> d[keys[i]] = values[i], eachindex(keys))
   return nothing
end

function getdim(x::T) where {T <: Union{AbstractArray, Tuple}}
   collect(size(x))
end