function getprop(value, name::String)
   getproperty(value, Symbol(name))
end

function setprop!(value, name::String, x)
   setproperty!(value, Symbol(name), x)
end


function getidx(collection, key)
   getindex(collection, key)
end

# get multiple elements
function getidx(collection, keys::Vector{Float64})
   map(k -> getindex(collection, Int(k)), keys)
end

# get one element in a multidimensional array
function getidx(collection, key::Vararg{Float64})
   intkey = map(Int, key)
   getindex(collection, intkey...)
end


function setidx!(collection, value, key)
   setindex!(collection, value, key)
end

# replace multiple elements
function setidx!(collection, values::Vector, keys::Vector{Float64})
   if length(values) != length(keys)
      error("Number of items to replace is not equal to replacement length")
   end
   map(i -> setindex!(collection, values[i], Int(keys[i])), eachindex(keys))
end

# replace single element in multidimensional array
function setidx!(collection, value, key::Vararg{Float64})
   intkey = map(Int, key)
   setindex!(collection, value, intkey...)
end
