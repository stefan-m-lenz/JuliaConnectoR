function writeInt32(outputstream, i::Int)
   write(outputstream, Int32(i))
end

function writeInt32s(outputstream, intarr::AbstractArray{Int})
   for i in intarr
      writeInt32(outputstream, i)
   end
end

function writeString(outputstream, str::String)
   writeInt32(outputstream, ncodeunits(str))
   write(outputstream, str)
end

function writeDimensions(outputstream, arr::AbstractArray)
   dims = size(arr)
   writeInt32(outputstream, length(dims))
   writeInt32s(outputstream, collect(dims))
end


function writeElement(outputstream, arr::AbstractArray{Int32})
   write(outputstream, TYPE_ID_INT)
   writeDimensions(arr)
   writeInt32s(outputstream, arr)
end

function writeElement(outputstream, arr::AbstractArray{String})
   write(outputstream, TYPE_ID_STRING)
   writeDimensions(arr)
   for str in arr
      writeString(outputstream, str)
   end
end

function writeElement(outputstream, arr::AbstractArray{<:Number})
   write(outputstream, TYPE_ID_FLOAT64)
   writeDimensions(outputstream, arr)
   for d in arr
      write(outputstream, Float64(d))
   end
end

function writeElement(outputstream, arr::AbstractArray{Bool})
   write(outputstream, TYPE_ID_BOOL)
   writeDimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function writeElement(outputstream, arr::AbstractArray) where T
   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))
   writeElement(outputstream,
         ElementList(Vector{Any}(arr), Vector{Symbol}(),
               Dict{Symbol, Any}(), attributes))
end

function writeElement(outputstream, fail::Fail)
   write(outputstream, TYPE_ID_FAIL)
   writeString(outputstream, fail.message)
end

function writeElement(outputstream, t::T) where T<:Union{Tuple, Pair}
   writeElement(outputstream, collect(t))
end

function writeElement(outputstream, ellist::ElementList)
   write(outputstream, TYPE_ID_LIST)
   writeList(outputstream, ellist)
end

function writeElement(outputstream, obj::T) where T
   if isstructtype(T)
      write(outputstream, TYPE_ID_LIST)
      names = fieldnames(T)
      fieldvalues = map(name -> getfield(obj, name), names)
      attributes = Dict{String, Any}("JLTYPE" => string(T))
      writeList(outputstream, ElementList(
            Vector(), collect(names),
            Dict{Symbol, Any}(zip(names, fieldvalues)),
            attributes))
      # TODO attributes
   else
      writeElement(outputstream, Fail("Lost in translation: $(string(obj))"))
   end
end

function writeElement(outputstream, i::Number)
   write(outputstream, TYPE_ID_FLOAT64)
   writeInt32(outputstream, 0)
   write(outputstream, Float64(i))
end

function writeElement(outputstream, i::Int32)
   write(outputstream, TYPE_ID_INT)
   writeInt32(outputstream, 0)
   writeInt32(outputstream, i)
end

function writeElement(outputstream, b::Bool)
   write(outputstream, TYPE_ID_BOOL)
   writeInt32(outputstream, 0)
   write(outputstream, b)
end

function writeElement(outputstream, str::String)
   write(outputstream, TYPE_ID_STRING)
   writeInt32(outputstream, 0)
   writeString(outputstream, str)
end

function writeElement(outputstream, n::Nothing)
   write(outputstream, TYPE_ID_NOTHING)
end


function writeList(outputstream, arr::AbstractArray)
   writeList(outputstream, ElementList(vec(arr)))
end

function writeList(outputstream, ellist::ElementList)
   writeInt32(outputstream, length(ellist.positionalelements))
   for el in ellist.positionalelements
      writeElement(outputstream, el)
   end

   writeInt32(outputstream, length(ellist.namedelements))
   for name in ellist.names # use the order of the names here
      writeString(outputstream, string(name))
      writeElement(outputstream, ellist.namedelements[name])
   end

   writeInt32(outputstream, length(ellist.attributes))
   for (key, value) in ellist.attributes
      writeString(outputstream, key)
      writeElement(outputstream, value)
   end
end