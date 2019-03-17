function write_message(outputstream, result)
   write(outputstream, RESULT_INDICATOR)
   write_element(outputstream, result)
end

function write_message(outputstream, fail::Fail)
   write(outputstream, FAIL_INDICATOR)
   write_string(outputstream, fail.message)
end


function write_int32(outputstream, i::Int)
   write(outputstream, Int32(i))
end

function write_int32s(outputstream, intarr::AbstractArray{Int})
   for i in intarr
      write_int32(outputstream, i)
   end
end

function write_string(outputstream, str::String)
   write_int32(outputstream, ncodeunits(str))
   write(outputstream, str)
end

function write_dimensions(outputstream, arr::AbstractArray)
   dims = size(arr)
   write_int32(outputstream, length(dims))
   write_int32s(outputstream, collect(dims))
end


function write_element(outputstream, arr::AbstractArray{Int32})
   write(outputstream, TYPE_ID_INT)
   write_dimensions(outputstream, arr)
   write_int32s(outputstream, arr)
end

function write_element(outputstream, arr::AbstractArray{String})
   write(outputstream, TYPE_ID_STRING)
   write_dimensions(outputstream, arr)
   for str in arr
      write_string(outputstream, str)
   end
end

function write_element(outputstream, arr::AbstractArray{<:Number})
   write(outputstream, TYPE_ID_FLOAT64)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, Float64(d))
   end
end

function write_element(outputstream, arr::AbstractArray{<:Complex})
   write(outputstream, TYPE_ID_COMPLEX)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, Float64(real(d)))
      write(outputstream, Float64(imag(d)))
   end
end

function write_element(outputstream, arr::AbstractArray{Bool})
   write(outputstream, TYPE_ID_BOOL)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function write_element(outputstream, arr::AbstractArray{UInt8})
   write(outputstream, TYPE_ID_RAW)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function write_element(outputstream, arr::AbstractArray) where T
   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))
   write_element(outputstream,
         ElementList(Vector{Any}(arr), Vector{Symbol}(),
               Dict{Symbol, Any}(), attributes))
end

function write_element(outputstream, f::Function)
   write(outputstream, TYPE_ID_CALLBACK)
   write_int32(outputstream, 0)
end

# TODO rewrite: cannot be given back to julia
function write_element(outputstream, t::T) where T <: Union{Tuple, Pair}
   write_element(outputstream, collect(t))
end

function write_element(outputstream, ellist::ElementList)
   write(outputstream, TYPE_ID_LIST)
   write_list(outputstream, ellist)
end

function write_element(outputstream, obj::Symbol)
   write_element(outputstream, ElementList(
         Vector{Any}(),
         [:name],
         Dict{Symbol, Any}(:name => string(obj)),
         Dict{String, Any}("JLTYPE" => "Symbol")))
end

function write_element(outputstream, obj::Module)
   modulename = string(obj)
   if startswith(modulename, "Main.")
      modulename = String(modulename[6:end])
   end
   write_element(outputstream, ElementList(
         Vector{Any}(),
         [:name],
         Dict{Symbol, Any}(:name => Symbol(modulename)),
         Dict{String, Any}("JLTYPE" => "Module")))
end

function write_element(outputstream, d::T) where {T2, T <: Type{T2}}
   write_expression(outputstream, string(d))
end

function write_element(outputstream, obj::T) where T
   if isstructtype(T)
      names = fieldnames(T)
      if isempty(names) # type without members
         write_expression(outputstream, string(T) * "()")
      else
         write(outputstream, TYPE_ID_LIST)
         fieldvalues = map(name -> getfield(obj, name), names)
         attributes = Dict{String, Any}("JLTYPE" => string(T))
         write_list(outputstream, ElementList(
               Vector(), collect(names),
               Dict{Symbol, Any}(zip(names, fieldvalues)),
               attributes))
      end
   else
      write_element(outputstream, Fail("Lost in translation: $(string(obj))"))
   end
end

function write_element(outputstream, i::Number)
   write(outputstream, TYPE_ID_FLOAT64)
   write_int32(outputstream, 0)
   write(outputstream, Float64(i))
end

function write_element(outputstream, i::Int32)
   write(outputstream, TYPE_ID_INT)
   write_int32(outputstream, 0)
   write_int32(outputstream, i)
end

function write_element(outputstream, b::Bool)
   write(outputstream, TYPE_ID_BOOL)
   write_int32(outputstream, 0)
   write(outputstream, b)
end

function write_element(outputstream, str::String)
   write(outputstream, TYPE_ID_STRING)
   write_int32(outputstream, 0)
   write_string(outputstream, str)
end

function write_element(outputstream, c::Complex)
   write(outputstream, TYPE_ID_COMPLEX)
   write_int32(outputstream, 0)
   write(outputstream, Float64(real(c)))
   write(outputstream, Float64(imag(c)))
end

function write_element(outputstream, u::UInt8)
   write(outputstream, TYPE_ID_RAW)
   write_int32(outputstream, 0)
   write(outputstream, u)
end

function write_element(outputstream, n::Nothing)
   write(outputstream, TYPE_ID_NOTHING)
end


function write_expression(outputstream, str::AbstractString)
   write(outputstream, TYPE_ID_EXPRESSION)
   write_string(outputstream, str)
end


function write_list(outputstream, arr::AbstractArray)
   write_list(outputstream, ElementList(vec(arr)))
end

function write_list(outputstream, ellist::ElementList)
   write_int32(outputstream, length(ellist.positionalelements))
   for el in ellist.positionalelements
      write_element(outputstream, el)
   end

   write_int32(outputstream, length(ellist.namedelements))
   for name in ellist.names # use the order of the names here
      write_string(outputstream, string(name))
      write_element(outputstream, ellist.namedelements[name])
   end

   write_int32(outputstream, length(ellist.attributes))
   for (key, value) in ellist.attributes
      write_string(outputstream, key)
      write_element(outputstream, value)
   end
end