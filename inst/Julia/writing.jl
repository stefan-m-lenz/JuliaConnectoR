function write_message(outputstream, result, callbacks::Vector{Function})
   write(outputstream, RESULT_INDICATOR)
   write_element(outputstream, result, callbacks)
end

function write_message(outputstream, fail::Fail, callbacks::Vector{Function})
   write(outputstream, FAIL_INDICATOR)
   write_string(outputstream, fail.message)
end


function write_callback_message(outputstream, callbackid::Int, args::ElementList)
   write(outputstream, CALL_INDICATOR)
   write_string(outputstream, string(callbackid))
   callbacks = Vector{Function}()
   write_list(outputstream, args, callbacks)
end


# attributes must be an iterable collection of keys (strings) and values.
function write_attributes(outputstream, attributes)
   write_nattributes(outputstream, length(attributes))
   for (key, value) in attributes
      write_string(outputstream, key)
      write_element(outputstream, value, Vector{Function}())
   end
end


function write_int32(outputstream, i::Int)
   write(outputstream, Int32(i))
end

function write_int32s(outputstream, intarr::AbstractArray{Int})
   for i in intarr
      write_int32(outputstream, i)
   end
end

function write_nattributes(outputstream, nattributes::Int)
   write(outputstream, convert(UInt8, nattributes))
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


function write_element(outputstream, arr::AbstractArray{String},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_STRING)
   write_dimensions(outputstream, arr)
   for str in arr
      write_string(outputstream, str)
   end
   # no attributes implemented yet (TODO implement undef)
   write(outputstream, 0x00)
end

function write_element(outputstream, arr::AbstractArray{F},
      callbacks::Vector{Function}) where {F <: Union{Float16, Float32}}

   attributes = (("JLTYPE", string(F)), )
   write_element(outputstream, convert(Array{Float64}, arr),
      callbacks, attributes)
end

function write_element(outputstream, arr::AbstractArray{Float64},
      callbacks::Vector{Function}, attributes = ())

   write(outputstream, TYPE_ID_FLOAT64)
   write_dimensions(outputstream, arr)
   for f in arr
      write(outputstream, f)
   end
   write_attributes(outputstream, attributes)
end

function write_element(outputstream, arr::AbstractArray{UInt8},
      callbacks::Vector{Function}, attributes = ())

   write(outputstream, TYPE_ID_RAW)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
   write_attributes(outputstream, attributes)
end

function write_element(outputstream, arr::AbstractArray{T},
      callbacks::Vector{Function}) where {T <: Union{Int8, Int16, UInt16}}

   if length(arr) == 1
      # add information, as the dimension only will not
      # be enough to determine that it is an array
      attributes = (("JLTYPE", string(T)), ("JLDIM", 1))
   else
      attributes = (("JLTYPE", string(T)), )
   end
   write_element(outputstream, convert(Array{Int32}, arr),
         callbacks, attributes)
end

function write_element(outputstream, arr::AbstractArray{Int32},
      callbacks::Vector{Function}, attributes = ())

   write(outputstream, TYPE_ID_INT)
   write_dimensions(outputstream, arr)
   for i in arr
      write(outputstream, i)
   end
   write_attributes(outputstream, attributes)
end

function write_element(outputstream, arr::AbstractArray{Int64},
      callbacks::Vector{Function})

   attributes = (("JLTYPE", "Int64"), )
   write_element(outputstream, convert(Array{Float64}, arr),
         callbacks, attributes) #TODO inexactness?
end

function write_element(outputstream, arr::AbstractArray{T},
      callbacks::Vector{Function}) where {T <: SEND_AS_RAW_TYPES}

   if length(arr) == 1
      attributes = (("JLTYPE", string(T)), ("JLDIM", collect(size(arr))))
   else
      attributes = (("JLTYPE", string(T)), )
   end
   write_element(outputstream, reinterpret(UInt8, arr),
         callbacks, attributes)
end

function write_element(outputstream, arr::AbstractArray{Complex{Float64}},
      callbacks::Vector{Function}, attributes = ())

   write(outputstream, TYPE_ID_COMPLEX)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, real(d))
      write(outputstream, imag(d))
   end
end

function write_element(outputstream, arr::AbstractArray{Complex{T}},
      callbacks::Vector{Function}
      ) where {T <: Union{Int32, Int64, Float16, Float32}}

   attributes = (("JLTYPE", string(T)), )
   write_element(outputstream, convert.(Complex{Float64}, arr),
         callbacks, attributes)
end

function write_element(outputstream, arr::AbstractArray{Bool},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_BOOL)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function write_element(outputstream, arr::AbstractArray,
      callbacks::Vector{Function})

   arr = arr[:] # TODO support multidimensional arrays

   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))
   ellist = ElementList(Vector{Any}(arr), Vector{Symbol}(),
         Dict{Symbol, Any}(), attributes)
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, dict::AbstractDict,
      callbacks::Vector{Function})

   # Assure that keys and values are always in a list,
   # to allow a straightforward reconstruction later on.
   # Strings and numbers would otherwise be converted to vectors in R.
   alwaysaslist(x) = x
   alwaysaslist(x::Array{N}) where {N <: Union{Number, String}} =
         ElementList(Vector{Any}(x))

   ellist = ElementList(Vector{Any}(),
         [:keys, :values],
         Dict{Symbol, Any}(:keys => alwaysaslist(collect(keys(dict))),
               :values => alwaysaslist(collect(values(dict)))),
         Dict{String, Any}("JLTYPE" => string(typeof(dict))))
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, set::AbstractSet,
      callbacks::Vector{Function})

   ellist = ElementList(Vector{Any}(collect(set)),
         Vector{Symbol}(), Dict{Symbol, Any}(),
         Dict{String, Any}("JLTYPE" => string(typeof(set))))
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, f::Function, callbacks::Vector{Function})
   callbackid = findfirst(isequal(f), callbacks)

   if callbackid == nothing
      callbackid = 0
   end

   write(outputstream, TYPE_ID_CALLBACK)
   write_int32(outputstream, callbackid)
end

function write_element(outputstream, t::Tuple, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_LIST)
   attributes = Dict{String, Any}("JLTYPE" => string(typeof(t)))
   ellist = ElementList(
         Vector{Any}(collect(t)),
         Vector{Symbol}(), Dict{Symbol, Any}(),
         attributes)
   write_list(outputstream, ellist, callbacks)
end

function write_element(outputstream, ellist::ElementList,
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_LIST)
   write_list(outputstream, ellist, callbacks)
end

function write_element(outputstream, obj::Symbol,
      callbacks::Vector{Function})

   ellist = ElementList(
         Vector{Any}(),
         [:name],
         Dict{Symbol, Any}(:name => string(obj)),
         Dict{String, Any}("JLTYPE" => "Symbol"))
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, obj::Module,
      callbacks::Vector{Function})

   modulename = string(obj)
   if startswith(modulename, "Main.")
      modulename = String(modulename[6:end])
   end
   ellist = ElementList(
      Vector{Any}(),
      [:name],
      Dict{Symbol, Any}(:name => modulename),
      Dict{String, Any}("JLTYPE" => "Module"))
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, d::T,
      callbacks::Vector{Function}) where {T2, T <: Type{T2}}
   write_expression(outputstream, string(d))
end

function write_element(outputstream, obj::T,
      callbacks::Vector{Function}) where T

   if isstructtype(T)
      names = fieldnames(T)
      if isempty(names) # type without members
         write_expression(outputstream, string(T) * "()")
      else
         write(outputstream, TYPE_ID_LIST)
         fieldvalues = map(name -> getfield(obj, name), names)
         attributes = Dict{String, Any}("JLTYPE" => string(T))
         ellist = ElementList(
               Vector(), collect(names),
               Dict{Symbol, Any}(zip(names, fieldvalues)),
               attributes)
         write_list(outputstream, ellist, callbacks)
      end
   else
      write_element(outputstream, Fail("Lost in translation: $(string(obj))"),
            callbacks)
   end
end

function write_element(outputstream, f::F, callbacks::Vector{Function}
      ) where {F <: Union{Float16, Float32}}

   attributes = (("JLTYPE", string(F)), )
   write_element(outputstream, Float64(f), callbacks, attributes)
end

function write_element(outputstream, f::Float64, callbacks::Vector{Function},
      attributes = ())

   write(outputstream, TYPE_ID_FLOAT64)
   write_int32(outputstream, 0)
   write(outputstream, f)
   write_attributes(outputstream, attributes)
end

function write_element(outputstream, i::T, callbacks::Vector{Function}
      ) where {T <: Union{Int8, Int16, UInt16}}

   attributes = (("JLTYPE", string(T)), )
   write_element(outputstream, Int32(i), callbacks, attributes)
end

function write_element(outputstream, i::Int32, callbacks::Vector{Function},
      attributes = ())
   write(outputstream, TYPE_ID_INT)
   write_int32(outputstream, 0)
   write(outputstream, i)
   write_attributes(outputstream, attributes)
end


function write_element(outputstream, i::Int64, callbacks::Vector{Function})
   if typemin(Int32) <= i <= typemax(Int32)
      attributes = ()
      write_element(outputstream, Int32(i), callbacks, attributes)
   else
      attributes = (("JLTYPE", "Int64"),)
      write_element(outputstream, Float64(i), callbacks) # TODO inexactness?
   end
end


function write_element(outputstream, i::T, callbacks::Vector{Function}
      ) where {T <: SEND_AS_RAW_TYPES}
   attributes = (("JLTYPE", string(T)), )
   write_element(outputstream, reinterpret(UInt8, [i]), callbacks, attributes)
end

function write_element(outputstream, b::Bool, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_BOOL)
   write_int32(outputstream, 0)
   write(outputstream, b)
end

function write_element(outputstream, str::String, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_STRING)
   write_int32(outputstream, 0)
   write_string(outputstream, str)
   write(outputstream, 0x00) # no attributes
end

function write_element(outputstream, c::Complex{Float64}, callbacks::Vector{Function},
      attributes = ())
   write(outputstream, TYPE_ID_COMPLEX)
   write_int32(outputstream, 0)
   write(outputstream, Float64(real(c)))
   write(outputstream, Float64(imag(c)))
   write_attributes(outputstream, attributes)
end

function write_element(outputstream, c::Complex{T}, callbacks::Vector{Function}
      ) where {T <: Union{Int32, Int64, Float16, Float32}}
   attributes = (("JLTYPE", string(T)), )
   write_element(outputstream, c, callbacks, attributes)
end

function write_element(outputstream, u::UInt8, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_RAW)
   write_int32(outputstream, 0)
   write(outputstream, u)
   write(outputstream, 0x00) # no attributes
end

function write_element(outputstream, n::Nothing, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_NOTHING)
end


function write_expression(outputstream, str::AbstractString)
   write(outputstream, TYPE_ID_EXPRESSION)
   write_string(outputstream, str)
end


function write_list(outputstream, arr::AbstractArray, callbacks::Vector{Function})
   write_list(outputstream, ElementList(vec(arr)), callbacks)
end

function write_list(outputstream, ellist::ElementList, callbacks::Vector{Function})
   write_int32(outputstream, length(ellist.positionalelements))
   for el in ellist.positionalelements
      write_element(outputstream, el, callbacks)
   end

   write_int32(outputstream, length(ellist.namedelements))
   for name in ellist.names # use the order of the names here
      write_string(outputstream, string(name))
      write_element(outputstream, ellist.namedelements[name], callbacks)
   end

   write_attributes(outputstream, ellist.attributes)
end