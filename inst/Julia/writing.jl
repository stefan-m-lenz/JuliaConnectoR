function write_message(communicator, result, callbacks::Vector{Function})
   start_result_message(communicator)
   write_bin(communicator, RESULT_INDICATOR)
   write_element(communicator, result, callbacks)
   end_result_message(communicator)
end

function write_message(communicator, fail::Fail, callbacks::Vector{Function})
   start_fail_message(communicator)
   write_bin(communicator, FAIL_INDICATOR)
   write_string(communicator, fail.message)
   end_fail_message(communicator)
end


function write_callback_message(communicator, callbackid::Int, args::ElementList)
   start_callback_message(communicator)
   write_bin(communicator, CALL_INDICATOR)
   write_string(communicator, string(callbackid))
   callbacks = Vector{Function}()
   write_list(communicator, args, callbacks)
   end_callback_message(communicator)
end


# attributes must be an iterable collection of keys (strings) and values.
function write_attributes(communicator, attributes)
   write_nattributes(communicator, length(attributes))
   for (key, value) in attributes
      write_string(communicator, key)
      write_element(communicator, value, Vector{Function}())
   end
end


function write_int32(communicator, i::Int)
   write_bin(communicator, Int32(i))
end

function write_int32s(communicator, intarr::AbstractArray{Int})
   for i in intarr
      write_int32(communicator, i)
   end
end

function write_nattributes(communicator, nattributes::Int)
   write_bin(communicator, convert(UInt8, nattributes))
end

function write_string(communicator, str::String)
   write_int32(communicator, ncodeunits(str))
   write_bin(communicator, str)
end

function write_dimensions(communicator, arr::AbstractArray)
   dims = size(arr)
   write_int32(communicator, length(dims))
   write_int32s(communicator, collect(dims))
end


function write_element(communicator, arr::AbstractArray{String},
      callbacks::Vector{Function})

   write_bin(communicator, TYPE_ID_STRING)
   write_dimensions(communicator, arr)
   for i in eachindex(arr)
      if isassigned(arr, i)
         write_string(communicator, arr[i])
      else
         write_string(communicator, "")
      end
   end
   # no attributes implemented yet (TODO implement undef)
   write_bin(communicator, 0x00)
end

function write_element(communicator, arr::AbstractArray{F},
      callbacks::Vector{Function}) where {F <: SEND_AS_DOUBLE}

   attributes = (("JLTYPE", string(F)), )
   write_element(communicator, convert(Array{Float64}, arr),
      callbacks, attributes)
end

function write_element(communicator, arr::AbstractArray{Float64},
      callbacks::Vector{Function}, attributes = ())

   write_bin(communicator, TYPE_ID_FLOAT64)
   write_dimensions(communicator, arr)
   for f in arr
      write_bin(communicator, f)
   end
   write_attributes(communicator, attributes)
end

function write_element(communicator, arr::AbstractArray{UInt8},
      callbacks::Vector{Function}, attributes = ())

   if length(arr) == 1
      attributes = tuple(attributes..., ("JLDIM", 1))
   end

   write_bin(communicator, TYPE_ID_RAW)
   write_dimensions(communicator, arr)
   for d in arr
      write_bin(communicator, d)
   end
   write_attributes(communicator, attributes)
end

function write_element(communicator, arr::AbstractArray{T},
      callbacks::Vector{Function}) where {T <: SEND_AS_INT32}

   attributes = (("JLTYPE", string(T)), )
   write_element(communicator, convert(Array{Int32}, arr),
         callbacks, attributes, true)
end

function write_element(communicator, arr::AbstractArray{Int32},
      callbacks::Vector{Function}, attributes = (), isothertype::Bool = false)

   if !isothertype
      attributes = (("JLTYPE", "Int32"), )
   end

   if length(arr) == 1
      attributes = tuple(attributes..., ("JLDIM", 1))
   end

   write_bin(communicator, TYPE_ID_INT)
   write_dimensions(communicator, arr)
   for i in arr
      write_bin(communicator, i)
   end
   write_attributes(communicator, attributes)
end

function write_element(communicator, arr::AbstractArray{Int64},
      callbacks::Vector{Function})

   arrmin, arrmax = extrema(arr)
   if arrmin >= typemin(Int32) && arrmax <= typemax(Int32)
      @static if Int == Int64
         attributes = ()
      else
         attributes = (("JLTYPE", "Int64"), )
      end
      write_element(communicator, convert(Array{Int32}, arr),
            callbacks, attributes, true)
   else
      attributes = (("JLTYPE", "Int64"), )
      write_element(communicator, convert(Array{Float64}, arr),
            callbacks, attributes) #TODO possible inexactness?
   end
end

function write_element(communicator, arr::AbstractArray{T},
      callbacks::Vector{Function}) where {T <: SEND_AS_RAW_TYPES}

   if length(arr) == 1
      attributes = (("JLTYPE", string(T)), ("JLDIM", collect(size(arr))))
   else
      attributes = (("JLTYPE", string(T)), )
   end
   write_element(communicator, reinterpret(UInt8, arr),
         callbacks, attributes)
end

function write_element(communicator, arr::AbstractArray{Complex{Float64}},
      callbacks::Vector{Function}, attributes = ())

   if length(arr) == 1
      attributes = tuple(attributes..., ("JLDIM", 1))
   end

   write_bin(communicator, TYPE_ID_COMPLEX)
   write_dimensions(communicator, arr)
   for d in arr
      write_bin(communicator, real(d))
      write_bin(communicator, imag(d))
   end
   write_attributes(communicator, attributes)
end

function write_element(communicator, arr::AbstractArray{C},
      callbacks::Vector{Function}) where {C <: SEND_AS_COMPLEX}

   attributes = (("JLTYPE", string(C)), )
   write_element(communicator, convert.(Complex{Float64}, arr),
         callbacks, attributes)
end

function write_element(communicator, arr::AbstractArray{Bool},
      callbacks::Vector{Function})

   write_bin(communicator, TYPE_ID_BOOL)
   write_dimensions(communicator, arr)
   for d in arr
      write_bin(communicator, d)
   end
end

function write_element(communicator, arr::AbstractArray,
      callbacks::Vector{Function})

   arrsize = size(arr)
   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))

   arr2 = arr[:] # R lists support only 1 dimension
   if length(arrsize) > 1
      # we have a multidimensional array:
      # add the dimensions as attribute to be able to reconstruct it
      attributes["JLDIM"] = collect(arrsize)
   end

   ellist = ElementList(undefs_replaced(arr2), Vector{Symbol}(),
         Dict{Symbol, Any}(), attributes)
   write_element(communicator, ellist, callbacks)
end

function write_element(communicator, dict::AbstractDict,
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
   write_element(communicator, ellist, callbacks)
end

function write_element(communicator, set::AbstractSet,
      callbacks::Vector{Function})

   ellist = ElementList(Vector{Any}(collect(set)),
         Vector{Symbol}(), Dict{Symbol, Any}(),
         Dict{String, Any}("JLTYPE" => string(typeof(set))))
   write_element(communicator, ellist, callbacks)
end

function write_element(communicator, f::Function, callbacks::Vector{Function})

   callbackid = findfirst(isequal(f), callbacks)
   if callbackid === nothing # it's not a callback function
      f_as_string = string(f)
      if endswith(f_as_string, "()")
         # an anonymous function will have a string representation like
         # "getfield(Main, Symbol(\"##5#6\"))()".
         # It cannot be transferred and instead a no-op function is transferred.
         callbackid = 0
      else
         # write an element for a named function
         write_bin(communicator, TYPE_ID_FUNCTION)
         write_string(communicator, f_as_string)
         return
      end
   end

   write_bin(communicator, TYPE_ID_CALLBACK)
   write_int32(communicator, callbackid)
end

function write_element(communicator, t::Tuple, callbacks::Vector{Function})
   write_bin(communicator, TYPE_ID_LIST)
   attributes = Dict{String, Any}("JLTYPE" => string(typeof(t)))
   ellist = ElementList(
         Vector{Any}(collect(t)),
         Vector{Symbol}(), Dict{Symbol, Any}(),
         attributes)
   write_list(communicator, ellist, callbacks)
end

function write_element(communicator, ellist::ElementList,
      callbacks::Vector{Function})

   write_bin(communicator, TYPE_ID_LIST)
   write_list(communicator, ellist, callbacks)
end

function write_element(communicator, obj::Symbol,
      callbacks::Vector{Function})

   ellist = ElementList(
         Vector{Any}(),
         [:name],
         Dict{Symbol, Any}(:name => string(obj)),
         Dict{String, Any}("JLTYPE" => "Symbol"))
   write_element(communicator, ellist, callbacks)
end

function write_element(communicator, obj::Module,
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
   write_element(communicator, ellist, callbacks)
end

function write_element(communicator, d::T,
      callbacks::Vector{Function}) where {T2, T <: Type{T2}}
   write_expression(communicator, string(d))
end

function write_element(communicator, obj::T,
      callbacks::Vector{Function}) where T

   if isstructtype(T)
      names = fieldnames(T)
      if isempty(names) # type without members
         write_expression(communicator, string(T) * "()")
      else
         write_bin(communicator, TYPE_ID_LIST)
         fieldvalues = map(name -> getfield(obj, name), names)
         attributes = Dict{String, Any}("JLTYPE" => string(T))
         ellist = ElementList(
               Vector(), collect(names),
               Dict{Symbol, Any}(zip(names, fieldvalues)),
               attributes)
         write_list(communicator, ellist, callbacks)
      end
   else
      write_element(communicator, Fail("Lost in translation: $(string(obj))"),
            callbacks)
   end
end

function write_element(communicator, f::F, callbacks::Vector{Function}
      ) where {F <: SEND_AS_DOUBLE}

   attributes = (("JLTYPE", string(F)), )
   write_element(communicator, Float64(f), callbacks, attributes)
end

function write_element(communicator, f::Float64, callbacks::Vector{Function},
      attributes = ())

   write_bin(communicator, TYPE_ID_FLOAT64)
   write_int32(communicator, 0)
   write_bin(communicator, f)
   write_attributes(communicator, attributes)
end

function write_element(communicator, i::T, callbacks::Vector{Function}
      ) where {T <: SEND_AS_INT32}

   attributes = (("JLTYPE", string(T)), )
   write_element(communicator, Int32(i), callbacks, attributes, true)
end

function write_element(communicator, i::Int32, callbacks::Vector{Function},
      attributes = (), isothertype::Bool = false)

   if !isothertype
      attributes = (("JLTYPE", "Int32"), )
   end

   write_bin(communicator, TYPE_ID_INT)
   write_int32(communicator, 0)
   write_bin(communicator, i)
   write_attributes(communicator, attributes)
end

function write_element(communicator, i::Int64, callbacks::Vector{Function})
   if typemin(Int32) <= i <= typemax(Int32)
      @static if Int == Int64
         attributes = ()
      else
         attributes = (("JLTYPE", "Int64"), )
      end
      write_element(communicator, Int32(i), callbacks, attributes, true)
   else
      attributes = (("JLTYPE", "Int64"), )
      # TODO inexactness?
      write_element(communicator, Float64(i), callbacks, attributes)
   end
end

function write_element(communicator, i::T, callbacks::Vector{Function}
      ) where {T <: SEND_AS_RAW_TYPES}
   attributes = (("JLTYPE", string(T)), )
   write_element(communicator, reinterpret(UInt8, [i]), callbacks, attributes)
end

function write_element(communicator, b::Bool, callbacks::Vector{Function})
   write_bin(communicator, TYPE_ID_BOOL)
   write_int32(communicator, 0)
   write_bin(communicator, b)
end

function write_element(communicator, str::String, callbacks::Vector{Function})
   write_bin(communicator, TYPE_ID_STRING)
   write_int32(communicator, 0)
   write_string(communicator, str)
   write_bin(communicator, 0x00) # no attributes
end

function write_element(communicator, c::Complex{Float64}, callbacks::Vector{Function},
      attributes = ())
   write_bin(communicator, TYPE_ID_COMPLEX)
   write_int32(communicator, 0)
   write_bin(communicator, Float64(real(c)))
   write_bin(communicator, Float64(imag(c)))
   write_attributes(communicator, attributes)
end

function write_element(communicator, c::C, callbacks::Vector{Function}
      ) where {C <: SEND_AS_COMPLEX}
   attributes = (("JLTYPE", string(C)), )
   write_element(communicator, convert(Complex{Float64}, c), callbacks, attributes)
end

function write_element(communicator, u::UInt8, callbacks::Vector{Function})
   write_bin(communicator, TYPE_ID_RAW)
   write_int32(communicator, 0)
   write_bin(communicator, u)
   write_bin(communicator, 0x00) # no attributes
end

function write_element(communicator, n::Nothing, callbacks::Vector{Function})
   write_bin(communicator, TYPE_ID_NOTHING)
end


function write_expression(communicator, str::AbstractString)
   write_bin(communicator, TYPE_ID_EXPRESSION)
   write_string(communicator, str)
end


function write_list(communicator, arr::AbstractArray, callbacks::Vector{Function})
   write_list(communicator, ElementList(vec(arr)), callbacks)
end

function write_list(communicator, ellist::ElementList, callbacks::Vector{Function})
   write_int32(communicator, length(ellist.positionalelements))
   for el in ellist.positionalelements
      write_element(communicator, el, callbacks)
   end

   write_int32(communicator, length(ellist.namedelements))
   for name in ellist.names # use the order of the names here
      write_string(communicator, string(name))
      write_element(communicator, ellist.namedelements[name], callbacks)
   end

   write_attributes(communicator, ellist.attributes)
end