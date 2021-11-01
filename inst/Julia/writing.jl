function write_message(communicator, result)
   start_result_message(communicator)
   write_bin(communicator, RESULT_INDICATOR)
   write_element(communicator, result)
   end_result_message(communicator)
end

function write_message(communicator, fail::Fail)
   start_fail_message(communicator)
   write_bin(communicator, FAIL_INDICATOR)
   write_string(communicator, fail.message)
   end_fail_message(communicator)
end


function write_callback_message(communicator, callbackid::String, args::ElementList)
   start_callback_message(communicator)
   write_bin(communicator, CALL_INDICATOR)
   write_string(communicator, callbackid)
   write_list(communicator, args)
   end_callback_message(communicator)
end


# attributes must be an iterable collection of keys (strings) and values.
function write_attributes(communicator, attributes)
   write_nattributes(communicator, length(attributes))
   for (key, value) in attributes
      write_string(communicator, key)
      write_element(communicator, value)
   end
end


function write_int32(communicator, i::Int)
   write_bin(communicator, Int32(i))
end

function write_int32s(communicator, intarr::Array{Int})
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


function write_element(communicator, arr::Array{String}, attributes = ())
   write_bin(communicator, TYPE_ID_STRING)
   write_dimensions(communicator, arr)
   for i in eachindex(arr)
      if isassigned(arr, i)
         write_string(communicator, arr[i])
      else
         write_string(communicator, "")
      end
   end
   write_attributes(communicator, attributes)
end

function write_element(communicator, arr::Array{Union{String, Missing}})
   missings = findall(ismissing, vec(arr))
   arr2 = copy(arr)
   arr2[missings] .= ""
   strarr = convert(Array{String}, arr2)
   attributes = (("NA", missings), )
   write_element(communicator, strarr, attributes)
end

function write_element(communicator, arr::Array{F}) where {F <: SEND_AS_DOUBLE}
   attributes = (("JLTYPE", string(F)), )
   write_element(communicator, convert(Array{Float64}, arr), attributes)
end

function write_element(communicator, arr::Array{Union{Missing,F}}
      ) where {F <: Union{SEND_AS_DOUBLE}}
   attributes = (("JLTYPE", "Union{Missing," * string(F)* "}"), )
   arr2 = replace(arr, missing => R_NA_REAL)
   write_element(communicator, arr2, attributes)
end

function write_element(communicator, arr::Array{Union{Missing,Float64}})
   arr2 = replace(arr, missing => R_NA_REAL)
   write_element(communicator, arr2)
end

function write_element(communicator, arr::Array{Float64}, attributes = ())
   write_bin(communicator, TYPE_ID_FLOAT64)
   write_dimensions(communicator, arr)
   for f in arr
      write_bin(communicator, f)
   end
   write_attributes(communicator, attributes)
end

function write_uint8array_element(communicator, arr::AbstractArray{UInt8},
      attributes = ())

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

function write_element(communicator, arr::Array{UInt8}, attributes = ())
   write_uint8array_element(communicator, arr, attributes)
end

function write_element(communicator, arr::Array{T},
      ) where {T <: SEND_AS_INT32}

   attributes = (("JLTYPE", string(T)), )
   write_element(communicator, convert(Array{Int32}, arr), attributes, true)
end

function write_element(communicator, arr::Array{Union{Missing, T}},
      ) where {T <: Union{Int32, SEND_AS_INT32}}

   attributes = (("JLTYPE", "Union{Missing," * string(T)* "}"), )
   arr2 = convert(Array{Int32}, replace(arr, missing => R_NA_INTEGER))
   write_element(communicator, arr2, attributes, true)
end

function write_element(communicator, arr::Array{Int32},
      attributes = (), isothertype::Bool = false)

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

function write_element(communicator, arr::Array{Int64}; missings = Int[])

   arrmin, arrmax = extrema(arr)
   if arrmin >= typemin(Int32) && arrmax <= typemax(Int32)
      @static if Int == Int64
         attributes = ()
      else
         attributes = (("JLTYPE", "Int64"), )
      end
      if !isempty(missings)
         @static if Int != Int64
            attributes = (("JLTYPE", "Union{Missing,Int64}"), )
         end
         arr[missings] .= R_NA_INTEGER
      end
      write_element(communicator, convert(Array{Int32}, arr),
            attributes, true)
   else
      attributes = (("JLTYPE", "Int64"), )
      arr2 = convert(Array{Float64}, arr)
      if !isempty(missings)
         attributes = (("JLTYPE", "Union{Missing,Int64}"), )
         arr2[missings] .= R_NA_REAL
      end
      write_element(communicator, arr2, attributes)
      #TODO possible inexactness?
   end
end

function write_element(communicator, arr::Array{Union{Missing, Int64}})
   write_element(communicator, replace(arr, missing => 0);
         missings = findall(ismissing, arr))
end

function write_element(communicator, arr::Array{T},
      ) where {T <: SEND_AS_RAW_TYPES}

   if length(arr) == 1
      attributes = (("JLTYPE", string(T)), ("JLDIM", collect(size(arr))))
   else
      attributes = (("JLTYPE", string(T)), )
   end
   write_uint8array_element(communicator, reinterpret(UInt8, arr), attributes)
end

function write_element(communicator, arr::Array{Complex{Float64}},
      attributes = ())

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

function write_element(communicator,
      arr::Array{Union{Missing,Complex{Float64}}})

   arr2 = replace(arr, missing => R_NA_COMPLEX)
   write_element(communicator, arr2)
end

function write_element(communicator, arr::Array{Union{Missing,C}},
      ) where {C <: SEND_AS_COMPLEX}

   arr2 = replace(arr, missing => R_NA_COMPLEX)
   attributes = (("JLTYPE", "Union{Missing," * string(C) * "}"), )
   write_element(communicator, arr2, attributes)
end

function write_element(communicator, arr::Array{C},
      ) where {C <: SEND_AS_COMPLEX}

   attributes = (("JLTYPE", string(C)), )
   write_element(communicator, convert.(Complex{Float64}, arr), attributes)
end

function write_element(communicator, arr::Array{Bool})
   write_bin(communicator, TYPE_ID_BOOL)
   write_dimensions(communicator, arr)
   for d in arr
      write_bin(communicator, d)
   end
end

function write_element(communicator, arr::Array{Union{Missing,Bool}})
   arr2 = replace(arr, true => Int32(1), false => Int32(0),
         missing => R_NA_INTEGER)
   attributes = (("R_LOGICAL", true), )
   write_element(communicator, arr2, attributes, true)
end

function write_array_element(communicator, arr::AbstractArray)

   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))

   arrsize = size(arr)

   arr2 = arr[:] # R lists support only 1 dimension
   if length(arrsize) > 1
      # we have a multidimensional array:
      # add the dimensions as attribute to be able to reconstruct it
      attributes["JLDIM"] = collect(arrsize)
   end

   ellist = ElementList(undefs_replaced(arr2), Vector{Symbol}(),
         Dict{Symbol, Any}(), attributes)
   write_element(communicator, ellist)
end

function write_element(communicator, arr::Array)
   if communicator.full_translation
      write_array_element(communicator, arr)
   else
      write_object_reference(communicator, arr, OBJECT_CLASS_ID_ARRAY)
   end
end

function write_element(communicator, arr::AbstractArray)
   if communicator.full_translation
      # translate as struct because you can never be sure of the implementation
      write_struct_element(communicator, arr)
   else
      # allow indexing as an array
      write_object_reference(communicator, arr, OBJECT_CLASS_ID_ARRAY)
   end
end

function write_element(communicator, dict::AbstractDict)

   if communicator.full_translation
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
      write_element(communicator, ellist)
   else
      write_object_reference(communicator, dict, OBJECT_CLASS_ID_STRUCT)
   end
end

function write_element(communicator, set::AbstractSet)
   if communicator.full_translation
      ellist = ElementList(Vector{Any}(collect(set)),
            Vector{Symbol}(), Dict{Symbol, Any}(),
            Dict{String, Any}("JLTYPE" => string(typeof(set))))
      write_element(communicator, ellist)
   else
      write_object_reference(communicator, set, OBJECT_CLASS_ID_STRUCT)
   end
end

function write_element(communicator, f::Function)

   callbackid = funtocallbackid(communicator, f)
   if callbackid == "" # it's not a callback function
      f_as_string = string(f)
      if endswith(f_as_string, "()") || startswith(f_as_string, '#')
         # Detected an anonymous function:
         # An anonymous function will have a string representation like
         # "getfield(Main, Symbol(\"##5#6\"))()" in Julia 1.0.
         # In Julia 1.3 it is something like "#3".
         ref = share_immutable_object!(communicator, f)
         write_bin(communicator, TYPE_ID_OBJECT_REFERENCE)
         write_bin(communicator, OBJECT_CLASS_ID_ANONYMOUS_FUNCTION)
         write_bin(communicator, ref)
         return

      else # Write an element for a named function.

         # In 1.0 f_as_string contains the full path, in 1.3 only the name.
         # We need the full path.
         # For operators, this does not work, because parsing fails then.
         if !('.' in f_as_string) && !Base.isoperator(Symbol(f_as_string))
            firstmodulepath = string(iterate(methods(f))[1].module)
            f_as_string = firstmodulepath * "." * f_as_string
         end

         write_bin(communicator, TYPE_ID_NAMED_FUNCTION)
         write_string(communicator, f_as_string)
         return
      end
   end

   write_bin(communicator, TYPE_ID_CALLBACK)
   write_string(communicator, callbackid)
end

function write_element(communicator, t::Tuple)
   if communicator.full_translation
      write_bin(communicator, TYPE_ID_LIST)
      attributes = Dict{String, Any}("JLTYPE" => string(typeof(t)))
      ellist = ElementList(
            Vector{Any}(collect(t)),
            Vector{Symbol}(), Dict{Symbol, Any}(),
            attributes)
      write_list(communicator, ellist)
   else
      write_object_reference(communicator, t, OBJECT_CLASS_ID_ARRAY)
   end
end

function write_element(communicator, ellist::ElementList)
   write_bin(communicator, TYPE_ID_LIST)
   write_list(communicator, ellist)
end

function write_element(communicator, obj::Symbol)
   write_bin(communicator, TYPE_ID_SYMBOL)
   write_string(communicator, string(obj))
end

function write_element(communicator, obj::Module)
   modulename = string(obj)
   if startswith(modulename, "Main.")
      modulename = String(modulename[6:end])
   end
   ellist = ElementList(
      Vector{Any}(),
      [:name],
      Dict{Symbol, Any}(:name => modulename),
      Dict{String, Any}("JLTYPE" => "Module"))
   write_element(communicator, ellist)
end

function write_element(communicator, obj::NamedTuple)
   if communicator.full_translation
      write_struct_element(communicator, obj)
   else
      write_object_reference(communicator, obj, OBJECT_CLASS_ID_STRUCT)
   end
end

function write_element(communicator, d::T) where {T2, T <: Type{T2}}
   write_expression(communicator, string(d))
end

function write_struct_element(communicator, obj::T,
      attributes::Dict{String, Any} = Dict{String, Any}()) where T

   names = fieldnames(T)
   if isempty(names) # type without members
      write_expression(communicator, string(T) * "()")
   else
      write_bin(communicator, TYPE_ID_LIST)
      fieldvalues = map(name -> getfield(obj, name), names)
      attributes["JLTYPE"] = string(T)
      ellist = ElementList(
            Vector(), collect(names),
            Dict{Symbol, Any}(zip(names, fieldvalues)),
            attributes)
      write_list(communicator, ellist)
   end
end

function write_mutable_struct_element(communicator, obj::T) where T

   ref = share_mutable_object!(communicator, obj)
   refexists = ref in communicator.objrefs
   push!(communicator.objrefs, ref)

   if refexists # recursion detected
      decrefcount!(communicator, ref) # not shared, actually
      write_struct_element(communicator, CircularReference(ref),
            Dict{String, Any}())
      return
   else
      # write object, giving reference to original object
      write_struct_element(communicator, obj,
            Dict{String, Any}("JLREF" => ref))
      return
   end
end

function write_element(communicator, obj::T) where T

   if isstructtype(T)
      if communicator.full_translation
         if !isimmutable(obj)
            write_mutable_struct_element(communicator, obj)
         else
            write_struct_element(communicator, obj)
         end
      else
         write_object_reference(communicator, obj, OBJECT_CLASS_ID_STRUCT)
      end
   else
      write_struct_element(communicator,
            Fail("Lost in translation: $(string(obj))"))
   end
end

function write_element(communicator, f::F) where {F <: SEND_AS_DOUBLE}
   attributes = (("JLTYPE", string(F)), )
   write_element(communicator, Float64(f), attributes)
end

function write_element(communicator, m::Missing)
   write_element(communicator, R_NA_REAL)
end

function write_element(communicator, m::Array{Missing})
   write_element(communicator, fill(R_NA_REAL, size(m)))
end

function write_element(communicator, f::Float64, attributes = ())
   write_bin(communicator, TYPE_ID_FLOAT64)
   write_int32(communicator, 0)
   write_bin(communicator, f)
   write_attributes(communicator, attributes)
end

function write_element(communicator, i::T) where {T <: SEND_AS_INT32}
   attributes = (("JLTYPE", string(T)), )
   write_element(communicator, Int32(i), attributes, true)
end

function write_element(communicator, i::Int32,
      attributes = (), isothertype::Bool = false)

   if !isothertype
      attributes = (("JLTYPE", "Int32"), )
   end

   write_bin(communicator, TYPE_ID_INT)
   write_int32(communicator, 0)
   write_bin(communicator, i)
   write_attributes(communicator, attributes)
end

function write_element(communicator, i::Int64)
   if typemin(Int32) <= i <= typemax(Int32)
      @static if Int == Int64
         attributes = ()
      else
         attributes = (("JLTYPE", "Int64"), )
      end
      write_element(communicator, Int32(i), attributes, true)
   else
      attributes = (("JLTYPE", "Int64"), )
      # TODO inexactness?
      write_element(communicator, Float64(i), attributes)
   end
end

function write_element(communicator, i::T) where {T <: SEND_AS_RAW_TYPES}
   attributes = (("JLTYPE", string(T)), )
   write_uint8array_element(communicator, reinterpret(UInt8, [i]), attributes)
end

function write_element(communicator, b::Bool)
   write_bin(communicator, TYPE_ID_BOOL)
   write_int32(communicator, 0)
   write_bin(communicator, b)
end

function write_element(communicator, str::String)
   write_bin(communicator, TYPE_ID_STRING)
   write_int32(communicator, 0)
   write_string(communicator, str)
   write_bin(communicator, 0x00) # no attributes
end

function write_element(communicator, c::Complex{Float64}, attributes = ())
   write_bin(communicator, TYPE_ID_COMPLEX)
   write_int32(communicator, 0)
   write_bin(communicator, Float64(real(c)))
   write_bin(communicator, Float64(imag(c)))
   write_attributes(communicator, attributes)
end

function write_element(communicator, c::C) where {C <: SEND_AS_COMPLEX}
   attributes = (("JLTYPE", string(C)), )
   write_element(communicator, convert(Complex{Float64}, c), attributes)
end

function write_element(communicator, u::UInt8)
   write_bin(communicator, TYPE_ID_RAW)
   write_int32(communicator, 0)
   write_bin(communicator, u)
   write_bin(communicator, 0x00) # no attributes
end

function write_element(communicator, n::Nothing)
   write_bin(communicator, TYPE_ID_NOTHING)
end

function write_element(communicator, p::EnforcedProxy{T}) where {T <: Array}
   write_object_reference(communicator, p.obj, OBJECT_CLASS_ID_SIMPLE_ARRAY)
end

function write_element(communicator, p::EnforcedProxy)
   write_object_reference(communicator, p.obj, OBJECT_CLASS_ID_NO_INFO)
end

function write_element(communicator, df::RDataFrame)
   if communicator.full_translation
      write_bin(communicator, TYPE_ID_LIST)
      write_list(communicator, getfield(df, :ellist))
   else
      write_object_reference(communicator, df, OBJECT_CLASS_ID_STRUCT)
   end
end

# This function and the struct serve only to communicate the CommunicatoR
# object of this client to R.
struct GetCommunicatoR
end
function write_element(communicator, _::GetCommunicatoR)
   ref = object_reference(communicator)
   write_object_reference(communicator, communicator, OBJECT_CLASS_ID_STRUCT)
end


function write_expression(communicator, str::AbstractString)
   write_bin(communicator, TYPE_ID_EXPRESSION)
   write_string(communicator, str)
end


function write_list(communicator, arr::AbstractArray)
   write_list(communicator, ElementList(vec(arr)))
end

function write_list(communicator, ellist::ElementList)
   write_int32(communicator, length(ellist.positionalelements))
   for el in ellist.positionalelements
      write_element(communicator, el)
   end

   write_int32(communicator, length(ellist.namedelements))
   for name in ellist.names # use the order of the names here
      write_string(communicator, string(name))
      write_element(communicator, ellist.namedelements[name])
   end

   write_attributes(communicator, ellist.attributes)
end


function write_object_reference(communicator, obj, object_class_id::UInt8)
   ref = shareobject!(communicator, obj)
   write_bin(communicator, TYPE_ID_OBJECT_REFERENCE)
   write_bin(communicator, object_class_id)
   write_bin(communicator, ref)
end

