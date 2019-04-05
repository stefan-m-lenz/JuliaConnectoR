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


function write_element(outputstream, arr::AbstractArray{String},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_STRING)
   write_dimensions(outputstream, arr)
   for str in arr
      write_string(outputstream, str)
   end
end

function write_element(outputstream, arr::AbstractArray{<:Number},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_FLOAT64)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, Float64(d))
   end
end

function write_element(outputstream, arr::AbstractArray{<:Complex},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_COMPLEX)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, Float64(real(d)))
      write(outputstream, Float64(imag(d)))
   end
end

function write_element(outputstream, arr::AbstractArray{Bool},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_BOOL)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function write_element(outputstream, arr::AbstractArray{UInt8},
      callbacks::Vector{Function})

   write(outputstream, TYPE_ID_RAW)
   write_dimensions(outputstream, arr)
   for d in arr
      write(outputstream, d)
   end
end

function write_element(outputstream, arr::AbstractArray,
      callbacks::Vector{Function})

   attributes = Dict{String, Any}("JLTYPE" => string(typeof(arr)))
   ellist = ElementList(Vector{Any}(arr), Vector{Symbol}(),
         Dict{Symbol, Any}(), attributes)
   write_element(outputstream, ellist, callbacks)
end

function write_element(outputstream, f::Function, callbacks::Vector{Function})
   callbackid = findfirst(isequal(f), callbacks)

   if callbackid == nothing
      callbackid = 0
   end

   print(callbackid)

   write(outputstream, TYPE_ID_CALLBACK)
   write_int32(outputstream, callbackid)
end

function write_element(outputstream, t::T, callbacks::Vector{Function}
      ) where T <: Union{Tuple, Pair}

   write(outputstream, TYPE_ID_LIST)
   attributes = Dict{String, Any}("JLTYPE" => string(T))
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
      Dict{Symbol, Any}(:name => Symbol(modulename)),
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

function write_element(outputstream, i::Number, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_FLOAT64)
   write_int32(outputstream, 0)
   write(outputstream, Float64(i))
end

function write_element(outputstream, i::Int32, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_INT)
   write_int32(outputstream, 0)
   write_int32(outputstream, i)
end

function write_element(outputstream, i::Int64, callbacks::Vector{Function})
   if typemin(Int32) <= i <= typemax(Int32)
      write(outputstream, TYPE_ID_INT)
      write_int32(outputstream, 0)
      write(outputstream, convert(Int32, i))
   else
      write_element(outputstream, Float64(i), callbacks)
   end
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
end

function write_element(outputstream, c::Complex, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_COMPLEX)
   write_int32(outputstream, 0)
   write(outputstream, Float64(real(c)))
   write(outputstream, Float64(imag(c)))
end

function write_element(outputstream, u::UInt8, callbacks::Vector{Function})
   write(outputstream, TYPE_ID_RAW)
   write_int32(outputstream, 0)
   write(outputstream, u)
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

   write_int32(outputstream, length(ellist.attributes))
   for (key, value) in ellist.attributes
      write_string(outputstream, key)
      write_element(outputstream, value, callbacks)
   end
end