mutable struct SharedObject
   obj::Any
   refcount::UInt64
end

function SharedObject(obj)
   SharedObject(obj, UInt32(1))
end


struct ObjectReference
   ref::UInt64
end

# global variables
const sharedheap = Dict{UInt64, SharedObject}()
const full_translation = Ref(false)

const finalized_callbacks = Vector{String}()
const registered_callbacks = Dict{Function, String}()


mutable struct CallbackFinalizer
   id::String
   f::Function
end

function finalize_callback(cb::CallbackFinalizer)
   delete!(registered_callbacks, cb.f)
   push!(finalized_callbacks, cb.id)
end

function CallbackFinalizer(id::String)
   cb = CallbackFinalizer(id, identity) # any function does the job
   finalizer(finalize_callback, cb)
   cb
end

const useless_counter = Ref(0)
function stayalivewithme(cb::CallbackFinalizer)
   # accessing a global variable will prevent this function from
   # being optimized out, even by super clever optimizers
   useless_counter.x += 1
end


function full_translation!(mode::Bool)
   full_translation.x = mode
end

function sharedheapref!(obj)
   ref = UInt64(pointer_from_objref(obj))
   sharedheap[ref] = SharedObject(obj)
   ref
end


mutable struct ImmutableObjectReference
   obj::Any
end


function share_mutable_object!(obj)
   ref = UInt64(pointer_from_objref(obj))
   if haskey(sharedheap, ref)
      sharedheap[ref].refcount += 1
   else
      sharedheap[ref] = SharedObject(obj)
   end
   ref
end


function share_immutable_object!(obj)
   obj2 = ImmutableObjectReference(obj)
   ref = UInt64(pointer_from_objref(obj2))
   # the object is newly created and cannot exists already
   sharedheap[ref] = SharedObject(obj2)
   ref
end


function shareobject!(obj)
   if isimmutable(obj)
      return share_immutable_object!(obj)
   else
      return share_mutable_object!(obj)
   end
end


function parseheapref(ref::Vector{UInt8})
   UInt64(reinterpret(UInt64, ref)[1])
end


function decrefcount(ref::UInt64)
   newrefcount = sharedheap[ref].refcount - 1
   if newrefcount == 0
      delete!(sharedheap, ref)
   else
      sharedheap[ref].refcount = newrefcount
   end
end

function decrefcounts(refs::Vector{UInt8})
   for ref in reinterpret(UInt64, refs)
      decrefcount(ref)
   end

   # return finalized callbacks
   ret = deepcopy(finalized_callbacks)
   empty!(finalized_callbacks)
   ret
end


function sharedfinalizer(newobj, oldobjref::UInt64)
   # If a new Julia heap object is recreated via backtranslation from R,
   # it must reference the original object such that this one lives on.
   # It must be prevented that the old object is garbage collected because
   # this might finalize resources that are needed for the copy to work.
   if haskey(sharedheap, oldobjref)
      sharedheap[oldobjref].refcount += 1
      finalizer(obj -> decrefcount(oldobjref), newobj)
   else
      @warn "Please be sure that the revived objects " *
            "do not contain external references."
   end
end


mutable struct EnforcedProxy{T}
   obj::T
end
