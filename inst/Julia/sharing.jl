struct ObjectReference
   ref::UInt64
end


mutable struct CallbackFinalizer
   id::String
   f::Function
   communicator::CommunicatoR
end

function finalize_callback(cb::CallbackFinalizer)
   lock(cb.communicator.callbacks_lock)
   try
      delete!(cb.communicator.registered_callbacks, cb.f)
      push!(cb.communicator.finalized_callbacks, cb.id)
   finally
      unlock(cb.communicator.callbacks_lock)
   end
end


function register_callback(communicator::CommunicatoR, f::Function, callbackid::String)
   lock(communicator.callbacks_lock)
   try
      communicator.registered_callbacks[f] = callbackid
   finally
      unlock(communicator.callbacks_lock)
   end
end


function funtocallbackid(communicator::CommunicatoR, f::Function)
   lock(communicator.callbacks_lock)
   callbackid = get(communicator.registered_callbacks, f, "")
   unlock(communicator.callbacks_lock)
   callbackid
end


function CallbackFinalizer(id::String, communicator::CommunicatoR)
   f = identity # any function does the job
   cb = CallbackFinalizer(id, f, communicator)
   finalizer(finalize_callback, cb)
   cb
end

const useless_counter = Ref(0)
function stayalivewithme(cb::CallbackFinalizer)
   # accessing a global variable will prevent this function from
   # being optimized out, even by super clever optimizers
   useless_counter.x += 1
end


function full_translation!(communicator::CommunicatoR, mode::Bool)
   communicator.full_translation = mode
end


function sharedheapget(communicator::CommunicatoR, ref::UInt64)
   lock(communicator.sharedheap_lock)
   ret = communicator.sharedheap[ref].obj
   unlock(communicator.sharedheap_lock)
   ret
end

function sharedheapget(communicator::CommunicatoR, objref::ObjectReference)
   sharedheapget(communicator, objref.ref)
end


mutable struct ImmutableObjectReference
   obj::Any
end


function object_reference(obj)
   UInt64(pointer_from_objref(obj))
end


function share_mutable_object!(communicator, obj)
   ref = object_reference(obj)
   lock(communicator.sharedheap_lock)
   try
      if haskey(communicator.sharedheap, ref)
         communicator.sharedheap[ref].refcount += 1
      else
         communicator.sharedheap[ref] = SharedObject(obj)
      end
   finally
      unlock(communicator.sharedheap_lock)
   end
   ref
end


function share_immutable_object!(communicator, obj)
   obj2 = ImmutableObjectReference(obj)
   ref = object_reference(obj2)
   # the object is newly created and cannot exists already
   lock(communicator.sharedheap_lock)
   try
      communicator.sharedheap[ref] = SharedObject(obj2)
   finally
      unlock(communicator.sharedheap_lock)
   end
   ref
end


function shareobject!(communicator, obj)
   if isimmutable(obj)
      return share_immutable_object!(communicator, obj)
   else
      return share_mutable_object!(communicator, obj)
   end
end


function parseheapref(ref::Vector{UInt8})
   UInt64(reinterpret(UInt64, ref)[1])
end


function decrefcount!(communicator::CommunicatoR, ref::UInt64)
   lock(communicator.sharedheap_lock)
   try
      newrefcount = communicator.sharedheap[ref].refcount - 1
      if newrefcount == 0
         delete!(communicator.sharedheap, ref)
      else
         communicator.sharedheap[ref].refcount = newrefcount
      end
   finally
      unlock(communicator.sharedheap_lock)
   end
end

function decrefcounts(communicator::CommunicatoR, refs::Vector{UInt8})
   for ref in reinterpret(UInt64, refs)
      decrefcount!(communicator, ref)
   end

   # return finalized callbacks and clear list
   lock(communicator.callbacks_lock)
   ret = deepcopy(communicator.finalized_callbacks)
   empty!(communicator.finalized_callbacks)
   unlock(communicator.callbacks_lock)

   ret
end


function sharedfinalizer(communicator::CommunicatoR, newobj, oldobjref::UInt64)
   # If a new Julia heap object is recreated via backtranslation from R,
   # it must reference the original object such that this one lives on.
   # It must be prevented that the old object is garbage collected because
   # this might finalize resources that are needed for the copy to work.
   lock(communicator.sharedheap_lock)
   try
      if haskey(communicator.sharedheap, oldobjref)
         communicator.sharedheap[oldobjref].refcount += 1
         finalizer(obj -> decrefcount!(communicator, oldobjref), newobj)
      else
         @warn "Please be sure that the revived objects " *
               "do not contain external references."
      end
   finally
      unlock(communicator.sharedheap_lock)
   end
end


mutable struct EnforcedProxy{T}
   obj::T
end
