module RConnector

using Sockets
using Logging

const TYPE_ID_NOTHING = 0x00
const TYPE_ID_FLOAT64 = 0x01
const TYPE_ID_COMPLEX = 0x02
const TYPE_ID_RAW = 0x03
const TYPE_ID_INT = 0x04
const TYPE_ID_BOOL = 0x05
const TYPE_ID_STRING = 0x06
const TYPE_ID_LIST = 0x07
const TYPE_ID_EXPRESSION = 0xee
const TYPE_ID_CALLBACK = 0xcb
const TYPE_ID_ANONYMOUS_FUNCTION = 0xaf
const TYPE_ID_NAMED_FUNCTION = 0xfc

const CALL_INDICATOR = 0x01
const RESULT_INDICATOR = 0x00
const STDOUT_INDICATOR = 0x50
const STDERR_INDICATOR = 0x5e
const FAIL_INDICATOR = 0xff
const BYEBYE = 0xbb

const NO_ATTRIBUTES = Base.ImmutableDict{String, Any}()

const SEND_AS_RAW_TYPES = Union{UInt64, Int128, UInt128, Ptr}
const SEND_AS_INT32 = Union{Int8, Int16, UInt16, Char}
const SEND_AS_DOUBLE = Union{Float16, Float32, UInt32}
const SEND_AS_COMPLEX = Complex{T} where T <: Union{
      Int8, Int16, Int32, Int64, Float16, Float32}

include("communicating.jl")
include("handling_undefined.jl")
include("reading.jl")
include("evaluating.jl")
include("writing.jl")
include("exporting.jl")

end