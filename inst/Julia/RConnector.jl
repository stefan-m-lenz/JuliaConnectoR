module RConnector

using Sockets
using Logging
import REPL
import Main.Tables: istable, columnaccess,
                    columns, AbstractColumns,
                    getcolumn, columnnames

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
const TYPE_ID_NAMED_FUNCTION = 0xfc
const TYPE_ID_OBJECT_REFERENCE = 0xce
const TYPE_ID_SYMBOL = 0x5b

const OBJECT_CLASS_ID_ARRAY = 0xaa
const OBJECT_CLASS_ID_SIMPLE_ARRAY = 0x5a
const OBJECT_CLASS_ID_ANONYMOUS_FUNCTION = 0xaf
const OBJECT_CLASS_ID_STRUCT = 0x5c
const OBJECT_CLASS_ID_NO_INFO = 0x00

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

const R_NA_INTEGER = Int32(-2147483648)
const R_NA_REAL = ntoh(reinterpret(Float64,
      UInt8[0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x07, 0xa2])[1])
const R_NA_COMPLEX = Complex{Float64}(R_NA_REAL, R_NA_REAL)

include("communicating.jl")
include("sharing.jl")
include("handling_undefined.jl")
include("reading.jl")
include("handling_dataframes.jl")
include("evaluating.jl")
include("writing.jl")
include("exporting.jl")
include("accessing_mutating.jl")

end