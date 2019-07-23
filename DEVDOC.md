# Serialization Grammar

    message -> '0x01' call
            | '0x00' element
            | '0xff' fail
            | byebye
    call -> string list
    list -> int32 {element} int32 {named_element} attributes
    attributes -> nattributes {named_element}
    nattributes -> uint8
    named_element -> string element
    element -> '0x00'
            | '0x01' dimensions {double} attributes
            | '0x02' dimensions {complex} attributes
            | '0x03' dimensions {raw} attributes
            | '0x04' dimensions {integer} attributes
            | '0x05' dimensions {boolean}
            | '0x06' dimensions {string} attributes
            | '0x07' list
            | '0xcb' callback
            | '0xee' expression
    callback -> int32
    string -> int32 utf8string
    dimensions -> ndimensions {int32}
    ndimensions -> int32
    fail -> string
    byebye -> '0xbb'

### Meaning of the int32 numbers
* A string is preceded by the number of bytes to UTF-8-encode the string.
* The sequence of unnamed/positional elements in a list is preceded by the number of (named) elements that follow.

### Types
Lists that have the attribute `JLTYPE` will be coerced to the corresponding Julia type.

## Conversion table for bitstypes:


| Julia type | R type|
| -----------|-------|
| `UInt8` | `raw` |
| `Int8`, `Int16`, `UInt16` | `integer` with original type as attribute |
| `Int32`/`Int64 `|  `integer` or  `double` TODO: concept that works for 32- and 64-bit systems |
| `UInt32`, `UInt64`, `Int128`, `UInt128` | `raw` with original type as attribute |
| `Float16`, `Float32` | `double` with original type as attribute |
| `Float64` | `double` |

