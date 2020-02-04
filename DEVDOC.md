# Serialization Grammar

    message -> '0x01' call
            | '0x00' element
            | '0x50' output
            | '0x5e' output
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
            | '0x5e' struct_reference
            | '0xaf' anonymous_function_reference
            | '0xcb' callback
            | '0xfc' named_function
            | '0xee' expression
    struct_reference -> 8 * byte
    anonymous_function_reference -> 8 * byte
    callback -> int32
    named_function -> string
    string -> int32 utf8string
    dimensions -> ndimensions {int32}
    ndimensions -> int32
    output -> int32 {byte}
    fail -> string
    byebye -> '0xbb'

### Meaning of the int32 numbers
* A string is preceded by the number of bytes to UTF-8-encode the string.
* The sequence of unnamed/positional elements in a list is preceded by
  the number of (named) elements that follow.
* Standard output (after '0x50') or standard error output (after '0x5e')
  is preceded by the number of bytes that are sent.
