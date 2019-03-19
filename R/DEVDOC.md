# Serialization Grammar

    message -> '0x01' call
        | '0x00' element
        | '0xff' fail
        | byebye
    call -> string list
    list    -> int32 {element} int32 {named_element} attributes
    attributes -> int32 {named_element}
    named_element -> string element
    element -> '0x00'
            | '0x01' dimensions {double}
            | '0x02' dimensions {complex}
            | '0x03' dimensions {raw}
            | '0x04' dimensions {integer}
            | '0x05' dimensions {boolean}
            | '0x06' dimensions {string}
            | '0x07' list
            | '0xcb' callback
            | '0xee' expression
    callback -> int32
    string -> int32 utf8string
    dimensions -> ndimensions {int32}
    ndimensions -> int32
    fail -> string
    byebye -> '0xbb'

# Meaning of the int32 numbers
* A string is preceded by the number of bytes to UTF-8-encode the string.
* The sequence of unnamed/positional elements in a list or in attributes of a list
  is preceded by the number of (named) elements that follow.

# Types
Lists that have an attributes `JL_TYPE` will be coerced to the corresponding Julia type.
This works only if there is a constructor that accepts all the arguments in the list as positional arguments
such as the default constructor.