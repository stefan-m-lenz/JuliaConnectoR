# Serialization Grammar

    message -> '0x01' functioncall
        | '0x00' element
        | byebye
    byebye -> '0xbb'
    functioncall -> string list
    list    -> int32 {element} int32 {named_element} attributes
    attributes -> int32 {named_element}
    named_element -> string element
    element -> '0x00'
            | '0x01' dimensions {double}
            | '0x02' dimensions {integer}
            | '0x03' dimensions {boolean}
            | '0x04' dimensions {string}
            | '0x05' list
            | '0xcb' callback
            | '0xee' expression
            | '0xff' fail
    callback -> int32
    fail -> string
    string -> int32 utf8string
    dimensions -> ndimensions {int32}
    ndimensions -> int32

# Meaning of the int32 numbers
* A string is preceded by the number of bytes to UTF-8-encode the string.
* The sequence of unnamed/positional elements in a list or in attributes of a list
  is preceded by the number of (named) elements that follow.

# Types
Lists that have an attributes `JL_TYPE` will be coerced to the corresponding Julia type.
This works only if there is a constructor that accepts all the arguments in the list as positional arguments
such as the default constructor.