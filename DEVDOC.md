# Serialization Grammar

## Testing categorical factor conversion

`juliaCategorical()` uses the optional Julia package CategoricalArrays.jl.
The server includes `inst/Julia/categorical.jl` at startup, but imports the
package only on the first valid conversion. Conversion never installs packages.
The helper uses public CategoricalArrays APIs and `Base.invokelatest` to support
the first call immediately after the lazy import.

Install the R package into a development library, then run the focused tests:

```r
Sys.setenv(NOT_CRAN = "true")
testthat::test_local(filter = "juliaCategorical")
```

These tests install CategoricalArrays.jl in the active Julia environment if it
is absent, following the existing optional Julia package test convention. For
offline testing, install it beforehand with
`juliaEval('import Pkg; Pkg.add("CategoricalArrays")')`. Missing dependencies,
installation failures and Julia runtime errors fail the tests outside CRAN;
only `skip_on_cran()` suppresses these tests. Both CI workflows explicitly set
`NOT_CRAN=true` and install CategoricalArrays before running checks, because
R CMD check runs documentation examples before tests. The focused tests also
execute the generated Rd examples.

Regenerate `man/juliaCategorical.Rd` with `roxygen2::roxygenise()`. The repository
maintains `NAMESPACE` manually, including the export for `juliaCategorical`.

## Protocol

    message = '0x01' call
            | '0x00' element
            | '0x50' output
            | '0x5e' output
            | '0xff' fail
            | byebye
    call = string list
    list = int32 {element} int32 {named_element} attributes
    attributes = nattributes {named_element}
    nattributes = uint8
    named_element = string element
    element = '0x00'
            | '0x01' dimensions {double} attributes
            | '0x02' dimensions {complex} attributes
            | '0x03' dimensions {raw} attributes
            | '0x04' dimensions {integer} attributes
            | '0x05' dimensions {boolean}
            | '0x06' dimensions {string} attributes
            | '0x07' list
            | '0x5b' string (* name of symbol *)
            | '0x5e' object_class_id object_reference
            | '0xcb' callback
            | '0xee' expression
            | '0xfc' named_function
    object_class_id = '0x5c' (* class JuliaStructProxy *)
                    | '0x5a' (* class JuliaSimpleArrayProxy *)
                    | '0xaf' (* anonymous function reference *)
                    | '0xaa' (* class JuliaArrayProxy *)
                    | '0x00' (* no information *)
    object_reference = 8 * byte
    anonymous_function_reference = 8 * byte
    callback = string
    named_function = string
    string = int32 utf8string
    dimensions = ndimensions {int32}
    ndimensions = int32
    output = int32 {byte}
    fail = string
    byebye = '0xbb'

### Meaning of the int32 numbers
* A string is preceded by the number of bytes to UTF-8-encode the string.
* The sequence of unnamed/positional elements in a list is preceded by
  the number of (named) elements that follow.
* Standard output (after '0x50') or standard error output (after '0x5e')
  is preceded by the number of bytes that are sent.
