# Translation of R dates, date-times and factors.
#
# R sends these as numbers or integer codes together with their attributes
# ("class", "levels"), which are used here to create the corresponding Julia
# objects. In the other direction, the attributes are added again so that R
# gets back a Date, POSIXct or factor.

const R_EPOCH_DATE = Dates.Date(1970, 1, 1)

const R_DATE_ATTRIBUTES = (("class", "Date"), )
const R_POSIXCT_ATTRIBUTES = (("class", ["POSIXct", "POSIXt"]), ("tzone", "UTC"))

const CATEGORICAL_ARRAYS_PKGID = Base.PkgId(
      Base.UUID("324d7699-5711-5eae-9e2f-1d82baa6b597"), "CategoricalArrays")


has_class(rclass::AbstractString, cls::String) = rclass == cls
has_class(rclass::AbstractArray, cls::String) = cls in rclass
has_class(rclass, cls::String) = false


"""
    convert_r_class(x, attributes, dimensions)
Translates a vector of R dates (class "Date"), date-times ("POSIXct")
or factors, given the attributes that R sent with it.
Returns `nothing` if `x` is not of one of these classes.
"""
function convert_r_class(x, attributes, dimensions)
   if haskey(attributes, "JLTYPE")
      return nothing
   end

   rclass = get(attributes, "class", nothing)
   if has_class(rclass, "factor")
      ret = r_factor_to_julia(x, get(attributes, "levels", String[]),
            has_class(rclass, "ordered"))
   elseif has_class(rclass, "Date")
      ret = map(r_day_to_date, x)
   elseif has_class(rclass, "POSIXct")
      ret = map(r_seconds_to_datetime, x)
   else
      return nothing
   end

   # (via invokelatest: a CategoricalArray's methods may be newer than this call)
   if ret === nothing
      return nothing
   elseif dimensions == 0
      return Base.invokelatest(getindex, ret, 1)
   elseif length(dimensions) == 1
      return ret
   else
      return Base.invokelatest(reshape, ret, dimensions...)
   end
end


function r_day_to_date(day)
   if ismissing(day) || !isfinite(day)
      return missing
   end
   # (R shows a date with a fractional day as the day it is in)
   R_EPOCH_DATE + Dates.Day(floor(Int, day))
end


function r_seconds_to_datetime(seconds)
   if ismissing(seconds) || !isfinite(seconds)
      return missing
   end
   # (rounded to milliseconds, the resolution of DateTime)
   Dates.unix2datetime(seconds)
end


function r_factor_to_julia(codes, levels, ordered::Bool)
   if levels isa AbstractString
      levels = [levels]
   end
   if any(ismissing, levels)
      # (a factor with NA as a level: leave it as integer codes)
      return nothing
   end
   levels = Vector{String}(levels)

   strings = [ismissing(code) ? missing : levels[code] for code in codes]
   if !any(ismissing, strings)
      strings = Vector{String}(strings)
   end

   categoricalarrays = categorical_arrays()
   if categoricalarrays === nothing
      return strings
   end
   Base.invokelatest(categoricalarrays.categorical, strings;
         levels = levels, ordered = ordered)
end


"""
    categorical_arrays()
Returns the module of the package CategoricalArrays if it is loaded or can be
loaded, otherwise `nothing`.
"""
function categorical_arrays()
   if Base.root_module_exists(CATEGORICAL_ARRAYS_PKGID)
      return Base.root_module(CATEGORICAL_ARRAYS_PKGID)
   end
   if Base.locate_package(CATEGORICAL_ARRAYS_PKGID) === nothing
      return nothing
   end
   try
      return Base.require(CATEGORICAL_ARRAYS_PKGID)
   catch ex
      @warn "CategoricalArrays could not be loaded, factors are translated to strings" exception = ex
      return nothing
   end
end


# Whether `x` is a CategoricalArray or CategoricalValue of the package
# CategoricalArrays (which is no dependency, so the types aren't known here)
function is_categorical(x)
   T = typeof(x)
   nameof(T) in (:CategoricalArray, :CategoricalValue) &&
         nameof(parentmodule(T)) == :CategoricalArrays
end


# Returns the integer codes, the levels as strings, and whether the levels
# are ordered, of a CategoricalArray or a CategoricalValue.
# (To be called via `invokelatest`, as the package may have been loaded after
# the call to RConnector started.)
function categorical_parts(x)
   categoricalarrays = parentmodule(typeof(x))
   values = x isa AbstractArray ? x : [x]
   codes = Int32[ismissing(v) ? R_NA_INTEGER : Int32(categoricalarrays.levelcode(v))
         for v in values]
   levels = String[string(level) for level in categoricalarrays.levels(x)]
   codes, levels, categoricalarrays.isordered(x)
end


function write_categorical_element(communicator, x)
   codes, levels, ordered = Base.invokelatest(categorical_parts, x)
   # (a 1-element array would come with the attribute JLDIM in R)
   attributes = (("levels", length(levels) == 1 ? levels[1] : levels),
         ("class", ordered ? ["ordered", "factor"] : "factor"))
   if x isa AbstractArray
      write_element(communicator, codes, attributes, true)
   else
      write_element(communicator, codes[1], attributes, true)
   end
end


r_day(d::Dates.Date) = Float64(Dates.value(d - R_EPOCH_DATE))
r_day(::Missing) = R_NA_REAL

r_seconds(dt::Dates.DateTime) = Dates.datetime2unix(dt)
r_seconds(::Missing) = R_NA_REAL


function write_element(communicator, d::Dates.Date)
   write_element(communicator, r_day(d), R_DATE_ATTRIBUTES)
end

function write_element(communicator,
      arr::Union{Array{Dates.Date}, Array{Union{Missing, Dates.Date}}})
   write_element(communicator, map(r_day, arr), R_DATE_ATTRIBUTES)
end

function write_element(communicator, dt::Dates.DateTime)
   write_element(communicator, r_seconds(dt), R_POSIXCT_ATTRIBUTES)
end

function write_element(communicator,
      arr::Union{Array{Dates.DateTime}, Array{Union{Missing, Dates.DateTime}}})
   write_element(communicator, map(r_seconds, arr), R_POSIXCT_ATTRIBUTES)
end
