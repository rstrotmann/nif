# Attach a time mapping to an sdtm object

The nominal time of observations (e.g., `PCTPT`) is not required to
follow a strict format and is in most cases provided as a composite
string. This function can be used to explicitly define the nominal
observation times (in hours) for the values of, e.g., `PCTPT`.

## Usage

``` r
add_time_mapping(obj, ...)
```

## Arguments

- obj:

  The SDTM object.

- ...:

  Mappings in the form '""=' with multiple mappings separated by commas.
  corresponds to the value in the PCTPT fields, and NTIME corresponds to
  the nominal time in hours.

## Value

The SDTM object
