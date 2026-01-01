# Read SDTM data

This function reads SDTM-formatted data as SAS or XPT files from a
folder location.

## Usage

``` r
read_sdtm(data_path, domain = NULL, format = "sas", delim = ",", ...)
```

## Arguments

- data_path:

  The file system path to the source folder as character.

- domain:

  The domain name(s) as character, defaults to all domains found in the
  folder.

- format:

  The format of the source files as character, either 'sas' (default),
  'xpt', or 'csv'.

- delim:

  Deliminator.

- ...:

  Further parameters, refer to readr::read_csv

## Value

A `sdtm` object.
