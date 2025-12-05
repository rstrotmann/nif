# Write NONMEM input formatted nif object to file

**\[deprecated\]**

## Usage

``` r
write_nif(obj, filename = NULL, fields = NULL, sep = NULL)
```

## Arguments

- obj:

  The NIF object.

- filename:

  The filename as string. If not filename is specified, the file is
  printed only.

- fields:

  The fields to export. If NULL (default), all fields will be exported.

- sep:

  The separating character, e.g. ',' or ';'. If NULL (default), the
  output has a space-separated, fixed-width format.

## Value

Nothing.
