# Index dosing regimen episodes

Identifies treatment regimen episodes from administration records.
Administrations within `admin_window` hours of each other (per subject)
are treated as co-administrations. A new `REG_ID` starts whenever the
set of co-administered analytes changes.

## Usage

``` r
index_regimen(obj, admin_window = 12, silent = NULL)
```

## Arguments

- obj:

  A nif object.

- admin_window:

  Time window in hours within which administrations are considered
  co-administrations. Defaults to `12`.

- silent:

  Suppress messages.

## Value

The input with treatment regimen episode ID column `REG_ID`, regimen
`REG` and dose level `DL` added.
