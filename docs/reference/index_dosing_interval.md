# Index dosing intervals

This function adds a column 'DI' that indicates the dosing interval per
parent. All baseline observations before the first dosing interval get
assigned to the first dosing interval.

## Usage

``` r
index_dosing_interval(obj, parent = NULL)
```

## Arguments

- obj:

  The NIF object.

- parent:

  The treatments to filter for. Defaults to all parents, if NULL.

## Value

A NIF object with the DI column added.
