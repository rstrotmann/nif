# Calculate time fields based on DTC

This function generates the following time fields:

- 'TIME' is the time in hours relative to the subject's first record, be
  it an administration or observation event.

- 'TAFD' is the time in hours relative to the subject's first
  administration of the respective parent. Note that if a subject has
  received multiple drugs (parents), the 'TAFD' field refers to the
  respective first administration.

- 'TAD' is the time in hours relative to the most recent administration
  of the parent compound.

## Usage

``` r
make_time(obj)
```

## Arguments

- obj:

  A nif object.

## Value

A nif object with TIME, TAFD, and TAD fields added.
