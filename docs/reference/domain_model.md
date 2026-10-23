# SDTM Domain Model variables

An overview on the variables included in the SDTM specification.

## Usage

``` r
domain_model
```

## Format

A data frame with 1969 rows and 6 columns.

- DOMAIN:

  Domain name

- VARNAM:

  Variable name

- VARLABEL:

  Long-form variable name

- DEFTYPE:

  Variable type

- LENGTH:

  Length of variable

- CORE:

  Level of requiredness

## Details

The first few lines of the data set for reference:

    DOMAIN   VARNAM                            VARLABEL DEFTYPE LENGTH CORE
        TA  STUDYID                    Study Identifier    text     10  Req
        TA   DOMAIN                 Domain Abbreviation    text      2  Req
        TA    ARMCD                    Planned Arm Code    text     20  Req
        TA      ARM          Description of Planned Arm    text    100  Req
        TA  TAETORD Planned Order of Element within Arm integer      8  Req
        TA     ETCD                        Element Code    text      8  Req
        TA  ELEMENT              Description of Element    text    200 Perm
        TA TABRANCH                              Branch    text    200  Exp
        TA  TATRANS                     Transition Rule    text    200  Exp
        TA    EPOCH                               Epoch    text    100  Req
        ...
