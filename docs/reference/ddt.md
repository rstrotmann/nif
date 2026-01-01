# Data definition table for nif object

Generate a data definition table from a nif object. Known nif columns
are annotated automatically, for unknown fields, NA values are produced.
Further specifications can be added manually using
[`add_dd()`](add_dd.md).

## Usage

``` r
ddt(obj, silent = NULL)
```

## Arguments

- obj:

  A nif object.

- silent:

  Suppress messages, defaults to nif_option setting if NULL.

## Value

A data table.

## Examples

``` r
ddt(examplinib_sad_nif)
#>          name                     definition      type
#> 1         REF      Consecutive record number   integer
#> 2     STUDYID                          Study character
#> 3          ID             Subject identifier   numeric
#> 4     USUBJID              USUBJID in source character
#> 5         AGE                            Age   numeric
#> 6         SEX                            Sex      0, 1
#> 7        RACE                           Race character
#> 8      HEIGHT                    Body height   numeric
#> 9      WEIGHT                    Body weight   numeric
#> 10        BMI                Body Mass Index   numeric
#> 11        DTC                       Datetime  datetime
#> 12       TIME  Time since start of treatment   numeric
#> 13      NTIME                   Nominal time   numeric
#> 14       TAFD          Time after first dose   numeric
#> 15        TAD           Time after last dose   numeric
#> 16       EVID                       Event ID      0, 1
#> 17        AMT                         Amount   numeric
#> 18    ANALYTE                        Analyte character
#> 19        CMT                    Compartment      1, 2
#> 20     PARENT                 Parent analyte character
#> 21 METABOLITE                     Metabolite   logical
#> 22       DOSE                           Dose   numeric
#> 23         DV             Dependent variable   numeric
#> 24        MDV                     Missing DV   numeric
#> 25 IMPUTATION                     Imputation character
#> 26   ACTARMCD                Actual arm code character
#> 27      TRTDY                  Treatment day   numeric
#> 28   BL_CREAT            Baseline creatinine   numeric
#> 29    BL_CRCL Baseline creatinnine clearance   numeric
#>                                                description     unit
#> 1                               Unique number for each row     <NA>
#> 2                              Study identification number     <NA>
#> 3                     Unique subject ID across all studies     <NA>
#> 4                               Unique subject ID in study     <NA>
#> 5                             Age of subjec at study start    years
#> 6                                     0 = Male, 1 = Female     <NA>
#> 7                                            Race category     <NA>
#> 8                                     Baseline body height       cm
#> 9                                     Baseline body weight       kg
#> 10                                            Baseline BMI   kg/m^2
#> 11                                  Date and time of event Datetime
#> 12     Individual time since individual start of treatment    hours
#> 13                                   Nominal time of event    hours
#> 14                 Actual time after individual first dose    hours
#> 15                         Time after individual last dose    hours
#> 16                     0 = Observation, 1 = Administration     <NA>
#> 17                                       Dose administered       mg
#> 18                  Assigned name to observation substrate     <NA>
#> 19       1 = RS2023 administration, 2 = RS2023 observation     <NA>
#> 20                    Reference drug name for observations     <NA>
#> 21                                         Metabolite flag     <NA>
#> 22                                   Last administerd dose       mg
#> 23              Dependent variable, NA for administrations     <NA>
#> 24                      0 = non-missing DV, 1 = Missing DV     <NA>
#> 25                       time imputation applied to record     <NA>
#> 26                              ACTARMCD as in SDTM source     <NA>
#> 27                               Day after treatment start     <NA>
#> 28                      Serum creatinine value at baseline   umol/l
#> 29 Creatinine clearance based on baseline serum creatinine   ml/min
#>                                 source
#> 1                             Produced
#> 2                          DM: STUDYID
#> 3                             Produced
#> 4                          DM: USUBJID
#> 5  DM: AGE or derived from DM: BRTHDTC
#> 6                 Derived from DM: SEX
#> 7                             DM: RACE
#> 8                VS: VSTESTCD = HEIGHT
#> 9                VS: VSTESTCD = WEIGHT
#> 10                   HEIGHT and WEIGHT
#> 11                         SDTM domain
#> 12                                 DTC
#> 13                         SDTM domain
#> 14                                TIME
#> 15                                TIME
#> 16                            Produced
#> 17                          EX: EXDOSE
#> 18         EXTRT, xxTESTCD or assigned
#> 19                Produced or assigned
#> 20  Automatically or manually assigned
#> 21  Automatically or manually assigned
#> 22                          EX: EXDOSE
#> 23                         SDTM domain
#> 24                            Produced
#> 25                            Produced
#> 26                        DM: ACTARMCD
#> 27                         SDTM domain
#> 28                                  LB
#> 29                              LB, DM
```
