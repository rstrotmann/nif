# Example: Multiple-dose study

``` r
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(nif)

theme_set(theme_bw())
```

## OVERVIEW

This vignette walks through the creation of a NONMEM Input Format (NIF)
data set for a multiple dose study (study ‘RS2023-0022’), followed by
some basic exploratory analyses.

The fictional SDTM data for this study are included as part of the NIF
package (`examplinib_poc`). Custom SDTM data can be loaded using
[`read_sdtm()`](../reference/read_sdtm.md).

### Study design

Study ‘RS2023-0022’ is a single-arm study in which subjects received
multiple doses of ‘examplinib’ (substance code ‘RS2023’). The treatment
duration is different across subjects. PK sampling was on Days 1 and 8
of the treatment period. The PK sampling schedule was rich in the
initial subset of subjects and sparse in the others.

### Study SDTM data

The package provides the ‘sdtm’ class as a wrapper to keep all SDTM
domain tables of a clinical study together in one object. The
`examplinib_poc` sdtm object contains the DM, EX, PC and VS domains.
Let’s summarize the `examplinib_poc` sdtm object for a high-level
overview:

``` r
summary(examplinib_poc)
#> -------- SDTM data set summary -------- 
#> Study 2023000022 
#> 
#> Data disposition
#>   DOMAIN   SUBJECTS   OBSERVATIONS   
#>   dm       103        103            
#>   vs       103        206            
#>   ex       80         468            
#>   pc       80         1344           
#>   lb       103        103            
#>   pp       13         432            
#> 
#> Arms (DM):
#>   ACTARMCD    ACTARM                 
#>   TREATMENT   Single Arm Treatment   
#>   SCRNFAIL    Screen Faillure        
#> 
#> Treatments (EX):
#>   EXAMPLINIB
#> 
#> PK sample specimens (PC):
#>   PLASMA
#> 
#> PK analytes (PC):
#>   PCTEST       PCTESTCD     
#>   RS2023       RS2023       
#>   RS2023487A   RS2023487A    
#> 
#> Hash: a839c3b7866a6c04a3007b1d7b877345
#> Last DTC: 2001-07-14 10:53:00
```

Note that in the EX domain, the administered drug is given as
‘EXAMPLINIB’ while in PC, the corresponding analyte is ‘RS2023’. The
other analyte, ‘RS2023487A’ is a metabolite.

## NIF DATA SET

Following the tutorial given in
[`vignette("nif-tutorial")`](../articles/nif-tutorial.md), we start with
a basic pharmacokinetic nif object from `examplinic_poc`:

``` r
sdtm <- examplinib_poc

nif_poc <- new_nif() %>% 
  add_administration(sdtm, extrt = "EXAMPLINIB", analyte = "RS2023") %>% 
  add_observation(sdtm, domain = "pc", testcd = "RS2023", analyte = "RS2023", cmt = 2) %>% 
  add_observation(sdtm, domain = "pc", testcd = "RS2023487A", parent = "RS2023", cmt = 3) 
```

Let’s add some further baseline data to our nif object. The serum
creatinine concentration can be used to calculate the individual
baseline creatinine clearance as an estimate for glomerular filtration
rate (eGFR).

We first add baseline creatinine from the ‘LB’ domain using the generic
[`add_baseline()`](../reference/add_baseline.md) function. Then,
creatinine clearance is calculated using
[`add_bl_crcl()`](../reference/add_bl_crcl.md). That function uses
further covariate data (sex, age, race and weight) and the
Cockcroft-Gault formula by default. For further options, see the
documentation for [`add_bl_crcl()`](../reference/add_bl_crcl.md).

``` r
nif_poc <- nif_poc %>% 
  add_baseline(sdtm, domain = "lb", testcd = "CREAT") %>% 
  add_bl_crcl()
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'
```

The nif data set now includes both baseline creatinine and baseline
creatinine clearance:

``` r
head(nif_poc, 3)
#    REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#  1   1  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#  2   2  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#  3   3  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#                    DTC TIME NTIME TAFD TAD EVID AMT    ANALYTE CMT PARENT TRTDY
#  1 2001-01-13 10:36:00    0     0    0   0    1 500     RS2023   1 RS2023     1
#  2 2001-01-13 10:36:00    0     0    0   0    0   0     RS2023   2 RS2023     1
#  3 2001-01-13 10:36:00    0     0    0   0    0   0 RS2023487A   3 RS2023     1
#    METABOLITE DOSE MDV  ACTARMCD IMPUTATION DV BL_CREAT  BL_CRCL
#  1      FALSE  500   1 TREATMENT            NA 72.78062 107.4689
#  2      FALSE  500   0 TREATMENT             0 72.78062 107.4689
#  3      FALSE  500   0 TREATMENT             0 72.78062 107.4689
```

## EXPLORATION

### Demographics

For an initial overview on the distribution of baseline parameters in
the study population, the administered drugs, analytes, observations,
etc., nif objects can be inspected with the
[`summary()`](https://rdrr.io/r/base/summary.html) function. After we
have added baseline creatinine clearance, the output also summarizes the
number of patients with normal renal function or impaired renal
function:

``` r
summary(nif_poc)
#  ----- NONMEM Input Format (NIF) data summary -----
#  Data from 80 subjects across one study:
#    STUDYID      N    
#    2023000022   80   
#  
#  Sex distribution:
#    SEX      N    percent   
#    male     47   58.8      
#    female   33   41.2      
#  
#  Renal impairment class:
#    CLASS      N    percent   
#    normal     31   38.8      
#    mild       34   42.5      
#    moderate   15   18.8      
#    severe     0    0         
#  
#  Treatments:
#    RS2023
#  
#  Analytes:
#    RS2023, RS2023487A
#  
#  Subjects per dose level:
#    RS2023   N    
#    500      80   
#  
#  1344 observations:
#    CMT   ANALYTE      N     
#    2     RS2023       672   
#    3     RS2023487A   672   
#  
#  Sampling schedule:
#    NTIME   RS2023   RS2023487A   
#    0       X        X            
#    0.5     X        X            
#    1       X        X            
#    1.5     X        X            
#    2       X        X            
#    3       X        X            
#    4       X        X            
#    6       X        X            
#    8       X        X            
#    10      X        X            
#    12      X        X            
#  
#  Subjects with dose reductions
#    RS2023   
#    25       
#  
#  Treatment duration overview:
#    PARENT   min   max   mean   median   
#    RS2023   56    99    78.5   79       
#  
#  Hash: 7f4823f1ab95211e7cbf45cf78297804
#  Last DTC: 2001-07-14 08:53:00
```

For a visual overview of the NIF data set
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) can be applied
to the nif summary object. Ignore the `invisible(capture.output())`
construct in the below code. Its purpose is to hide non-graphical
output.

``` r
invisible(capture.output(
  plot(summary(nif_poc))
))
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-7-1.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-2.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-3.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-4.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-5.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-6.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-7.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-8.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-9.png)![](multiple-dose-example_files/figure-html/unnamed-chunk-7-10.png)

### Exposure

In this study, all 80 subject received the same dose level:

``` r
nif_poc %>%
  dose_levels() %>% 
  kable(caption="Dose levels")
```

| RS2023 |   N |
|-------:|----:|
|    500 |  80 |

Dose levels

However, there were subjects with dose reductions, as we can see when
filtering the nif data set for `EVID == 1` (i.e., administrations) and
summarizing the administered dose:

``` r
nif_poc %>% 
  filter(EVID == 1) %>% 
  group_by(DOSE) %>% 
  summarize(n = n()) %>% 
  kable()
```

| DOSE |    n |
|-----:|-----:|
|  250 |  916 |
|  500 | 5362 |

To identify the subjects with dose reductions, we can use the
[`dose_red_sbs()`](../reference/dose_red_sbs.md) function provided by
the nif package:

``` r
nif_poc %>%
  dose_red_sbs()
#  # A tibble: 25 × 2
#        ID USUBJID          
#     <dbl> <chr>            
#   1    62 20230000221060015
#   2    66 20230000221070005
#   3    44 20230000221050006
#   4     3 20230000221010005
#   5    17 20230000221030004
#   6    76 20230000221070016
#   7    77 20230000221070018
#   8    10 20230000221020007
#   9    22 20230000221030010
#  10    46 20230000221050008
#  # ℹ 15 more rows
```

Let’s have a plot of the doses over time in these subjects:

``` r
nif_poc %>% 
  filter(ID %in% (dose_red_sbs(nif_poc))$ID) %>% 
  filter(EVID == 1) %>% 
  ggplot(aes(x = TIME, y = DOSE, color = as.factor(ID))) + 
  geom_point() +
  geom_line() +
  theme(legend.position="none") 
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-11-1.png)

We see that dose reductions happened at different times during
treatment. Another way of visualizing this is per the
[`mean_dose_plot()`](../reference/mean_dose_plot.md) function:

``` r
nif_poc %>%
  mean_dose_plot()
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-12-1.png)

The upper panel shows the mean dose over time, and we can see that after
~Day 13, the mean dose across all treated subjects drops due to dose
reductions in some subjects. To put this into context, the lower panel
shows the number of subjects on treatment over time, and we see that
most subjects had treatment durations of around 30 days. Note the
fluctuations that indicate single missed doses in individual subjects!

### PK sampling

The PK sampling time points in this study were:

``` r
nif_poc %>% 
  filter(EVID == 0) %>% 
  group_by(NTIME, ANALYTE) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = "ANALYTE", values_from = "n") %>% 
  kable(caption = "Observations by time point and analyte")
```

| NTIME | RS2023 | RS2023487A |
|------:|-------:|-----------:|
|   0.0 |    160 |        160 |
|   0.5 |     24 |         24 |
|   1.0 |     24 |         24 |
|   1.5 |    160 |        160 |
|   2.0 |     24 |         24 |
|   3.0 |     24 |         24 |
|   4.0 |    160 |        160 |
|   6.0 |     24 |         24 |
|   8.0 |     24 |         24 |
|  10.0 |     24 |         24 |
|  12.0 |     24 |         24 |

Observations by time point and analyte

From the different numbers of samplings per nominal time point, we guess
that only a subset of subjects had a rich sampling scheme. Let’s
identify those:

``` r
nif_poc %>% 
  rich_sampling_sbs(analyte = "RS2023", max_time = 24, n = 6)
#   [1]  1  4  5 15 28 29 40 50 51 52 63 64
```

For details, see the documentation to
[`rich_sampling_sbs()`](../reference/rich_sampling_sbs.md).

### Plasma concentration data

Let’s plot the individual and mean plasma concentration profiles on Day
1 for the parent, RS2023, and the metabolite, RS2023487A:

``` r
temp <- nif_poc %>% 
  filter(ID %in% (rich_sampling_sbs(nif_poc, analyte = "RS2023", n=4)))

temp %>%
  plot(dose = 500, points = TRUE, title = "Rich sampling subjects")
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-15-1.png)

For single and multiple dose administrations separately:

``` r
temp <- temp %>% 
  index_rich_sampling_intervals()

temp %>% filter(RICH_N == 1) %>% 
  plot(analyte = "RS2023", mean = TRUE, title = "Single-dose PK")
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-16-1.png)

``` r

temp %>% filter(RICH_N == 2) %>% 
  plot(analyte = "RS2023", title = "Multiple-dose PK", time = "NTIME", mean = T)
```

![](multiple-dose-example_files/figure-html/unnamed-chunk-16-2.png)

The above code made use of the helper function
[`index_rich_sampling_intervals()`](../reference/index_rich_sampling_intervals.md)
that identifies dosing intervals with rich PK sampling. See the
documentation for details.

### Non-compartmental analysis

The nif package includes functions for non-compartmental PK analysis.
Essentially, [`nca()`](../reference/nca.md) is a wrapper around
[`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md)
from the popular
[PKNCA](https://cran.r-project.org/web/packages/PKNCA/index.html)
package.

``` r
nca <- examplinib_poc_nif %>% 
  index_rich_sampling_intervals(analyte = "RS2023", min_n = 4) %>% 
  nca("RS2023", group = "RICH_N")

nca %>% 
  nca_summary_table(group = "RICH_N") %>% 
  kable()
```

| RICH_N | DOSE | n | aucinf.obs | auclast | cmax | half.life | tmax |
|:---|---:|---:|:---|:---|:---|:---|:---|
| 1 | 250 | 4 | 16962.71 (18) | 15764.51 (19) | 3050.03 (23) | 2.82 (10) | 2.37 (2; 2.67) |
| 1 | 500 | 12 | 21147.25 (37) | 19382.1 (37) | 3530.09 (37) | 3 (14) | 2.53 (2; 2.78) |
| 2 | 250 | 4 | NA | NA | 3051.88 (24) | 2.85 (6) | 2.07 (1.7; 2.75) |
| 2 | 500 | 12 | NA | NA | 3559.39 (36) | 3.05 (12) | 2.51 (1.68; 3.03) |
