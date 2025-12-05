# Test for dose linearity

**\[experimental\]**

Currently experimental. Don't use in production!

Using the power model described by [Hummel,
2009](https://doi.org/10.1002/pst.326). In brief, a power model is
fitted with

ln(AUC) = mu + beta\* ln(dose)

and the 90% CI of beta compared to the below acceptance criteria,
assuming

theta_L of 0.8 and theta_U of 1.25:

(beta_L, beta_U) = ( 1 + ln(theta_L) / ln(r), 1 + lntheta_U) / ln(r) )

with ln(r) the logarithm of the ratio of the highest dose to the lowest
dose.

## Usage

``` r
dose_lin(nca, parameters = c("aucinf.obs", "cmax"), lower = 0.8, upper = 1.25)
```

## Arguments

- nca:

  The non-compartmental analysis data.

- parameters:

  The NCA parameters to investigate for dose linearity.

- lower:

  The lower threshold for Rdnm.

- upper:

  the upper threshold for Rdnm.

## Value

A data frame.
