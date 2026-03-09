# Corrects the Limited Mobility Bias Using Bootstrap

The `lmbias` function applies a bootstrapping technique to correct the
limited mobility bias (often referred to as small sample bias) in
fixed-effects models. This implementation is based on the method
proposed by Azkarate-Askasua and Zerecero (2022). It uses a Rademacher
random vector and the fixest package's model specifications for its
calculations.

## Usage

``` r
lmbias(
  femodel,
  data,
  vcov = "HC1",
  R = 1000,
  xvarname = "xbeta",
  parallel = c("no", "multicore", "snow"),
  cluster = NULL,
  ncpus = getOption("boot.ncpus", 1L),
  cl = NULL
)
```

## Arguments

- femodel:

  A model object of class `fixest` (from the `fixest` package) that
  contains the fixed effects estimates to be corrected. Must have at
  least two fixed effects.

- data:

  A data.frame or data.table object that contains the dataset used for
  the fixed effects model.

- vcov:

  A character string specifying the type of heteroskedasticity
  correction to be used. Must be one of `"HC0"` or `"HC1"` (default).

- R:

  An integer that sets the number of bootstrap replications. Default is
  1000.

- xvarname:

  A character string giving the name of the column in `data` that
  contains the linear predictor (X \* beta). Default is `"xbeta"`.

- parallel:

  A character string that determines the type of parallel operation to
  be used (if any). Possible values are `"no"`, `"multicore"`, and
  `"snow"`. Default is `"no"`.

- cluster:

  An optional vector or factor that provides the data's cluster IDs if
  cluster bootstrapping is desired.

- ncpus:

  An integer specifying the number of CPUs to be used for parallel
  processing. This argument is relevant only if `parallel` is set to
  `"multicore"` or `"snow"`. Default uses the option set in
  `getOption("boot.ncpus", 1L)`.

- cl:

  An optional parallel or snow cluster for use with `parallel = "snow"`.
  If not supplied and `parallel = "snow"`, a cluster on the local
  machine is created.

## Value

A 3x4 matrix with rows `"Original"`, `"Bias"`, and `"Corrected"`, and
columns for variance of each fixed effect, covariance, and correlation.

## References

Azkarate-Askasua, M. and Zerecero, M. (2022). "The Macroeconomics of
Firms' Financing." Working Paper.
<https://github.com/mazkarate/bias_correction>

## See also

[`simlabormarket()`](https://hsantanna88.github.io/labormarket/reference/simlabormarket.md)
for simulating labor market data suitable for this function.

## Examples

``` r
# \donttest{
set.seed(1)
# Simulate a labor market with high mobility for connectivity
lm_sim <- simlabormarket(nk = 2, nl = 2, nt = 5, ni = 2000, lambda = 0.5)
#> -- LaborMarket simulation ---
#> 
#>   Parameters
#>     Firm types (nk):     2 
#>     Worker types (nl):   2 
#>     Time periods (nt):   5 
#>     Individuals (ni):    2000 
#>     Female ratio:        0.45 
#>     Mobility (lambda):   0.5 
#> 
#>   Summary statistics
#>     Observations:        10000 
#>     Mean wage:           4.801 
#>     Mean firm effect:    2.534 
#>     Mean worker effect:  2.272 
#> 
panel <- lm_sim@panel
panel$fid <- as.factor(panel$fid)
panel$i   <- as.factor(panel$i)

# Restrict to the largest connected set
cf <- lfe::compfactor(list(panel$i, panel$fid))
largest <- names(which.max(table(cf)))
panel <- panel[cf == largest, ]
panel$i   <- droplevels(panel$i)
panel$fid <- droplevels(panel$fid)

# Fit a two-way fixed effects model
model <- fixest::feols(lw ~ 1 | i + fid, data = panel)

# Add the linear predictor column (match model obs)
panel <- panel[!is.na(predict(model, newdata = panel)), ]
panel$xbeta <- 0

# Correct limited mobility bias (small R for demo)
lmbias(model, panel, R = 50)
#>             Var of i Var of fid   Covariance Correlation
#> Original  0.18802624 0.26887455  0.002965913   0.0131909
#> Bias      0.13758109 0.05266605 -0.025543702  -0.3000814
#> Corrected 0.05044515 0.21620850  0.028509615   0.3132723
# }
```
