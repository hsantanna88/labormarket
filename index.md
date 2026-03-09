# labormarket

**labormarket** is an R package for simulating worker-firm matched
datasets and correcting limited mobility bias in two-way fixed effects
models. It is designed as a teaching and research tool for labor
economics.

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("hsantanna88/labormarket")
```

## Quick Start

### Simulate a Labor Market

``` r
library(labormarket)

# Simulate a labor market with 6 firm types, 10 worker types, 4 periods, 100k workers
lm_obj <- simlabormarket(nk = 6, nl = 10, nt = 4, ni = 100000, lambda = 0.05)

# Access the panel data
panel <- lm_obj@panel
head(panel)
```

### Correct Limited Mobility Bias

After fitting a two-way fixed effects (AKM) model, use
[`lmbias()`](https://hsantanna88.github.io/labormarket/reference/lmbias.md)
to correct the limited mobility bias following Azkarate-Askasua and
Zerecero (2022):

``` r
library(fixest)

# Fit an AKM model
panel$fid <- as.factor(panel$fid)
panel$i   <- as.factor(panel$i)
model <- feols(lw ~ 1 | i + fid, data = panel)

# Add predicted values
panel$xbeta <- predict(model, type = "link")

# Bootstrap bias correction
results <- lmbias(model, panel, R = 100)
print(results)
```

## References

- Azkarate-Askasua, M. and Zerecero, M. (2022). “The Macroeconomics of
  Firms’ Financing.”

## License

MIT
