# Introduction to labormarket

## Overview

The **labormarket** package provides tools for:

1.  **Simulating labor markets** with worker and firm heterogeneity,
    sorting, and gender bias
2.  **Correcting limited mobility bias** in two-way fixed effects (AKM)
    wage decompositions

## Simulating Labor Markets

The
[`simlabormarket()`](https://hsantanna88.github.io/labormarket/reference/simlabormarket.md)
function generates a panel dataset of workers and firms. Key parameters:

| Parameter | Description                 | Default |
|-----------|-----------------------------|---------|
| `nk`      | Number of firm types        | 6       |
| `nl`      | Number of worker types      | 10      |
| `nt`      | Number of time periods      | 4       |
| `ni`      | Number of individuals       | 100,000 |
| `ratiog`  | Fraction of females         | 0.45    |
| `lambda`  | Probability of moving firms | 0.05    |
| `mincer`  | Use Mincer wage equation    | FALSE   |

### Basic simulation

``` r
library(labormarket)

set.seed(123)
lm_obj <- simlabormarket(nk = 4, nl = 5, nt = 3, ni = 500, lambda = 0.1)
#> -- LaborMarket simulation ---
#> 
#>   Parameters
#>     Firm types (nk):     4 
#>     Worker types (nl):   5 
#>     Time periods (nt):   3 
#>     Individuals (ni):    500 
#>     Female ratio:        0.45 
#>     Mobility (lambda):   0.1 
#> 
#>   Summary statistics
#>     Observations:        1500 
#>     Mean wage:           7.076 
#>     Mean firm effect:    2.133 
#>     Mean worker effect:  4.938
```

The returned `LaborMarket` object stores the panel data and all
generating parameters:

``` r
panel <- lm_obj@panel
head(panel)
#> Key: <i, spell>
#>        i     l     k     t   fid spell gender       psi    alpha     time_fe
#>    <int> <num> <num> <int> <int> <num>  <num>     <num>    <num>       <num>
#> 1:     1     1     1     1    28     0      0 0.6799806 5.397201 0.005807621
#> 2:     1     1     1     2    28     0      0 0.9973784 6.791738 0.032980083
#> 3:     1     1     1     3    28     0      0 1.2960007 6.055003 0.022608009
#> 4:     2     3     4     1    67     0      1 2.7277843 4.117644 0.005807621
#> 5:     2     3     4     2    67     0      1 2.9259664 5.357491 0.032980083
#> 6:     2     3     4     3    67     0      1 3.1947476 3.883151 0.022608009
#>          lw  educ   age experience
#>       <num> <num> <num>      <num>
#> 1: 5.251428     0     0          0
#> 2: 7.242395     0     0          0
#> 3: 7.762032     0     0          0
#> 4: 6.334311     0     0          0
#> 5: 8.297153     0     0          0
#> 6: 6.634714     0     0          0
```

### Mincer wage equation

When `mincer = TRUE`, wages include returns to education, experience,
and experience squared, in addition to worker and firm effects:

$$\log w_{it} = 0.07 \cdot \text{educ}_{i} + 0.02 \cdot \text{exp}_{it} - 0.0005 \cdot \text{exp}_{it}^{2} + \alpha_{i} + \psi_{j{(i,t)}} + \varepsilon_{it}$$

``` r
lm_mincer <- simlabormarket(nk = 4, nl = 5, nt = 3, ni = 500,
                             lambda = 0.1, mincer = TRUE)
#> -- LaborMarket simulation ---
#> 
#>   Parameters
#>     Firm types (nk):     4 
#>     Worker types (nl):   5 
#>     Time periods (nt):   3 
#>     Individuals (ni):    500 
#>     Female ratio:        0.45 
#>     Mobility (lambda):   0.1 
#> 
#>   Summary statistics
#>     Observations:        1500 
#>     Mean wage:           5.916 
#>     Mean firm effect:    2.118 
#>     Mean worker effect:  2.515 
#>     Mean age:            37.9 
#>     Mean education:      16.6 
#>     Mean experience:     14
head(lm_mincer@panel[, .(i, t, age, educ, experience, lw)])
#> Key: <i>
#>        i     t   age  educ experience       lw
#>    <int> <int> <num> <num>      <num>    <num>
#> 1:     1     1    34    15         13 6.819594
#> 2:     1     2    35    15          0 4.586625
#> 3:     1     3    36    15          1 5.852040
#> 4:     2     1    30     4         20 5.787266
#> 5:     2     2    31     4         21 4.086527
#> 6:     2     3    32     4         22 6.161698
```

## The AKM Decomposition

The classic Abowd, Kramarz, and Margolis (1999) decomposition writes:

$$\log w_{it} = \alpha_{i} + \psi_{j{(i,t)}} + X_{it}\prime\beta + \varepsilon_{it}$$

We can estimate this using
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html):

``` r
library(fixest)

set.seed(42)
lm_obj <- simlabormarket(nk = 2, nl = 2, nt = 5, ni = 2000, lambda = 0.5)
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
#>     Mean wage:           7.122 
#>     Mean firm effect:    2.308 
#>     Mean worker effect:  4.817
panel <- lm_obj@panel
panel$fid <- as.factor(panel$fid)
panel$i   <- as.factor(panel$i)

# Restrict to the largest connected set
cf <- lfe::compfactor(list(panel$i, panel$fid))
largest <- names(which.max(table(cf)))
panel <- panel[cf == largest, ]
panel$i   <- droplevels(panel$i)
panel$fid <- droplevels(panel$fid)

model <- feols(lw ~ 1 | i + fid, data = panel)
summary(model)
#> OLS estimation, Dep. Var.: lw
#> Observations: 10,000
#> Fixed-effects: i: 2,000,  fid: 500
#> RMSE: 1.10751   Adj. R2: 0.092137
```

## Limited Mobility Bias Correction

When the number of movers between firms is small, the variance of
estimated fixed effects is upward-biased. The
[`lmbias()`](https://hsantanna88.github.io/labormarket/reference/lmbias.md)
function implements the bootstrap correction from Azkarate-Askasua and
Zerecero (2022).

``` r
# Add predicted values for the bootstrap
panel$xbeta <- 0  # intercept-only model

# Run bias correction (small R for vignette speed)
results <- lmbias(model, panel, R = 20)
print(results)
#>             Var of i Var of fid  Covariance Correlation
#> Original  0.37190443  0.3775042  0.00865995  0.02311207
#> Bias      0.30312312  0.1141722 -0.05559410 -0.29884021
#> Corrected 0.06878132  0.2633321  0.06425405  0.32195228
```

The output is a 3x4 matrix:

- **Original**: Variance, covariance, and correlation of the estimated
  fixed effects
- **Bias**: Estimated upward bias from limited mobility
- **Corrected**: Bias-corrected estimates

## References

- Abowd, J. M., Kramarz, F., and Margolis, D. N. (1999). “High Wage
  Workers and High Wage Firms.” *Econometrica*, 67(2), 251-333.
- Azkarate-Askasua, M. and Zerecero, M. (2022). “The Macroeconomics of
  Firms’ Financing.”
