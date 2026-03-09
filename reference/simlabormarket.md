# Simulate a Labor Market with Gender Bias

This function creates a simulated labor market with parameters that
control the number of firm types, the fraction of females in the labor
market, percentage of movers, etc. The simulated panel includes worker
and firm fixed effects, match effects, and optionally Mincer-style human
capital variables (age, education, experience).

## Usage

``` r
simlabormarket(
  nk = 6,
  ratiog = 0.45,
  lambda = 0.05,
  nl = 10,
  nt = 4,
  ni = 1e+05,
  mincer = FALSE
)
```

## Arguments

- nk:

  Integer \>= 2. Number of firm types or clusters.

- ratiog:

  Numeric in (0, 1). Fraction of females in the labor market.

- lambda:

  Numeric in (0, 1). Probability of a worker moving firms each period.

- nl:

  Integer \>= 2. Number of worker types or clusters.

- nt:

  Integer \>= 2. Number of time periods.

- ni:

  Positive integer. Number of individuals.

- mincer:

  Logical. If `TRUE`, wages are generated using a Mincer equation
  (returns on education, experience, and experience squared) together
  with worker and firm random effects. Default is `FALSE`.

## Value

A
[LaborMarket](https://hsantanna88.github.io/labormarket/reference/LaborMarket-class.md)
S4 object containing the simulated panel and parameters.

## See also

[`lmbias()`](https://hsantanna88.github.io/labormarket/reference/lmbias.md)
for correcting limited mobility bias using the simulated data,
[LaborMarket](https://hsantanna88.github.io/labormarket/reference/LaborMarket-class.md)
for the S4 class definition.

## Examples

``` r
# Simulate a small labor market
lm_obj <- simlabormarket(nk = 3, nl = 3, nt = 3, ni = 100, lambda = 0.3)
#> -- LaborMarket simulation ---
#> 
#>   Parameters
#>     Firm types (nk):     3 
#>     Worker types (nl):   3 
#>     Time periods (nt):   3 
#>     Individuals (ni):    100 
#>     Female ratio:        0.45 
#>     Mobility (lambda):   0.3 
#> 
#>   Summary statistics
#>     Observations:        300 
#>     Mean wage:           4.48 
#>     Mean firm effect:    1.684 
#>     Mean worker effect:  2.794 
#> 

# With Mincer wage equation
lm_mincer <- simlabormarket(nk = 3, nl = 3, nt = 3, ni = 100,
                             lambda = 0.3, mincer = TRUE)
#> -- LaborMarket simulation ---
#> 
#>   Parameters
#>     Firm types (nk):     3 
#>     Worker types (nl):   3 
#>     Time periods (nt):   3 
#>     Individuals (ni):    100 
#>     Female ratio:        0.45 
#>     Mobility (lambda):   0.3 
#> 
#>   Summary statistics
#>     Observations:        300 
#>     Mean wage:           5.916 
#>     Mean firm effect:    1.733 
#>     Mean worker effect:  3.006 
#>     Mean age:            35.2 
#>     Mean education:      16.1 
#>     Mean experience:     9 
#> 
```
