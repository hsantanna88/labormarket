# S4 Class for Representing a Simulated Labor Market

A class to represent the state of a simulated labor market including the
panel data, transition/steady-state matrices, and all generating
parameters.

## Slots

- `panel`:

  A `data.table` object representing the simulated panel data.

- `init.params`:

  A list of initial parameters (`nk`, `nl`, `nt`, `ni`, `ratiog`,
  `lambda`).

- `tranM`:

  An array representing transition matrices across firm types.

- `steadyM`:

  An array representing steady-state distributions.

- `alpha_mean`:

  Numeric vector of mean worker effects by type.

- `psi_mean`:

  Numeric vector of mean firm effects by type.

- `psi_sd`:

  Standard deviation of the firm effect distribution.

- `alpha_sd`:

  Numeric vector of standard deviations for worker effects by type.

- `csort`:

  Sorting effect parameter.

- `cnetw`:

  Network effect parameter.

- `csig`:

  Standard deviation of the moving probability.

- `fsize`:

  Base firm size parameter.

- `w_sigma`:

  Standard deviation of the wage shock.

- `neduc`:

  Number of education levels.

- `sort_gap`:

  Sorting gap for gender bias.

- `shocks`:

  Numeric vector of time-period shocks.

## See also

[`simlabormarket()`](https://hsantanna88.github.io/labormarket/reference/simlabormarket.md)
which creates instances of this class.

## Examples

``` r
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
#>     Mean wage:           8.032 
#>     Mean firm effect:    2.207 
#>     Mean worker effect:  5.896 
#> 
lm_obj        # calls show()
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
#>     Mean wage:           8.032 
#>     Mean firm effect:    2.207 
#>     Mean worker effect:  5.896 
#> 
lm_obj@panel  # access the panel data
#> Key: <i, spell>
#>          i     l     k     t   fid spell gender       psi    alpha      time_fe
#>      <int> <num> <num> <int> <int> <num>  <num>     <num>    <num>        <num>
#>   1:     1     3     2     1    49     0      0 2.8242404 6.110738 -0.003369476
#>   2:     1     3     2     2    49     0      0 2.9987083 6.696686 -0.009003110
#>   3:     1     3     2     3    49     0      0 1.9536951 7.283835  0.015785113
#>   4:     2     1     1     1   188     0      1 1.2744474 5.259667 -0.003369476
#>   5:     2     1     1     2   188     0      1 0.9740943 5.880635 -0.009003110
#>  ---                                                                           
#> 296:    99     2     2     2   104     0      0 1.6199073 5.716100 -0.009003110
#> 297:    99     2     2     3   104     0      0 2.3505737 5.732177  0.015785113
#> 298:   100     3     2     1    20     0      1 2.5468849 9.255256 -0.003369476
#> 299:   100     3     2     2    20     0      1 2.2934058 9.496968 -0.009003110
#> 300:   100     3     2     3    20     0      1 2.9866137 8.121551  0.015785113
#>             lw  educ   age experience
#>          <num> <num> <num>      <num>
#>   1:  9.103802     0     0          0
#>   2:  8.908107     0     0          0
#>   3:  9.139371     0     0          0
#>   4:  6.991645     0     0          0
#>   5:  7.394048     0     0          0
#>  ---                                 
#> 296:  6.590413     0     0          0
#> 297:  8.492547     0     0          0
#> 298: 12.308843     0     0          0
#> 299: 12.317176     0     0          0
#> 300: 11.072451     0     0          0
```
