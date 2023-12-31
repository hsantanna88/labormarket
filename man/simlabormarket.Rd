% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simLaborMarket.R
\name{simlabormarket}
\alias{simlabormarket}
\title{A Function that simulates a labor market with gender bias.}
\usage{
simlabormarket(
  nk = 6,
  ratiog = 0.45,
  lambda = 0.05,
  nl = 10,
  nt = 4,
  ni = 1e+05
)
}
\arguments{
\item{nk}{Number of firm types or clusters.}

\item{ratiog}{Fraction of females in the labor market.}

\item{lambda}{Percentage of movers.}

\item{nl}{Number of worker types or clusters.}

\item{nt}{Number of time periods.}

\item{ni}{Number of individuals.}
}
\value{
an object representing the labor market with the following features.
}
\description{
This function creates a simulated labor market with parameters that control
the number of firm types, the fraction of females in the labor market,
percentage of movers, etc. It can also plot the transition and steady-state
matrices if requested.
}
\examples{
# To create a default labor market simulation:
labormarket <- simlabormarket(nk = 5, ratiog = 0.5, lambda = 0.1,
                              nl = 3, nt = 10, ni = 100)

}
