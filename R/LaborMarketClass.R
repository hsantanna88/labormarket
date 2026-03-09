#' S4 Class for Representing a Simulated Labor Market
#'
#' A class to represent the state of a simulated labor market including the
#' panel data, transition/steady-state matrices, and all generating parameters.
#'
#' @slot panel A `data.table` object representing the simulated panel data.
#' @slot init.params A list of initial parameters (`nk`, `nl`, `nt`, `ni`, `ratiog`, `lambda`).
#' @slot tranM An array representing transition matrices across firm types.
#' @slot steadyM An array representing steady-state distributions.
#' @slot alpha_mean Numeric vector of mean worker effects by type.
#' @slot psi_mean Numeric vector of mean firm effects by type.
#' @slot psi_sd Standard deviation of the firm effect distribution.
#' @slot alpha_sd Numeric vector of standard deviations for worker effects by type.
#' @slot csort Sorting effect parameter.
#' @slot cnetw Network effect parameter.
#' @slot csig Standard deviation of the moving probability.
#' @slot fsize Base firm size parameter.
#' @slot w_sigma Standard deviation of the wage shock.
#' @slot neduc Number of education levels.
#' @slot sort_gap Sorting gap for gender bias.
#' @slot shocks Numeric vector of time-period shocks.
#'
#' @seealso [simlabormarket()] which creates instances of this class.
#'
#' @examples
#' lm_obj <- simlabormarket(nk = 3, nl = 3, nt = 3, ni = 100, lambda = 0.3)
#' lm_obj        # calls show()
#' lm_obj@panel  # access the panel data

setClass("LaborMarket",
  representation(
    panel = "data.table",
    init.params = "list", 
    tranM = "array",
    steadyM = "array",
    alpha_mean = "numeric",
    psi_mean = "numeric",
    psi_sd = "numeric",
    alpha_sd = "numeric",
    csort = "numeric",
    cnetw = "numeric",
    csig = "numeric",
    fsize = "numeric",
    w_sigma = "numeric",
    neduc = "numeric",
    sort_gap = "numeric",
    shocks = "numeric"
  )
)


setMethod("show", "LaborMarket",
  function(object) {
    old_scipen <- getOption("scipen")
    options(scipen = 999)
    on.exit(options(scipen = old_scipen))

    p <- object@init.params
    d <- object@panel

    cat(blue("-- LaborMarket simulation"), "---\n\n")

    cat("  Parameters\n")
    cat("    Firm types (nk):    ", p$nk, "\n")
    cat("    Worker types (nl):  ", p$nl, "\n")
    cat("    Time periods (nt):  ", p$nt, "\n")
    cat("    Individuals (ni):   ", p$ni, "\n")
    cat("    Female ratio:       ", p$ratiog, "\n")
    cat("    Mobility (lambda):  ", p$lambda, "\n")

    cat("\n  Summary statistics\n")
    cat("    Observations:       ", p$nt * p$ni, "\n")
    cat("    Mean wage:          ", round(mean(d$lw), 3), "\n")
    cat("    Mean firm effect:   ", round(mean(d$psi), 3), "\n")
    cat("    Mean worker effect: ", round(mean(d$alpha), 3), "\n")
    if (mean(d$age) > 0) {
      cat("    Mean age:           ", round(mean(d$age), 1), "\n")
      cat("    Mean education:     ", round(mean(d$educ), 1), "\n")
      cat("    Mean experience:    ", round(mean(d$experience), 1), "\n")
    }
    cat("\n")
  }
)

