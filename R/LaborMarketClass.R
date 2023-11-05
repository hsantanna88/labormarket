#' S4 Class for Representing a Labor Market
#'
#' A class to represent the state of a labor market including parameters and data.
#'
#' @slot panel A data.table object representing panel data.
#' @slot init.params A list of initial parameters.
#' @slot tranM An array representing transition matrices.
#' @slot steadyM An array representing steady-state matrices.
#' @slot alpha_mean Mean of the alpha parameter.
#' @slot psi_mean Mean of the psi parameter.
#' @slot psi_sd Standard deviation of the psi parameter.
#' @slot alpha_sd Standard deviation of the alpha parameter.
#' @slot csort Cost of sorting.
#' @slot cnetw Cost of networking.
#' @slot csig Cost of signalling.
#' @slot fsize Firm size.
#' @slot w_sigma Standard deviation of wages.
#' @slot neduc Number of education levels.
#' @slot sort_gap Sorting gap.
#' @slot shocks Shocks parameter.

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