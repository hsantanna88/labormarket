#' S4 Class for Representing a Labor Market
#'
#' A class to represent the state of a labor market including parameters and data.
#'
#' @slot panel A data.table object representing panel data.
#' @slot init.params Initial parameters list.
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


setMethod("show", "LaborMarket",
  function(object) {
    cat(yellow("                         .=\"=.\n",
              "                      _/.-.-.\\_     _\n",
              "                     ( ( o o ) )    ))\n",
              "                      |/  \"  \\|    //\n",
              "     .-------.         \\`---\\'/    //\n",
              "    _|~~ ~~  |_       /\\`\"\"\"\\`\\  ((\n",
              "  =(_|_______|_)=    / /_,_\\ \\ \\ \\\\ \n",
              "    |:::::::::|      \\_\\_\\'__/ \\  ))\n",
              "    |:::::::[]|       /`  /`~\\  |//\n",
              "    |o=======.|      /   /    \\  / \n",
              "    \\\`\"\"\"\"\"\"\"\"\\\` ,--\`,--\'\\/\\    / \n",
              "                  '-- \"--'  '--'\"\n"))

    cat(blue("\nA simulated labor market with the following "), red("features: \n\n"))
    cat(green("-------------------------------------------------\n"))
    cat("Types of Firms: ", object@init.params$nk, "\n")
    cat("Types of Workers: ", object@init.params$nl, "\n")
    cat("Time Periods: ", object@init.params$nt, "\n")
    cat("Number of Individuals: ", object@init.params$ni, "\n")
    cat("Female Ratio: ", object@init.params$ratiog, "\n")
    cat("Average Firm Effect: ", mean(object@panel$psi), "\n")
    cat("Average Match Effect: ", mean(object@panel$alpha), "\n")
    cat("Average Wage: ", mean(object@panel$lw), "\n")
    cat("Average Age: ", mean(object@panel$age), "\n")
    cat("Average Experience: ", mean(object@panel$experience), "\n")
    cat("Average Education: ", mean(object@panel$educ), "\n")
    cat("Total Observations: ", (object@init.params$nt)*(object@init.params$ni), "\n")
    cat(green("-------------------------------------------------\n\n"))
    cat("First Rows: \n")
    print(head(object@panel, n = 3))
    cat("Last Rows: \n")
    print(tail(object@panel, n = 3))
  }
)

