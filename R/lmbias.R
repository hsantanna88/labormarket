#' Corrects the Limited Mobility Bias Using Bootstrap
#'
#' The `lmbias` function applies a bootstrapping technique to correct the limited mobility bias 
#' (often referred to as small sample bias) in fixed-effects models. This implementation is based 
#' on the method proposed by Azkarate-Askasua and Zerecero (2022). It uses a Rademacher random 
#' vector and the fixest package's model specifications for its calculations.
#'
#' @param femodel A model object of class `fixest` (from the `lfe` package) that contains the 
#'        fixed effects estimates to be corrected.
#' @param data A data.table object that contains the dataset used for the fixed effects model.
#' @param vcov A character string specifying the type of heteroskedasticity correction to be 
#'        used. Currently, only "HC0" and "HC1" are supported.
#' @param R An integer that sets the number of bootstrap replications. Default is 1000.
#' @param parallel A character string that determines the type of parallel operation to be used 
#'        (if any). Possible values are "no", "multicore", and "snow". Default is "no".
#' @param cluster An optional vector or factor that provides the data's cluster IDs if cluster 
#'        bootstrapping is desired.
#' @param ncpus An integer specifying the number of CPUs to be used for parallel processing. 
#'        This argument is relevant only if `parallel` is set to "multicore" or "snow". 
#'        Default uses the option set in `getOption("boot.ncpus", 1L)`.
#' @param cl An optional parallel or snow cluster for use with `parallel = "snow"`. If not 
#'        supplied and `parallel = "snow"`, a cluster on the local machine is created.
#' 
#' @return A matrix that presents the variance, covariance, and correlation for the original 
#'         fixed effects, the estimated bias, and the corrected measures.
#' 
#' @import Matrix
#' @importFrom boot boot
#' @importFrom lfe compfactor
#' @import fixest
#' @export lmbias
#' 
#' @examples 
#' # Assuming example data and model exist:
#' # data <- data.table(...)
#' # model <- feols(y ~ x1 + x2 | fe1 + fe2, data = data)
#' # lmbias(model, data)

lmbias <- function(femodel, data, vcov = "HC1", R = 1000,
                    parallel = c("no", "multicore", "snow"), cluster =  NULL,
                    ncpus = getOption("boot.ncpus", 1L), cl = NULL) {

# Checking if the variables are correct
  if (!inherits(femodel, "fixest")) {
    stop("The model must be a feols object")
  }

  if(!("data.frame" %in% class(data))) {
    stop("The data must be a data.table object")
  }

  # fixed effects names
  fe1 <- femodel$fixef_vars[1]
  fe2 <- femodel$fixef_vars[2]

  data[[fe1]] <- factor(data[[fe1]])
  data[[fe2]] <- factor(data[[fe2]])

  # fixed effects (originals)
  beta_fe1_original <- fixef(femodel)[[1]]
  beta_fe2_original <- fixef(femodel)[[2]]

  # #obs
  nobs <- femodel$nobs
  
  nfe1 <- length(femodel$fixef_sizes[[1]])
  nfe2 <- length(femodel$fixef_sizes[[2]])
  ncovar <- length(femodel$coefficients)
  k  <- nfe1 + nfe2 + ncovar

  # converting to factors
  data[[fe1]] <- as.factor(data[[fe1]])
  data[[fe2]] <- as.factor(data[[fe2]])

  # checking the connected set
  if(levels(compfactor(list(factor(data[[fe1]]), factor(data[[fe2]])))) != 1) {
    stop("The fixed effects must be connected in a single graph. \\n
         Please check the data and the model with lfe::compfactor.")
  }

  # left hand side 
  lhs <- as.character(formula(femodel))[3]

    # bootstrap function
  X1 <- sparse.model.matrix(
    formula(paste0("~",
      paste(femodel$fixef_vars[1]), "-1"
    )),
    data = data
  )
  X2 <- sparse.model.matrix(
    formula(paste0("~",
      paste(femodel$fixef_vars[2]), "-1"
    )),
    data = data
  )
  Xcovar <- sparse.model.matrix(
    formula(paste0("~",
      paste(names(femodel$coefficients), collapse = " + "), "-1"
    )),
    data = data
  )

  # what type of vcov
  if( vcov == "HC0") {
    phi <- sqrt(resid(femodel)^2)
  } else if (vcov == "HC1") {
    phi <- sqrt(resid(femodel)^2 * nobs / (nobs - k))
  } #else if (vcov == "HC2") {
    #phi <- sqrt(resid(femodel)^2 / (1 - diag(cbind(Xcovar, X1, X2) %*% solve(crossprod(cbind(Xcovar, X1, X2)), t(cbind(Xcovar, X1, X2))))))
  #}



  # Clustering method
  calculate_bias <- function(data = data, indices, phi, nobs, lhs, nfe1,
                              nfe2, ncovar, X1, X2,
                              Xcovar, cluster) {

    # generating the random -1, 1 vetor (rademacher)
    if(is.null(cluster)) {
      
      r  <- sample(c(-1, 1), nobs, replace = TRUE)
      data$r <- r * phi

    } else {

      groups <- unique(data[[cluster]])
      g <- length(groups)
      r  <- sample(c(-1, 1), g, replace = TRUE)
      data$r <- r[match(data[[cluster]], groups)] * phi

    }

    
    new_fml <- formula(paste0("r ~ ", lhs))
    model <- feols(new_fml, data = data)
    fe <- fixef(model)
    beta_fe1 <- fe[[1]]
    beta_fe2 <- fe[[2]]


    # creating the function: beta * A * beta
    # I will exploit the fact we only need a scalar in the end to bypass
    #    the gigantic dense matrix in the middle

    # Generating the vector of ones
    ones <- rep(1, nobs)

    # Moment 1 --------------------------------------------------------------
    m1 <- t(beta_fe1) %*% crossprod(X1) %*% beta_fe1
    m1 <- (1/(nobs - 1)) * (m1 - (1/nobs) * (t(beta_fe1) %*% t(X1) %*% ones %*% t(ones) %*% X1 %*% beta_fe1))

    
    # Moment 2 --------------------------------------------------------------
    m2 <- (t(beta_fe2) %*% crossprod(X2) %*% beta_fe2)
    m2 <- (1/(nobs - 1)) * (m2 - (1/nobs) * (t(beta_fe2) %*% t(X2) %*% ones %*% t(ones) %*% X2 %*% beta_fe2))

    # Moment 3 --------------------------------------------------------------
    m3_1 <- t(beta_fe2) %*% crossprod(X2, X1) %*% beta_fe1
    m3_1 <- (0.5/(nobs - 1)) * (m3_1 - (1/nobs) * (t(beta_fe2) %*% t(X2) %*% ones %*% t(ones) %*% X1 %*% beta_fe1))

    m3_2 <- t(beta_fe1) %*% crossprod(X1, X2) %*% beta_fe2
    m3_2 <- (0.5/(nobs - 1)) * (m3_2 - (1/nobs) * (t(beta_fe1) %*% t(X1) %*% ones %*% t(ones) %*% X2 %*% beta_fe2))

    m3 <- m3_1 + m3_2

    return(c(as.numeric(m1), as.numeric(m2), as.numeric(m3)))
  }

  # Starting the bootstrapping
  boot_mbias <- boot(data, statistic = calculate_bias, phi = phi,
                      nobs = nobs, lhs = lhs, R = R, parallel = parallel, cluster = cluster,
                      ncpus = ncpus, cl = cl, X1 = X1, X2 = X2, Xcovar = Xcovar)

  boot_res <- colMeans(boot_mbias$t)
  results <- cbind(
    var(beta_fe1_original[femodel$fixef_id[[1]]]),
    var(beta_fe2_original[femodel$fixef_id[[2]]]),
    cov(beta_fe1_original[femodel$fixef_id[[1]]], beta_fe2_original[femodel$fixef_id[[2]]]),
    cor(beta_fe1_original[femodel$fixef_id[[1]]], beta_fe2_original[femodel$fixef_id[[2]]])
  )

  results <- rbind(results, cbind(t(boot_res), (boot_res[3]/(sqrt(boot_res[1]) * sqrt(boot_res[2])))))
  results <- rbind(results, results[1,] - results[2,])

  colnames(results) <- c(
    paste0("Var of ", fe1),
    paste0("Var of ", fe2),
    paste0("Covariance"),
    paste0("Correlation")
  )
  rownames(results) <- c("Original", "Bias", "Corrected")
  
  return(results)
}