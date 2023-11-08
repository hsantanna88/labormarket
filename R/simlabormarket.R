#' A Function that simulates a labor market with gender bias.
#'
#' This function creates a simulated labor market with parameters that control
#' the number of firm types, the fraction of females in the labor market,
#' percentage of movers, etc. It can also plot the transition and steady-state
#' matrices if requested.
#'
#' @param nk Number of firm types or clusters.
#' @param ratiog Fraction of females in the labor market.
#' @param lambda Percentage of movers.
#' @param nl Number of worker types or clusters.
#' @param nt Number of time periods.
#' @param ni Number of individuals.
#' @return an object representing the labor market with the following features.
#' 
#' @importFrom reshape2 melt
#' @importFrom data.table data.table setnames setkey
#' @import lattice
#' @import gridExtra
#' @import ggplot2
#' @importFrom crayon blue green red yellow
#' @import futile.logger
#' @import feather
#' @import methods
#' @importFrom stats cor cov dexp dnorm formula qnorm rbeta resid rnorm runif var
#' @export simlabormarket
#'
#' @examples
#' # To create a default labor market simulation:
#' labormarket <- simlabormarket(nk = 5, ratiog = 0.5, lambda = 0.1,
#'                               nl = 3, nt = 10, ni = 100)
#'

simlabormarket <- function(nk = 6, ratiog = 0.45, lambda = 0.05, nl = 10, nt = 4, ni = 100000) {

  # Labor Market Inner Parameters
  #-------------------------------------------------------------------------------------------------

  # alpha specific sd (mixture model)
  alpha_sd = rnorm(nl)
  # firm fixed effect standard deviation
  psi_sd = 1
  # Drawing the random effects averages for firms
  psi_mean = qnorm(1:nk/(nk+1)) * psi_sd
  # Drawing the match effects seeds for individuals
  alpha_mean = qnorm(1:nl/(nl+1))

  # sorting effect
  csort = 0.5
  # network effect
  cnetw = 0.2
  # probability of moving standard deviation
  csig = 0.5
	# Starting point for making firms
	fsize = 10
  # standard deviation for wages
  w_sigma = 0.5
  # maximum years of education
  neduc = 24


  # Matrix Building
  #-------------------------------------------------------------------------------------------------

  # imposing a sorting bias
  sort_gap = min(psi_mean)/10

  # worker gender (male = 1/female = 2)
  ng = 2

  # Generating the transition matrix based on the sorting effect and network effect
  # we expect closer to zero values to have higher moving probability

  # transition matrix
  G = array(0,c(nl, nk, nk, ng))
  for (g in 1:ng) for (l in 1:nl) for (k in 1:nk){

    if(g == 2) {
      G[l, k, , g] = dnorm(psi_mean  - cnetw * psi_mean[k] - csort * alpha_mean[l] - sort_gap, sd = csig)
    } else {
      G[l, k, ,g] = dnorm(psi_mean - cnetw * psi_mean[k] - csort * alpha_mean[l], sd = csig)
    }
    # normalize to get transition matrix
    G[l, k, , g] = G[l, k, , g]/sum(G[l, k, , g])
  }

  # Steady State Matrix
  #-------------------------------------------------------------------------------------------------

  # we then solve for the stationary distribution over psis for each alpha value
  H = array(1/nk,c(nl,nk, ng)) # make a initial matrix for L x K
  for (g in 1:2) for (l in 1:nl) for (i in 1:100) {
    H[l, ,g] = t(G[l, , ,g]) %*% H[l, ,g]
  }

  # creating the education matrix
  #-------------------------------------------------------------------------------------------------

  # generating the matrix
  educ_prob = matrix(0, nrow = nl, ncol = neduc)

  # Parameters for the beta distribution for the extreme types
  alpha_low = 2  # Lower alpha for lower education (type 1)
  beta_high = 1.5    # Higher beta for lower education (type 1)
  alpha_high = 2.5   # Higher alpha for higher education (type 10)
  beta_low = 1   # Lower beta for higher education (type 10)

  # generating quantiles
  quantiles <- seq(1/(2*neduc), 1 - 1/(2*neduc), length.out = neduc)

  # Interpolating alpha and beta parameters for the types
  alphas = seq(alpha_low, alpha_high, length.out = nl)
  betas = seq(beta_high, beta_low, length.out = nl)


  # populating the matrix
  for (l in 1:nl) {

    # Generate probabilities for each education year for the individual type
    probs = dbeta(quantiles, alphas[l], betas[l])
    educ_prob[l,] = probs
  
  }

  # normalizing the matrix rowwise
  educ_prob = educ_prob/rowSums(educ_prob)

  # we simulate a panel
  network    = array(0,c(ni,nt))
  spellcount = array(0,c(ni,nt))
  age        = array(0,c(ni,nt))
  experience = array(0,c(ni,nt))

  # age where we start education
  start_age = 6

  # time invariante worker matrix (worker type, gender, education)
  A = array(0,c(ni, 3))

  for (i in 1:ni) {

    # we draw the worker type
    l = sample.int(nl,1)
    # we draw the gender type
    g = ifelse(runif(1) >= ratiog, 1, 2)
    # Get the probability weights for education
    prob_educ = educ_prob[l, ]
    # we draw the education based on worker type
    e = sample(x = 1:ncol(educ_prob), size = 1, prob = prob_educ)
    
    # We build the matrix A
    A[i,] = c(l, g, e)

    # at time 1, we draw from H, set other initial paramemters
    # network matrix stands for the "history" of worker jobs

    # network matrix -----------------------------------------------------------
    network[i,1] = sample.int(nk,1,prob = H[l, , g])

    # age matrix ---------------------------------------------------------------
    # drawing the age
    # Generate age pyramid that follows an exponential distribution

    if((e + start_age) < 18) {
      # I don't want individuals younger than 18
      p_age = dexp(seq_along(18:65), rate = 0.05)
      p_age = p_age/sum(p_age)
      age[i,1] = sample(18:65, 1, prob = p_age)
    } else {
      p_age = dexp(seq_along((e + start_age):65), rate = 0.05)
      p_age = p_age/sum(p_age)
      age[i,1] = sample((e + start_age):65, 1, prob = p_age)
    }

    # experience matrix ---------------------------------------------------------
    # start by allowing xp to be the difference between age and education
    experience[i,1] = age[i,1] - e - start_age

    # updating spellcount and network matrix ------------------------------------
    for (t in 2:nt) {
      if (runif(1)<lambda) {
        network[i,t] = sample.int(nk,1,prob = G[l,network[i,t-1], , g])
        spellcount[i,t] = spellcount[i,t-1] + 1
        # reset experience
        experience[i,t] = 0
      } else {
        network[i,t]    = network[i,t-1]
        spellcount[i,t] = spellcount[i,t-1]
        experience[i,t] = experience[i,t-1] + 1
      }
      age[i,t] = age[i,t-1] + 1
    }

  }

  # Creating the dataset
  #-------------------------------------------------------------------------------------------------

	data  = data.table(melt(network, c('i', 't')))
	data2 = data.table(melt(spellcount, c('i', 't')))
  data3 = data.table(melt(age, c('i', 't')))
  data4 = data.table(melt(experience, c('i', 't')))

	setnames(data, "value", "k")

  # Code directly from the loop
	data[, spell := data2$value]
  data[, age := data3$value]
  data[, experience := data4$value]

  # worker, education level and gender
  data[, l := A[i,1], i]
  data[, gender := A[i,2], i]
  data[, educ := A[i,3], i]

  # generating firm ids
  #-------------------------------------------------------------------------------------------------

  last_max_fid = 0
  f_class_count = ni/(fsize*nk*nt) # number of firm classes

  # creating the spell counter dataset (keep track whenever an individual moves from a firm to another)
  dspell = data[,list(len=.N),list(i,spell,k)]

  # Calculate and assign unique fids for each k
  dspell[, fid := {
    current_max_fid = pmax(1, sum(len) / f_class_count)
    fid = sample((last_max_fid + 1):(last_max_fid + current_max_fid), .N, replace=TRUE)
    last_max_fid <<- max(fid)
    .(fid)
  }, by=k]

  setkey(data,i,spell)
  setkey(dspell,i,spell)
    
  data[, fid := dspell[data, fid]]

  # generating random effects
  #-------------------------------------------------------------------------------------------------
  
  # bargaining gap
  barg_gap = 0.9

  # match matrix with bargaining gap across gender
  match = array(0,c(nl,nk, ng))
  match[,,1] = alpha_mean %*% t(psi_mean)
  match[,,2] = match[,,1] * barg_gap # Bargaining power heterogeneity. 

  # random effects (we modify here for match effects and firm effects -  time invariant for now)
  # I draw from a normal distribution and allow match effects to also have different sds
  data[, psi := rnorm(.N, psi_mean[k], sd = 0.5), by = .(k)]
	data[, alpha  := rnorm(.N, match[l,k,gender], sd = abs(alpha_sd[l])), by = .(k,l,gender)]

  # as a final step, I shift the entire effects to have a positive minimum
  # For alpha
  min_alpha = min(data$alpha)
  if(min_alpha < 0) {
      data[, alpha := alpha - min_alpha + 0.01]
  }

  # For psi
  min_psi = min(data$psi)
  if(min_psi < 0) {
      data[, psi := psi - min_psi + 0.01]
  }

  data = data[, .(i, l, k, t, fid, spell, gender, age, educ, experience, psi, alpha)]
  # female is 1, male is 0 (making the var binary)
  data$gender = data$gender - 1

  # Final wrangling for data creation
  #-------------------------------------------------------------------------------------------------

  # year shock dummies, just an uniform distribution. For simplicity, my model is static for now
  shocks = runif(nt, -0.01, 0.05)
  data[, time_fe := shocks[t]]

  data[, lw  := 0.07 * educ + 0.02 * experience - 0.0005 * experience^2 + 
                  alpha + psi + time_fe + w_sigma * rnorm(.N)]

  # Creating the labor market simulator type of class
  #-------------------------------------------------------------------------------------------------

  # Define the constructor
  create_labor_m = function(panel, init.params, tranM, steadyM, alpha_mean, psi_mean, psi_sd, alpha_sd, csort, cnetw, csig, fsize, w_sigma, neduc, sort_gap, shocks) {
      new("LaborMarket", 
          panel = panel,
          init.params = init.params,
          tranM = tranM,
          steadyM = steadyM,
          alpha_mean = alpha_mean,
          psi_mean = psi_mean,
          psi_sd = psi_sd,
          alpha_sd = alpha_sd,
          csort = csort,
          cnetw = cnetw,
          csig = csig,
          fsize = fsize,
          w_sigma = w_sigma,
          neduc = neduc,
          sort_gap = sort_gap,
          shocks = shocks
      )
  }

  # create the instance of the class
  lmarket = create_labor_m(data, list(nk = nk, nl = nl, nt = nt, ni = ni, ratiog = ratiog, lambda = lambda), G, H, alpha_mean, psi_mean, psi_sd, alpha_sd, csort, cnetw, csig, fsize, w_sigma, neduc, sort_gap, shocks)

  # output the class
  print(lmarket)
  invisible(lmarket)

}
