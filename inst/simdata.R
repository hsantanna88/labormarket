#' Simulating Employer-Employee Matched Dataset
#'
#' This code is inspired by https://floswald.github.io/ScPo-Labor/lab-akm.html
#'
library(data.table)
library(reshape)
library(lattice)
library(gridExtra)
library(ggplot2)
library(futile.logger)
library(feather)
library(crayon)

dnorm(0, sd = 1)

# Function to create a simulated labor market
simlabormarket <- function(nk = 30, nl = 10, alpha_sd = 1, psi_sd = 1, lambda = 0.05, csort = 0.5,
                            nt = 5, ni = 100000, w_sigma = 0.5,
                            cnetw = 0.2, csig = 0.5, pl = FALSE) {


	# Starting point for making firms
	fsize = 10

  # Drawing the fix effects
  psi   = qnorm(1:nk/(nk+1)) * psi_sd
  alpha = qnorm(1:nl/(nl+1)) * alpha_sd


  # Generating the transition matrix based on the sorting effect and network effect
  # Observe that the transition matrix is a 3D array
  # It also leverages the property of a normal distribution by using dnorm(0) as the highest probability
  G = array(0,c(nl,nk,nk))
  for (l in 1:nl) for (k in 1:nk) {
    # prob of moving is highest if dnorm(0)
    G[l,k,] = dnorm(psi - cnetw * psi[k] - csort * alpha[l], sd = csig)

    # normalize to get transition matrix
    G[l,k,] = G[l,k,]/sum(G[l,k,])
  }



  # we then solve for the stationary distribution over psis for each alpha value
  H = array(1/nk,c(nl,nk)) # make a initial matrix for L x K
  for (l in 1:nl) for (i in 1:100) {
    H[l,] = t(G[l,,]) %*% H[l,]
  }

  # plotting the transition matrix and the steady-state matrix
  if(pl) {
    Plot1 = wireframe(G[1,,],aspect = c(1,1),xlab = "previous firm",ylab="next firm")
    Plot2 = wireframe(G[nl,,],aspect = c(1,1),xlab = "previous firm",ylab="next firm")
    grid.arrange(Plot1, Plot2,nrow=1)

    wireframe(H,aspect = c(1,1),xlab = "worker",ylab="firm")
  }


  # we simulate a panel
  network    = array(0,c(ni,nt))
  spellcount = array(0,c(ni,nt))
  A = rep(0,ni)

  for (i in 1:ni) {
    # we draw the worker type
    l = sample.int(nl,1)
    A[i]=l
    # at time 1, we draw from H
    network[i,1] = sample.int(nk,1,prob = H[l,])
    for (t in 2:nt) {
      if (runif(1)<lambda) {
        network[i,t] = sample.int(nk,1,prob = G[l,network[i,t-1],])
        spellcount[i,t] = spellcount[i,t-1] +1
      } else {
        network[i,t]    = network[i,t-1]
        spellcount[i,t] = spellcount[i,t-1]
      }
    }
  }

	data = data.table(melt(network, c('i', 't')))
	data2 = data.table(melt(spellcount, c('i', 't')))
	setnames(data, "value", "k")

	data[, spell := data2$value]
	data[, l := A[i], i]
	data[, alpha := alpha[l], l]
	data[, psi := psi[k], k]

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

  # Final wrangling for data creation
  #-------------------------------------------------------------------------------------------------

  #data[, age := sample(18:65,.N,replace=TRUE)]
  data[, lw  := alpha + psi + w_sigma * rnorm(.N) ]
  data[, lw  := lw - min(lw) + 0.1]


	format_number <- function(n) {
		# Define thresholds
		billion <- 1e9
		million <- 1e6
		thousand <- 1e3
		
		# Check and format accordingly
		if (n >= billion) {
			return(paste0(round(n/billion, 1), "G"))
		} else if (n >= million) {
			if (n %% million == 0) {
				return(paste0(n/million, "M"))
			} else {
				return(paste0("~", round(n/million), "M"))
			}
		} else if (n >= thousand) {
			if (n %% thousand == 0) {
				return(paste0(n/thousand, "k"))
			} else {
				return(paste0("~", round(n/thousand), "k"))
			}
		} else {
			return(as.character(n))
		}
	}

  
	cat(blue("\nLabor Market Simulated."), red("Features: \n"))
	cat(green("-------------------------------------------------\n\n"))
	cat("Types of Firms: ", nk, "\n")
	cat("Types of Workers: ", nl, "\n")
	cat("Time Periods: ", nt, "\n")
	cat("Number of Individuals: ", format_number(ni), "\n")
	cat("Number of Firms: ", length(unique(data$fid)), "\n")
	cat("Total Observations: ", format_number(ni*nt), "\n")
	cat(green("-------------------------------------------------\n\n"))
	cat("First Rows: \n")
	print(head(data))

	return(data)
}
