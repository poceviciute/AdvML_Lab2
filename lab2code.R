# Advanced machine learning

#Lab 2

# 10 sectors
# equal prob. if stays or moves to next sector
# will report that robot is in a sector in [i-2,i+2] with equal prob. when it's in i
# i-2,i-1,i,i+1,i+2 1/5=0.2
library(HMM)

#1
n <- 10 #number of states

#states are hidden
z_states <- c(paste0("z", 1:n))

#symbols
x_symbols <- c(paste0("x", 1:n))

#emissions are observed
e_prob <- 0.2
emission_probs <- e_prob*diag(nrow = n)
diag(emission_probs[-1,]) <- rep(e_prob, n-1)
diag(emission_probs[3:n,]) <- rep(e_prob, n-2)
diag(emission_probs[1:2,(n-1):n]) <- rep(e_prob, 2)
emission_probs[1,n] <- e_prob
emission_probs2 <- t(emission_probs)
emission_probs <- emission_probs + emission_probs2 - e_prob*diag(nrow = n)

t_prob <- 0.5
trans_probs <- t_prob*diag(nrow = 10)
diag(trans_probs[-1,]) <- rep(t_prob, n-1)
trans_probs <- t(trans_probs)
trans_probs[n,1] <- t_prob

hmm_init <- initHMM(z_states, x_symbols, transProbs = trans_probs, emissionProbs = emission_probs)

#2
set.seed(123456789)
hmm_sim1 <- simHMM(hmm_init, length=100)

#3

hmm_probs <- function(hmm_model, hmm_obs){
  smoothed_prob <- 0
  filtered_prob <- 0
  most_probable_path <- 0
  return(data.frame("smoothed"=smoothed_prob, "filtered"=filtered_prob, "probable path"=most_probable_path))
}

hmm_probs(hmm_init, hmm_sim1$observation)

#4


#5


#6


#7

