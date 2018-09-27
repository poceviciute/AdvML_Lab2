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
diag(emission_probs[-(1:2),]) <- rep(e_prob, n-2)
diag(emission_probs[1:2,(n-1):n]) <- rep(e_prob, 2)
emission_probs[1,n] <- e_prob
emission_probs <- emission_probs + t(emission_probs) - diag(emission_probs)
rowSums(emission_probs)==1

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

hmm_probs <- function(hmm_model, hmm_obs, states){
  smoothed_prob <- prop.table(exp(backward(hmm = hmm_model, observation = hmm_obs)))
  smoothed_path <- as.vector(apply(smoothed_prob, 2, function(x){states[which.max(x)]}))
  filtered_prob <- prop.table(exp(forward(hmm = hmm_model, observation = hmm_obs)))
  filtered_path <- as.vector(apply(filtered_prob, 2, function(x){states[which.max(x)]}))
  most_probable_path <- viterbi(hmm = hmm_model, observation = hmm_obs)
  return(list("smoothed"=smoothed_prob,"smoothed_path"=smoothed_path, "filtered"=filtered_prob, "filtered_path"=filtered_path,"viterbi_path"=most_probable_path))
}

hmm_probs1<-hmm_probs(hmm_init, hmm_sim1$observation, z_states)
hmm_probs1$smoothed_path
hmm_probs1$filtered_path
hmm_probs1$viterbi_path

#4
path_comp <- function(path1, path2){
  return(table(path1==path2))
}

true_path <- hmm_sim1$states
path_comp(hmm_probs1$smoothed_path, true_path)
path_comp(hmm_probs1$filtered_path, true_path)
path_comp(hmm_probs1$viterbi_path, true_path)

#5

set.seed(12345)
hmm_sim2 <- simHMM(hmm_init, length=100)
hmm_probs2<-hmm_probs(hmm_init, hmm_sim2$observation, z_states)
path_comp(hmm_probs2$smoothed_path, true_path)
path_comp(hmm_probs2$filtered_path, true_path)
path_comp(hmm_probs2$viterbi_path, true_path)

set.seed(98765)
hmm_sim3 <- simHMM(hmm_init, length=100)
hmm_probs3<-hmm_probs(hmm_init, hmm_sim3$observation, z_states)
path_comp(hmm_probs3$smoothed_path, true_path)
path_comp(hmm_probs3$filtered_path, true_path)
path_comp(hmm_probs3$viterbi_path, true_path)

#6


#7

