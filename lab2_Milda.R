## QUestions for Jose
# Can the robot move one sector back? That is, if robot stands in sector i, then it can move to sectors i or i+1?
# If robot is at sector 10, is 10 + 1 = sector 1?
# Are smooth probabilities given by the backward algorithm (backward function)?


################################################################################
## Questions 1

# Preparing states and readings

grid_states <- c(paste0("Sector ",1:10))
Readings <- grid_states

# Building states probabilities
Tprobs1 <- c(rep.int(0.5,2),rep.int(0,8))
Tprobs2 <- c(0,rep.int(0.5,2),rep.int(0,7))
Tprobs3 <- c(rep.int(0,2),rep.int(0.5,2),rep.int(0,6))
Tprobs4 <- c(rep.int(0,3),rep.int(0.5,2),rep.int(0,5))
Tprobs5 <- c(rep.int(0,4),rep.int(0.5,2),rep.int(0,4))
Tprobs6 <- c(rep.int(0,5),rep.int(0.5,2),rep.int(0,3))
Tprobs7 <- c(rep.int(0,6),rep.int(0.5,2),rep.int(0,2))
Tprobs8 <- c(rep.int(0,7),rep.int(0.5,2),rep.int(0,1))
Tprobs9 <- c(rep.int(0,8),rep.int(0.5,2))
Tprobs10 <- c(0.5,rep.int(0,8),0.5)

transProbs <- matrix(c(Tprobs1,Tprobs2,Tprobs3,Tprobs4,Tprobs5,Tprobs6,Tprobs7,Tprobs8,Tprobs9,Tprobs10),ncol=10,nrow=10)

# Building Observed readings probabilities

probs1 <- c(rep.int(0.2,3),rep.int(0,5),rep.int(0.2,2))
probs2 <- c(rep.int(0.2,4),rep.int(0,5),rep.int(0.2,1))
probs3 <- c(rep.int(0.2,5),rep.int(0,5))
probs4 <- c(0,rep.int(0.2,5),rep.int(0,4))
probs5 <- c(0,0,rep.int(0.2,5),rep.int(0,3))
probs6 <- c(rep.int(0,3),rep.int(0.2,5),rep.int(0,2))
probs7 <- c(rep.int(0,4),rep.int(0.2,5),rep.int(0,1))
probs8 <- c(rep.int(0,5),rep.int(0.2,5))
probs9 <- c(0.2,rep.int(0,5),rep.int(0.2,4))
probs10 <- c(rep.int(0.2,2),rep.int(0,5),rep.int(0.2,3))

emissProbs <- matrix(c(probs1,probs2,probs3,probs4,probs5,probs6,probs7,probs8,probs9,probs10),ncol=10,nrow=10)

Robot_hmm <- initHMM(States=grid_states, Symbols=Readings, startProbs=rep.int(0.1,10), transProbs=transProbs, emissionProbs=emissProbs)

## Question 2
Robot_path <- simHMM(Robot_hmm,100)
Path_df <- data.frame(hidden_state=Robot_path$states,observation=Robot_path$observation)

## Question 3
# P(Zt|X1:t)

robot_moves <- function(hmm,xs){
    # Filtering
    forward_probs <- forward(hmm = hmm, observation = xs)
    
    # Smoothing - in wikipedia it says it should be forward-backward algorithm, but hmm package does not have it. 
    # Instead it has backward algorithm
    
    
    backward_probs <- backward(hmm = hmm, observation = xs)
    
    # Normalising the probabilities 
    e_forw <- exp(forward_probs)
    distr_forw <- prop.table(e_forw,2)
    e_back <- exp(backward_probs)
    distr_back <- prop.table(e_back,2)
    
    # The most likely path calculated by the Viterbi Algorithm
    viterbi_path <- viterbi(hmm = hmm, observation = xs)
    return(list(frw_probs = distr_forw, bwrd_probs = distr_back, viterbiP = viterbi_path))
}

path_finder <- function(line){
    # select the path with the highest probability
    m <- max(line)
    list_m <- which(line == m)
    
    if (length(list_m) != 1){
        # if some hav ethe same highest prob, randomly select one of them
        index <- sample(list_m,1)
    } else {
        index <- list_m
    }
    return(grid_states[index])
}

result3 <- robot_moves(Robot_hmm,Path_df$observation)




## Question 4

# Path calculated by hand based on the forward probabilities
forward_path <- apply(result3$frw_probs,2,path_finder)
True_path <- Path_df$hidden_state

plotdf1 <- data.frame(time = 1:100,vpath=result3$viterbiP,real=True_path)
library(ggplot2)
ggplot(plotdf1, aes(x=time))+
    geom_point(aes(y=vpath,colour="Viterbi"))+
    geom_point(aes(y=real,colour="Actual"), size = 2)

plot(x=plotdf1$time,y=plotdf1$vpath,type="l",col="red")
points(x=plotdf1$time,y=plotdf1$vpath,col="red")
lines(x=plotdf1$time,y=plotdf1$real,col="blue")
points(x=plotdf1$time,y=plotdf1$real,col="blue")

# Path calculated by hand based on the backward probabilities
backward_path <- apply(result3$bwrd_probs,2,path_finder)

accuracy <- function(pathA, trueP){
    compare <- pathA==trueP
    t <- table(compare)
    accur <- t[2]/(sum(t)) 
    return(list(accuracy=accur,table=t))
}


result_f <- accuracy(forward_path,True_path)
result_v <- accuracy(viterbi_path,True_path)
result_b <- accuracy(backward_path,True_path)
    

## Question 5

sim_probs <- function(hmm,sampleN){
    
    print(sampleN)
    simulation <- simHMM(hmm=hmm,length=sampleN)
    
    print("after simulation")
    xs <- simulation$observation
    # Filtering
    print("before filtering")
    forward_probs <- forward(hmm = hmm, observation = xs)
    
    # Smoothing - in wikipedia it says it should be forward-backward algorithm, but hmm package does not have it. 
    # Instead it has backward algorithm
    print("before smoothing")
    backward_probs <- backward(hmm = hmm, observation = xs)
    
    print("before normalisation")
    # Normalising the probabilities 
    e_forw <- exp(forward_probs)
    distr_forw <- prop.table(e_forw,2)
    e_back <- exp(backward_probs)
    distr_back <- prop.table(e_back,2)
    
    return(list(Xs=xs,distr_forw=distr_forw,distr_back=distr_back,Zs=simulation$states))
}

do_everything <- function(step_a,hmm){
    #step_a <- sim_probs(hmm,sampleN)
    print("before path finders")
    # The most likely path calculated by the Viterbi Algorithm
    print(hmm)
    print(step_a$Xs)
    v_path <- viterbi(hmm = hmm, observation = step_a$Xs)
    # Path calculated manually based on the forward and backward probabilities
    f_path <- lapply(step_a$distr_forw,path_finder)
    b_path <- lapply(step_a$distr_back,path_finder)
    print("before true states")
    True_p <- step_a$Zs
    print("before accurancy")

    accurF <- accuracy(f_path,True_p)
    accurB <- accuracy(b_path,True_p)
    accurV <- accuracy(v_path,True_p)
    print("before return")
    return(list(resultF = accurF,resultB = accurB,resultV = accurV))
    #return(list(resultF = accurF,resultB = accurB))
}




# Simulate with different sample sizes
samples <- seq(500,10000,by=500)
n <- length(samples)
robot_sim <- lapply(samples, do_everything,hmm=Robot_hmm)
#robot_10000 <- simHMM(Robot_hmm,10000)

aaa <- sim_probs(Robot_hmm,500)
bbb <- forward(hmm = Robot_hmm, observation = aaa$Xs)
e_bbb <- exp(bbb)
distr_bbb <- prop.table(e_bbb,2)
aaa2 <- do_everything(aaa,Robot_hmm)

f_path <- lapply(aaa$distr_back,path_finder)

forward_path100 <- apply(result3$frw_probs,2,path_finder)

# Why forward and backward algorithms produces NaN for some observations?.. Is my methods wrong?
