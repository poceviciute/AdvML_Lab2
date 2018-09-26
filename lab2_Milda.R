## QUestions for Jose
# Can the robot move one sector back? That is, if robot stands in sector i, then it can move to sectors i or i+1?
# If robot is at sector 10, is 10 + 1 = sector 1?



################################################################################
## Questions 1

robot_moves <- function(init_pos){
    init_pos <- as.numeric(init_pos)
    if (init_pos == 10){
        states <- c(init_pos,1)   
    }else{
        states <- c(init_pos,init_pos+1)  
    }
    transProbs <- matrix(rep.int(0.5,4),2)
    
}
# Preparing states and readings

grid_states <- c()
a< -c()
for (i in 1:10){
    a <- paste0("Sector ",i)
    grid_states[i] <- a
}

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

# Filtering
Path_df$observation
forward_probs <- forward(hmm = Robot_hmm, observation = Path_df$observation)

# Path calculated by hand based on the forward probabilities
the_path <- apply(forward_probs,2,function(line){
    # select the path with the highest probability
    index <- which.max(line)
    return(grid_states[index])
})



# Smoothing - in wikipedia it says its hsould be forward-backward algorithm, but hmm package does not have it. 
# Instead it has backward algorithm


backward_probs <- backward(hmm = Robot_hmm, observation = Path_df$observation)

# The most likely path calculated by the Viterbi Algorithm

viterbi_path <- viterbi(hmm = Robot_hmm, observation = Path_df$observation)
True_path <- Path_df$hidden_state

plotdf1 <- data.frame(time = 1:100,vpath=viterbi_path,real=True_path)
library(ggplot2)
ggplot(plotdf1, aes(x=time))+
    geom_point(aes(y=vpath,colour="Viterbi"))+
    geom_point(aes(y=real,colour="Actual"), size = 2)

plot(x=plotdf1$time,y=plotdf1$vpath,type="l",col="red")
points(x=plotdf1$time,y=plotdf1$vpath,col="red")
lines(x=plotdf1$time,y=plotdf1$real,col="blue")
points(x=plotdf1$time,y=plotdf1$real,col="blue")

## Question 4

# Normalising the probabilities 
e_forw <- exp(forward_probs)
distr_forw <- prop.table(e_forw,2)
e_back <- exp(backward_probs)
distr_back <- prop.table(e_back,2)


# Path calculated by hand based on the forward probabilities
forward_path <- apply(distr_forw,2,function(line){
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
})

# Path calculated by hand based on the backward probabilities
backward_path <- apply(distr_back,2,function(line){
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
})

compare1 <- forward_path==True_path
table(compare1)

compare2 <- viterbi_path==True_path
table(compare2)

compare3 <- backward_path==True_path
table(compare3)
