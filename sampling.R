# i think using sample from the sample function with probability vector 
# supplied by the dph function would suffice, and there wouldn't 
# be a need to use some accept-reject sampling as the sample space
# is finite 


# -------- sampling the pooled-hypergeometric
# testing input 

source('dph.R');

theta_obs <- 0.01

alpha<-0.05

  
# parameters for model
k <- 100; m <- 20; N <- 5000; theta = 0.021; delta <- theta*N; 
# for sample


# start off with naive estimator of delta

# delta is predetermined for sample size estimation
delta_0 <- floor(theta*N)

r <- max(0, m-(N-delta_0)/k):min(m, delta_0)

# calculate the densities
probs <- dph(r, k, m, N, theta = delta_0/N)

#grab a random sample
sampler  <- . %>% sample(prob=dph(.,k,m,N,theta)) 
samps <- sample(r, size=m*k, replace=T, prob=dph(r, k, m, N, theta=delta_0/N))

delta_samps <- numeric(length(samps))
## maximizer function
## 
##
maxim<-function(r, k, m, N){
  # check r
  
  # intialize an empty vector
  delta <- numeric(N)
  
  # if for a chosen r and 
  # delta, etc if the delta is 
  # such that the r is not in the support, 
  # then set the delta to NA
  for(d in 1:N){
    if(min(m, d-1) >= r &
      max(0, m - (N-(d-1))/k) <= r){
      delta[d] <- d-1
      
    }
    else{
      delta[d] <- NA
    }
  }
  
  delta <- delta[-is.na(delta)]
  # if the parameter
  # is not in the possible parameter
  # space, then set the likelihood = 0 
  l <- numeric(length(delta))
  for(d in 1:length(delta)){
    if(is.na(delta[d])){
      l[d] <- 0
    }else{
      l[d] <- dph(r,k,m,N,theta = delta[d]/N)
    }
  }
  
  mle <- delta[which.max(l)]
  return(mle)
}

maxim_fun <- function(samps, k, m, N){
  
  uni_samps <- unique(samps)
  
  delta_samples <- unlist(
    lapply(1:length(uni_samps), FUN=function(j){
      delta <- maxim(uni_samps[j], k, m, N) 
      return(delta)
  }))
  
  ## ---- map the unique samples
  delta <- numeric(length(samps))
  
  for(s in 1:length(uni_samps)){
    s.which <- which(samps == uni_samps[s])
    delta[s.which] <- delta_samples[s]
  }
  return(delta)
}

delta_samps<-maxim_fun(samps,k,m,N)
delta_samps <- delta_samps[which(delta_samps < sd(delta_samps))]
theta_samps <- delta_samps


# --- interesting, both the mle and the 
# --- estimator have a huge bias problem

reject_samp <- function(r, k, m, N, shape1, shape2, C){
  ## rejection sampling 
  sim_beta <- C*rbeta(1, shape1=shape1, shape2=shape2)
  f <- dph(r,k,m,N,theta=sim_beta)*dbeta(sim_beta, shape1, shape2)
  U <- runif(1)
  
  alph <- f/(C*dbeta(sim_beta, shape1, shape2))
  if(alph >= U){
    return(sim_beta)
  }
  else{
    return(NA)
  }
}
#### --- 
# Determine the reject rate for levels of the constant
#
##
reject_rate <- numeric(length(seq(0.01, 1, by=0.01)))
for(C in seq(.01, 1, by=0.01)){
  reps <- unlist(
    replicate(10000, expr=reject_samp(r, k, m, N, shape1=0.5, shape2=0.5, C=C)))
    reject_rate[C*100] <- mean(is.na(reps))
}
C <- seq(.01, 1, by=.01)
reject <- data.frame(C, reject_rate)
library(ggplot2)
ggplot(aes(x=C, y = reject_rate), data=reject) + geom_point() + geom_line()


# this appears to indicate that c=0.01 may be the best for reducing the rejection rate


reps <- unlist(
  replicate(100000, expr=reject_samp(r, k, m, N, shape1=2, shape2=2, C=0.01)))

reps_comp <- reps[-which(is.na(reps))]
hist(reps_comp, breaks=1000)


mean(reps_comp > 0.001)