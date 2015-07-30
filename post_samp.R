# post_samp.R
# This file contains the posterior sampling
# algorithm and sample size estimator
# for the bayesian model


reject_samp <- function(r, k, m, N, shape1, shape2, C){
  ## rejection sampling 
  sim_beta <- C*rbeta(1, shape1=shape1, shape2=shape2)
  f <- dph(r,k,m,N,theta=sim_beta)*dbeta(sim_beta, shape1, shape2)
  U <- runif(1)
  alph <- f/(C*dbeta(sim_beta, shape1, shape2))
  if(is.null(alph)){
    alph <-0
  }
  if(alph >= U){
    return(sim_beta)
  }
  else{
    return(NA)
  }
}


post_samp <- function(r, m, N, theta_0, beta, thresh){
 shape_1 <- (theta_0/(1-theta_0))*beta
 
 effect <- thresh - theta_0
 print(effect)
 # --- start of algorithm
 
 # setting the bounds on k
  if( m - (N-N*theta_0)/k <= 0 ){
    k_max <- (N-N*theta_0)/m
  }else{
    k_max <- (N-N*theta_0)/(m-r)
  }
 
  # k <- 1:k_max
  k <- 1:20
  p <- lapply(k, FUN=function(size){
   print(size)
   samps <- replicate(1,expr=reject_samp(r,k=size, m , N, shape1= 2, shape2=2, C = 0.01))
   rr <- mean(is.na(samps))
   samps<- samps[-which(is.na(samps))]
   print(samps)
   p <- mean((samps <= effect)) 
   return(p)
  })
 p <- unlist(p)
 k_maxi <- k[which.max(p)]
 p_max <- max(p)
 print(p)
 samps <- reject_samp(r, k_maxi, m, N, shape1=2, shape2=2, C = 0.01)
 samps <- samps[-which(is.na(samps))]
 rr <- mean(is.na(samps))
 return(p)
 #return(list('max seed lot size' = k_maxi, 'power' = p_max, 'samples' = samps,
 #             'reject rate' = rr))

}


## testing the rejection sampling and sample size estimator
r <- 2; m<-10; N <- 2000; theta_0 <- 0.01; beta<-10000; thresh <- 0.05

res <- post_samp(r, m, N, theta_0, beta, thresh)


## test

beta<- 1
theta_0 <- 0.01
alph <- (theta_0/(1-theta_0))*beta
eps <- .95
delt <- 1
crit <- abs(alph/beta - 1) > eps
while(crit){
  beta <- beta + delt
  alph <- (theta_0/(1-theta_0))*beta
  crit <- abs(alph/beta - 1) > eps
}


