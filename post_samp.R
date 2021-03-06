# post_samp.R
# This file contains the posterior sampling
# algorithm and sample size estimator
# for the bayesian model
source('dph.R')

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

post_samp <- function(r, m, N, theta_0, beta, thresh, num_iter){
   
    #### -----
    ## This part is or parallelization of the sampling of the posterior 
    #create cluster
    #library(parallel)
    #cl <- makeCluster(detectCores()-1, outfile="log4.txt")  
    #clusterEvalQ(cl, library(magrittr))
    #put objects in place that might be needed for the code
    effect <- thresh - theta_0
    #library(foreach)
    #library(doParallel)
    #registerDoParallel(cl)
    

    
   # --- start of algorithm
   
   # setting the bounds on k
    if( m - (N-N*theta_0)/k <= 0 ){
      k_max <- (N-N*theta_0)/m
    }else{
      k_max <- (N-N*theta_0)/(m-r) 
    }
   
   
   # set shape1 s.t. the prior mode is equal to theta_0
   
   shape_1 <- (1 + theta_0*beta - 2*theta_0)/(1-theta_0)
   k <- 2:(k_max - 1)
   # clusterExport(cl=cl, list("reject_samp", "r", "N", "theta_0", "beta", 
    #                         "effect", "thresh", "num_iter", "dph", "k"),
    #             envir=environment())
   
    p <- lapply(k, FUN = function(size){
      # lapply performs parallelized sampling on
      # the posterior distribution specified using
      # the reject_samp function
      samps <- replicate(num_iter, expr=reject_samp(r,k=size,m,N, 
                           shape1=shape_1, shape2=beta, C = 0.01), simplify="array")
      rr <- mean(is.na(samps))
      samps<- samps[-which(is.na(samps))]
      pow <- mean(samps <= effect) 
      print(pow)
      return(pow)
    })
   p <- unlist(p)
   k_maxi <- k[which.max(p)]
   p_max <- max(p)
   samps <- reject_samp(r, k_maxi, m, N, shape1=2, shape2=beta, C = 0.01)
   samps <- samps[-which(is.na(samps))]
   rr <- mean(is.na(samps))
   return(list('max seed lot size' = k_maxi, 'power' = p_max, 'samples' = samps,
                'reject rate' = rr))
  
   #stopCluster(cl)
}

## testing the rejection sampling and sample size estimator
r <- 2; m<-10; N <- 2000; theta_0 <- 0.001; beta<-2; thresh <- 0.005

res <- post_samp(r, m, N, theta_0, beta=2, thresh, num_iter=1000)


