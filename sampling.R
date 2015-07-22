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


