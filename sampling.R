# i think using sample from the sample function with probability vector 
# supplied by the dph function would suffice, and there wouldn't 
# be a need to use some accept-reject sampling as the sample space
# is finite 


# -------- sampling the pooled-hypergeometric
# testing input 

source('dph.R');

theta_obs <- 0.01

alpha<-0.05

B <- 2

boot_iter <- 1:B
t_boot <- unlist(lapply(boot_iter, FUN=function(x){
  
  # parameters for model
  k <- 100; m <- 20; N <- 5000; theta = 0.021; delta <- theta*N; 
  # for sample
  r <- max(0, m-(N-delta)/k):min(m, delta)
  
  # calculate the densities
  probs <- dph(r, k, m, N, theta)
  
  #grab a random sample
  sampler  <- . %>% sample(prob=dph(.,k,m,N,theta)) 
  samps <- sample(r, size=m*k, replace=T, prob=dph(r, k, m, N, theta))
  
  # convert to theta hat
  theta_hat <- function(r,k,m){
    1-(1-r/m)^(1/k) %>% return
  }
  
  theta_samps <- theta_hat(samps, k, m)
  return(theta_samps)
  
}))


q_lo <- quantile(t_boot, alpha/(2*B))
q_up <- quantile(t_boot, (1-alpha/(2*B)))

# the bootstrap confidence interval
conf_int <- c(min(0, 2*theta_obs - q_lo), max(1, 2*theta_obs - q_up))

