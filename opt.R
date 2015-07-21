# normal distribution approximation for the sample
# size using the normal theory wald style interval
# inversion with a finite sample size correction.

# The derivation of this technique is given in the documentation


# -------------- setting up the functionals 

require("magrittr")

# implementation of the normal theory 
# interval inversion estimate
# this function steps through candidate values of 
# k (the group size, assuming symmetric group sizes)
# and selects the minimal group size that reduces the confidence
# bounds below the precision amount specified //

opt_group_size <- function(eps, N, m, theta_hat, alpha, k_0 = 1, k_max, type="AC"){
  
  # -------------- checking the inputs for sanity
  if(eps <= 0 || eps > 1){
    stop("precision is out of bounds")
  }  
  if(theta_hat > 1 || theta_hat < 0){
    stop("proportion estimate is out of bounds")
  }
  if(m <= 0 || m >= N){
    stop("The number of groups certainly cannot be larger than
         the population size, no?")
  }
  if(!is.character(type)){
    stop("Please enter a character string")
  }
  if(k_max >= N/m){
    stop("The maximum group size cannot be larger than the population size 
         divided by the number of groups")
  }
 
  # constants
  z <- qnorm(1-alpha/2)
  
  
  if(type == "wald"){
    # setting up a grid of k-values, passing these to the def
    # of the confidence bound, then find the one that 
    # reduces ci below 
    
    ci_bound <- function(z,k,m,N,theta_hat){
      (z*sqrt(((1-theta_hat)^(k) - (1-theta_hat)^(2*k))/m*(N-m*k)/(N-k))) %>% return
    }
  
    # set up a grid of group sizes
    k <- k_0:k_max;
    
    # calculate the confidence intervals and then return them
    cis <- 1-(1-ci_bound(z, k,m, N, theta_hat))^(1/k)
    
    if(mean(cis <= eps) == 0){
      # return NA's if the optimal group size DNE
     return(list(optimal_group_size = NA, size_of_ci = NA))
    }else{
      # create a list of results 
     return(list(optimal_group_size = min(which(cis <= eps)), 
            size_of_ci = cis[min(which(cis <= eps))])) 
    }
  }
  
  
  if(type == "AC"){
    
    
    
    # do the necessary transformations
    m_t <- m+4; f_t <- m_t*k/N;
    r <- m*(1-(1-theta_hat)^k)
    
    if(k_max >= N/m_t){
      k_max <- (m/m_t)*k_max
    }
    
    theta_kt <- (r+2)/m_t
      
    ci_bound <- function(z, m_t, f_t, p_t){
      z*sqrt(1-f_t)*sqrt(p_t*(1-p_t)/(m_t-1)) %>% return
    }
    
    # if there are no confidence bounds that are smaller than the precision, 
    # then just return the smallest confidence bound with corresponding
    # group size
    
    cis <-1-(1-ci_bound(z,m_t,f_t, p_t = theta_kt))^(1/k)
    
    # check that there is at least one confidence band that
    # is smaller than the precision otherwise, 
    # give the smallest confidence bound and group size
    
    if(mean(cis <= eps) == 0){
      return(list(optimal_group_size = NA, size_of_ci = NA))
    }
    else{
      return(list(optimal_group_size = min(which(cis <= eps)), 
                  size_of_ci = cis[min(which(cis <= eps))])) 
    }
  }
}

theta_hat <- 0.001

# do the necessary transformations
m_t <- m+4; f_t <- m_t*k/N;
r <- m*(1-(1-theta_hat)^k)

if(k_max >= N/m_t){
  k_max <- N/m_t
}

theta_kt <- (r+2)/m_t

ci_bound <- function(z, m_t, f_t, p_t){
  z*sqrt(1-f_t)*sqrt(p_t*(1-p_t)/(m_t-1)) %>% return
}

# if there are no confidence bounds that are smaller than the precision, 
# then just return the smallest confidence bound with corresponding
# group size

cis <- 1 - (1-ci_bound(z,m_t,f_t, p_t = theta_kt))^(1/k)

opt_group_size(eps=0.005, N=3000, m=30,theta_hat=0.01, alpha=0.10,k_0=10, k_max=30, type="wald")
