# ---- Version 0.1: I will make some adjustments to optimize
# the magrittr-ing at some later point, maybe, if I have time



# ----------- a tool for F#-like piping
library(magrittr)
# haven't used it explicitly yet, but may become
# useful in some code down the line

# ----------- Pooled hypergeometric distribution
dph <- function(r,k,m,N,theta){
  
  delta <- theta*N
  
  q <-  function(r, m,k,N,theta){    
    # assign all of the scalar values to fit into the dhyper function  
    i <- 0:r; m. <- N - delta; x. <- (m-r+i)*k; n. <- delta; k. <- (m-r+i)*k
  
    abs(choose(m,r)*sum((-1)^(i)*choose(r,i)*dhyper(x=x.,m=m.,n=n.,k=k.))) %>% return
  } 
  # if r is out of support
  if(r > min(m,delta) || r < max(0, m-(N-delta)/k)){
    stop("The number of deviant pools specified is not in support
         of probability density function, please see documentation")
  }
  if(is.vector(r) == FALSE){
    q(r,m,k,N, theta) %>% return
  }
  # -----------
  # return a vector of probability density
  # values associated with the 
  # corresponding quantile r values.
  # ----------- this hopefully does that!
  if(is.vector(r)){
    p <- lapply(r, FUN=function(x){
      # use do.call to pass the addition arguments to
      # the q-function that will calculate the probability
      # density for each argument of r

      do.call(what= "q", args=list(r=x, m=m, k=k, N=N, theta=theta)) %>% return 
    }
   )
  }
   p %>% unlist %>% return 
}

m <- 10
delta <- 5
N <- 2000
k <- 20

r <- max(0, m-(N-delta)/k):min(m,delta)


# maxmimizer

maxim<-function(r, k, m, N){
  # check r
  
  delta <- r:N
  l <- numeric(N)
  for(d in delta){
    print(d)
    if(r >= min(m,d) || r <= max(0, m-(N-d)/k)){
      l[d+1] <- NA
    }else{
      l[d+1] <- dph(r,k,m,N,theta = delta/N)
    }
  }
  
  mle <- delta[which.max(l)]
  return(l)
}

maxim(5, k, m, N)