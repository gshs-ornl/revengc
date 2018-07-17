# #A dpois_trunc gives right-truncation Poisson probabilities
# dpois_trunc<-function(n,lam,upperbound){
#   final = dpois(n,lam)*(1/(1-(ppois((upperbound-1),lam,lower.tail=FALSE))))
#   return(final)} #end dpois_trunc
# 
# #"ppois_truc" is a right truncated Poisson distribution function
# #This will be used in loglikelihood_univariatecase function
# 
# ppois_trunc<-function(n,lam,upperbound,lower.tail){
#   if (lower.tail==FALSE) {
#     x_to_up<-data.frame((n+1):upperbound) #greater than (>). NOT greater than AND equal to (>= or +)
#     dtsum=NULL
#     for (i in 1:nrow(x_to_up)){
#       dtsum[i]=dpois_trunc(x_to_up[i,],lam,upperbound)}
#     return(sum(dtsum))
#   }
# 
#   if (lower.tail==TRUE) {
#     zero_to_n<-data.frame(0:n) #less than AND equal to (<=). NOT less than (<)
#     dtsum=NULL
#     for (i in 1:nrow(zero_to_n)){
#       dtsum[i]=dpois_trunc(zero_to_n[i,],lam,upperbound)}
#     return(sum(dtsum))
#   }
# 
# } #end ppois_truc function


# dqpois = function(n, lambda, quasipoisson_phi) {
#   mu = lambda
#   k = mu/(quasipoisson_phi - 1)
#   r = dnbinom(n, mu = mu, size = k)
#   return(r)
# }
# 
# pqpois = function(n, lambda, quasipoisson_phi, lower.tail) {
#   mu = lambda
#   k = mu/(quasipoisson_phi - 1)
#   r = pnbinom(n, mu = mu, size = k, lower.tail = lower.tail)
#   return(r)
# }
# 
# 

# #A dpois_trunc gives right-truncation Poisson probabilities with quasipoisson_phi value
dqpois_trunc<-function(n,lambda,quasipoisson_phi, upperbound){
  dqpois = function(n, lambda, quasipoisson_phi) {
    mu = lambda
    k = mu/(quasipoisson_phi - 1)
    r = dnbinom(n, mu = mu, size = k)
    return(r)
  }

  pqpois = function(n, lambda, quasipoisson_phi, lower.tail) {
    mu = lambda
    k = mu/(quasipoisson_phi - 1)
    r = pnbinom(n, mu = mu, size = k, lower.tail = lower.tail)
    return(r)
  }
  density.output = dqpois(n,lambda, quasipoisson_phi)*(1/(1-(pqpois((upperbound-1),lambda,quasipoisson_phi, lower.tail=FALSE))))
  return(density.output)} #end dpois_trunc_quasi
# 
# #"ppois_truc" is a right truncated Poisson distribution function
# #This will be used in loglikelihood_univariatecase function
# ppois_trunc_quasi<-function(n,lambda,quasipoisson_phi, upperbound,lower.tail){
#   if (lower.tail==FALSE) {
#     x_to_up<-data.frame((n+1):upperbound) #greater than (>). NOT greater than AND equal to (>= or +)
#     dtsum=NULL
#     for (i in 1:nrow(x_to_up)){
#       dtsum[i]=dqpois_trunc(x_to_up[i,],lambda,quasipoisson_phi, upperbound)}
#     return(sum(dtsum))
#   }
#   
#   if (lower.tail==TRUE) {
#     zero_to_n<-data.frame(0:n) #less than AND equal to (<=). NOT less than (<)
#     dtsum=NULL
#     for (i in 1:nrow(zero_to_n)){
#       dtsum[i]=dqpois_trunc(zero_to_n[i,],lambda,quasipoisson_phi, upperbound)}
#     return(sum(dtsum))
#   }
#   
# } #end ppois_truc_quasi function