Gibbs_Sampling <- function(X,initial,prior,iter) {
    
    

# (Initialisation)
n <- length(X)
mu1 <- initial$mu1
mu2 <- initial$mu2
pi <- initial$pi
delta1 <- prior$delta1
delta2 <- prior$delta2
lambda <- prior$lambda

loglik <- rep(NA, iter)
vec_mu1 <- rep(NA, iter)
vec_mu2 <- rep(NA, iter)

for(t in 1:(iter)){

# (Simulation des variables latentes z_i sachant X et mu)
P <- matrix(NA, length(X), 2)
P[,1]<-pi*dnorm(X,mu1,1)/(pi*dnorm(X,mu1,1)+(1-pi)*dnorm(X,mu2,1))
P[,2]<-(1-pi)*dnorm(X,mu2,1)/(pi*dnorm(X,mu1,1)+(1-pi)*dnorm(X,mu2,1))
Z <- apply(P,1,function(row)sample(1:2, size=1, prob=row))

# (Générer nouveaux candidats)
n1 <- sum(Z==1)
n2 <- sum(Z==2)

s1 <- sum(X[Z==1])
s2 <- sum(X[Z==2])

mu1.new <- rnorm(1, (lambda*delta1+s1)/(lambda+n1), 1/(lambda+n1))
mu2.new <- rnorm(1, (lambda*delta2+s2)/(lambda+n2), 1/(lambda+n2))

# (Mise à jour)
mu1 <- mu1.new
mu2 <- mu2.new

vec_mu1[t] <- mu1
vec_mu2[t] <- mu2

loglik[t] <- logvrais(X,list(pi=c(pi,1-pi), mu=c(mu1,mu2), sigma=c(1,1)))
}

res <- list(mu1=vec_mu1, mu2=vec_mu2, loglik=loglik)
return (res)
}

# (on crée une fonction pour la densité du mélange)

f = function(x, m1, m2, p) {
  return(p*dnorm(x, mean = m1, sd = 1)+(1-p)*dnorm(x, mean = m2, sd = 1))
}

Metropolis_Hastings = function(X, initial, prior, zeta, iter) {
  
  # (initialisation)
  n = length(X)
  mu1 = initial$mu1
  mu2 = initial$mu2
  pi = initial$pi
  delta1 = prior$delta1
  delta2 = prior$delta2
  lambda = prior$lambda
  loglik = rep(NA, iter)
  vec_mu1 = rep(NA, iter)
  vec_mu2 = rep(NA, iter)
  
  # (Itérations)
  for(t in 1:iter) {
      
    y1 = rnorm(mean = mu1, sd = zeta)
    y2 = rnorm(mean = mu2, sd = zeta)
    p1 = prod(f(X, mu1, mu2, pi)/f(X, y1, y2, pi))
      
    # (Taux d'acceptation)  
    rho1 = min(1, p*dnorm(x = y1, mean = delta1, sd = 1/lambda)/dnorm(x = mu1, mean = delta1, sd = 1/lambda))
    rho2 = min(1, p*dnorm(x = y2, mean = delta2, sd = 1/lambda)/dnorm(x = mu2, mean = delta2, sd = 1/lambda))
    
    
    u1 = runif(min = 0, max = 1)
    u2 = runif(min = 0, max = 1)
      
    if(u1 < rho1) {
      mu1 = y1
    }
    if(u2 < rho2){
      mu2 = y2
    }
    vec_mu1[t] = mu1
    vec_mu2[t] = mu2
  }
}
