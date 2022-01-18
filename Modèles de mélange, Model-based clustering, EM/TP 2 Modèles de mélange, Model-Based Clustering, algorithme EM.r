library(MASS)
library(Rmixmod)
library(ggplot2)
library(cowplot)

modele_melange<-function(theta,x){
  mg<-numeric(length(x))
  for (i in 1:length(theta$pi)){
  mg<-mg+theta$pi[i]*dnorm(x,theta$mu[i],theta$sig[i])
  }
  return (mg)
}

theta_bimodale <- list(pi=c(1/2,1/2),mu=c(0,10),sig=c(1,1))
theta_unimodale <- list(pi=c(3/4,1/4),mu=c(0,1),sig=c(1,1))
theta_oblique <- list(pi=c(1/4,3/4),mu=c(0,5),sig=c(1,5))
theta <- list(pi=c(1/3,1/6,1/2), mu=c(0,5,10), sig=c(1,1,4))

x4 <- seq(min(theta_bimodale$mu)-10,max(theta_bimodale$mu)+10,0.1)
mod_bi <- modele_melange(theta_bimodale,x4)
plot_1 <- ggplot()+geom_line(aes(x=x4,y=mod_bi),col='black')+labs(title="Densité de mélange bimodale",y='Densité',x='x')

x3 <- seq(min(theta_unimodale$mu)-10,max(theta_unimodale$mu)+10,0.1)
mod_uni <- modele_melange(theta_unimodale,x3)
plot_2 <- ggplot()+geom_line(aes(x=x3,y=mod_uni),col='black')+labs(title="Densité de mélange unimodale",y='Densité',x='x')

x1 <- seq(min(theta_oblique$mu)-10,max(theta_oblique$mu)+10,0.1)
mod_ob <- modele_melange(theta_oblique,x1)
plot_3 <- ggplot()+geom_line(aes(x=x1,y=mod_ob),col='black')+labs(title="Densité de mélange oblique",y='Densité',x='x')

x2 <- seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
mod <- modele_melange(theta,x2)
plot_4 <- ggplot()+geom_line(aes(x=x2,y=mod),col='black')+labs(title="Densité de mélange 3 composantes",y='Densité',x='x')


cowplot::plot_grid(plot_1,plot_2,plot_3,plot_4)

par(mfrow=c(2,2))

curve(0.5*dnorm(x,0,1)+0.3*dnorm(x,0,1.3)+0.2*dnorm(x,0,0.5),col="red",from=-3,to=3,ylab="")  
title("Melange Gaussien Unimodale")
curve(0.5*dnorm(x,4,1)+0.3*dnorm(x,0,1)+0.2*dnorm(x,0,1.2),col="red",from=-2,to=6,ylab="") 
title("Melange Gaussien Bimodal")
curve(0.6*dnorm(x,0,1)+0.2*dnorm(x,2,1)+0.2*dnorm(x,2,2),col="red",from=-2,to=4,ylab="") 
title("Melange Gaussien Oblique")

mean<-runif(3,0,10)
var<-runif(3,0,4)

curve(1/3*dnorm(x,mean[1],var[1])+1/3*dnorm(x,mean[2],var[2])+1/3*dnorm(x,mean[3],var[3]),col="red",from=-14,to=14,ylab="") 
title("Melange Gaussien Aleatoire")

simul <- function(n=200,theta){
  res <- list(obs=numeric(n),group=numeric(n))
  obs <- rep(NA,n)
  group <- sample(c(1:length(theta$pi)),size=n,replace=TRUE,prob=theta$pi) 
  for (k in 1:length(theta$pi)){
    for (i in 1:n){
      if (group[i]==k){obs[i]<-rnorm(1,theta$mu[k],theta$sig[k])}
    }
  }
  res$obs <- obs
  res$group <- group
  return(res)}

theta_bimodale <- list(pi=c(1/2,1/2),mu=c(0,10),sig=c(1,1))
theta_unimodale <- list(pi=c(3/4,1/4),mu=c(0,1),sig=c(1,1))
theta_oblique <- list(pi=c(1/4,3/4),mu=c(0,5),sig=c(1,5))
theta <- list(pi=c(1/3,1/6,1/2), mu=c(0,5,10), sig=c(1,1,4))

Simul_bi <- simul(200,theta_bimodale)
Simul_univ <- simul(200,theta_unimodale)
Simul_ob <- simul(200,theta_oblique)
Simul <- simul(200,theta)

par(mfrow=c(2,2))

hist(Simul_bi$obs,main="Observations loi de mélange bimodale",ylab="nombre d'observations",xlab="Observations",breaks=20)

hist(Simul_univ$obs,main="Observations loi de mélange unimodale",ylab="nombre d'observations",xlab="Observations",breaks=20)

hist(Simul_ob$obs,main="Observations loi de mélange oblique",ylab="nombre d'observations",xlab="Observations",breaks=20)

hist(Simul$obs,main="Observations loi de mélange 3 composantes",ylab="densité",xlab="Observations",breaks=20,prob=TRUE)

x <- seq(min(theta$mu)-10,max(theta$mu)+10,0.1)

lines(x,modele_melange(theta,x),col='black', xlim = c(0,0.3),ylim = c(0,0.3))

lines(x,1/3*dnorm(x,0,1),col='red',xlim = c(0,0.3),ylim = c(0,0.3))

lines(x,1/6*dnorm(x,5,1),col='blue',xlim = c(0,0.3),ylim = c(0,0.3))

lines(x,1/2*dnorm(x,10,4),col='green',xlim = c(0,0.3),ylim = c(0,0.3))

A <- simul(theta=theta,n=200)

hist(A$obs,breaks=20,prob=TRUE,main='loi de mélange \n a trois composantes gaussienne',xlab='Observations', ylim = c(0,0.17))
lines(x,modele_melange(theta,x),col='black', xlim = c(0,0.3),ylim = c(0,0.3))
lines(x,1/3*dnorm(x,0,1),col='red',xlim = c(0,0.3),ylim = c(0,0.3))
lines(x,1/6*dnorm(x,5,1),col='blue',xlim = c(0,0.3),ylim = c(0,0.3))
lines(x,1/2*dnorm(x,10,4),col='green',xlim = c(0,0.3),ylim = c(0,0.3))

simula<-c()
for (i in 1:200){
  z<-runif(1,0,1)
  if(z<1/6){
    simula<-c(simula,rnorm(1,5,1))
  }
  else if(z<1/2){
    simula<-c(simula,rnorm(1,0,1))
  }
  else{
    simula<-c(simula,rnorm(1,10,4))
  }
}

plot(simula,0.5*dnorm(simula,10,4)+1/3*dnorm(simula,0,1)+1/6*dnorm(simula,5,1),ylim=c(0,0.15),ylab="",xlab="")

curve(0.5*dnorm(x,10,4)+1/3*dnorm(x,0,1)+1/6*dnorm(x,5,1),add=T,col="red",from=-5,to=20,ylab="")

title("Simulation d'un Mélange Gaussien et graphe de sa densité")

loi_1<-c()
loi_2<-c()
loi_3<-c()

for (i in 1:200){
  z<-runif(1,0,1)
  if(z<1/6){
    loi_1<-c(loi_1,rnorm(1,5,1))
  }
  else if(z<1/2){
    loi_2<-c(loi_2,rnorm(1,0,1))
  }
  else{
    loi_3<-c(loi_3,rnorm(1,10,4))
  }
}

col<-c(rep(2,length(loi_1)),rep(3,length(loi_2)),rep(4,length(loi_3)))

plot(c(loi_1,loi_2,loi_3),0.5*dnorm(c(loi_1,loi_2,loi_3),10,4)+1/3*dnorm(c(loi_1,loi_2,loi_3),0,1)+1/6*dnorm(c(loi_1,loi_2,loi_3),5,1),ylim=c(0,0.15),col=col,ylab = "",xlab="")

curve(0.5*dnorm(x,10,4)+1/3*dnorm(x,0,1)+1/6*dnorm(x,5,1),add=T,col="orange",from=-5,to=20)

title("Simulation d'un Mélange Gaussien avec vrais classes")

legend(12,0.15,legend=c("N(5,1)","N(0,1)","N(10,4)"),col=c(2,3,4),lty=1,cex=0.8)


theta_unif <-list(pi= c(3/4,1/4) ,min = c(0,4), max = c(4,8))

y<-seq(min(theta_unif$min)-10,max(theta_unif$min)+10,0.1)

modele_melange_unif<-function(theta,y){
  mg<-numeric(length(y))
  for (i in 1:length(theta$pi)){
    mg<-mg+theta$pi[i]*dunif(y,theta$min[i],theta$max[i])
  }
  return (mg)
}


simul_unif<-function(n=200,theta){
  res<-list(obs=numeric(n),group=numeric(n))
  obs<-rep(NA,n)
  group<-sample(c(1:length(theta$pi)),size=n,replace=TRUE,prob=theta$pi) 
  for (k in 1:length(theta$pi)){
    for (i in 1:n){
      if (group[i]==k){obs[i]<-runif(1,theta$min[k],theta$max[k])}
    }
  }
  res$obs<-obs
  res$group<-group
  return(res)}

theta_unif <- list(pi= c(3/4,1/4) ,min = c(0,4), max = c(4,8))

B <- simul_unif(n=200,theta=theta_unif)

hist(B$obs,breaks=20,prob=TRUE,main='loi de mélange uniforme à deux\n composantes ',xlab='Observations')
mm <- modele_melange_unif(theta_unif,y)
points(y,mm,col='black', xlim = c(0,0.3),ylim = c(0,0.3))
lines(y,3/4*dunif(y,0,4),col='red',xlim = c(0,0.3),ylim = c(0,0.3))
lines(y,1/4*dunif(y,4,8),col='blue',xlim = c(0,0.3),ylim = c(0,0.3))

A <- simul(theta=theta,n=200)

v <- ggplot()+geom_histogram(aes(x=A$obs,y=..density..),col='grey',bins=20)+geom_line(aes(x=x,y=modele_melange(theta,x)),col='black')+geom_line(aes(x=x,y=1/3*dnorm(x,0,1)),col='red')+geom_line(aes(x=x,y=1/6*dnorm(x,5,1)),col='blue')+geom_line(aes(x=x,y=1/2*dnorm(x,10,4)),col='green')+geom_point(aes(x=A$obs,y=rep(0,200)),col=c('red','blue','green')[A$group])+labs(title='Avec les vraies classes',x='observations')

MAP <- function(obs,theta){
  groupe<-numeric(length(obs))
  for(j in 1:length(obs)){
    m<-c(dnorm(obs[j],theta$mu[1],theta$sig[1]),dnorm(obs[j],theta$mu[2],theta$sig[2]),dnorm(obs[j],theta$mu[3],theta$sig[3]))
    groupe[j]<-which.max(m)
  }
  return(groupe)
}

w <- ggplot()+geom_histogram(aes(x=A$obs,y=..density..),col='grey',bins=20)+geom_line(aes(x=x,y=modele_melange(theta,x)),col='black')+geom_line(aes(x=x,y=1/3*dnorm(x,0,1)),col='red')+geom_line(aes(x=x,y=1/6*dnorm(x,5,1)),col='blue')+geom_line(aes(x=x,y=1/2*dnorm(x,10,4)),col='green')+geom_point(aes(x=A$obs,y=rep(0,200)),col=c('red','blue','green')[MAP(A$obs,theta=theta)])+labs(title='Classes données par MAP',x='observations')

plot_grid(v,w)

MAP2 <- function(simula){
  f1<-1/6*dnorm(simula,5,1)/(1/6*dnorm(simula,5,1)+0.5*dnorm(simula,10,4)+1/3*dnorm(simula,0,1))
  f2<-1/3*dnorm(simula,0,1)/(1/6*dnorm(simula,5,1)+0.5*dnorm(simula,10,4)+1/3*dnorm(simula,0,1))
  f3<-0.5*dnorm(simula,10,4)/(1/6*dnorm(simula,5,1)+0.5*dnorm(simula,10,4)+1/3*dnorm(simula,0,1))
  
  if(f1>f2 && f1>f3){
    return (2)
  }
  else if(f2>f1 && f2>f3){
    return(3)
  }
  else{
    return(4)
  }
}
color_MAP<-as.numeric(Map(MAP2,c(loi_1,loi_2,loi_3)))


par(mfrow=c(2,1))
plot(c(loi_1,loi_2,loi_3),0.5*dnorm(c(loi_1,loi_2,loi_3),10,4)+1/3*dnorm(c(loi_1,loi_2,loi_3),0,1)+1/6*dnorm(c(loi_1,loi_2,loi_3),5,1),ylim=c(0,0.15),col=col,ylab = "",xlab="")
curve(0.5*dnorm(x,10,4)+1/3*dnorm(x,0,1)+1/6*dnorm(x,5,1),add=T,col="orange",from=-5,to=20)
title("Simulation d'un Mélange Gaussien avec vrais classes")
legend(12,0.15,legend=c("N(5,1)","N(0,1)","N(10,4)"),col=c(2,3,4),lty=1, cex=0.4)


plot(c(loi_1,loi_2,loi_3),0.5*dnorm(c(loi_1,loi_2,loi_3),10,4)+1/3*dnorm(c(loi_1,loi_2,loi_3),0,1)+1/6*dnorm(c(loi_1,loi_2,loi_3),5,1),ylim=c(0,0.15),col=color_MAP,ylab = "",xlab="")
curve(0.5*dnorm(x,10,4)+1/3*dnorm(x,0,1)+1/6*dnorm(x,5,1),add=T,col="orange",from=-5,to=20)
title("Simulation d'un Mélange Gaussien avec les classes MAP")
legend(12,0.15,legend=c("N(5,1)","N(0,1)","N(10,4)"),col=c(2,3,4),lty=1, cex=0.4)


lvraisnorm <- function(theta, obs){    
  logvrais <- sum(log(modele_melange(theta=theta, x=obs)))
  return(logvrais)
}

obs<-simul(1000,theta=theta)$obs
mu1<-seq(-2,2,by=0.1)
mu2<-seq(3,7,by=0.1)
L<-matrix(NA,nrow=length(mu1),ncol=length(mu2))
for (k in 1:length(mu1)){
  for (l in 1:length(mu2)) {
    theta$mu<-c(mu1[k],mu2[l],theta$mu[3])
    L[k,l]<-lvraisnorm(theta=theta,obs = obs)
  }
}

image(mu1,mu2,L)
contour(mu1,mu2,L,add=TRUE,nlevels=20,col='grey')

vrai<-function(obs,x,y){
  dens <-0
  for (k in 1:length(obs)){
    dens <- dens+log(( 0.5*dnorm(obs[k],10,4)+1/3*dnorm(obs[k],0,1)+1/6*dnorm(obs[k],x,y)))
  }                                         
  return(dens)
}

# On voit bien le maximum ici 
g <- expand.grid(x = seq(0,15,0.1), y = seq(1,8,0.1))

z<-vrai(simula,g$x,g$y)

plot3d(x=g$x,y=g$y,z=z,col = "blue",alpha=0.5,type = "l",xlab = "mu",ylab = "sigma",zlab = "vrai",
       main=paste0("Vraisemblance du modele de melange en fonction de deux parametres"))

max_vrai<-which (z==max(z))

points3d(g[max_vrai,1],g[max_vrai,2],z[max_vrai],c="red")

theta <- list(pi=c(1/2,1/2),mu=c(0,15),sig=c(2,2))

x <- seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
mod <- modele_melange(theta,x)
kd <- kde2d(x,mod)

par(mfrow=c(1,2))

image(kd,ylim=c(0,0.05))
contour(kd,add=TRUE)

observ1 <- simul(1000,theta)$obs
hist(observ1,xlab='observations',ylab="nombre d'occurences",main="Modele 1",breaks=20)

theta <- list(pi=c(1/2,1/2),mu=c(0,15),sig=c(4,4))
x <- seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
mod <- modele_melange(theta,x)
kd <- kde2d(x,mod)

par(mfrow=c(1,2))

image(kd,ylim=c(0,0.05))
contour(kd,add=TRUE)

observ1 <- simul(1000,theta)$obs
hist(observ1,xlab='observations',ylab="nombre d'occurences",main="Modele 2",breaks=20)

theta <- list(pi=c(3/4,1/4),mu=c(0,15),sig=c(2,2))
x <- seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
mod <- modele_melange(theta,x)
kd <- kde2d(x,mod)

par(mfrow=c(1,2))

image(kd,ylim=c(0,0.05))
contour(kd,add=TRUE)

observ1 <- simul(1000,theta)$obs
hist(observ1,xlab='observations',ylab="nombre d'occurences",main="Modele 3",breaks=20)

update.theta <- function(obs, theta){
  nb <- length(obs)
  K<-length(theta$pi)
  alpha <- matrix(NA, nrow=K, ncol=nb)
  for (j in 1:K){
    alpha[j,] <- theta$pi[j]*dnorm(obs,theta$mu[j],theta$sig[j])
  }
  p.theta <- apply(alpha, 2, 'sum')
  alpha   <- alpha/matrix(p.theta, byrow=TRUE, nrow=K, ncol=nb)
  
  pi.new  <- apply(alpha, 1, 'mean')
  mu.new  <- c(alpha%*%obs/nb/pi.new)
  mat     <- (matrix(rep(obs,each=K), nrow=K) -matrix(rep(mu.new, times=nb), nrow=K))^2
  sig.new <- sqrt(apply(alpha*mat, 1, 'mean')/pi.new)
  
  theta.new <-  list(pi =pi.new, mu=mu.new, sig=sig.new)
  return(theta.new)
}

algoEM <- function(obs, theta.init, R=200, epsilon=1e-3){   
  theta.old <- theta.init
  crit_arret <- FALSE
  log.vrais.old <- lvraisnorm(theta.init, obs)
  log<-c(log.vrais.old)
  it <- 0
  while (!crit_arret && (it < R)){ 
    theta.new <- update.theta(obs, theta.old)
    log.vrais.new <- lvraisnorm(theta.new, obs)
    crit_arret <- (abs((log.vrais.new - log.vrais.old)/log.vrais.old) < epsilon)
    log.vrais.old <- log.vrais.new
    theta.old <- theta.new
    it <- it + 1
    log<-c(log,log.vrais.old)
  }
  resultat <- list(emv = theta.new, nb.iter = it, logvraisemblance= log)
  return(resultat)
}

dessin<-function(theta,obs){
  x<-seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
  mg<-numeric(length(x))
  for (i in 1:length(theta$pi)){
    mg<-mg+theta$pi[i]*dnorm(x,theta$mu[i],theta$sig[i])
  }
  groupe<-numeric(length(obs))
  for(j in 1:length(obs)){
    m<-c(dnorm(obs[j],theta$mu[1],theta$sig[1]),dnorm(obs[j],theta$mu[2],theta$sig[2]),dnorm(obs[j],theta$mu[3],theta$sig[3]))
    groupe[j]<-which.max(m)
  }
  ggplot()+geom_line(aes(x=x,y=mg))+geom_point(aes(x=obs,y=rep(0,length(obs))),col=c('red','blue','green')[groupe])+labs(title=paste0("Densite d'un melange gaussien de parametre ",as.vector(theta)),x='observations')
}

theta <- list(pi=c(1/3,1/6,1/2), mu=c(0,5,10), sig=c(1,1,4))
obs <- simul(theta = theta,n=1000)
theta_init1 <- list(pi=c(8/10,1/10,1/10),mu=c(0,5,10),sig=c(1,1,4))

mod <- algoEM(obs=obs$obs,theta_init1)

print(mod$logvraisemblance)

theta_init1<-list(pi=c(1/3,1/6,1/2), mu=c(0,5,10), sig=c(1,1,4))

obs<-simul(theta = theta,n=1000)
res1<-algoEM(obs$obs,theta_init1)

ggplot()+geom_histogram(aes(obs$obs,y=..density..),bins=30,col='grey')+geom_line(aes(x=x,y=modele_melange(x,theta=theta)),col='red')+geom_line(aes(x=x,modele_melange(res1$emv,x)),col='blue')+labs(title='Si on initialise les pi au hasard',x='observations')

theta_init2<-list(pi=c(1/3,1/6,1/2),mu=c(0,5,10),sig=c(5,5,5))

res_i<-algoEM(obs$obs,theta_init2)
i<-ggplot()+geom_histogram(aes(obs$obs,y=..density..),bins=30,col='grey')+geom_line(aes(x=x,y=modele_melange(x,theta=theta)),col='red')+geom_line(aes(x=x,modele_melange(res_i$emv,x)),col='blue')+labs(title="Si on initialise les sigmas au 'hasard'",x='observations')

theta_init3<-list(pi=c(1/3,1/6,1/2),mu=c(0,5,10),sig=c(10,3,7))

res_j<-algoEM(obs$obs,theta_init3)
j<-ggplot()+geom_histogram(aes(obs$obs,y=..density..),bins=30,col='grey')+geom_line(aes(x=x,y=modele_melange(x,theta=theta)),col='red')+geom_line(aes(x=x,modele_melange(res_j$emv,x)),col='blue')+labs(title='Si on initialise les sigmas au hasard',x='observations')

plot_grid(i,j)


theta_init4<-list(pi=c(1/3,1/6,1/2),mu=c(15,20,5),sig=c(1,1,4))

res_<-algoEM(obs$obs,theta_init4)

ggplot()+geom_histogram(aes(obs$obs,y=..density..),bins=30,col='grey')+geom_line(aes(x=x,y=modele_melange(x,theta=theta)),col='red')+geom_line(aes(x=x,modele_melange(res_$emv,x)),col='blue')+labs(title='Si on initialise les mu au hasard',x='observations')

obs<-simul(theta = theta,n=1000)
Theta_res<-matrix(NA,ncol=9,nrow=200)
log<-numeric(200)
for (i in 1:200){
mu<-sample(obs$obs,size=3,replace=FALSE)
sig<-runif(3,0,10)
pi<-c(1/3,1/3,1/3)
theta_init4<-list(pi=pi,mu=mu,sig=sig)
res4<-algoEM(obs$obs,theta_init4,R=5)
Theta_res[i,]<-c(res4$emv$pi,mu=res4$emv$mu,sig=res4$emv$sig)
log[i]<-res4$logvraisemblance[res4$nb.iter+1]
}

j<-which.max(log)
theta_EM<-Theta_res[j,]
theta_EM<-list(pi=theta_EM[1:3],mu=theta_EM[4:6],sig=theta_EM[7:9])
theta_EM<-algoEM(theta.init=theta_EM,obs=obs$obs)$emv

theta_EM
ggplot()+geom_histogram(aes(obs$obs,y=..density..),bins=30,col='grey')+geom_line(aes(x=x,y=modele_melange(x,theta=theta)),col='red')+geom_line(aes(x=x,modele_melange(theta_EM,x)),col='blue')+labs(title='Avec la méthode pour trouver le bon paramètre',x='observations')


lvraisnorm <- function(param, obs){ 
  dens <- rep(0, length(obs))
  K <- length(param$pi)
  for (k in 1:K){
    dens <- dens + param$pi[k]*dnorm(obs,param$mu[k], param$sig[k])
  }                                         
  return(sum(log(dens)))
}

update_param<-function(obs,param){
  nb <- length(obs) 
  K  <- length(param$mu) 
  
  alpha <- matrix(NA, nrow=K, ncol=nb)
  for (j in 1:K){
    alpha[j,] <- param$pi[j]*dnorm(obs,param$mu[j],param$sig[j])
  }
  cx <- apply(alpha, 2, 'sum')
  alpha <- alpha/matrix(cx, byrow=TRUE, nrow=K, ncol=nb)
  

  pi_new  <- apply(alpha, 1, 'mean')
  mu_new  <- c(alpha%*%obs/nb/pi_new)
  mat     <- (matrix(rep(obs,each=K), nrow=K) -matrix(rep(mu_new, times=nb), nrow=K))^2
  sig_new <- sqrt(apply(alpha*mat, 1, 'mean')/pi_new)
  
  param_new <-  list(pi =pi_new, mu=mu_new, sig=sig_new)
  return(param_new)
}

algoEM <- function(obs,param,iterMax=200,epsilon=1e-5){   
  crit_arret<- FALSE
  param_old<-param
  param_new<-param
  logvrais_old<-lvraisnorm(param,obs)
  logvrais_new<-lvraisnorm(param,obs)
  iter<- 0
  vrai<-c()
  
  while (crit_arret==FALSE && (iter<iterMax)){ 
    param_new<-update_param(obs,param_old)
    logvrais_new<-lvraisnorm(param_new,obs)
    vrai<-c(vrai,logvrais_new)
    crit_arret <- (abs(( logvrais_new -  logvrais_old)/ logvrais_old)<epsilon)
    logvrais_old <- logvrais_new
    param_old <- param_new
    iter <- iter + 1
  }
  resultat <- list("emv" = param_new,"nb_iter"=iter,"vrai"=vrai)
  return(resultat)
}


pi_aleatoire<-runif(1,0,1)
pi_aleatoire2<-runif(1,0,1-pi_aleatoire)
pi_aleatoire3<-1-pi_aleatoire-pi_aleatoire2
pi_aleat<-sample(c(pi_aleatoire,pi_aleatoire2,pi_aleatoire3))

param<-list(pi=pi_aleat, mu=runif(3,0,10), sig=runif(3,0,8)) 
res<-algoEM(simula,param)



curve(0.5*dnorm(x,10,4)+1/3*dnorm(x,0,1)+1/6*dnorm(x,5,1),col="blue",from=-5,to=20,ylab="",xlab="",main="Densité Originale et Densité ajustée")
curve(expr =res$emv$pi[1]*dnorm(x,res$emv$mu[1],res$emv$sig[1])+res$emv$pi[2]*dnorm(x,res$emv$mu[2],res$emv$sig[2])+res$emv$pi[3]*dnorm(x,res$emv$mu[3],res$emv$sig[3]),add=T,col="red",from=-5,to=20)
legend(10,0.12,legend=c("Originale","Ajustée"),col=c("blue","red"),lty=1, cex=0.8)



color_MAP2<-c()
simul<-c(loi_1,loi_2,loi_3)
for (i in 1:length(c(loi_1,loi_2,loi_3))){
  f1<-res$emv$pi[1]*dnorm(simul[i],res$emv$mu[1],res$emv$sig[1])/(res$emv$pi[1]*dnorm(simul[i],res$emv$mu[1],res$emv$sig[1])+res$emv$pi[2]*dnorm(simul[i],res$emv$mu[2],res$emv$sig[2])+res$emv$pi[3]*dnorm(simul[i],res$emv$mu[3],res$emv$sig[3]))
  f2<-res$emv$pi[2]*dnorm(simul[i],res$emv$mu[2],res$emv$sig[2])/(res$emv$pi[1]*dnorm(simul[i],res$emv$mu[1],res$emv$sig[1])+res$emv$pi[2]*dnorm(simul[i],res$emv$mu[2],res$emv$sig[2])+res$emv$pi[3]*dnorm(simul[i],res$emv$mu[3],res$emv$sig[3]))
  f3<-res$emv$pi[3]*dnorm(simul[i],res$emv$mu[3],res$emv$sig[3])/(res$emv$pi[1]*dnorm(simul[i],res$emv$mu[1],res$emv$sig[1])+res$emv$pi[2]*dnorm(simul[i],res$emv$mu[2],res$emv$sig[2])+res$emv$pi[3]*dnorm(simul[i],res$emv$mu[3],res$emv$sig[3]))
  
  if(f1>f2 && f1>f3){
    color_MAP2<-c(color_MAP2,2)
    
  }
  else if(f2>f1 && f2>f3){
    color_MAP2<-c(color_MAP2,3)
    
  }
  else{
    color_MAP2<-c(color_MAP2,4)
    
  }
}




plot(c(loi_1,loi_2,loi_3),res$emv$pi[1]*dnorm(c(loi_1,loi_2,loi_3),res$emv$mu[1],res$emv$sig[1])+res$emv$pi[2]*dnorm(c(loi_1,loi_2,loi_3),res$emv$mu[2],res$emv$sig[2])+res$emv$pi[3]*dnorm(c(loi_1,loi_2,loi_3),res$emv$mu[3],res$emv$sig[3]),ylim=c(0,0.15),col=color_MAP2,ylab = "",xlab="")
title("Mélange Gaussien classif EM")

theta_init1
theta_init3
theta_init4

hist(obs$obs,breaks=20,prob=TRUE,xlab = "Observations",ylim = c(0,0.17), main="Classification par MAP des\n observations obtenues en initialisant\n l'algorithme EM avec des proportions erronées"  )
res<-algoEM(obs$obs,theta_init1, epsilon=1e-5)
lines(x,modele_melange(res$emv,x))
points(A$obs,rep(0,200),col=c('red','blue','green')[MAP(A$obs,theta=theta_init1)])

hist(obs$obs,breaks=20,prob=TRUE,xlab = "Observations",ylim = c(0,0.17), main="Classification par MAP des\n observations obtenues en initialisant\n l'algorithme EM avec des écart-types erronés"  )
res<-algoEM(obs$obs,theta_init3, epsilon=1e-5)
lines(x,modele_melange(res$emv,x))
points(A$obs,rep(0,200),col=c('red','blue','green')[MAP(A$obs,theta=theta_init3)])

res_<-algoEM(obs$obs,theta_init4,epsilon=1e-6)
hist(obs$obs,breaks=20, prob = TRUE, xlab = "Observations",ylim = c(0,0.17),main="Classification par MAP des\n observations obtenues en initialisant\n l'algorithme EM avec des proportions moyennes" )
lines(x,modele_melange(res_$emv,x),type='l', col='red')
points(A$obs,rep(0,200),col=c('red','blue','green')[MAP(A$obs,theta=theta_init4)])

lvraisnorm <- function(param, obs){ 
  dens <- rep(0, length(obs))
  K <- length(param$pi)
  for (k in 1:K){
    dens <- dens + param$pi[k]*dnorm(obs,param$mu[k], param$sig[k])
  }                                         
  return(sum(log(dens)))
}

update_param<-function(obs,param){
  nb <- length(obs) 
  K  <- length(param$mu) 
  
  alpha <- matrix(NA, nrow=K, ncol=nb)
  for (j in 1:K){
    alpha[j,] <- param$pi[j]*dnorm(obs,param$mu[j],param$sig[j])
  }
  cx <- apply(alpha, 2, 'sum')
  alpha <- alpha/matrix(cx, byrow=TRUE, nrow=K, ncol=nb)
  

  pi_new  <- apply(alpha, 1, 'mean')
  mu_new  <- c(alpha%*%obs/nb/pi_new)
  mat     <- (matrix(rep(obs,each=K), nrow=K) -matrix(rep(mu_new, times=nb), nrow=K))^2
  sig_new <- sqrt(apply(alpha*mat, 1, 'mean')/pi_new)
  
  param_new <-  list(pi =pi_new, mu=mu_new, sig=sig_new)
  return(param_new)
}

algoEM <- function(obs,param,iterMax=200,epsilon=1e-5){   
  crit_arret<- FALSE
  param_old<-param
  param_new<-param
  logvrais_old<-lvraisnorm(param,obs)
  logvrais_new<-lvraisnorm(param,obs)
  iter<- 0
  vrai<-c()
  
  while (crit_arret==FALSE && (iter<iterMax)){ 
    param_new<-update_param(obs,param_old)
    logvrais_new<-lvraisnorm(param_new,obs)
    vrai<-c(vrai,logvrais_new)
    crit_arret <- (abs(( logvrais_new -  logvrais_old)/ logvrais_old)<epsilon)
    logvrais_old <- logvrais_new
    param_old <- param_new
    iter <- iter + 1
  }
  resultat <- list("emv" = param_new,"nb_iter"=iter,"vrai"=vrai)
  return(resultat)
}

param<-list(pi=c(0.05014786,0.10363385,0.84621829), mu=c(9.2502664,8.9100328,0.3349248),sig=c(5.396038,2.358926,6.927824))
res<-algoEM(simula,param)  

par(mfrow=c(1,1))
plot(seq(1,length(res$vrai)),res$vrai,col="red",ylab="Vraissemblance",xlab="Nombre d'itérations",main="Evolution de la log-vraissemblance par palier")

library(Rmixmod)

theta<-list(pi=c(1/3,1/6,1/2),mu=c(0,5,10),sig=c(1,1,4))
obs<-simul(1000,theta)

mod=mixmodCluster(obs$obs,nbCluster=3)
summary(mod)

x=seq(min(theta$mu)-10,max(theta$mu)+10,0.1)
hist(mod)
plot(x,modele_melange(x=x,theta=theta),type='l')

mod[10]

mod[6]

mod[12]

par(mfrow=c(1,2))
mod<-mixmodCluster(obs$obs,nbCluster=3, model = mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_C")))
summary(mod)
mod<-mixmodCluster(obs$obs,nbCluster=3, model = mixmodGaussianModel(listModels = c("Gaussian_p_L_Bk")))
summary(mod)

mod<-mixmodCluster(obs$obs,nbCluster=6)
summary(mod)

mod<-mixmodCluster(obs$obs,nbCluster=2:8)
summary(mod)

BIC<-list()
for (i in 2:8){
mod10<-mixmodCluster(obs$obs,nbCluster=i)
BIC<-c(BIC,mod10[16])
}
plot(2:8,BIC,xlab = "nombre de clusters" , ylim=c(5700,6000),type='l',main="Evolution du BIC en fonction du nombre de clusters")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,5),sig=c(1,1))
obs_5<-simul(n=50,theta=theta_5)
BIC<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i)
  BIC<-c(BIC,mod10[16])
}
plot(1:8,BIC,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution de BIC avec petit échantillon")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,0.3),sig=c(1,1))
obs_5<-simul(n=200,theta=theta_5)
BIC<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i)
  BIC<-c(BIC,mod10[16])
}
plot(1:8,BIC,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution de BIC avec espérances proches")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,1000),sig=c(1,1))
obs_5<-simul(n=200,theta=theta_5)
BIC<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i)
  BIC<-c(BIC,mod10[16])
}
plot(1:8,BIC,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution de BIC avec espérances éloignées")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,1000),sig=c(1,1))
obs_5<-simul(n=200,theta=theta_5)
ICL<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i,criterion='ICL')
  ICL<-c(ICL,mod10[16])
}
plot(1:8,ICL,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution d'ICL avec espérances éloignées")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,0.3),sig=c(1,1))
obs_5<-simul(n=200,theta=theta_5)
ICL<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i,criterion='ICL')
  ICL<-c(ICL,mod10[16])
}
plot(1:8,ICL,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution d'ICL avec espérances proches")

theta_5<-list(pi=c(1/2,1/2),mu=c(0,10),sig=c(1,1))
obs_5<-simul(n=50,theta=theta_5)
ICL<-list()
for (i in 1:8){
  mod10<-mixmodCluster(obs_5$obs,nbCluster=i,criterion='ICL')
  ICL<-c(ICL,mod10[16])
}
plot(1:8,ICL,type='l',xlab="nb de classes",ylab="BIC",xlim=c(0,8),main="Evolution d'ICL avec petit échantillon")

seeds_dataset <- read.table("seeds_dataset.txt") 

BIC<-list()
models<-list()
for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='BIC',models=mixmodGaussianModel(family="all"))
  BIC<-c(BIC,mod10[16])
  models<-c(models,mod10[15])
}
plot(1:8,BIC,type='l',xlab="nombre de classes")
print(models)

ICL<-list()
models<-list()
for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='ICL',models=mixmodGaussianModel(family="all"))
  ICL<-c(ICL,mod10[16])
  models<-c(models,mod10[15])
}
plot(1:8,ICL,type='l',xlab="nombre de classes")
print(models)

BIC<-list()

for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='BIC',models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_I")))
  BIC<-c(BIC,mod10[16])
}
plot(1:8,BIC,type='l',xlab="nombre de classes")

ICL<-list()
for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='ICL',models=mixmodGaussianModel(listModels = c("Gaussian_pk_L_I")))
  ICL<-c(ICL,mod10[16])
}
plot(1:8,ICL,type='l',xlab="nombre de classes")

BIC<-list()

for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='BIC',models=mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_C")))
  BIC<-c(BIC,mod10[16])
}
plot(1:8,BIC,type='l',xlab="nombre de classes")

ICL<-list()
for (i in 1:8){
  mod10<-mixmodCluster(seeds_dataset[,-8],nbCluster=i,criterion='ICL',models=mixmodGaussianModel(listModels = c("Gaussian_pk_Lk_C")))
  ICL<-c(ICL,mod10[16])
}
plot(1:8,ICL,type='l',xlab="nombre de classes")
