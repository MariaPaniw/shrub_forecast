# This script performs analyses as part of Paniw et al. XXXX
# Please read the README file on GitHub for a workflow of the analyses

# Created by: Maria Paniw
# Date created: June 20, 2022

rm(list = ls())

library(plyr)
library(ggplot2)
library(ggnewscale)
library(boot)
library(patchwork)
library(popbio)

setwd("SET_WORKING_DIRECTORY")
### Parameters 

load("allShrubs.Rdata")

### Predictions
precip=read.csv("precipPalacios.csv")

rain=as.numeric(scale(precip$Prec.[precip$year%in%c(2007:2021)]))


# Halilimium halimifolium 

survM.h1 <- function(rain,neigh){
  
  mu=inv.logit(out1$mean$a0.h1 +out1$mean$a1.h1*rain
               + out1$mean$a3.h1*neigh)
  
  return(mu)
}


load("Hal1_rec.Rdata")

rec.h1=rec.mod


survSap.h1 <- function(rain,ad){
  
  mu=inv.logit(out1$mean$b0.h1 + out1$mean$b2.h1*ad )
  
  return(mu)
}


#### Halimium commutatum

survM.h2 <- function(ad,neigh){
  
  mu=inv.logit(out1$mean$a0.h2 + out1$mean$a2.h2*ad + out1$mean$a3.h2*neigh)
  
  return(mu)
}

load("Hal2_rec.Rdata")


rec.h2=rec.mod


survSap.h2 <- function(ad,neigh){
  
 mu=inv.logit(out1$mean$b0.h2 + out1$mean$b2.h2*ad + out1$mean$b3.h2*neigh)
 
  return(mu)
}


#### Lavandula stoechas

survM.l <- function(neigh){
  
  mu=inv.logit(out1$mean$a0.l + out1$mean$a3.l*neigh)
  
  return(mu)
}


load("Lav_rec.Rdata")

rec.l=rec.mod


survSap.l <- function(ad,neigh){
  
  mu=inv.logit(out1$mean$b0.l + out1$mean$b2.l*ad + out1$mean$b3.l*neigh)
 
  return(mu)
}

#### Rosmarinus officinalis

survM.r <- function(ad,neigh){
  
  mu=inv.logit(out1$mean$a0.r + out1$mean$a2.r*ad + out1$mean$a3.r*neigh)

  return(mu)
}

load("Ros_rec.Rdata")

rec.r=rec.mod



survSap.r <- function(ad,neigh){
  
  mu=inv.logit(out1$mean$b0.r + out1$mean$b2.r*ad + out1$mean$b3.r*neigh)

  return(mu)
}
#### Cistus libanotis

survM.c <- function(rain,ad,neigh){
  
  mu=inv.logit(out1$mean$a0.c +out1$mean$a1.c*rain
               + out1$mean$a2.c*ad + out1$mean$a3.c*neigh)
  
  return(mu)
}

load("Cistus_rec.Rdata")

rec.c=rec.mod


survSap.c <- out1$mean$phiS.c


####### LAMBDA
n.stage=3

coefs=expand.grid(rain=rain,neigh=seq(0,6,length.out = 20),ad=seq(0,6,length.out = 20),
                  seed=seq(1,50,length.out=10))

l.h1=l.h2=l.l=l.r=l.c=rep(NA,nrow(coefs))

for(s in 1:nrow(coefs)){

  rain.t=coefs$rain[s]
  seed.t=coefs$seed[s]
  
  
  neigh.h1=coefs$neigh[s]
    neigh.h2=coefs$neigh[s]
    neigh.l=coefs$neigh[s]
    neigh.r=coefs$neigh[s]
    neigh.c=coefs$neigh[s]
    
    ad.h1=coefs$ad[s]
    ad.h2=coefs$ad[s]
    ad.l=coefs$ad[s]
    ad.r=coefs$ad[s]
    ad.c=coefs$ad[s]
    
    # Hal 1 
    survS.h1 = out1$mean$gamma.h1/seed.t
    
    if(!is.finite(survS.h1)) survS.h1 <- 0
    if(survS.h1>1) survS.h1 <- 1
    
    mat.h1 = matrix(c(0,survS.h1,0,
                      0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                      rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
    
    l.h1[s] =lambda(mat.h1)
    
    # Hal 2 
    survS.h2 = out1$mean$gamma.h2/seed.t
    
    if(!is.finite(survS.h2)) survS.h2 <- 0
    if(survS.h2>1) survS.h2 <- 1
    
    
    mat.h2 = matrix(c(0,survS.h2,0,
                      0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2),
                      rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2)),n.stage,n.stage)
    
    
    l.h2[s] =lambda(mat.h2)
    
    # Lav
    
    survS.l = out1$mean$gamma.l/seed.t
    
    if(!is.finite(survS.l)) survS.l <- 0
    if(survS.l>1) survS.l <- 1
    
    mat.l = matrix(c(0,survS.l,0,
                     0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l),
                     rec.l,0,survM.l(neigh=neigh.l)),n.stage,n.stage)
    
    
    l.l[s] =lambda(mat.l)
    
    # Ros 
    
    survS.r = 0.3/seed.t
    
    if(!is.finite(survS.r)) survS.r <- 0
    if(survS.r>1) survS.r <- 1
    
    mat.r = matrix(c(0,survS.r,0,
                     0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r),
                     rec.r,0,survM.r(ad=ad.r,neigh=neigh.r)),n.stage,n.stage)
    
    
    l.r[s] =lambda(mat.r)
    
    # Cistus 
    
    survS.c = out1$mean$gamma.c/seed.t
    
    if(!is.finite(survS.c)) survS.c <- 0
    if(survS.c>1) survS.c <- 1
    
    mat.c = matrix(c(0,survS.c,0,
                     0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                     rec.c,0,survM.c(rain=rain.t,ad=ad.c,neigh=neigh.c)),n.stage,n.stage)
    
    
    l.c[s] =lambda(mat.c)
    
  
}

stable.h1=sample(which(l.h1>0.999&l.h1<1.001),100)
stable.h2=sample(which(l.h2>0.999&l.h2<1.001),100)
stable.l=sample(which(l.l>0.999&l.l<1.001),100)
stable.r=sample(which(l.r>0.999&l.r<1.001),100)
stable.c=sample(which(l.c>0.999&l.c<1.001),100)

# PERTURB RAINFALL: 

dl.h1=NULL

for(s in 1:length(stable.h1)){
  
  rain.t=coefs$rain[stable.h1[s]]
  rain.t.pert=coefs$rain[stable.h1[s]]+0.1*abs(coefs$rain[stable.h1[s]])
  
  seed.t=coefs$seed[stable.h1[s]]

  neigh.h1=coefs$neigh[stable.h1[s]]

  ad.h1=coefs$ad[stable.h1[s]]
  
  survS.h1 = out1$mean$gamma.h1/seed.t
  
  if(!is.finite(survS.h1)) survS.h1 <- 0
  if(survS.h1>1) survS.h1 <- 1
  
  control = matrix(c(0,survS.h1,0,
                     0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                     rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
  
  pert1=matrix(c(0,survS.h1,0,
                 0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t.pert,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t.pert,ad=ad.h1),
                 rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
  
  pert2 = matrix(c(0,survS.h1,0,
                   0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                   rec.h1,0,survM.h1(rain=rain.t.pert,neigh=neigh.h1)),n.stage,n.stage)
  
  pert3 = matrix(c(0,survS.h1,0,
                   0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t.pert,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t.pert,ad=ad.h1),
                   rec.h1,0,survM.h1(rain=rain.t.pert,neigh=neigh.h1)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  
  dl.h1=rbind(dl.h1,temp)
}

dl.h1$species="H. halimifolium"

dl.h2=NULL

for(s in 1:length(stable.h2)){
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,NA,NA))
  
  dl.h2=rbind(dl.h2,temp)
}

dl.h2$species="H. commutatum"


dl.l=NULL

for(s in 1:length(stable.l)){

  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,NA,NA))

  dl.l=rbind(dl.l,temp)
}

dl.l$species="L. stoechas"

dl.r=NULL

for(s in 1:length(stable.r)){

  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,NA,NA))

  dl.r=rbind(dl.r,temp)
}
dl.r$species="R. officinalis"

dl.c=NULL

for(s in 1:length(stable.c)){
  
  rain.t=coefs$rain[stable.c[s]]
  rain.t.pert=coefs$rain[stable.c[s]]+0.1*abs(coefs$rain[stable.c[s]])
  
  seed.t=coefs$seed[stable.c[s]]
  
  neigh.c=coefs$neigh[stable.c[s]]
  
  ad.c=coefs$ad[stable.c[s]]
  
  survS.c = out1$mean$gamma.c/seed.t
  
  if(!is.finite(survS.c)) survS.c <- 0
  if(survS.c>1) survS.c <- 1
  
  control = matrix(c(0,survS.c,0,
                     0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                     rec.c,0,survM.c(rain=rain.t,ad=ad.c,neigh=neigh.c)),n.stage,n.stage)
  
  
  pert2 = matrix(c(0,survS.c,0,
                   0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                   rec.c,0,survM.c(rain=rain.t.pert,ad=ad.c,neigh=neigh.c)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,
                            (lambda(pert2)-lambda(control))/lambda(control),
                            NA))
  dl.c=rbind(dl.c,temp)
}

dl.c$species="C. libanotis"

### Plots 

df.R=rbind(dl.h1,dl.h2,dl.l,dl.r,dl.c)

df.R$pert="Rain"

##### PERTURB INTRASPECIFIC DENSITY

dl.h1=NULL

for(s in 1:length(stable.h1)){
  
  rain.t=coefs$rain[stable.h1[s]]
  seed.t=coefs$seed[stable.h1[s]]
  
  neigh.h1=coefs$neigh[stable.h1[s]]
  
  ad.h1=coefs$ad[stable.h1[s]]
  ad.h1.pert= coefs$ad[stable.h1[s]]+0.1*coefs$ad[stable.h1[s]]
  
  survS.h1 = out1$mean$gamma.h1/seed.t
  
  if(!is.finite(survS.h1)) survS.h1 <- 0
  if(survS.h1>1) survS.h1 <- 1
  
  control = matrix(c(0,survS.h1,0,
                     0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                     rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
  
  pert1=matrix(c(0,survS.h1,0,
                 0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1.pert),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1.pert),
                 rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),NA,NA))
  
  dl.h1=rbind(dl.h1,temp)
}

dl.h1$species="H. halimifolium"

dl.h2=NULL

for(s in 1:length(stable.h2)){
  
  rain.t=coefs$rain[stable.h2[s]]
  seed.t=coefs$seed[stable.h2[s]]
  
  neigh.h2=coefs$neigh[stable.h2[s]]
  
  ad.h2=coefs$ad[stable.h2[s]]
  
  ad.h2.pert= coefs$ad[stable.h2[s]]+0.1*coefs$ad[stable.h2[s]]
  
  
  survS.h2 = out1$mean$gamma.h2/seed.t
  
  if(!is.finite(survS.h2)) survS.h2 <- 0
  if(survS.h2>1) survS.h2 <- 1
  
  
  control = matrix(c(0,survS.h2,0,
                     0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2),
                     rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2)),n.stage,n.stage)
  
  pert1=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2.pert,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2.pert,neigh=neigh.h2),
                 rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2)),n.stage,n.stage)
  
  pert2=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2),
                 rec.h2,0,survM.h2(ad=ad.h2.pert,neigh=neigh.h2)),n.stage,n.stage)
  
  pert3=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2.pert,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2.pert,neigh=neigh.h2),
                 rec.h2,0,survM.h2(ad=ad.h2.pert,neigh=neigh.h2)),n.stage,n.stage)
  
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  dl.h2=rbind(dl.h2,temp)
}

dl.h2$species="H. commutatum"


dl.l=NULL

for(s in 1:length(stable.l)){
  
  rain.t=coefs$rain[stable.l[s]]
  seed.t=coefs$seed[stable.l[s]]
  
  neigh.l=coefs$neigh[stable.l[s]]
  
  ad.l=coefs$ad[stable.l[s]]
  
  ad.l.pert= coefs$ad[stable.l[s]]+0.1*coefs$ad[stable.l[s]]
  
  survS.l = out1$mean$gamma.l/seed.t
  
  if(!is.finite(survS.l)) survS.l <- 0
  if(survS.l>1) survS.l <- 1
  
  control = matrix(c(0,survS.l,0,
                     0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l),
                     rec.l,0,survM.l(neigh=neigh.l)),n.stage,n.stage)
  
  
  pert1 = matrix(c(0,survS.l,0,
                   0,(1-out1$mean$t.l)*survSap.l(ad=ad.l.pert,neigh=neigh.l),out1$mean$t.l*survSap.l(ad=ad.l.pert,neigh=neigh.l),
                   rec.l,0,survM.l(neigh=neigh.l)),n.stage,n.stage)
  
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),NA,NA))
  
  dl.l=rbind(dl.l,temp)
}

dl.l$species="L. stoechas"

dl.r=NULL

for(s in 1:length(stable.r)){
  
  rain.t=coefs$rain[stable.r[s]]
  seed.t=coefs$seed[stable.r[s]]
  
  neigh.r=coefs$neigh[stable.r[s]]
  
  ad.r=coefs$ad[stable.r[s]]
  
  ad.r.pert= coefs$ad[stable.r[s]]+0.1*coefs$ad[stable.r[s]]
  
  
  survS.r = 0.35/seed.t
  
  if(!is.finite(survS.r)) survS.r <- 0
  if(survS.r>1) survS.r <- 1
  
  control =  matrix(c(0,survS.r,0,
                      0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r),
                      rec.r,0,survM.r(ad=ad.r,neigh=neigh.r)),n.stage,n.stage)
  
  pert1 =  matrix(c(0,survS.r,0,
                    0,(1-out1$mean$t.r)*survSap.r(ad=ad.r.pert,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r.pert,neigh=neigh.r),
                    rec.r,0,survM.r(ad=ad.r,neigh=neigh.r)),n.stage,n.stage)
  
  pert2 =  matrix(c(0,survS.r,0,
                    0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r),
                    rec.r,0,survM.r(ad=ad.r.pert,neigh=neigh.r)),n.stage,n.stage)
  
  pert3 =  matrix(c(0,survS.r,0,
                    0,(1-out1$mean$t.r)*survSap.r(ad=ad.r.pert,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r.pert,neigh=neigh.r),
                    rec.r,0,survM.r(ad=ad.r.pert,neigh=neigh.r)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  
  dl.r=rbind(dl.r,temp)
}
dl.r$species="R. officinalis"

dl.c=NULL

for(s in 1:length(stable.c)){
  
  rain.t=coefs$rain[stable.c[s]]
  seed.t=coefs$seed[stable.c[s]]
  
  neigh.c=coefs$neigh[stable.c[s]]
  
  neigh.c.pert= coefs$neigh[stable.c[s]]+0.1*coefs$neigh[stable.c[s]]
  
  ad.c=coefs$ad[stable.c[s]]
  
  ad.c.pert= coefs$ad[stable.c[s]]+0.1*coefs$ad[stable.c[s]]
  
  survS.c = out1$mean$gamma.c/seed.t
  
  if(!is.finite(survS.c)) survS.c <- 0
  if(survS.c>1) survS.c <- 1
  
  control = matrix(c(0,survS.c,0,
                     0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                     rec.c,0,survM.c(rain=rain.t,ad=ad.c,neigh=neigh.c)),n.stage,n.stage)
  
  
  pert2 = matrix(c(0,survS.c,0,
                   0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                   rec.c,0,survM.c(rain=rain.t,ad=ad.c.pert,neigh=neigh.c)),n.stage,n.stage)
  
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,
                            (lambda(pert2)-lambda(control))/lambda(control),
                            NA))
  
  dl.c=rbind(dl.c,temp)
}

dl.c$species="C. libanotis"

### Plots 

df.D=rbind(dl.h1,dl.h2,dl.l,dl.r,dl.c)

df.D$pert="Dens"

######## PERTURB INTERSPECIFIC DENSITY

dl.h1=NULL

for(s in 1:length(stable.h1)){
  
  rain.t=coefs$rain[stable.h1[s]]
  seed.t=coefs$seed[stable.h1[s]]
  
  neigh.h1=coefs$neigh[stable.h1[s]]
  
  neigh.h1.pert= coefs$neigh[stable.h1[s]]+0.1*coefs$neigh[stable.h1[s]]
  
  ad.h1=coefs$ad[stable.h1[s]]
  
  survS.h1 = out1$mean$gamma.h1/seed.t
  
  if(!is.finite(survS.h1)) survS.h1 <- 0
  if(survS.h1>1) survS.h1 <- 1
  
  control = matrix(c(0,survS.h1,0,
                     0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                     rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1)),n.stage,n.stage)
  
  pert2=matrix(c(0,survS.h1,0,
                 0,(1-out1$mean$t.h1)*survSap.h1(rain=rain.t,ad=ad.h1),out1$mean$t.h1*survSap.h1(rain=rain.t,ad=ad.h1),
                 rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1.pert)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,(lambda(pert2)-lambda(control))/lambda(control),NA))
  
  dl.h1=rbind(dl.h1,temp)
}

dl.h1$species="H. halimifolium"

dl.h2=NULL

for(s in 1:length(stable.h2)){
  
  rain.t=coefs$rain[stable.h2[s]]
  seed.t=coefs$seed[stable.h2[s]]
  
  neigh.h2=coefs$neigh[stable.h2[s]]
  
  neigh.h2.pert= coefs$neigh[stable.h2[s]]+0.1*coefs$neigh[stable.h2[s]]
  
  ad.h2=coefs$ad[stable.h2[s]]
  
  survS.h2 = out1$mean$gamma.h2/seed.t
  
  if(!is.finite(survS.h2)) survS.h2 <- 0
  if(survS.h2>1) survS.h2 <- 1
  
  
  control = matrix(c(0,survS.h2,0,
                     0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2),
                     rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2)),n.stage,n.stage)
  
  
  pert1=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2.pert),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2.pert),
                 rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2)),n.stage,n.stage)
  
  pert2=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2),
                 rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2.pert)),n.stage,n.stage)
  
  pert3=matrix(c(0,survS.h2,0,
                 0,(1-out1$mean$t.h2)*survSap.h2(ad=ad.h2,neigh=neigh.h2.pert),out1$mean$t.h2*survSap.h2(ad=ad.h2,neigh=neigh.h2.pert),
                 rec.h2,0,survM.h2(ad=ad.h2,neigh=neigh.h2.pert)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  
  dl.h2=rbind(dl.h2,temp)
}

dl.h2$species="H. commutatum"


dl.l=NULL

for(s in 1:length(stable.l)){
  
  rain.t=coefs$rain[stable.l[s]]
  seed.t=coefs$seed[stable.l[s]]
  
  neigh.l=coefs$neigh[stable.l[s]]
  
  neigh.l.pert= coefs$neigh[stable.l[s]]+0.1*coefs$neigh[stable.l[s]]
  
  ad.l=coefs$ad[stable.l[s]]
  
  survS.l = out1$mean$gamma.l/seed.t
  
  if(!is.finite(survS.l)) survS.l <- 0
  if(survS.l>1) survS.l <- 1
  
  control = matrix(c(0,survS.l,0,
                     0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l),
                     rec.l,0,survM.l(neigh=neigh.l)),n.stage,n.stage)
  
  
  pert1 = matrix(c(0,survS.l,0,
                   0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l.pert),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l.pert),
                   rec.l,0,survM.l(neigh=neigh.l)),n.stage,n.stage)
  
  pert2 = matrix(c(0,survS.l,0,
                   0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l),
                   rec.l,0,survM.l(neigh=neigh.l.pert)),n.stage,n.stage)
  
  pert3 = matrix(c(0,survS.l,0,
                   0,(1-out1$mean$t.l)*survSap.l(ad=ad.l,neigh=neigh.l.pert),out1$mean$t.l*survSap.l(ad=ad.l,neigh=neigh.l.pert),
                   rec.l,0,survM.l(neigh=neigh.l.pert)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  
  dl.l=rbind(dl.l,temp)
}

dl.l$species="L. stoechas"

dl.r=NULL

for(s in 1:length(stable.r)){
  
  rain.t=coefs$rain[stable.r[s]]
  seed.t=coefs$seed[stable.r[s]]
  
  neigh.r=coefs$neigh[stable.r[s]]
  
  neigh.r.pert= coefs$neigh[stable.r[s]]+0.1*coefs$neigh[stable.r[s]]
  
  ad.r=coefs$ad[stable.r[s]]
  
  survS.r = 0.35/seed.t
  
  if(!is.finite(survS.r)) survS.r <- 0
  if(survS.r>1) survS.r <- 1
  
  control = matrix(c(0,survS.r,0,
                     0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r),
                     rec.r,0,survM.r(ad=ad.r,neigh=neigh.r)),n.stage,n.stage)
  
  
  pert1 = matrix(c(0,survS.r,0,
                   0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r.pert),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r.pert),
                   rec.r,0,survM.r(ad=ad.r,neigh=neigh.r)),n.stage,n.stage)
  
  
  pert2 = matrix(c(0,survS.r,0,
                   0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r),
                   rec.r,0,survM.r(ad=ad.r,neigh=neigh.r.pert)),n.stage,n.stage)
  
  
  pert3 = matrix(c(0,survS.r,0,
                   0,(1-out1$mean$t.r)*survSap.r(ad=ad.r,neigh=neigh.r.pert),out1$mean$t.r*survSap.r(ad=ad.r,neigh=neigh.r.pert),
                   rec.r,0,survM.r(ad=ad.r,neigh=neigh.r.pert)),n.stage,n.stage)
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c((lambda(pert1)-lambda(control))/lambda(control),
                            (lambda(pert2)-lambda(control))/lambda(control),
                            (lambda(pert3)-lambda(control))/lambda(control)))
  
  dl.r=rbind(dl.r,temp)
}
dl.r$species="R. officinalis"

dl.c=NULL

for(s in 1:length(stable.c)){
  
  rain.t=coefs$rain[stable.c[s]]
  seed.t=coefs$seed[stable.c[s]]
  
  neigh.c=coefs$neigh[stable.c[s]]
  
  neigh.c.pert= coefs$neigh[stable.c[s]]+0.1*coefs$neigh[stable.c[s]]
  
  ad.c=coefs$ad[stable.c[s]]
  
  survS.c = out1$mean$gamma.c/seed.t
  
  if(!is.finite(survS.c)) survS.c <- 0
  if(survS.c>1) survS.c <- 1
  
  control = matrix(c(0,survS.c,0,
                     0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                     rec.c,0,survM.c(rain=rain.t,ad=ad.c,neigh=neigh.c)),n.stage,n.stage)
  
  pert2 = matrix(c(0,survS.c,0,
                   0,(1-out1$mean$t.c)*survSap.c,out1$mean$t.c*survSap.c,
                   rec.c,0,survM.c(rain=rain.t,ad=ad.c,neigh=neigh.c.pert)),n.stage,n.stage)
  
  
  temp = data.frame(vr=c("survS","survM","survS/M"),
                    delta=c(NA,(lambda(pert2)-lambda(control))/lambda(control),NA))
  
  dl.c=rbind(dl.c,temp)
}

dl.c$species="C. libanotis"


df.BI=rbind(dl.h1,dl.h2,dl.l,dl.r,dl.c)

df.BI$pert="BI"

#### PLOT

library(viridis)

df=rbind(df.BI,df.D,df.R)
df=df[!df$vr%in%"survS/M",]
df$vr=factor(df$vr,levels=c("survS","survM"))

levels(df$vr) =c("Sapling\n\ survival (\u03B8)","Adult\n\ survival (\u03C6)")

df$delta[is.na(df$delta)]=0

df$pert=factor(df$pert,levels=c("Rain","Dens","BI"))

levels(df$pert)= c("Rain\n\ ","Intraspecific\n\ density","Interspecific\n\ density")

ggplot(df,aes(vr,delta,col=pert))+
  geom_boxplot(outlier.shape = NA)+
  scale_color_viridis(discrete = TRUE, alpha=1,end=0.9) +
  facet_wrap(.~species,scales = "free_y",ncol=3)+
  geom_hline(yintercept=0, 
             color = "black", size=0.5)+
  ylab(expression(paste("% change ", lambda)))+xlab("Demographic rate")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  labs(fill='Perturbed',colour='Perturbed')+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = c(0.82,0.18),
        legend.background = element_rect(color=NA),
        panel.border = element_rect(colour = "black"))


