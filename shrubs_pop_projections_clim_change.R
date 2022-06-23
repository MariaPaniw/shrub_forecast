
# This script performs analyses as part of Paniw et al. XXXX
# Please read the README file on GitHub for a workflow of the analyses

# Created by: Maria Paniw
# Date created: June 20, 2022

rm(list = ls())

library(coda)
library(MCMCvis)
library(plyr)
library(ggplot2)
library(ggnewscale)
library(boot)
library(patchwork)


setwd("SET_WORKING_DIRECTORY")

### Parameters 

load("allShrubs.Rdata")

#### COVARIATES & INITIAL DENSITIES 

### Interspecific densties 

load("Cistus libanotis.Rdata")

neigh1=C

load("Halimium commutatum.Rdata")

neigh2=C

load("Lavandula stoechas.Rdata")

neigh3=C


load("Rosmarinus officinalis.Rdata")

neigh4=C


load("Halimium halimifolium.Rdata")

neigh5=C

neighHal1=log(neigh1+neigh2+neigh3+neigh4)
neighHal2=log(neigh1+neigh3+neigh4+neigh5)
neighLav=log(neigh1+neigh2+neigh4+neigh5)
neighRos=log(neigh1+neigh2+neigh3+neigh5)
neighCis=log(neigh2+neigh3+neigh4+neigh5)

### Rainfall
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


survSap.h2 <- function(rain,ad,neigh){
  
  mu=inv.logit(out1$mean$b0.h2 +out1$mean$b1.h2*rain
                 + out1$mean$b2.h2*ad + out1$mean$b3.h2*neigh)
  
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

survSap.c <- function(rain,ad){
  
  mu=inv.logit(out1$mean$b0.c + out1$mean$b1.c*rain + out1$mean$b2.c*ad)
 
  return(mu)
}

####### METAPOPULATION MODEL

#define number of sites:
n.site=18
# define number of stages in MPM:
n.stage=3

# vec permutation approach
B.h1=B.l=B.c=B.r=B.h2=matrix(0,n.stage*n.site,n.stage*n.site) # placeholder block-diagonal demography matrix
M.h1=M.h2=M.l=M.r=M.c=matrix(0,n.site*n.stage,n.site*n.stage) # placeholder block-diagonal dispersal matrix

diag(M.h1)=diag(M.h2)=diag(M.l)=diag(M.r)=diag(M.c)=1

P=matrix(0,n.site*n.stage,n.site*n.stage) # vec‐permutation matrix

for(i in 1:n.stage){
  for(j in 1:n.site){
    E=matrix(0,n.stage,n.site)
    E[i,j]=1
    P=P+(E%x%t(E))
  }
} 
P.h1=P.h2=P.l=P.r=P.c=P

#### INITAL CONDITIONS

years=50 # years (or time steps) of simulations


num=read.csv("shrub_number.csv")

temp=num[num$species%in%"Halimium halimifolium"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.h1=NULL
for(k in 1:18){
  dens.h1=c(dens.h1,as.numeric(temp[k,]))
}


ad.h1=log(num[num$species%in%"Halimium halimifolium"&num$year%in%2010,c("X.adults")]+0.001)

temp=num[num$species%in%"Halimium commutatum"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.h2=NULL
for(k in 1:18){
  dens.h2=c(dens.h2,as.numeric(temp[k,]))
}


ad.h2=log(num[num$species%in%"Halimium commutatum"&num$year%in%2010,c("X.adults")])

temp=num[num$species%in%"Lavandula stoechas"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]
dens.l=NULL
for(k in 1:18){
  dens.l=c(dens.l,as.numeric(temp[k,]))
}


ad.l=log(num[num$species%in%"Lavandula stoechas"&num$year%in%2010,c("X.adults")]+0.001)

temp=num[num$species%in%"Rosmarinus officinalis"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.r=NULL
for(k in 1:18){
  dens.r=c(dens.r,as.numeric(temp[k,]))
}


ad.r=log(num[num$species%in%"Rosmarinus officinalis"&num$year%in%2010,c("X.adults")])

temp=num[num$species%in%"Cistus libanotis"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.c=NULL
for(k in 1:18){
  dens.c=c(dens.c,as.numeric(temp[k,]))
}


ad.c=log(num[num$species%in%"Cistus libanotis"&num$year%in%2010,c("X.adults")]+0.001)

neigh.h2=neighHal2[,4]
neigh.l=neighLav[,4]
neigh.r=neighRos[,4]
neigh.c=neighCis[,4]

length.sim=100

sim.df.h1=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.h2=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.l=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.r=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.c=array(0,c(n.site,n.stage,years,length.sim)) 

sim.rain=array(NA,c(years,length.sim))
sim.neigh=array(NA,c(years,length.sim))

for(s in 1:length.sim){
  print(paste("simulation", s))
  
  ### CLIMATE CHANGE 
  
  env=sample(c(1,2),years,prob=c(0.8,0.2),replace=T) # 1 is bad, 2 is good 
  
  for(i in 1:years){
    
    rain.t=ifelse(env[i]==1,sample(rain[rain<0],1),sample(rain[rain>=0],1))
    
    # FOR BASELINE: rain.t = sample(rain,1)
    
    sim.rain[i,s] = rain.t
    
    sim.neigh[i,s] = which(rain==rain.t)+1

    # Update abundance
    dens.h1=matrix(dens.h1,18,n.stage,byrow=T)
    dens.h2=matrix(dens.h2,18,n.stage,byrow=T)
    dens.l=matrix(dens.l,18,n.stage,byrow=T)
    dens.r=matrix(dens.r,18,n.stage,byrow=T)
    dens.c=matrix(dens.c,18,n.stage,byrow=T)
    
    sim.df.h1[,,i,s]= dens.h1
    sim.df.h2[,,i,s]= dens.h2
    sim.df.l[,,i,s]= dens.l
    sim.df.r[,,i,s]= dens.r
    sim.df.c[,,i,s]= dens.c

    dens.h1=c(t(dens.h1))
    dens.h2=c(t(dens.h2))
    dens.l=c(t(dens.l))
    dens.r=c(t(dens.r))
    dens.c=c(t(dens.c))
    
    neigh.h1=log(sim.df.h2[,n.stage,i,s] + sim.df.l[,n.stage,i,s] + sim.df.r[,n.stage,i,s] + sim.df.c[,n.stage,i,s])
    neigh.h2=log(sim.df.h1[,n.stage,i,s] + sim.df.l[,n.stage,i,s] + sim.df.r[,n.stage,i,s] + sim.df.c[,n.stage,i,s])
    neigh.l=log(sim.df.h2[,n.stage,i,s] + sim.df.h1[,n.stage,i,s] +sim.df.r[,n.stage,i,s] + sim.df.c[,n.stage,i,s])
    neigh.r=log(sim.df.h2[,n.stage,i,s] + sim.df.l[,n.stage,i,s] + sim.df.h1[,n.stage,i,s] + sim.df.c[,n.stage,i,s])
    neigh.c=log(sim.df.h2[,n.stage,i,s] + sim.df.l[,n.stage,i,s] + sim.df.r[,n.stage,i,s] + sim.df.h1[,n.stage,i,s])
    
    ad.h1=log(sim.df.h1[,n.stage,i,s]+0.0001)
    ad.h2=log(sim.df.h2[,n.stage,i,s]+0.0001)
    ad.l=log(sim.df.l[,n.stage,i,s]+0.0001)
    ad.r=log(sim.df.r[,n.stage,i,s]+0.0001)
    ad.c=log(sim.df.c[,n.stage,i,s]+0.0001)
    
    
    # LOCAL DEMOGRAPHY
    
    
    for(j in 1:18){
      ### Seedling numbers:
      
      seed.h1=sim.df.h1[j,1,i,s]
      seed.h2=sim.df.h2[j,1,i,s]
      seed.l=sim.df.l[j,1,i,s]
      seed.r=sim.df.r[j,1,i,s]
      seed.c=sim.df.c[j,1,i,s]
      
      # Hal1 
      survS.h1 = out1$mean$gamma.h1/seed.h1
      
      if(!is.finite(survS.h1)) survS.h1 <- 0
      if(survS.h1>1) survS.h1 <- 1
      
      mat.h1 = matrix(c(0,survS.h1,0,
                        0,1-survSap.h1(rain=rain.t,ad=ad.h1[j]),survSap.h1(rain=rain.t,ad=ad.h1[j]),
                        rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1[j])),n.stage,n.stage)
      
      survS.h2 = out1$mean$gamma.h2/seed.h2
      
      if(!is.finite(survS.h2)) survS.h2 <- 0
      if(survS.h2>1) survS.h2 <- 1
      
    
      mat.h2 = matrix(c(0,survS.h2,0,
                        0,1-survSap.h2(rain=rain.t,ad=ad.h2[j],neigh=neigh.h2[j]),survSap.h2(rain=rain.t,ad=ad.h2[j],neigh=neigh.h2[j]),
                        rec.h2,0,survM.h2(ad=ad.h2[j],neigh=neigh.h2[j])),n.stage,n.stage)
      
      survS.l = out1$mean$gamma.l/seed.l
      
      if(!is.finite(survS.l)) survS.l <- 0
      if(survS.l>1) survS.l <- 1
      
      mat.l = matrix(c(0,survS.l,0,
                       0,1-survSap.l(ad=ad.l[j],neigh=neigh.l[j]),survSap.l(ad=ad.l[j],neigh=neigh.l[j]),
                       rec.l,0,survM.l(neigh=neigh.l[j])),n.stage,n.stage)
      
      
      # survS.r = out1$mean$gamma.r/seed.r
      
      survS.r = 0.35/seed.r
      
      if(!is.finite(survS.r)) survS.r <- 0
      if(survS.r>1) survS.r <- 1
      
      mat.r = matrix(c(0,survS.r,0,
                       0,1-survSap.r(ad=ad.r[j],neigh=neigh.r[j]),survSap.r(ad=ad.r[j],neigh=neigh.r[j]),
                       rec.r,0,survM.r(ad=ad.r[j],neigh=neigh.r[j])),n.stage,n.stage)
      
      
      survS.c = out1$mean$gamma.c/seed.c
      
      if(!is.finite(survS.c)) survS.c <- 0
      if(survS.c>1) survS.c <- 1
      
      mat.c = matrix(c(0,survS.c,0,
                       0,1-survSap.c(rain=rain.t,ad=ad.c[j]),survSap.c(rain=rain.t,ad=ad.c[j]),
                       rec.c,0,survM.c(rain=rain.t,ad=ad.c[j],neigh=neigh.c[j])),n.stage,n.stage)
      
      if(j==1){
        
        B.h1[1:n.stage,1:n.stage] = mat.h1
        B.l[1:n.stage,1:n.stage] = mat.l
        B.c[1:n.stage,1:n.stage] = mat.c
        B.h2[1:n.stage,1:n.stage] = mat.h2
        B.r[1:n.stage,1:n.stage] = mat.r
        
      }else{
        
        B.h1[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.h1
        B.l[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.l
        B.c[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.c
        B.h2[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.h2
        B.r[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.r
        
        
      }
      
      # Dispersal for all! 
      # decide if disperse
      
      mig=sample(c(0,1),5,prob=c(0.7,0.3),replace = T)
      
      if(mig[1]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[2]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[3]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[4]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[5]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
      }
      
      
      
    }
    
    # UPDATE DENSITIES
    
    
    dens.h1=t(P.h1)%*%M.h1%*%P.h1%*%B.h1%*%dens.h1
    
    dens.h2=t(P.h2)%*%M.h2%*%P.h2%*%B.h2%*%dens.h2
    
    dens.l=t(P.l)%*%M.l%*%P.l%*%B.l%*%dens.l
    
    dens.r=t(P.r)%*%M.r%*%P.r%*%B.r%*%dens.r
    
    dens.c=t(P.c)%*%M.c%*%P.c%*%B.c%*%dens.c
    
    # UPDATE DISPERSAL
    M.h1=M.h2=M.l=M.r=M.c=matrix(0,n.site*n.stage,n.site*n.stage) # placeholder block-diagonal dispersal matrix
    
    diag(M.h1)=diag(M.h2)=diag(M.l)=diag(M.r)=diag(M.c)=1
    
}
  
}

sim.df.h1.cc=sim.df.h1
sim.df.h2.cc=sim.df.h2
sim.df.l.cc=sim.df.l
sim.df.r.cc=sim.df.r
sim.df.c.cc=sim.df.c

### PERTURBATION OF NEIGHBORS

#### INITAL CONDITIONS

years=50 # years (or time steps) of simulations


num=read.csv("shrub_number.csv")

temp=num[num$species%in%"Halimium halimifolium"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.h1=NULL
for(k in 1:18){
  dens.h1=c(dens.h1,as.numeric(temp[k,]))
}

ad.h1=log(num[num$species%in%"Halimium halimifolium"&num$year%in%2010,c("X.adults")]+0.001)

temp=num[num$species%in%"Halimium commutatum"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.h2=NULL
for(k in 1:18){
  dens.h2=c(dens.h2,as.numeric(temp[k,]))
}


ad.h2=log(num[num$species%in%"Halimium commutatum"&num$year%in%2010,c("X.adults")])

temp=num[num$species%in%"Lavandula stoechas"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]
dens.l=NULL
for(k in 1:18){
  dens.l=c(dens.l,as.numeric(temp[k,]))
}



ad.l=log(num[num$species%in%"Lavandula stoechas"&num$year%in%2010,c("X.adults")]+0.001)

temp=num[num$species%in%"Rosmarinus officinalis"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.r=NULL
for(k in 1:18){
  dens.r=c(dens.r,as.numeric(temp[k,]))
}


ad.r=log(num[num$species%in%"Rosmarinus officinalis"&num$year%in%2010,c("X.adults")])

temp=num[num$species%in%"Cistus libanotis"&num$year%in%2010,c("X.seedlings","X.saplings","X.adults")]

dens.c=NULL
for(k in 1:18){
  dens.c=c(dens.c,as.numeric(temp[k,]))
}


ad.c=log(num[num$species%in%"Cistus libanotis"&num$year%in%2010,c("X.adults")]+0.001)

neigh.h2=neighHal2[,4]
neigh.l=neighLav[,4]
neigh.r=neighRos[,4]
neigh.c=neighCis[,4]

length.sim=100

sim.df.h1=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.h2=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.l=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.r=array(0,c(n.site,n.stage,years,length.sim)) 
sim.df.c=array(0,c(n.site,n.stage,years,length.sim)) 


for(s in 1:length.sim){
  print(paste("simulation", s))
  
  ### CLIMATE CHANGE 
  
  for(i in 1:years){
    
    rain.t=sim.rain[i,s]
   
    # Update abundance
    dens.h1=matrix(dens.h1,18,n.stage,byrow=T)
    dens.h2=matrix(dens.h2,18,n.stage,byrow=T)
    dens.l=matrix(dens.l,18,n.stage,byrow=T)
    dens.r=matrix(dens.r,18,n.stage,byrow=T)
    dens.c=matrix(dens.c,18,n.stage,byrow=T)
    
    sim.df.h1[,,i,s]= dens.h1
    sim.df.h2[,,i,s]= dens.h2
    sim.df.l[,,i,s]= dens.l
    sim.df.r[,,i,s]= dens.r
    sim.df.c[,,i,s]= dens.c
    
    
    dens.h1=c(t(dens.h1))
    dens.h2=c(t(dens.h2))
    dens.l=c(t(dens.l))
    dens.r=c(t(dens.r))
    dens.c=c(t(dens.c))
    
    neigh.h1=neighHal1[,sim.neigh[i,s]]
    neigh.h2=neighHal2[,sim.neigh[i,s]]
    neigh.l=neighLav[,sim.neigh[i,s]]
    neigh.r=neighRos[,sim.neigh[i,s]]
    neigh.c=neighCis[,sim.neigh[i,s]]
    
    ad.h1=log(sim.df.h1[,n.stage,i,s]+0.0001)
    ad.h2=log(sim.df.h2[,n.stage,i,s]+0.0001)
    ad.l=log(sim.df.l[,n.stage,i,s]+0.0001)
    ad.r=log(sim.df.r[,n.stage,i,s]+0.0001)
    ad.c=log(sim.df.c[,n.stage,i,s]+0.0001)
    
    
    # LOCAL DEMOGRAPHY
    
    
    for(j in 1:18){
      ### Seedling numbers:
      
      seed.h1=sim.df.h1[j,1,i,s]
      seed.h2=sim.df.h2[j,1,i,s]
      seed.l=sim.df.l[j,1,i,s]
      seed.r=sim.df.r[j,1,i,s]
      seed.c=sim.df.c[j,1,i,s]
      # Hal1 
      survS.h1 = out1$mean$gamma.h1/seed.h1
      
      if(!is.finite(survS.h1)) survS.h1 <- 0
      if(survS.h1>1) survS.h1 <- 1
      
      mat.h1 = matrix(c(0,survS.h1,0,
                        0,1-survSap.h1(rain=rain.t,ad=ad.h1[j]),survSap.h1(rain=rain.t,ad=ad.h1[j]),
                        rec.h1,0,survM.h1(rain=rain.t,neigh=neigh.h1[j])),n.stage,n.stage)
      
      survS.h2 = out1$mean$gamma.h2/seed.h2
      
      if(!is.finite(survS.h2)) survS.h2 <- 0
      if(survS.h2>1) survS.h2 <- 1
      
      
      mat.h2 = matrix(c(0,survS.h2,0,
                        0,1-survSap.h2(rain=rain.t,ad=ad.h2[j],neigh=neigh.h2[j]),survSap.h2(rain=rain.t,ad=ad.h2[j],neigh=neigh.h2[j]),
                        rec.h2,0,survM.h2(ad=ad.h2[j],neigh=neigh.h2[j])),n.stage,n.stage)
      
      survS.l = out1$mean$gamma.l/seed.l
      
      if(!is.finite(survS.l)) survS.l <- 0
      if(survS.l>1) survS.l <- 1
      
      mat.l = matrix(c(0,survS.l,0,
                       0,1-survSap.l(ad=ad.l[j],neigh=neigh.l[j]),survSap.l(ad=ad.l[j],neigh=neigh.l[j]),
                       rec.l,0,survM.l(neigh=neigh.l[j])),n.stage,n.stage)
      
      
      # survS.r = out1$mean$gamma.r/seed.r
      
      survS.r = 0.35/seed.r
      
      if(!is.finite(survS.r)) survS.r <- 0
      if(survS.r>1) survS.r <- 1
      
      mat.r = matrix(c(0,survS.r,0,
                       0,1-survSap.r(ad=ad.r[j],neigh=neigh.r[j]),survSap.r(ad=ad.r[j],neigh=neigh.r[j]),
                       rec.r,0,survM.r(ad=ad.r[j],neigh=neigh.r[j])),n.stage,n.stage)
      
      
      survS.c = out1$mean$gamma.c/seed.c
      
      if(!is.finite(survS.c)) survS.c <- 0
      if(survS.c>1) survS.c <- 1
      
      mat.c = matrix(c(0,survS.c,0,
                       0,1-survSap.c(rain=rain.t,ad=ad.c[j]),survSap.c(rain=rain.t,ad=ad.c[j]),
                       rec.c,0,survM.c(rain=rain.t,ad=ad.c[j],neigh=neigh.c[j])),n.stage,n.stage)
      
      if(j==1){
        
        B.h1[1:n.stage,1:n.stage] = mat.h1
        B.l[1:n.stage,1:n.stage] = mat.l
        B.c[1:n.stage,1:n.stage] = mat.c
        B.h2[1:n.stage,1:n.stage] = mat.h2
        B.r[1:n.stage,1:n.stage] = mat.r
        
      }else{
        
        B.h1[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.h1
        B.l[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.l
        B.c[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.c
        B.h2[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.h2
        B.r[((j-1)*n.stage+1):(j*n.stage),((j-1)*n.stage+1):(j*n.stage)] = mat.r
        
        
      }
      
      # Dispersal for all! 
      # decide if disperse
      
      mig=sample(c(0,1),5,prob=c(0.7,0.3),replace = T)
      
      if(mig[1]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.h1[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[2]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.h2[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[3]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.l[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[4]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.r[site,18*2+j]=0.001/5
        }
        
      }
      
      if(mig[5]==1){
        
        if(j%in%c(1:6)){
          site=c(1:6)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(7:12)){
          site=c(7:12)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
        if(j%in%c(13:18)){
          site=c(13:18)
          site=site[-j]
          M.c[site,18*2+j]=0.001/5
        }
        
      }
      
      
      
    }
    
    # UPDATE DENSITIES
    
    
    dens.h1=t(P.h1)%*%M.h1%*%P.h1%*%B.h1%*%dens.h1
    
    dens.h2=t(P.h2)%*%M.h2%*%P.h2%*%B.h2%*%dens.h2
    
    dens.l=t(P.l)%*%M.l%*%P.l%*%B.l%*%dens.l
    
    dens.r=t(P.r)%*%M.r%*%P.r%*%B.r%*%dens.r
    
    dens.c=t(P.c)%*%M.c%*%P.c%*%B.c%*%dens.c
    
    # UPDATE DISPERSAL
    M.h1=M.h2=M.l=M.r=M.c=matrix(0,n.site*n.stage,n.site*n.stage) # placeholder block-diagonal dispersal matrix
    
    diag(M.h1)=diag(M.h2)=diag(M.l)=diag(M.r)=diag(M.c)=1
    
    
  }
  
}

sim.df.h1.cc.pert=sim.df.h1
sim.df.h2.cc.pert=sim.df.h2
sim.df.l.cc.pert=sim.df.l
sim.df.r.cc.pert=sim.df.r
sim.df.c.cc.pert=sim.df.c

#### PLOTS

# Abundances from baseline simulaitons (not run here)

load("h1_base.Rdata")
load("h2_base.Rdata")
load("l_base.Rdata")
load("r_base.Rdata")
load("c_base.Rdata")

df=NULL
df.mu=NULL
df.var=NULL

temp=adply(sim.df.h1,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. halimifolium"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. halimifolium"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. halimifolium"

df.var=rbind(df.var,tempV2)

# Hal2

temp=adply(sim.df.h2,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. commutatum"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. commutatum"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. commutatum"

df.var=rbind(df.var,tempV2)


temp=adply(sim.df.l,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="L. stoechas"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="L. stoechas"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="L. stoechas"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.r,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="R. officinalis"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="R. officinalis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="R. officinalis"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.c,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="C. libanotis"

df=rbind(df,tempV2)

df$year=as.numeric(df$year)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="C. libanotis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="C. libanotis"

df.var=rbind(df.var,tempV2)

df.mu$clim="base"
df.var$clim="base"

df.mu$pert="no"
df.var$pert="no"

all.mu=df.mu
all.var=df.var

df.mu.b1=df.mu
df.var.b1=df.var


############# BASE PERTURBED (not run)

load("h1_base_pert.Rdata")
load("h2_base_pert.Rdata")
load("l_base_pert.Rdata")
load("r_base_pert.Rdata")
load("c_base_pert.Rdata")

df=NULL
df.mu=NULL
df.var=NULL

temp=adply(sim.df.h1,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. halimifolium"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. halimifolium"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. halimifolium"

df.var=rbind(df.var,tempV2)

# Hal2

temp=adply(sim.df.h2,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. commutatum"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. commutatum"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. commutatum"

df.var=rbind(df.var,tempV2)


temp=adply(sim.df.l,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="L. stoechas"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="L. stoechas"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="L. stoechas"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.r,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="R. officinalis"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="R. officinalis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="R. officinalis"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.c,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="C. libanotis"

df=rbind(df,tempV2)

df$year=as.numeric(df$year)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="C. libanotis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="C. libanotis"

df.var=rbind(df.var,tempV2)

df.mu$clim="base"
df.var$clim="base"

df.mu$pert="yes"
df.var$pert="yes"

all.mu=rbind(all.mu,df.mu)
all.var=rbind(all.var,df.var)

df.mu.b2=df.mu
df.var.b2=df.var


############# CLIMATE CHANGE 

df=NULL
df.mu=NULL
df.var=NULL

temp=adply(sim.df.h1.cc,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. halimifolium"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. halimifolium"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. halimifolium"

df.var=rbind(df.var,tempV2)

# Hal2

temp=adply(sim.df.h2.cc,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. commutatum"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. commutatum"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. commutatum"

df.var=rbind(df.var,tempV2)


temp=adply(sim.df.l.cc,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="L. stoechas"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="L. stoechas"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="L. stoechas"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.r.cc,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="R. officinalis"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="R. officinalis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="R. officinalis"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.c.cc,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="C. libanotis"

df=rbind(df,tempV2)

df$year=as.numeric(df$year)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="C. libanotis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="C. libanotis"

df.var=rbind(df.var,tempV2)

df.mu$clim="cc"
df.var$clim="cc"

df.mu$pert="no"
df.var$pert="no"

all.mu=rbind(all.mu,df.mu)
all.var=rbind(all.var,df.var)

df.mu.cc1=df.mu
df.var.cc1=df.var

############# CLIMATE CHANGE PERTURBED

df=NULL
df.mu=NULL
df.var=NULL

temp=adply(sim.df.h1.cc.pert,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. halimifolium"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. halimifolium"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. halimifolium"

df.var=rbind(df.var,tempV2)

# Hal2

temp=adply(sim.df.h2.cc.pert,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="H. commutatum"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="H. commutatum"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="H. commutatum"

df.var=rbind(df.var,tempV2)


temp=adply(sim.df.l.cc.pert,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="L. stoechas"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="L. stoechas"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="L. stoechas"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.r.cc.pert,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="R. officinalis"

df=rbind(df,tempV2)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="R. officinalis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="R. officinalis"

df.var=rbind(df.var,tempV2)

temp=adply(sim.df.c.cc.pert,c(1,2,3,4))

colnames(temp)=c("site","stage","year","pu","density")

levels(temp$stage)=c("R","S","A")

tempV2=aggregate(density~stage+pu+site,mean,data=temp) 
tempV3=tempV2 

tempV2$species="C. libanotis"

df=rbind(df,tempV2)

df$year=as.numeric(df$year)

tempV2=aggregate(density~stage+site,mean,data=tempV3) 

tempV2$species="C. libanotis"

df.mu=rbind(df.mu,tempV2)

tempV2=aggregate(density~stage+site,CV,data=tempV3) 

tempV2$species="C. libanotis"

df.var=rbind(df.var,tempV2)

df.mu$clim="cc"
df.var$clim="cc"

df.mu$pert="yes"
df.var$pert="yes"

all.mu=rbind(all.mu,df.mu)
all.var=rbind(all.var,df.var)

df.mu.cc2=df.mu
df.var.cc2=df.var

### PLOT
library(dplyr)

df.mu.b1$ab.cc=left_join(df.mu.b1,df.mu.cc1,by=c("stage","site","species"))$density.y
df.mu.b2$ab.cc=left_join(df.mu.b2,df.mu.cc2,by=c("stage","site","species"))$density.y

df.mu.b1$contrast=df.mu.b1$ab.cc-df.mu.b1$density
df.mu.b2$contrast=df.mu.b2$ab.cc-df.mu.b2$density

all.mu.cont=rbind(df.mu.b1,df.mu.b2)

a=ggplot(all.mu.cont[all.mu.cont$stage%in%"A",],aes(species,contrast,col=pert))+
  geom_boxplot(outlier.shape = NA)+
  scale_color_manual(values=c("black","red")) +
  geom_hline(yintercept=0, 
             color = "black", size=0.5)+
  ylab("Change in mean abundance")+xlab("")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  labs(fill='Dynamic',colour='Dynamic')+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"),
        legend.background = element_rect(color=NA),
        panel.border = element_rect(colour = "black"))

a



b=ggplot(all.mu.cont,aes(species,contrast,col=pert))+
  geom_boxplot(outlier.shape = NA)+
  scale_color_manual(values=c("black","red")) +
  facet_grid(stage~.,scales = "free")+
  geom_hline(yintercept=0, 
             color = "black", size=0.5)+
  ylab("Change in mean abundance")+xlab("")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  labs(fill='Dynamic',colour='Dynamic')+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"),
        legend.background = element_rect(color=NA),
        panel.border = element_rect(colour = "black"))

b

df.var.b1$ab.cc=left_join(df.var.b1,df.var.cc1,by=c("stage","site","species"))$density.y
df.var.b2$ab.cc=left_join(df.var.b2,df.var.cc2,by=c("stage","site","species"))$density.y

df.var.b1$contrast=df.var.b1$ab.cc-df.var.b1$density
df.var.b2$contrast=df.var.b2$ab.cc-df.var.b2$density

all.var.cont=rbind(df.var.b1,df.var.b2)


c=ggplot(all.var.cont,aes(species,contrast,col=pert))+
  geom_boxplot(outlier.shape = NA)+
  scale_color_manual(values=c("black","red")) +
  facet_grid(stage~.,scales = "free")+
  geom_hline(yintercept=0, 
             color = "black", size=0.5)+
  ylab("Change in CV in abundance")+xlab("")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  labs(fill='Dynamic',colour='Dynamic')+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"),
        legend.background = element_rect(color=NA),
        panel.border = element_rect(colour = "black"))

c



