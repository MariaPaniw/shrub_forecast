
# This script performs analyses as part of Paniw et al. XXXX
# Please read the README file on GitHub for a workflow of the analyses

# Created by: Maria Paniw
# Date created: June 20, 2022

rm(list = ls())

library(ggplot2)
library(Epi)
library(dplyr)

setwd("...") # Set the working directory to where the data are in 

# Number of seedling, saplings, and adult shrubs per plot, year, and species  

num=read.csv("shrub_number.csv")

### Rainfall data

precip=read.csv("precipPalacios.csv")


### INTERSPECIFIC DENSITY

# These files are derived from shrub_number.csv: 
# Essentially, numbers of each species in an array (18 sites, 15 years).
# Note, that abundances for years that were not measured (3,5-6,8-10,12) were obtained by averaging across previous years. 
# These averages are however not used in anaylses! 

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

# Define interspecific density for each shrub species 

neighHal1=log(neigh1+neigh2+neigh3+neigh4) 
neighHal2=log(neigh1+neigh3+neigh4+neigh5)
neighLav=log(neigh1+neigh2+neigh4+neigh5)
neighRos=log(neigh1+neigh2+neigh3+neigh5)
neighCis=log(neigh2+neigh3+neigh4+neigh5)

### PREPARE DATA FOR ANALYSES 

# Create arrays for juveniles and adults

# Note that I average abundances from 2008 & 2010 for 2009; and from 2017 and 2019 for 2018

# HALIMIUM HALIMIFOLIUM

# ADULTS 

hal1=num_sub
hal1=rbind(hal1[,c(2,3,5)],data.frame(plot=unique(hal1$plot),year=2018,
                                             X.adults=round(c((hal1$X.adults[hal1$year==2017]+hal1$X.adults[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.adults=round(c((hal1$X.adults[hal1$year==2008]+hal1$X.adults[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.adults=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.adults")])
    
    
  }
  
}

# Bundle and summarize data set


CA.hal1=C

# SAPLINGS 

hal1=num_sub
hal1=rbind(hal1[,c(2,3,6)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.saplings=round(c((hal1$X.saplings[hal1$year==2017]+hal1$X.saplings[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.saplings=round(c((hal1$X.saplings[hal1$year==2008]+hal1$X.saplings[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.saplings=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.saplings")])
    
    
  }
  
}

# Bundle and summarize data set

C[3,c(2,3)] =0

CS.hal1=C

  #### HALIMIUM COMMOTATUM

num_sub=num[num$species%in%"Halimium commutatum",]

### ADULTS

hal1=num_sub
hal1=rbind(hal1[,c(2,3,5)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.adults=round(c((hal1$X.adults[hal1$year==2017]+hal1$X.adults[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.adults=round(c((hal1$X.adults[hal1$year==2008]+hal1$X.adults[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.adults=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.adults")])
    
    
  }
  
}

CA.hal2=C

# SAPLLING

hal1=num_sub
hal1=rbind(hal1[,c(2,3,6)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.saplings=round(c((hal1$X.saplings[hal1$year==2017]+hal1$X.saplings[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.saplings=round(c((hal1$X.saplings[hal1$year==2008]+hal1$X.saplings[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.saplings=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.saplings")])
    
    
  }
  
}

# Bundle and summarize data set

C[3,c(1,2,3)] =0

CS.hal2=C

##### LAVANDULA STOECHAS

num_sub=num[num$species%in%"Lavandula stoechas",]

# ADULTS

hal1=num_sub
hal1=rbind(hal1[,c(2,3,5)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.adults=round(c((hal1$X.adults[hal1$year==2017]+hal1$X.adults[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.adults=round(c((hal1$X.adults[hal1$year==2008]+hal1$X.adults[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.adults=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.adults")])
    
    
  }
  
}

CA.lav=C

# SAPLING

hal1=num_sub
hal1=rbind(hal1[,c(2,3,6)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.saplings=round(c((hal1$X.saplings[hal1$year==2017]+hal1$X.saplings[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.saplings=round(c((hal1$X.saplings[hal1$year==2008]+hal1$X.saplings[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.saplings=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.saplings")])
    
    
  }
  
}

CS.lav=C

##### ROSMARINUS OFFICINALIS

num_sub=num[num$species%in%"Rosmarinus officinalis",]

# ADULTS 

hal1=num_sub
hal1=rbind(hal1[,c(2,3,5)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.adults=round(c((hal1$X.adults[hal1$year==2017]+hal1$X.adults[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.adults=round(c((hal1$X.adults[hal1$year==2008]+hal1$X.adults[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.adults=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.adults")])
    
    
  }
  
}

CA.ros=C

# SAPLING

hal1=num_sub
hal1=rbind(hal1[,c(2,3,6)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.saplings=round(c((hal1$X.saplings[hal1$year==2017]+hal1$X.saplings[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.saplings=round(c((hal1$X.saplings[hal1$year==2008]+hal1$X.saplings[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.saplings=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.saplings")])
    
    
  }
  
}


CS.ros=C

##### CISTUS LIBANOTIS

num_sub=num[num$species%in%"Cistus libanotis",]

# ADULTS

hal1=num_sub
hal1=rbind(hal1[,c(2,3,5)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.adults=round(c((hal1$X.adults[hal1$year==2017]+hal1$X.adults[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.adults=round(c((hal1$X.adults[hal1$year==2008]+hal1$X.adults[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.adults=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.adults=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.adults")])
    
    
  }
  
}

CA.cis=C
CA.cis[4,c(2,3)]=1
CA.cis[4,c(12,13)]=0

# SAPLING

hal1=num_sub
hal1=rbind(hal1[,c(2,3,6)],data.frame(plot=unique(hal1$plot),year=2018,
                                      X.saplings=round(c((hal1$X.saplings[hal1$year==2017]+hal1$X.saplings[hal1$year==2019])/2))),
           data.frame(plot=unique(hal1$plot),year=2009,
                      X.saplings=round(c((hal1$X.saplings[hal1$year==2008]+hal1$X.saplings[hal1$year==2010])/2))),
           data.frame(plot=unique(hal1$plot),year=2011,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2012,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2014,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2015,
                      X.saplings=NA),
           data.frame(plot=unique(hal1$plot),year=2016,
                      X.saplings=NA))

hal1=hal1[order(hal1$year,hal1$plot),]

n.site=length(unique(hal1$plot))

n.years=length(unique(hal1$year))


C=array(NA,c(n.site,n.years))

for(i in 1:n.site){ # site loop
  for(k in 1:n.years){
    
    C[i,k]=as.numeric(hal1[hal1$plot%in%unique(hal1$plot)[i]&hal1$year%in%unique(hal1$year)[k],c("X.saplings")])
    
    
  }
  
}

CS.cis=C

##########

str(bdata <- list(nA.hal1 = CA.hal1, CA.hal1=CA.hal1,
                  nS.hal1 = CS.hal1, CS.hal1=CS.hal1, 
                  nA.hal2 = CA.hal2, CA.hal2=CA.hal2,
                  nS.hal2 = CS.hal2, CS.hal2=CS.hal2,
                  nA.lav = CA.lav, CA.lav=CA.lav,
                  nS.lav = CS.lav, CS.lav=CS.lav,
                  nA.ros = CA.ros, CA.ros=CA.ros,
                  nS.ros = CS.ros, CS.ros=CS.ros, 
                  nA.cis = CA.cis, CA.cis=CA.cis,
                  nS.cis = CS.cis, CS.cis=CS.cis,
                  nsites = dim(CA.hal1)[1],
                  rain=as.numeric(scale(precip$Prec.[precip$year%in%c(2007:2021)])),
                  neigh.cis=neighCis,
                  neigh.hal1=neighHal1,
                  neigh.lav=neighLav,
                  neigh.hal2=neighHal2,
                  neigh.ros=neighRos,
                  nyears = dim(CA.hal1)[2]
                  ))

# Specify model in BUGS language

# THIS IS THE FINAL MODEL

# "S" in variable name defines saplings
# "A" in variable name defines adult

cat(file = "allShrubs.txt","
    model {
    # Priors

    a0.h1 ~ dnorm( 0 , 1.0E-05 )
    a1.h1 ~ dnorm( 0 , 1.0E-05 ) 
    a3.h1 ~ dnorm( 0 , 1.0E-05 )
    


    b0.h1 ~ dnorm( 0 , 1.0E-05 )
    b2.h1 ~ dnorm( 0 , 1.0E-05 )

    a0.h2 ~ dnorm( 0 , 1.0E-05 )
    a2.h2 ~ dnorm( 0 , 1.0E-05 )
    a3.h2 ~ dnorm( 0 , 1.0E-05 )
    


    b0.h2 ~ dnorm( 0 , 1.0E-05 )
    b1.h2 ~ dnorm( 0 , 1.0E-05 )
    b2.h2 ~ dnorm( 0 , 1.0E-05 )
    b3.h2 ~ dnorm( 0 , 1.0E-05 )

    a0.l ~ dnorm( 0 , 1.0E-05 )
    a3.l ~ dnorm( 0 , 1.0E-05 )
    


    b0.l ~ dnorm( 0 , 1.0E-05 )
    b2.l ~ dnorm( 0 , 1.0E-05 )
    b3.l ~ dnorm( 0 , 1.0E-05 )
    
    a0.r ~ dnorm( 0 , 1.0E-05 ) 
    a2.r ~ dnorm( 0 , 1.0E-05 )
    a3.r ~ dnorm( 0 , 1.0E-05 )
    


    b0.r ~ dnorm( 0 , 1.0E-05 )
    b2.r ~ dnorm( 0 , 1.0E-05 )
    b3.r ~ dnorm( 0 , 1.0E-05 )

    a0.c ~ dnorm( 0 , 1.0E-05 )
    a1.c ~ dnorm( 0 , 1.0E-05 ) 
    a2.c ~ dnorm( 0 , 1.0E-05 )
    a3.c ~ dnorm( 0 , 1.0E-05 )
    


    b0.c ~ dnorm( 0 , 1.0E-05 )
    b1.c ~ dnorm( 0 , 1.0E-05 )
    b2.c ~ dnorm( 0 , 1.0E-05 )

    # Recruitment
    gamma.h1 ~ dunif(0, 10) 
    gamma.h2 ~ dunif(0, 10) 
    gamma.l ~ dunif(0, 10)
    gamma.r ~ dunif(0, 10)
    gamma.c ~ dunif(0, 10)

    for(i in 1:nsites) { # Loop over sites
    
    #Initial abundance
    
    NA.h1[i,1] <- CA.hal1[i,1]
    NS.h1[i,1] <- CS.hal1[i,1]
    NA.h2[i,1] <- CA.hal2[i,1]
    NS.h2[i,1] <- CS.hal2[i,1]
    NA.l[i,1] <- CA.lav[i,1]
    NS.l[i,1] <- CS.lav[i,1]
    NA.r[i,1] <- CA.ros[i,1]
    NS.r[i,1] <- CS.ros[i,1]
    NA.c[i,1] <- CA.cis[i,1]
    NS.c[i,1] <- CS.cis[i,1]

# Initial neighbor

    neigh.c[i,1] <- neigh.cis[i,1]
    neigh.l[i,1] <- neigh.lav[i,1]
    neigh.h2[i,1] <- neigh.hal2[i,1]
    neigh.h1[i,1] <- neigh.hal1[i,1]
    neigh.r[i,1] <- neigh.ros[i,1]

    #Specify the model for years 2 through nYears
   
    for(t in 1:(nyears-1)) {
    
    
# Halimium halimifolium

    nA.hal1[i,t+1] ~ dpois(NA.h1[i,t+1])
    nS.hal1[i,t+1] ~ dpois(NS.h1[i,t+1])
    
    phiA.h1[i,t] <- 1/(1+exp(-( a0.h1 + a1.h1 *rain[t]  + a3.h1 * neigh.h1[i,t])))
    phiS.h1[i,t] <- 1/(1+exp(-( b0.h1 + b2.h1 * log(NA.h1[i,t]+0.001))))
    
    G.h1[i,t] ~ dpois(gamma.h1)
    
    NA.h1[i,t+1] <- phiA.h1[i,t] *  NA.h1[i,t] +  phiS.h1[i,t] *  NS.h1[i,t]
    NS.h1[i,t+1] <- (1 - phiS.h1[i,t] ) *  NS.h1[i,t] + G.h1[i,t]

    
 # Halimium commutatum

    nA.hal2[i,t+1] ~ dpois(NA.h2[i,t+1])
    nS.hal2[i,t+1] ~ dpois(NS.h2[i,t+1])
    
    phiA.h2[i,t] <- 1/(1+exp(-( a0.h2 + a2.h2 * log(NA.h2[i,t]+0.001) + a3.h2 * neigh.h2[i,t] )))
    phiS.h2[i,t] <- 1/(1+exp(-( b0.h2 + b1.h2 *rain[t] + b2.h2 * log(NA.h2[i,t]+0.001)+ b3.h2 * neigh.h2[i,t])))
    
    G.h2[i,t] ~ dpois(gamma.h2)
    
    NA.h2[i,t+1] <- phiA.h2[i,t] *  NA.h2[i,t] +  phiS.h2[i,t] *  NS.h2[i,t]
    NS.h2[i,t+1] <- (1 - phiS.h2[i,t] ) *  NS.h2[i,t] + G.h2[i,t]


# Lavandula stoechas 

    nA.lav[i,t+1] ~ dpois(NA.l[i,t+1])
    nS.lav[i,t+1] ~ dpois(NS.l[i,t+1])
    
     phiA.l[i,t] <- 1/(1+exp(-( a0.l + a3.l * neigh.l[i,t]  )))
     phiS.l[i,t] <- 1/(1+exp(-( b0.l + b2.l * log(NA.l[i,t]+0.001) + b3.l * neigh.l[i,t])))

    G.l[i,t] ~ dpois(gamma.l)
    
    NA.l[i,t+1] <- phiA.l[i,t] *  NA.l[i,t] +  phiS.l[i,t] *  NS.l[i,t]
    NS.l[i,t+1] <- (1 - phiS.l[i,t] ) *  NS.l[i,t] + G.l[i,t]

     
## Rosmarinus officinalis

    nA.ros[i,t+1] ~ dpois(NA.r[i,t+1])
    nS.ros[i,t+1] ~ dpois(NS.r[i,t+1])
    
    phiA.r[i,t] <- 1/(1+exp(-( a0.r + a2.r * log(NA.r[i,t]+0.001) + a3.r * neigh.r[i,t])))
    phiS.r[i,t] <- 1/(1+exp(-( b0.r + b2.r * log(NA.r[i,t]+0.001) + b3.r * neigh.r[i,t])))
    
    G.r[i,t] ~ dpois(gamma.r)
    
    NA.r[i,t+1] <- phiA.r[i,t] *  NA.r[i,t] +  phiS.r[i,t] *  NS.r[i,t]
    NS.r[i,t+1] <- (1 - phiS.r[i,t] ) *  NS.r[i,t] + G.r[i,t]

## Cistus libanotis 

    nA.cis[i,t+1] ~ dpois(NA.c[i,t+1])
    nS.cis[i,t+1] ~ dpois(NS.c[i,t+1])
    
    phiA.c[i,t] <- 1/(1+exp(-( a0.c + a1.c *rain[t] + a2.c * log(NA.c[i,t]+0.001) + a3.c * neigh.c[i,t])))
    phiS.c[i,t] <- 1/(1+exp(-( b0.c + b1.c *rain[t] + b2.c * log(NA.c[i,t]+0.001) )))
    
    G.c[i,t] ~ dpois(gamma.c)
    
    NA.c[i,t+1] <- phiA.c[i,t] *  NA.c[i,t] +  phiS.c[i,t] *  NS.c[i,t]
    NS.c[i,t+1] <- (1 - phiS.c[i,t] ) *  NS.c[i,t] + G.c[i,t]

    
###   Neighborhood

    neigh.c[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
    neigh.h1[i,t+1] <- log(NA.h2[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
    neigh.h2[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
    neigh.l[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
    neigh.r[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.l[i,t+1]+NA.h2[i,t+1])
    

    }

    }

    for( t in 2:nyears){
    NtotA.h1[t] <- sum(NA.h1[,t])
    NtotS.h1[t] <- sum(NS.h1[,t])
    
    NtotA.h2[t] <- sum(NA.h2[,t])
    NtotS.h2[t] <- sum(NS.h2[,t])
    
    NtotA.l[t] <- sum(NA.l[,t])
    NtotS.l[t] <- sum(NS.l[,t])
    
    NtotA.r[t] <- sum(NA.r[,t])
    NtotS.r[t] <- sum(NS.r[,t])
    
    NtotA.c[t] <- sum(NA.c[,t])
    NtotS.c[t] <- sum(NS.c[,t])
    
    }
    
}
    ")

inits <- function(){list(a0.h1=rnorm(1,0,0.01),
                         a1.h1=rnorm(1,0,0.01),
                         a3.h1=rnorm(1,0,0.01),
                         b0.h1=rnorm(1,0,0.01),
                         b2.h1=rnorm(1,0,0.01),
                         gamma.h1=1,
                         a0.h2=rnorm(1,0,0.01),
                         a2.h2=rnorm(1,0,0.01),
                         a3.h2=rnorm(1,0,0.01),
                         b0.h2=rnorm(1,0,0.01),
                         b1.h2=rnorm(1,0,0.01),
                         b2.h2=rnorm(1,0,0.01),
                         b3.h2=rnorm(1,0,0.01),
                         gamma.h2=1,
                         a0.l=rnorm(1,0,0.01),
                         a3.l=rnorm(1,0,0.01),
                         b0.l=rnorm(1,0,0.01),
                         b2.l=rnorm(1,0,0.01),
                         b3.l=rnorm(1,0,0.01),
                         gamma.l=1,
                         a0.r=rnorm(1,0,0.01),
                         a2.r=rnorm(1,0,0.01),
                         a3.r=rnorm(1,0,0.01),
                         b0.r=rnorm(1,0,0.01),
                         b2.r=rnorm(1,0,0.01),
                         b3.r=rnorm(1,0,0.01),
                         gamma.r=1,
                         a0.c=rnorm(1,0,0.01),
                         a1.c=rnorm(1,0,0.01),
                         a2.c=rnorm(1,0,0.01),
                         a3.c=rnorm(1,0,0.01),
                         b0.c=rnorm(1,0,0.01),
                         b1.c=rnorm(1,0,0.01),
                         b2.c=rnorm(1,0,0.01),
                         gamma.c=1)}

params <- c( "a0.h1",
             "a1.h1",
             "b0.h1",
             "b2.h1",
             "a3.h1",
             "gamma.h1",
             "NtotA.h1",
             "NtotS.h1",
             "a0.h2",
             "a2.h2",
             "b0.h2",
             "b1.h2",
             "b2.h2",
             "a3.h2",
             "b3.h2",
             "gamma.h2",
             "NtotA.h2",
             "NtotS.h2",
             "a0.l",
             "a3.l",
             "b0.l",
             "b2.l",
             "b3.l",
             "gamma.l",
             "NtotA.l",
             "NtotS.l",
             "a0.r",
             "a2.r",
             "b0.r",
             "b2.r",
             "a3.r",
             "b3.r",
             "gamma.r",
             "NtotA.r",
             "NtotS.r",
             "a0.c",
             "a1.c",
             "a2.c",
             "b0.c",
             "b1.c",
             "b2.c",
             "a3.c",
             "gamma.c",
             "NtotA.c",
             "NtotS.c")

library(jagsUI)

# MCMC settings
na <- 10000 ; ni <- 450000 ; nt <- 500 ; nb <- 100000 ; nc <- 3


out1 <- jags(bdata, inits, params, "allShrubs.txt", n.adapt = na, n.chains = nc,
             n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)

par(mfrow = c(2, 3)) ; traceplot(out1) # the output is not too great (the betas look horrible), but it's a start

print(out1, 3)

# save output 

save(out1,file = "allShrubs.Rdata")

