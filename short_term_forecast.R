
# This script performs analyses as part of Paniw et al. XXXX
# Please read the README file on GitHub for a workflow of the analyses

# Created by: Maria Paniw
# Date created: June 20, 2022

rm(list = ls())

library(ggplot2)
library(Epi)
library(dplyr)
library(stringr)
library(patchwork)

setwd("SET_WORKING_DIRECTORY")

## Rainfall 2022 

dat1=read.csv("/Users/maria/Dropbox/demoDonana/climData/csv/Palacio 2021 - 2022.csv",na.strings = c("NA"," "))

dat1$month=sapply(str_split(dat1$day,".",4), "[", 4)

dat1=dat1[!is.na(dat1$month),]

precip2022=sum(dat1$Prec.[dat1$month%in%c("Oct","Nov","Dec","Jan","Feb","Mar","Apr")],na.rm=T)

mu.r=458.9971 
sd.r=130.7483 

precip2022=(precip2022-mu.r)/sd.r

num=read.csv("shrub_number.csv")

head(num)

### Get clim data

precip=read.csv("precipPalacios.csv")

head(precip)

### INTERSPECIFIC DENSITY

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


# CREATE ARRAYS

##### Halimium halimifolium

num_sub=num[num$species%in%"Halimium halimifolium",]

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


C[3,c(2,3)] =0

CS.hal1=C

#### Halimium comutatum 

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


C[3,c(1,2,3)] =0

CS.hal2=C

##### Lavandula stoechas

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

##### Rosmarinus officinalis

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

##### Cistus libanotis

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

nsites = dim(CA.hal1)[1]
nyears = dim(CA.hal1)[2]+1

nA.hal1 = array(NA,c(nsites,nyears,2100)) 
nS.hal1 = array(NA,c(nsites,nyears,2100))
nA.hal2 = array(NA,c(nsites,nyears,2100))
nS.hal2 = array(NA,c(nsites,nyears,2100))
nA.lav = array(NA,c(nsites,nyears,2100))
nS.lav = array(NA,c(nsites,nyears,2100))
nA.ros = array(NA,c(nsites,nyears,2100))
nS.ros = array(NA,c(nsites,nyears,2100))
nA.cis = array(NA,c(nsites,nyears,2100))
nS.cis = array(NA,c(nsites,nyears,2100))
rain=c(as.numeric(scale(precip$Prec.[precip$year%in%c(2007:2021)])),precip2022)
neigh.cis=neighCis
neigh.hal1=neighHal1
neigh.lav=neighLav
neigh.hal2=neighHal2
neigh.ros=neighRos


# LOAD PARAMETERS

load("allShrubs.Rdata")


NA.h1=NA.h2=NA.l=NA.r=NA.c=array(NA,c(nsites,nyears))
NS.h1=NS.h2=NS.l=NS.r=NS.c=array(NA,c(nsites,nyears))

neigh.h1=neigh.h2=neigh.l=neigh.r=neigh.c=array(NA,c(nsites,nyears))

phiA.h1=phiA.h2=phiA.l=phiA.r=phiA.c=array(NA,c(nsites,nyears))
phiS.h1=phiS.h2=phiS.l=phiS.r=phiS.c=array(NA,c(nsites,nyears))

G.h1=G.h2=G.l=G.r=G.c=array(NA,c(nsites,nyears))

for(xx in 1:2100){
  
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
      
      phiA.h1[i,t] <- 1/(1+exp(-( out1$sims.list$a0.h1[xx] + out1$sims.list$a1.h1[xx] *rain[t]  + out1$sims.list$a3.h1[xx] * neigh.h1[i,t])))
      phiS.h1[i,t] <- 1/(1+exp(-( out1$sims.list$b0.h1[xx] + out1$sims.list$b2.h1[xx] * log(NA.h1[i,t]+0.001))))
      
      G.h1[i,t] = rpois(1,out1$sims.list$gamma.h1[xx])
      
      NA.h1[i,t+1] <- phiA.h1[i,t] *  NA.h1[i,t] +  out1$sims.list$t.h1[xx]*phiS.h1[i,t] *  NS.h1[i,t]
      NS.h1[i,t+1] <- (1 - out1$sims.list$t.h1[xx])*phiS.h1[i,t] *  NS.h1[i,t] + G.h1[i,t]
      
      nA.hal1[i,t+1,xx] = rpois(1,NA.h1[i,t+1])
      nS.hal1[i,t+1,xx] = rpois(1,NS.h1[i,t+1])
      
      
      
      # Halimium commutatum
      
      phiA.h2[i,t] <- 1/(1+exp(-( out1$sims.list$a0.h2[xx] + out1$sims.list$a2.h2[xx] * log(NA.h2[i,t]+0.001) + out1$sims.list$a3.h2[xx] * neigh.h2[i,t] )))
      phiS.h2[i,t] <- 1/(1+exp(-( out1$sims.list$b0.h2[xx] +  out1$sims.list$b2.h2[xx] * log(NA.h2[i,t]+0.001)+ out1$sims.list$b3.h2[xx] * neigh.h2[i,t])))
      
      G.h2[i,t] = rpois(1,out1$sims.list$gamma.h2[xx])
      
      NA.h2[i,t+1] <- phiA.h2[i,t] *  NA.h2[i,t] +  out1$sims.list$t.h2[xx]*phiS.h2[i,t] *  NS.h2[i,t]
      NS.h2[i,t+1] <- (1 - out1$sims.list$t.h2[xx])*phiS.h2[i,t] *  NS.h2[i,t] + G.h2[i,t]
      
      nA.hal2[i,t+1,xx] = rpois(1,NA.h2[i,t+1])
      nS.hal2[i,t+1,xx] = rpois(1,NS.h2[i,t+1])
      
      
      # Lavandula stoechas 
      
      phiA.l[i,t] <- 1/(1+exp(-( out1$sims.list$a0.l[xx] + out1$sims.list$a3.l[xx] * neigh.l[i,t]  )))
      phiS.l[i,t] <- 1/(1+exp(-( out1$sims.list$b0.l[xx] + out1$sims.list$b2.l[xx] * log(NA.l[i,t]+0.001) + out1$sims.list$b3.l[xx] * neigh.l[i,t])))
      
      G.l[i,t] = rpois(1,out1$sims.list$gamma.l[xx])
      
      NA.l[i,t+1] <- phiA.l[i,t] *  NA.l[i,t] +  out1$sims.list$t.l[xx]*phiS.l[i,t] *  NS.l[i,t]
      NS.l[i,t+1] <- (1 - out1$sims.list$t.l[xx])*phiS.l[i,t] *  NS.l[i,t] + G.l[i,t]
      
      nA.lav[i,t+1,xx] = rpois(1,NA.l[i,t+1])
      nS.lav[i,t+1,xx] = rpois(1,NS.l[i,t+1])
      
      ## Rosmarinus officinalis
      
      phiA.r[i,t] <- 1/(1+exp(-( out1$sims.list$a0.r[xx] + out1$sims.list$a2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$a3.r[xx] * neigh.r[i,t])))
      phiS.r[i,t] <- 1/(1+exp(-( out1$sims.list$b0.r[xx] + out1$sims.list$b2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$b3.r[xx] * neigh.r[i,t])))
      
      G.r[i,t] = rpois(1,out1$sims.list$gamma.r[xx])
      
      NA.r[i,t+1] <- phiA.r[i,t] *  NA.r[i,t] + out1$sims.list$t.r[xx]* phiS.r[i,t] *  NS.r[i,t]
      NS.r[i,t+1] <- (1 - out1$sims.list$t.r[xx])*phiS.r[i,t] *  NS.r[i,t] + G.r[i,t]
      
      nA.ros[i,t+1,xx] = rpois(1,NA.r[i,t+1])
      nS.ros[i,t+1,xx] = rpois(1,NS.r[i,t+1])
      
      ## Cistus libanotis 
      
      phiA.c[i,t] <- 1/(1+exp(-( out1$sims.list$a0.c[xx] + out1$sims.list$a1.c[xx] *rain[t] + out1$sims.list$a2.c[xx] * log(NA.c[i,t]+0.001) + out1$sims.list$a3.c[xx] * neigh.c[i,t])))
      phiS.c[i,t] <- out1$sims.list$phiS.c[xx]
      
      G.c[i,t] = rpois(1,out1$sims.list$gamma.c[xx])
      
      NA.c[i,t+1] <- phiA.c[i,t] *  NA.c[i,t] +  out1$sims.list$t.c[xx]*phiS.c[i,t] *  NS.c[i,t]
      NS.c[i,t+1] <- (1 - out1$sims.list$t.c[xx])*phiS.c[i,t] *  NS.c[i,t] + G.c[i,t]
      
      nA.cis[i,t+1,xx] = rpois(1,NA.c[i,t+1])
      nS.cis[i,t+1,xx] = rpois(1,NS.c[i,t+1])
      
      ###   Neighborhood
      
      neigh.c[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
      neigh.h1[i,t+1] <- log(NA.h2[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
      neigh.h2[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
      neigh.l[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
      neigh.r[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.l[i,t+1]+NA.h2[i,t+1])
      
    }
    
  }
  
}


h1A=data.frame(mu=apply(nA.hal1[,nyears,],c(1),mean),
               L=apply(nA.hal1[,nyears,],c(1),quantile,0.025),
               U=apply(nA.hal1[,nyears,],c(1),quantile,0.975),
               species="H. halimifolium",
               scen="base")

h1S=data.frame(mu=apply(nS.hal1[,nyears,],c(1),mean),
               L=apply(nS.hal1[,nyears,],c(1),quantile,0.025),
               U=apply(nS.hal1[,nyears,],c(1),quantile,0.975),
               species="H. halimifolium",
               scen="base")

h2A=data.frame(mu=apply(nA.hal2[,nyears,],c(1),mean),
               L=apply(nA.hal2[,nyears,],c(1),quantile,0.025),
               U=apply(nA.hal2[,nyears,],c(1),quantile,0.975),
               species="H. commutatum",
               scen="base")

h2S=data.frame(mu=apply(nS.hal2[,nyears,],c(1),mean),
               L=apply(nS.hal2[,nyears,],c(1),quantile,0.025),
               U=apply(nS.hal2[,nyears,],c(1),quantile,0.975),
               species="H. commutatum",
               scen="base")

lA=data.frame(mu=apply(nA.lav[,nyears,],c(1),mean),
               L=apply(nA.lav[,nyears,],c(1),quantile,0.025),
               U=apply(nA.lav[,nyears,],c(1),quantile,0.975),
               species="L. stoechas",
              scen="base")

lS=data.frame(mu=apply(nS.lav[,nyears,],c(1),mean),
               L=apply(nS.lav[,nyears,],c(1),quantile,0.025),
               U=apply(nS.lav[,nyears,],c(1),quantile,0.975),
               species="L. stoechas",
              scen="base")


rA=data.frame(mu=apply(nA.ros[,nyears,],c(1),mean),
              L=apply(nA.ros[,nyears,],c(1),quantile,0.025),
              U=apply(nA.ros[,nyears,],c(1),quantile,0.975),
              species="R. officinalis",
              scen="base")

rS=data.frame(mu=apply(nS.ros[,nyears,],c(1),mean),
              L=apply(nS.ros[,nyears,],c(1),quantile,0.025),
              U=apply(nS.ros[,nyears,],c(1),quantile,0.975),
              species="R. officinalis",
              scen="base")

cA=data.frame(mu=apply(nA.cis[,nyears,],c(1),mean),
              L=apply(nA.cis[,nyears,],c(1),quantile,0.025),
              U=apply(nA.cis[,nyears,],c(1),quantile,0.975),
              species="C. libanotis",
              scen="base")

cS=data.frame(mu=apply(nS.cis[,nyears,],c(1),mean),
              L=apply(nS.cis[,nyears,],c(1),quantile,0.025),
              U=apply(nS.cis[,nyears,],c(1),quantile,0.975),
              species="C. libanotis",
              scen="base")

################# <PERTUB NEIGHBORHOOD 

nA.hal1 = array(NA,c(nsites,nyears,2100)) 
nS.hal1 = array(NA,c(nsites,nyears,2100))
nA.hal2 = array(NA,c(nsites,nyears,2100))
nS.hal2 = array(NA,c(nsites,nyears,2100))
nA.lav = array(NA,c(nsites,nyears,2100))
nS.lav = array(NA,c(nsites,nyears,2100))
nA.ros = array(NA,c(nsites,nyears,2100))
nS.ros = array(NA,c(nsites,nyears,2100))
nA.cis = array(NA,c(nsites,nyears,2100))
nS.cis = array(NA,c(nsites,nyears,2100))
neigh.cis=neighCis
neigh.hal1=neighHal1
neigh.lav=neighLav
neigh.hal2=neighHal2
neigh.ros=neighRos


# LOAD PARAMETERS

NA.h1=NA.h2=NA.l=NA.r=NA.c=array(NA,c(nsites,nyears))
NS.h1=NS.h2=NS.l=NS.r=NS.c=array(NA,c(nsites,nyears))

neigh.h1=neigh.h2=neigh.l=neigh.r=neigh.c=array(NA,c(nsites,nyears))

phiA.h1=phiA.h2=phiA.l=phiA.r=phiA.c=array(NA,c(nsites,nyears))
phiS.h1=phiS.h2=phiS.l=phiS.r=phiS.c=array(NA,c(nsites,nyears))

G.h1=G.h2=G.l=G.r=G.c=array(NA,c(nsites,nyears))

for(xx in 1:2100){
  
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
      
      phiA.h1[i,t] <- 1/(1+exp(-( out1$sims.list$a0.h1[xx] + out1$sims.list$a1.h1[xx] *rain[t]  + out1$sims.list$a3.h1[xx] * neigh.h1[i,t])))
      phiS.h1[i,t] <- 1/(1+exp(-( out1$sims.list$b0.h1[xx] + out1$sims.list$b2.h1[xx] * log(NA.h1[i,t]+0.001))))
      
      G.h1[i,t] = rpois(1,out1$sims.list$gamma.h1[xx])
      
      NA.h1[i,t+1] <- phiA.h1[i,t] *  NA.h1[i,t] +  out1$sims.list$t.h1[xx]*phiS.h1[i,t] *  NS.h1[i,t]
      NS.h1[i,t+1] <- (1 - out1$sims.list$t.h1[xx])*phiS.h1[i,t] *  NS.h1[i,t] + G.h1[i,t]
      
      nA.hal1[i,t+1,xx] = rpois(1,NA.h1[i,t+1])
      nS.hal1[i,t+1,xx] = rpois(1,NS.h1[i,t+1])
      
      
      
      # Halimium commutatum
      
      phiA.h2[i,t] <- 1/(1+exp(-( out1$sims.list$a0.h2[xx] + out1$sims.list$a2.h2[xx] * log(NA.h2[i,t]+0.001) + out1$sims.list$a3.h2[xx] * neigh.h2[i,t] )))
      phiS.h2[i,t] <- 1/(1+exp(-( out1$sims.list$b0.h2[xx] +  out1$sims.list$b2.h2[xx] * log(NA.h2[i,t]+0.001)+ out1$sims.list$b3.h2[xx] * neigh.h2[i,t])))
      
      G.h2[i,t] = rpois(1,out1$sims.list$gamma.h2[xx])
      
      NA.h2[i,t+1] <- phiA.h2[i,t] *  NA.h2[i,t] +  out1$sims.list$t.h2[xx]*phiS.h2[i,t] *  NS.h2[i,t]
      NS.h2[i,t+1] <- (1 - out1$sims.list$t.h2[xx])*phiS.h2[i,t] *  NS.h2[i,t] + G.h2[i,t]
      
      nA.hal2[i,t+1,xx] = rpois(1,NA.h2[i,t+1])
      nS.hal2[i,t+1,xx] = rpois(1,NS.h2[i,t+1])
      
      
      # Lavandula stoechas 
      
      phiA.l[i,t] <- 1/(1+exp(-( out1$sims.list$a0.l[xx] + out1$sims.list$a3.l[xx] * neigh.l[i,t]  )))
      phiS.l[i,t] <- 1/(1+exp(-( out1$sims.list$b0.l[xx] + out1$sims.list$b2.l[xx] * log(NA.l[i,t]+0.001) + out1$sims.list$b3.l[xx] * neigh.l[i,t])))
      
      G.l[i,t] = rpois(1,out1$sims.list$gamma.l[xx])
      
      NA.l[i,t+1] <- phiA.l[i,t] *  NA.l[i,t] +  out1$sims.list$t.l[xx]*phiS.l[i,t] *  NS.l[i,t]
      NS.l[i,t+1] <- (1 - out1$sims.list$t.l[xx])*phiS.l[i,t] *  NS.l[i,t] + G.l[i,t]
      
      nA.lav[i,t+1,xx] = rpois(1,NA.l[i,t+1])
      nS.lav[i,t+1,xx] = rpois(1,NS.l[i,t+1])
      
      ## Rosmarinus officinalis
      
      phiA.r[i,t] <- 1/(1+exp(-( out1$sims.list$a0.r[xx] + out1$sims.list$a2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$a3.r[xx] * neigh.r[i,t])))
      phiS.r[i,t] <- 1/(1+exp(-( out1$sims.list$b0.r[xx] + out1$sims.list$b2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$b3.r[xx] * neigh.r[i,t])))
      
      G.r[i,t] = rpois(1,out1$sims.list$gamma.r[xx])
      
      NA.r[i,t+1] <- phiA.r[i,t] *  NA.r[i,t] + out1$sims.list$t.r[xx]* phiS.r[i,t] *  NS.r[i,t]
      NS.r[i,t+1] <- (1 - out1$sims.list$t.r[xx])*phiS.r[i,t] *  NS.r[i,t] + G.r[i,t]
      
      nA.ros[i,t+1,xx] = rpois(1,NA.r[i,t+1])
      nS.ros[i,t+1,xx] = rpois(1,NS.r[i,t+1])
      
      ## Cistus libanotis 
      
      phiA.c[i,t] <- 1/(1+exp(-( out1$sims.list$a0.c[xx] + out1$sims.list$a1.c[xx] *rain[t] + out1$sims.list$a2.c[xx] * log(NA.c[i,t]+0.001) + out1$sims.list$a3.c[xx] * neigh.c[i,t])))
      phiS.c[i,t] <- out1$sims.list$phiS.c[xx]
      
      G.c[i,t] = rpois(1,out1$sims.list$gamma.c[xx])
      
      NA.c[i,t+1] <- phiA.c[i,t] *  NA.c[i,t] +  out1$sims.list$t.c[xx]*phiS.c[i,t] *  NS.c[i,t]
      NS.c[i,t+1] <- (1 - out1$sims.list$t.c[xx])*phiS.c[i,t] *  NS.c[i,t] + G.c[i,t]
      
      nA.cis[i,t+1,xx] = rpois(1,NA.c[i,t+1])
      nS.cis[i,t+1,xx] = rpois(1,NS.c[i,t+1])
      
      ###   Neighborhood
      
      if(t==14){
        
        neigh.c[i,t+1] <- apply(neigh.cis,1,mean)[i]
        neigh.h1[i,t+1] <- apply(neigh.hal1,1,mean)[i]
        neigh.h2[i,t+1] <- apply(neigh.hal2,1,mean)[i]
        neigh.l[i,t+1] <- apply(neigh.lav,1,mean)[i]
        neigh.r[i,t+1] <- apply(neigh.ros,1,mean)[i]
        
      }else{
        neigh.c[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
        neigh.h1[i,t+1] <- log(NA.h2[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
        neigh.h2[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
        neigh.l[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
        neigh.r[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.l[i,t+1]+NA.h2[i,t+1])
        
        
      }
      
      
    }
    
  }
  
}



h1A=rbind(h1A,data.frame(mu=apply(nA.hal1[,nyears,],c(1),mean),
               L=apply(nA.hal1[,nyears,],c(1),quantile,0.025),
               U=apply(nA.hal1[,nyears,],c(1),quantile,0.975),
               species="H. halimifolium",
               scen="pert"))

h1S=rbind(h1S,data.frame(mu=apply(nS.hal1[,nyears,],c(1),mean),
               L=apply(nS.hal1[,nyears,],c(1),quantile,0.025),
               U=apply(nS.hal1[,nyears,],c(1),quantile,0.975),
               species="H. halimifolium",
               scen="pert"))

h2A=rbind(h2A,data.frame(mu=apply(nA.hal2[,nyears,],c(1),mean),
               L=apply(nA.hal2[,nyears,],c(1),quantile,0.025),
               U=apply(nA.hal2[,nyears,],c(1),quantile,0.975),
               species="H. commutatum",
               scen="pert"))

h2S=rbind(h2S,data.frame(mu=apply(nS.hal2[,nyears,],c(1),mean),
               L=apply(nS.hal2[,nyears,],c(1),quantile,0.025),
               U=apply(nS.hal2[,nyears,],c(1),quantile,0.975),
               species="H. commutatum",
               scen="pert"))

lA=rbind(lA,data.frame(mu=apply(nA.lav[,nyears,],c(1),mean),
              L=apply(nA.lav[,nyears,],c(1),quantile,0.025),
              U=apply(nA.lav[,nyears,],c(1),quantile,0.975),
              species="L. stoechas",
              scen="pert"))

lS=rbind(lS,data.frame(mu=apply(nS.lav[,nyears,],c(1),mean),
              L=apply(nS.lav[,nyears,],c(1),quantile,0.025),
              U=apply(nS.lav[,nyears,],c(1),quantile,0.975),
              species="L. stoechas",
              scen="pert"))


rA=rbind(rA,data.frame(mu=apply(nA.ros[,nyears,],c(1),mean),
              L=apply(nA.ros[,nyears,],c(1),quantile,0.025),
              U=apply(nA.ros[,nyears,],c(1),quantile,0.975),
              species="R. officinalis",
              scen="pert"))

rS=rbind(rS,data.frame(mu=apply(nS.ros[,nyears,],c(1),mean),
              L=apply(nS.ros[,nyears,],c(1),quantile,0.025),
              U=apply(nS.ros[,nyears,],c(1),quantile,0.975),
              species="R. officinalis",
              scen="pert"))

cA=rbind(cA,data.frame(mu=apply(nA.cis[,nyears,],c(1),mean),
              L=apply(nA.cis[,nyears,],c(1),quantile,0.025),
              U=apply(nA.cis[,nyears,],c(1),quantile,0.975),
              species="C. libanotis",
              scen="pert"))

cS=rbind(cS,data.frame(mu=apply(nS.cis[,nyears,],c(1),mean),
              L=apply(nS.cis[,nyears,],c(1),quantile,0.025),
              U=apply(nS.cis[,nyears,],c(1),quantile,0.975),
              species="C. libanotis",
              scen="pert"))

# OBSERVED 2022

obs=read.csv("Doñana Number Plants 2022.csv")

obs=obs[obs$year%in%2022,]

h1.obsA=c(obs$X.adults[obs$species%in%"Halimium halimifolium"][1:4],0,obs$X.adults[obs$species%in%"Halimium halimifolium"][5:17])
h1.obsS=c(obs$X.saplings[obs$species%in%"Halimium halimifolium"][1:4],0,obs$X.saplings[obs$species%in%"Halimium halimifolium"][5:17])

h1A$obs=rep(h1.obsA,2)
h1S$obs=rep(h1.obsS,2)

h2.obsA=c(obs$X.adults[obs$species%in%"Halimium commutatum"][1:4],0,obs$X.adults[obs$species%in%"Halimium commutatum"][5:17])
h2.obsS=c(obs$X.saplings[obs$species%in%"Halimium commutatum"][1:4],0,obs$X.saplings[obs$species%in%"Halimium commutatum"][5:17])

h2A$obs=rep(h2.obsA,2)
h2S$obs=rep(h2.obsS,2)

l.obsA=c(obs$X.adults[obs$species%in%"Lavandula stoechas"][1:4],0,obs$X.adults[obs$species%in%"Lavandula stoechas"][5:17])
l.obsS=c(obs$X.saplings[obs$species%in%"Lavandula stoechas"][1:4],0,obs$X.saplings[obs$species%in%"Lavandula stoechas"][5:17])

lA$obs=rep(l.obsA,2)
lS$obs=rep(l.obsS,2)

r.obsA=c(obs$X.adults[obs$species%in%"Rosmarinus officinalis"][1:4],0,obs$X.adults[obs$species%in%"Rosmarinus officinalis"][5:17])
r.obsS=c(obs$X.saplings[obs$species%in%"Rosmarinus officinalis"][1:4],0,obs$X.saplings[obs$species%in%"Rosmarinus officinalis"][5:17])

rA$obs=rep(r.obsA,2)
rS$obs=rep(r.obsS,2)


c.obsA=c(obs$X.adults[obs$species%in%"Cistus libanotis"][1:5],0,obs$X.adults[obs$species%in%"Cistus libanotis"][6:17])
c.obsS=c(obs$X.saplings[obs$species%in%"Cistus libanotis"][1:5],0,obs$X.saplings[obs$species%in%"Cistus libanotis"][6:17])

cA$obs=rep(c.obsA,2)
cS$obs=rep(c.obsS,2)

allA = rbind(h1A,h2A,lA,rA,cA)

allS = rbind(h1S,h2S,lS,rS,cS)

allA$deviance=(allA$obs-allA$mu)^2

allS$deviance=(allS$obs-allS$mu)^2


MSE.A.base=sum(allA[allA$scen%in%"base","deviance"])
MSE.A.pert=sum(allA[allA$scen%in%"pert","deviance"])

MSE.S.base=sum(allS[allS$scen%in%"base","deviance"])
MSE.S.pert=sum(allS[allS$scen%in%"pert","deviance"])

 