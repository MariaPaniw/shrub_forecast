
rm(list = ls())

library(ggplot2)
library(Epi)
library(dplyr)


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


# PUT DATA IN ARRAYS

##### Halimium halimifolium

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

# Bundle and summarize data set

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
nyears = dim(CA.hal1)[2]
nA.hal1 = array(NA,c(nsites,nyears)) 
nS.hal1 = array(NA,c(nsites,nyears))
nA.hal2 = array(NA,c(nsites,nyears))
nS.hal2 = array(NA,c(nsites,nyears))
nA.lav = array(NA,c(nsites,nyears))
nS.lav = array(NA,c(nsites,nyears))
nA.ros = array(NA,c(nsites,nyears))
nS.ros = array(NA,c(nsites,nyears))
nA.cis = array(NA,c(nsites,nyears))
nS.cis = array(NA,c(nsites,nyears))

rain=as.numeric(scale(precip$Prec.[precip$year%in%c(2007:2021)]))
neigh.cis=neighCis
neigh.hal1=neighHal1
neigh.lav=neighLav
neigh.hal2=neighHal2
neigh.ros=neighRos

                  

# LOAD PARAMETERS

load("allShrubs.Rdata")

fit1.data.h1=fit1.pred.h1=fit1.data.h2=fit1.pred.h2=fit1.data.l=fit1.pred.l=fit1.data.r=fit1.pred.r=fit1.data.c=fit1.pred.c=rep(NA,2100)

fit1.dataS.h1=fit1.predS.h1=fit1.dataS.h2=fit1.predS.h2=fit1.dataS.l=fit1.predS.l=fit1.dataS.r=fit1.predS.r=fit1.dataS.c=fit1.predS.c=rep(NA,2100)

NA.h1=NA.h2=NA.l=NA.r=NA.c=array(NA,c(nsites,nyears))
NS.h1=NS.h2=NS.l=NS.r=NS.c=array(NA,c(nsites,nyears))

neigh.h1=neigh.h2=neigh.l=neigh.r=neigh.c=array(NA,c(nsites,nyears))

phiA.h1=phiA.h2=phiA.l=phiA.r=phiA.c=array(NA,c(nsites,nyears))
phiS.h1=phiS.h2=phiS.l=phiS.r=phiS.c=array(NA,c(nsites,nyears))

G.h1=G.h2=G.l=G.r=G.c=array(NA,c(nsites,nyears))

resid1.h1=resid1.h2=resid1.l=resid1.r=resid1.c=array(NA,c(nsites,nyears))
resid1.pred.h1=resid1.pred.h2=resid1.pred.l=resid1.pred.r=resid1.pred.c=array(NA,c(nsites,nyears))

resid1S.h1=resid1S.h2=resid1S.l=resid1S.r=resid1S.c=array(NA,c(nsites,nyears))
resid1S.pred.h1=resid1S.pred.h2=resid1S.pred.l=resid1S.pred.r=resid1S.pred.c=array(NA,c(nsites,nyears))


 for(xx in 1:2100){ # Loop over posterior parameter values
    
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
      
      NA.h1[i,t+1] <- phiA.h1[i,t] *  NA.h1[i,t] +  phiS.h1[i,t] *  NS.h1[i,t]
      NS.h1[i,t+1] <- (1 - phiS.h1[i,t] ) *  NS.h1[i,t] + G.h1[i,t]

    nA.hal1[i,t+1] = rpois(1,NA.h1[i,t+1])
    nS.hal1[i,t+1] = rpois(1,NS.h1[i,t+1])
    
    
    
 # Halimium commutatum

    phiA.h2[i,t] <- 1/(1+exp(-( out1$sims.list$a0.h2[xx] + out1$sims.list$a2.h2[xx] * log(NA.h2[i,t]+0.001) + out1$sims.list$a3.h2[xx] * neigh.h2[i,t] )))
    phiS.h2[i,t] <- 1/(1+exp(-( out1$sims.list$b0.h2[xx] + out1$sims.list$b1.h2[xx] *rain[t] + out1$sims.list$b2.h2[xx] * log(NA.h2[i,t]+0.001)+ out1$sims.list$b3.h2[xx] * neigh.h2[i,t])))
    
    G.h2[i,t] = rpois(1,out1$sims.list$gamma.h2[xx])
    
    NA.h2[i,t+1] <- phiA.h2[i,t] *  NA.h2[i,t] +  phiS.h2[i,t] *  NS.h2[i,t]
    NS.h2[i,t+1] <- (1 - phiS.h2[i,t] ) *  NS.h2[i,t] + G.h2[i,t]
    
    nA.hal2[i,t+1] = rpois(1,NA.h2[i,t+1])
    nS.hal2[i,t+1] = rpois(1,NS.h2[i,t+1])


# Lavandula stoechas 

    phiA.l[i,t] <- 1/(1+exp(-( out1$sims.list$a0.l[xx] + out1$sims.list$a3.l[xx] * neigh.l[i,t]  )))
    phiS.l[i,t] <- 1/(1+exp(-( out1$sims.list$b0.l[xx] + out1$sims.list$b2.l[xx] * log(NA.l[i,t]+0.001) + out1$sims.list$b3.l[xx] * neigh.l[i,t])))
    
    G.l[i,t] = rpois(1,out1$sims.list$gamma.l[xx])
    
    NA.l[i,t+1] <- phiA.l[i,t] *  NA.l[i,t] +  phiS.l[i,t] *  NS.l[i,t]
    NS.l[i,t+1] <- (1 - phiS.l[i,t] ) *  NS.l[i,t] + G.l[i,t]
    
    nA.lav[i,t+1] = rpois(1,NA.l[i,t+1])
    nS.lav[i,t+1] = rpois(1,NS.l[i,t+1])

## Rosmarinus officinalis
   
    phiA.r[i,t] <- 1/(1+exp(-( out1$sims.list$a0.r[xx] + out1$sims.list$a2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$a3.r[xx] * neigh.r[i,t])))
    phiS.r[i,t] <- 1/(1+exp(-( out1$sims.list$b0.r[xx] + out1$sims.list$b2.r[xx] * log(NA.r[i,t]+0.001) + out1$sims.list$b3.r[xx] * neigh.r[i,t])))
    
    G.r[i,t] = rpois(1,out1$sims.list$gamma.r[xx])
    
    NA.r[i,t+1] <- phiA.r[i,t] *  NA.r[i,t] +  phiS.r[i,t] *  NS.r[i,t]
    NS.r[i,t+1] <- (1 - phiS.r[i,t] ) *  NS.r[i,t] + G.r[i,t]
    
    nA.ros[i,t+1] = rpois(1,NA.r[i,t+1])
    nS.ros[i,t+1] = rpois(1,NS.r[i,t+1])
 
## Cistus libanotis 

    phiA.c[i,t] <- 1/(1+exp(-( out1$sims.list$a0.c[xx] + out1$sims.list$a1.c[xx] *rain[t] + out1$sims.list$a2.c[xx] * log(NA.c[i,t]+0.001) + out1$sims.list$a3.c[xx] * neigh.c[i,t])))
    phiS.c[i,t] <- 1/(1+exp(-( out1$sims.list$b0.c[xx] + out1$sims.list$b1.c[xx] *rain[t] + out1$sims.list$b2.c[xx] * log(NA.c[i,t]+0.001) )))
    
    G.c[i,t] = rpois(1,out1$sims.list$gamma.c[xx])
    
    NA.c[i,t+1] <- phiA.c[i,t] *  NA.c[i,t] +  phiS.c[i,t] *  NS.c[i,t]
    NS.c[i,t+1] <- (1 - phiS.c[i,t] ) *  NS.c[i,t] + G.c[i,t]
    nA.cis[i,t+1] = rpois(1,NA.c[i,t+1])
    nS.cis[i,t+1] = rpois(1,NS.c[i,t+1])

###   Neighborhood

    neigh.c[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
    neigh.h1[i,t+1] <- log(NA.h2[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
    neigh.h2[i,t+1] <- log(NA.h1[i,t+1]+NA.l[i,t+1] +NA.r[i,t+1]+NA.c[i,t+1])
    neigh.l[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.r[i,t+1]+NA.h2[i,t+1])
    neigh.r[i,t+1] <- log(NA.h1[i,t+1]+NA.c[i,t+1] +NA.l[i,t+1]+NA.h2[i,t+1])
    

    # # Goodness of fit ADULTS
    # Hal 1
    n.pred.h1 = rpois(1,NA.h1[i,t+1])
    e1 <- NA.h1[i,t+1]
    diff= CA.hal1[i,t+1] - e1
    resid1.h1[i,t+1] <- diff
    
    diff.pred.h1 = n.pred.h1 - e1
    resid1.pred.h1[i,t+1] <- diff.pred.h1
    
    # Hal 2
    n.pred.h2 = rpois(1,NA.h2[i,t+1])
    e1 <- NA.h2[i,t+1]
    diff= CA.hal2[i,t+1] - e1
    resid1.h2[i,t+1] <- diff
    
    diff.pred.h2 = n.pred.h2 - e1
    resid1.pred.h2[i,t+1] <- diff.pred.h2
    
    # Lav
    n.pred.l = rpois(1,NA.l[i,t+1])
    e1 <- NA.l[i,t+1]
    diff= CA.lav[i,t+1] - e1
    resid1.l[i,t+1] <- diff
    
    diff.pred.l = n.pred.l - e1
    resid1.pred.l[i,t+1] <- diff.pred.l
    
    # Ros
    n.pred.r = rpois(1,NA.r[i,t+1])
    e1 <- NA.r[i,t+1]
    diff= CA.ros[i,t+1] - e1
    resid1.r[i,t+1] <- diff
    
    diff.pred.r = n.pred.r - e1
    resid1.pred.r[i,t+1] <- diff.pred.r
    
    # Cis
    n.pred.c = rpois(1,NA.c[i,t+1])
    e1 <- NA.c[i,t+1]
    diff= CA.cis[i,t+1] - e1
    resid1.c[i,t+1] <- diff
    
    diff.pred.c = n.pred.c - e1
    resid1.pred.c[i,t+1] <- diff.pred.c
    
    # # Goodness of fit SAPLINGS
    # Hal 1
    n.pred.h1 = rpois(1,NS.h1[i,t+1])
    e1 <- NS.h1[i,t+1]
    diff= CS.hal1[i,t+1] - e1
    resid1S.h1[i,t+1] <- diff
    
    diff.pred.h1 = n.pred.h1 - e1
    resid1S.pred.h1[i,t+1] <- diff.pred.h1
    
    # Hal 2
    n.pred.h2 = rpois(1,NS.h2[i,t+1])
    e1 <- NS.h2[i,t+1]
    diff= CS.hal2[i,t+1] - e1
    resid1S.h2[i,t+1] <- diff
    
    diff.pred.h2 = n.pred.h2 - e1
    resid1S.pred.h2[i,t+1] <- diff.pred.h2
    
    # Lav
    n.pred.l = rpois(1,NS.l[i,t+1])
    e1 <- NS.l[i,t+1]
    diff= CS.lav[i,t+1] - e1
    resid1S.l[i,t+1] <- diff
    
    diff.pred.l = n.pred.l - e1
    resid1S.pred.l[i,t+1] <- diff.pred.l
    
    # Ros
    n.pred.r = rpois(1,NS.r[i,t+1])
    e1 <- NS.r[i,t+1]
    diff= CS.ros[i,t+1] - e1
    resid1S.r[i,t+1] <- diff
    
    diff.pred.r = n.pred.r - e1
    resid1S.pred.r[i,t+1] <- diff.pred.r
    
    # Cis
    n.pred.c = rpois(1,NS.c[i,t+1])
    e1 <- NS.c[i,t+1]
    diff= CS.cis[i,t+1] - e1
    resid1S.c[i,t+1] <- diff
    
    diff.pred.c = n.pred.c - e1
    resid1S.pred.c[i,t+1] <- diff.pred.c
    
    }

    }
     
   # ADULTS ( observed data are available for years 2:4,7,11:15; but initial years 2:4 provide a bad fit to the data for adults - user can check this by adding these years)
        
        fit1.data.h1[xx] <- sum(resid1.h1[,c(7,11:15)]) 
        fit1.pred.h1[xx] <- sum(resid1.pred.h1[,c(7,11:15)])

        fit1.data.h2[xx] <- sum(resid1.h2[,c(7,11:15)])
        fit1.pred.h2[xx] <- sum(resid1.pred.h2[,c(7,11:15)])
        
        fit1.data.l[xx] <- sum(resid1.l[,c(7,11:15)])
        fit1.pred.l[xx] <- sum(resid1.pred.l[,c(7,11:15)])
        
        fit1.data.r[xx] <- sum(resid1.r[,c(7,11:15)])
        fit1.pred.r[xx] <- sum(resid1.pred.r[,c(7,11:15)])
        
        fit1.data.c[xx] <- sum(resid1.c[,c(7,11:15)])
        fit1.pred.c[xx] <- sum(resid1.pred.c[,c(7,11:15)])
        
        # SAPLINGS 
        fit1.dataS.h1[xx] <- sum(resid1S.h1[,c(2:4,7,11:15)]) 
        fit1.predS.h1[xx] <- sum(resid1S.pred.h1[,c(2:4,7,11:15)])
        
        fit1.dataS.h2[xx] <- sum(resid1S.h2[,c(2:4,7,11:15)])
        fit1.predS.h2[xx] <- sum(resid1S.pred.h2[,c(2:4,7,11:15)])
        
        fit1.dataS.l[xx] <- sum(resid1S.l[,c(2:4,7,11:15)])
        fit1.predS.l[xx] <- sum(resid1S.pred.l[,c(2:4,7,11:15)])
        
        fit1.dataS.r[xx] <- sum(resid1S.r[,c(2:4,7,11:15)])
        fit1.predS.r[xx] <- sum(resid1S.pred.r[,c(2:4,7,11:15)])
        
        fit1.dataS.c[xx] <- sum(resid1S.c[,c(2:4,7,11:15)])
        fit1.predS.c[xx] <- sum(resid1S.pred.c[,c(2:4,7,11:15)])
 }


### ADULTS

# Bayesian p-value
mean(fit1.pred.h1 > fit1.data.h1)

plot(fit1.pred.h1, fit1.data.h1)
abline(1,1)


mean(fit1.pred.h2 > fit1.data.h2)

plot(fit1.pred.h2, fit1.data.h2)
abline(1,1)

mean(fit1.pred.l > fit1.data.l)

plot(fit1.pred.l, fit1.data.l)
abline(1,1)

mean(fit1.pred.r > fit1.data.r)

plot(fit1.pred.r, fit1.data.r)
abline(1,1)

mean(fit1.pred.c > fit1.data.c)

plot(fit1.pred.c, fit1.data.c)
abline(1,1)

# SAPLINGS
# Bayesian p-value

mean(fit1.predS.h1 > fit1.dataS.h1)

plot(fit1.predS.h1, fit1.dataS.h1)
abline(1,1)


mean(fit1.predS.h2 > fit1.dataS.h2)

plot(fit1.predS.h2, fit1.dataS.h2)
abline(1,1)

mean(fit1.predS.l > fit1.dataS.l)

plot(fit1.predS.l, fit1.dataS.l)
abline(1,1)

mean(fit1.predS.r > fit1.dataS.r)

plot(fit1.predS.r, fit1.dataS.r)
abline(1,1)

mean(fit1.predS.c > fit1.dataS.c)

plot(fit1.predS.c, fit1.dataS.c)
abline(1,1)

