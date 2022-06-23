
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

years=12 # years (or time steps) of simulations


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

sim.df.h1=array(0,c(n.site,n.stage,years)) 
sim.df.h2=array(0,c(n.site,n.stage,years)) 
sim.df.l=array(0,c(n.site,n.stage,years)) 
sim.df.r=array(0,c(n.site,n.stage,years)) 
sim.df.c=array(0,c(n.site,n.stage,years)) 

rain.sub=rain[4:length(rain)]

for(i in 1:years){
    
    # Update abundance
    dens.h1=matrix(dens.h1,18,n.stage,byrow=T)
    dens.h2=matrix(dens.h2,18,n.stage,byrow=T)
    dens.l=matrix(dens.l,18,n.stage,byrow=T)
    dens.r=matrix(dens.r,18,n.stage,byrow=T)
    dens.c=matrix(dens.c,18,n.stage,byrow=T)
    
    sim.df.h1[,,i]= dens.h1
    sim.df.h2[,,i]= dens.h2
    sim.df.l[,,i]= dens.l
    sim.df.r[,,i]= dens.r
    sim.df.c[,,i]= dens.c
    
    if(i==years) break
    
    rain.t=rain.sub[i]
    
 
    dens.h1=c(t(dens.h1))
    dens.h2=c(t(dens.h2))
    dens.l=c(t(dens.l))
    dens.r=c(t(dens.r))
    dens.c=c(t(dens.c))
    
    neigh.h1=log(sim.df.h2[,n.stage,i] + sim.df.l[,n.stage,i] + sim.df.r[,n.stage,i] + sim.df.c[,n.stage,i])
    neigh.h2=log(sim.df.h1[,n.stage,i] + sim.df.l[,n.stage,i] + sim.df.r[,n.stage,i] + sim.df.c[,n.stage,i])
    neigh.l=log(sim.df.h2[,n.stage,i] + sim.df.h1[,n.stage,i] +sim.df.r[,n.stage,i] + sim.df.c[,n.stage,i])
    neigh.r=log(sim.df.h2[,n.stage,i] + sim.df.l[,n.stage,i] + sim.df.h1[,n.stage,i] + sim.df.c[,n.stage,i])
    neigh.c=log(sim.df.h2[,n.stage,i] + sim.df.l[,n.stage,i] + sim.df.r[,n.stage,i] + sim.df.h1[,n.stage,i])
    
    ad.h1=log(sim.df.h1[,n.stage,i]+0.0001)
    ad.h2=log(sim.df.h2[,n.stage,i]+0.0001)
    ad.l=log(sim.df.l[,n.stage,i]+0.0001)
    ad.r=log(sim.df.r[,n.stage,i]+0.0001)
    ad.c=log(sim.df.c[,n.stage,i]+0.0001)
    

    # LOCAL DEMOGRAPHY

    for(j in 1:18){
     
       ### Seedling numbers:
     
        seed.h1=sim.df.h1[j,1,i]
        seed.h2=sim.df.h2[j,1,i]
        seed.l=sim.df.l[j,1,i]
        seed.r=sim.df.r[j,1,i]
        seed.c=sim.df.c[j,1,i]

      
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



### Plots 

library(plyr)
library(ggplot2)

df=NULL

temp=adply(sim.df.h1,c(1,2,3))

colnames(temp)=c("site","stage","year","density")

levels(temp$stage)=c("R","S","A")


temp$species="H. halimifolium"

df=rbind(df,temp)

temp=adply(sim.df.h2,c(1,2,3))

colnames(temp)=c("site","stage","year","density")

levels(temp$stage)=c("R","S","A")


temp$species="H. commutatum"

df=rbind(df,temp)

temp=adply(sim.df.l,c(1,2,3))

colnames(temp)=c("site","stage","year","density")

levels(temp$stage)=c("R","S","A")


temp$species="L. stoechas"

df=rbind(df,temp)

temp=adply(sim.df.r,c(1,2,3))

colnames(temp)=c("site","stage","year","density")

levels(temp$stage)=c("R","S","A")

temp$species="R. officinalis"

df=rbind(df,temp)

temp=adply(sim.df.c,c(1,2,3))

colnames(temp)=c("site","stage","year","density")

levels(temp$stage)=c("R","S","A")

temp$species="C. libanotis"

df=rbind(df,temp)



df$year=as.numeric(df$year)

df$sp_site=paste(as.character(df$species),as.character(df$site))

#test
sub.n=num[num$species%in%c("Halimium halimifolium","Halimium commutatum","Lavandula stoechas","Rosmarinus officinalis",
                           "Cistus libanotis"),]
sub.n$site=sub.n$plot
sub.n$site=factor(sub.n$site)
levels(sub.n$site)=1:18

tot=aggregate(X.adults~year+species,sum,data=sub.n)

tot.mu=aggregate(X.adults~species,mean,data=tot); tot.mu

# Observed
sub.n=num[num$species%in%c("Halimium halimifolium","Halimium commutatum","Lavandula stoechas","Rosmarinus officinalis",
                           "Cistus libanotis")&num$year>=2010,]
sub.n$site=sub.n$plot
sub.n$site=factor(sub.n$site)
levels(sub.n$site)=1:18

sub.n$species =factor(sub.n$species)
levels(sub.n$species) = c("C. libanotis","H. commutatum", "H. halimifolium","L. stoechas", "R. officinalis")


sub.n$year=factor(sub.n$year)

levels(sub.n$year) =c(1,4,8,10,11,12)

temp1=sub.n[,c("year","site","species","X.seedlings")]
colnames(temp1)[4] = "density"

temp1$stage="R"

temp2=sub.n[,c("year","site","species","X.saplings")]
colnames(temp2)[4] = "density"
temp2$stage="S"

temp3=sub.n[,c("year","site","species","X.adults")]
colnames(temp3)[4] = "density"
temp3$stage="A"

obs=rbind(temp1,temp2,temp3)
obs$year=as.numeric(as.character(obs$year))

obs$sp_site=paste(as.character(obs$species),as.character(obs$site))
obs$stage=factor(obs$stage,levels=c("R","S","A"))

df.s=df[df$species%in%"H. halimifolium",]
obs.s=obs[obs$species%in%"H. halimifolium",]

a=ggplot(df.s,aes(year,density,group=site))+
  geom_line(col="grey")+
  geom_jitter(data=obs.s, size=2)+
  facet_grid(stage~.,scales = "free_y")+
  scale_color_manual(values=c("black","purple","blue","red","orange"))+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Halimium halimifolium")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "top",
        legend.title = element_blank())+
  scale_x_continuous(breaks = c(1,4,8,10,12))

a

df.s=df[df$species%in%"H. commutatum",]
obs.s=obs[obs$species%in%"H. commutatum",]

b=ggplot(df.s,aes(year,density,group=site))+
  geom_line(col="grey")+
  geom_jitter(data=obs.s, size=2)+
  facet_grid(stage~.,scales = "free_y")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Halimium commutatum")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "top")+
  scale_x_continuous(breaks = c(1,4,8,10,12))

b

df.s=df[df$species%in%"L. stoechas",]
obs.s=obs[obs$species%in%"L. stoechas",]

c=ggplot(df.s,aes(year,density,group=site))+
  geom_line(col="grey")+
  geom_jitter(data=obs.s, size=2)+
  facet_grid(stage~.,scales = "free_y")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Lavandula stoechas")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "top")+
  scale_x_continuous(breaks = c(1,4,8,10,12))

c

df.s=df[df$species%in%"R. officinalis",]
obs.s=obs[obs$species%in%"R. officinalis",]

d=ggplot(df.s,aes(year,density,group=site))+
  geom_line(col="grey")+
  geom_jitter(data=obs.s, size=2)+
  facet_grid(stage~.,scales = "free_y")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Rosmarinus officinalis")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "top")+
  scale_x_continuous(breaks = c(1,4,8,10,12))

d

df.s=df[df$species%in%"C. libanotis",]
obs.s=obs[obs$species%in%"C. libanotis",]

e=ggplot(df.s,aes(year,density,group=site))+
  geom_line(col="grey")+
  geom_jitter(data=obs.s, size=2)+
  facet_grid(stage~.,scales = "free_y")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Cistus libanotis")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "top")+
  scale_x_continuous(breaks = c(1,4,8,10,12))

e
