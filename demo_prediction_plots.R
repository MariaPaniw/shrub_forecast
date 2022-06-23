
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

# #### PLOTS OF POSTERIORS 

# Note that we added the parameters obtained from our own run of the Bayesian model to GitHub

load("allShrubs.Rdata")

# convert to mcmc list

names=c( "a0.h1",
         "a1.h1",
         "b0.h1",
         "b2.h1",
         "a3.h1",
         "gamma.h1",
         "a0.h2",
         "a2.h2",
         "b0.h2",
         "b1.h2",
         "b2.h2",
         "a3.h2",
         "b3.h2",
         "gamma.h2",
         "a0.l",
         "a3.l",
         "b0.l",
         "b2.l",
         "b3.l",
         "gamma.l",
         "a0.r",
         "a2.r",
         "b0.r",
         "b2.r",
         "a3.r",
         "b3.r",
         "gamma.r",
         "a0.c",
         "a1.c",
         "a2.c",
         "b0.c",
         "b1.c",
         "b2.c",
         "a3.c",
         "gamma.c")

param=NULL

for(i in 1:length(names)){
  
  param=cbind(param,out1$sims.list[[names[i]]])
  
}
    


colnames(param)=names

# convert param from a dataframe to an MCMC object required by coda
param.coda=mcmc.list(list(mcmc(param[1:700,]),mcmc(param[701:1400,]),mcmc(param[1401:2100,])))


MCMCtrace(param.coda,pdf=T,filename="all_Shrubs",wd="SPECIFY WORKING DIRECTORY")

pdf("mcmc.plotAllShrubs.pdf",width=6,height=10)

MCMCplot(param.coda,ref_ovl = T,xlab="")

dev.off()

### Predictions
precip=read.csv("precipPalacios.csv")

rain=as.numeric(scale(precip$Prec.[precip$year%in%c(2007:2020)]))
rain=rain[order(rain)]

neigh=c(0,2,5) # Interspecific densities

# Halilimium halimifolium 

pu.sam=sample(1:2100,100,replace = F) # sample 100 posterior values

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(neigh)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$a0.h1[pu.sam[i]] + out1$sims.list$a1.h1[pu.sam[i]]*rain
                                    + out1$sims.list$a3.h1[pu.sam[i]]*neigh[j]),
                     rain=rain,
                     neigh=neigh[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
 
}

pred$pu=factor(pred$pu)

mu.pred=NULL

for(j in 1:length(neigh)){
  
  temp=data.frame(pred=inv.logit(out1$mean$a0.h1 +out1$mean$a1.h1*rain
                                 + out1$mean$a3.h1*neigh[j]),
                  rain=rain,
                  neigh=neigh[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$neigh=factor(pred$neigh)

mu.pred$neigh=factor(mu.pred$neigh)

pred$pu=paste(pred$pu,pred$neigh)

survA=ggplot(data=pred,aes(rain,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  geom_line(data=mu.pred,aes(rain,pred,group=neigh),size=1,alpha=1,col="black")+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  xlab("Rainfall")+
  ylab("Adult survival")+
  theme_bw(base_size=20)+
  ggtitle("Halimium halimifolium")+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.17,0.7),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survA

pred=NULL

for(i in 1:length(pu.sam)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$b0.h1[pu.sam[i]] + out1$sims.list$b2.h1[pu.sam[i]]*seq(0,5,length.out = 15)),
                    ad=seq(0,5,length.out = 15))
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  
}

pred$pu=factor(pred$pu)

mu.pred=NULL


  
mu.pred=data.frame(pred=inv.logit(out1$mean$b0.h1 + out1$mean$b2.h1*seq(0,5,length.out = 15) ),
                  ad=seq(0,5,length.out = 15))
  


mu.pred$pu=NA

survS=ggplot(data=pred,aes(ad,pred,group=pu))+
  geom_line(size=0.1,alpha=0.3,col="grey")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  geom_line(data=mu.pred,aes(ad,pred),size=1,alpha=1,col="black")+
  xlab("Log(# adult intraspecific)")+
  ylab("Sapling survival/transition")+
  theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.85,0.85),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survS


x= survA + survS

x

##############################

#### Halimium commutatum

### Predictions

ad=seq(0,5,length.out = 15) # intraspecific density
neigh=c(0,2,5)

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(neigh)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$a0.h2[pu.sam[i]] + out1$sims.list$a2.h2[pu.sam[i]]*ad + out1$sims.list$a3.h2[pu.sam[i]]*neigh[j]),
                    ad=ad,
                    neigh=neigh[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=paste(pred$pu,pred$neigh)

mu.pred=NULL

for(j in 1:length(neigh)){
  
  temp=data.frame(pred=inv.logit(out1$mean$a0.h2 + out1$mean$a2.h2*ad + out1$mean$a3.h2*neigh[j]),
                  ad=ad,
                  neigh=neigh[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$neigh=factor(pred$neigh)

mu.pred$neigh=factor(mu.pred$neigh)


survA=ggplot(data=pred,aes(ad,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  geom_line(data=mu.pred,aes(ad,pred,group=neigh,col=neigh),size=1,alpha=1)+
  xlab("Log(# adult intraspecific)")+
  ylab("Adult survival")+
  theme_bw(base_size=20)+
  ggtitle("Halimium commutatum")+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.87,0.75),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survA

ad=c(0,2,5)
neigh=c(0,2,5)

new=expand.grid(ad,neigh)

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:nrow(new)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$b0.h2[pu.sam[i]] + out1$sims.list$b1.h2[pu.sam[i]]*rain
                                   + out1$sims.list$b2.h2[pu.sam[i]]*new$Var1[j] + out1$sims.list$b3.h2[pu.sam[i]]*new$Var2[j]),
                    rain=rain,
                    ad=new$Var1[j],
                    neigh=new$Var2[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=factor(pred$pu)

mu.pred=NULL

for(j in 1:nrow(new)){
  
  temp=data.frame(pred=inv.logit(out1$mean$b0.h2 +out1$mean$b1.h2*rain
                                 + out1$mean$b2.h2*new$Var1[j] + out1$mean$b3.h2*new$Var2[j]),
                  rain=rain,
                  ad=new$Var1[j],
                  neigh=new$Var2[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$ad=factor(pred$ad)
pred$neigh=factor(pred$neigh)

mu.pred$ad=factor(mu.pred$ad)
mu.pred$neigh=factor(mu.pred$neigh)

pred$pu=paste(pred$pu,pred$neigh)

survS=ggplot(data=pred,aes(rain,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Neighbors")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  geom_line(data=mu.pred,aes(rain,pred,group=neigh),size=1,alpha=1,col="black")+
  facet_grid(.~ad)+
  xlab("Rainfall")+
  ylab("Sapling survival/transition")+
  theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survS

x= survA + survS

x
##############################

#### Lavandula stoechas

pred=NULL

ad=seq(0,5,length.out = 15)
for(i in 1:length(pu.sam)){
  
  temp1=data.frame(pred=inv.logit(out1$sims.list$a0.l[pu.sam[i]] + out1$sims.list$a3.l[pu.sam[i]]*ad),
                   ad=ad)
  
  
  temp=rbind(temp1)
  temp$pu=i
  
  pred=rbind(pred,temp)
  
}

pred$pu=factor(pred$pu)

mu1=data.frame(pred=inv.logit(out1$mean$a0.l + out1$mean$a3.l*ad),
               ad=ad)


mu.pred=rbind(mu1)
mu.pred$pu=NA


survA=ggplot(data=pred,aes(ad,pred,group=pu))+
  geom_line(size=0.1,alpha=0.3,col="grey")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  geom_line(data=mu.pred,aes(ad,pred),size=1,alpha=1,col="black")+
  xlab("Log(# adult interspecific)")+
  ylab("Adult survival")+
  theme_bw(base_size=20)+
  ggtitle("Lavandula stoechas")+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survA

ad=seq(0,5,length.out = 15)
neigh=c(0,2,5)

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(neigh)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$b0.l[pu.sam[i]] + out1$sims.list$b2.l[pu.sam[i]]*ad + out1$sims.list$b3.l[pu.sam[i]]*neigh[j]),
                    ad=ad,
                    neigh=neigh[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=paste(pred$pu,pred$neigh)

mu.pred=NULL

for(j in 1:length(neigh)){
  
  temp=data.frame(pred=inv.logit(out1$mean$b0.l + out1$mean$b2.l*ad + out1$mean$b3.l*neigh[j]),
                  ad=ad,
                  neigh=neigh[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$neigh=factor(pred$neigh)

mu.pred$neigh=factor(mu.pred$neigh)


survS=ggplot(data=pred,aes(ad,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  geom_line(data=mu.pred,aes(ad,pred,group=neigh,col=neigh),size=1,alpha=1)+
  xlab("Log(# adult intraspecific)")+
  ylab("Sapling survival/transition")+
  theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survS


x= survA + survS

x

##############################

#### Rosmarinus officinalis

ad=seq(0,5,length.out = 15)
neigh=c(0,2,5)


pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(neigh)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$a0.r[pu.sam[i]] + out1$sims.list$a2.r[pu.sam[i]]*ad + out1$sims.list$a3.r[pu.sam[i]]*neigh[j]),
                    ad=ad,
                    neigh=neigh[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=paste(pred$pu,pred$neigh)

mu.pred=NULL

for(j in 1:length(neigh)){
  
  temp=data.frame(pred=inv.logit(out1$mean$a0.r + out1$mean$a2.r*ad + out1$mean$a3.r*neigh[j]),
                  ad=ad,
                  neigh=neigh[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$neigh=factor(pred$neigh)

mu.pred$neigh=factor(mu.pred$neigh)


survA=ggplot(data=pred,aes(ad,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  geom_line(data=mu.pred,aes(ad,pred,group=neigh,col=neigh),size=1,alpha=1)+
  xlab("Log(# adult intraspecific)")+
  ylab("Adult survival")+
  theme_bw(base_size=20)+
  ggtitle("Rosmarinus officinalis")+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.87,0.7),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survA

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(neigh)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$b0.r[pu.sam[i]] + out1$sims.list$b2.r[pu.sam[i]]*ad + out1$sims.list$b3.r[pu.sam[i]]*neigh[j]),
                    ad=ad,
                    neigh=neigh[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=paste(pred$pu,pred$neigh)

mu.pred=NULL

for(j in 1:length(neigh)){
  
  temp=data.frame(pred=inv.logit(out1$mean$b0.r + out1$mean$b2.r*ad + out1$mean$b3.r*neigh[j]),
                  ad=ad,
                  neigh=neigh[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$neigh=factor(pred$neigh)

mu.pred$neigh=factor(mu.pred$neigh)


survS=ggplot(data=pred,aes(ad,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  geom_line(data=mu.pred,aes(ad,pred,group=neigh,col=neigh),size=1,alpha=1)+
  xlab("Log(# adult intraspecific)")+
  ylab("Sapling survival/transition")+
  theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survS



x= survA + survS

x

##############################

#### Cistus libanotis

ad=c(0,2,5)
neigh=c(0,2,5)

new=expand.grid(ad,neigh)

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:nrow(new)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$a0.c[pu.sam[i]] + out1$sims.list$a1.c[pu.sam[i]]*rain
                                   + out1$sims.list$a2.c[pu.sam[i]]*new$Var1[j] + out1$sims.list$a3.c[pu.sam[i]]*new$Var2[j]),
                    rain=rain,
                    ad=new$Var1[j],
                    neigh=new$Var2[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=factor(pred$pu)

mu.pred=NULL

for(j in 1:nrow(new)){
  
  temp=data.frame(pred=inv.logit(out1$mean$a0.c +out1$mean$a1.c*rain
                                 + out1$mean$a2.c*new$Var1[j] + out1$mean$a3.c*new$Var2[j]),
                  rain=rain,
                  ad=new$Var1[j],
                  neigh=new$Var2[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$ad=factor(pred$ad)
pred$neigh=factor(pred$neigh)

mu.pred$ad=factor(mu.pred$ad)
mu.pred$neigh=factor(mu.pred$neigh)

pred$pu=paste(pred$pu,pred$neigh)

survA=ggplot(data=pred,aes(rain,pred,group=pu,col=neigh))+
  geom_line(size=0.1,alpha=0.3)+
  
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  geom_line(data=mu.pred,aes(rain,pred,group=neigh),size=1,alpha=1,col="black")+
  scale_color_manual(values=c("black","#ffc425","#f37735"),name="Inter")+
  facet_grid(.~ad)+
  xlab("Rainfall")+
  ylab("Adult survival")+
  theme_bw(base_size=20)+
  ggtitle("Cistus libanotis")+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survA 


ad=c(0,2,5)

pred=NULL

for(i in 1:length(pu.sam)){
  
  for(j in 1:length(ad)){
    
    temp=data.frame(pred=inv.logit(out1$sims.list$b0.c[pu.sam[i]] + out1$sims.list$b1.c[pu.sam[i]]*rain + out1$sims.list$b2.c[pu.sam[i]]*ad[j]),
                    rain=rain,
                    ad=ad[j])
    
    temp$pu=i
    
    pred=rbind(pred,temp)
  }
  
}

pred$pu=paste(pred$pu,pred$ad)

mu.pred=NULL

for(j in 1:length(ad)){
  
  temp=data.frame(pred=inv.logit(out1$mean$b0.c + out1$mean$b1.c*rain + out1$mean$b2.c*ad[j]),
                  rain=rain,
                  ad=ad[j])
  
  mu.pred=rbind(mu.pred,temp)
}


mu.pred$pu=NA

pred$ad=factor(pred$ad)

mu.pred$ad=factor(mu.pred$ad)


survS=ggplot(data=pred,aes(rain,pred,group=pu,col=ad))+
  geom_line(size=0.1,alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))+
  scale_color_manual(values=c("black","purple","blue"),name="Intra")+
  geom_line(data=mu.pred,aes(rain,pred,group=ad,col=ad),size=1,alpha=1)+
  xlab("Raifall")+
  ylab("Sapling survival/transition")+
  theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.2,0.7),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(colour="black"))

survS


x= survA + survS

x
