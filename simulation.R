
### Code by Maria Paniw
### Created 09-10-2022

### Simulated data to test parameter recovery

# We simulated five species in 20 sites

### Here are the parameters we used for simulations: 

a0.S1=c(5,5,5,8,5,5,5,2.5,5,6,4)
a1.S1=c(0.4,0.6,0.4,0.4,0.4,0.5,0.4,0,0.4,0.4,0.4)
a2.S1=c(0,0,-0.9,0,-1,-1,-1,0.2,0,0,0)
a3.S1=c(-0.9,-0.8,0,-1.8,0,0,0,-0.4,-0.8,-1,-0.9)
b0.S1=c(4,3.8,2,2,2,0.5,2,NA,NA,2,rep(NA,5))
b1.S1=c(0.8,0.8,0.3,0,0,0,0,NA,NA,0,rep(NA,5))
b2.S1=c(0,0,0,0,-0.3,rep(0,10))
b3.S1=c(-0.1,-0.1,0,-0.3,0,1,-0.3,NA,NA,-0.3,rep(NA,5))
gamma.S1=c(4.8,4.5,5,5,1,1.7,1,1.5,1,1,1)
tau.S1=c(0.2,0.4,0.2,0.2,0.2,0.25,0.2,0.5,0.2,0.2,0.2)

a0.S2=c(4,4.5,4,6,4,4,4,7,5,4,5)
a1.S2=c(rep(0,7),0.7,0,0,0)
a2.S2=c(-0.7,-0.9,rep(-0.7,5),0,-0.7,-1.5,-1.7)
a3.S2=c(-0.2,-0.4,-0.3,rep(-0.2,4),-0.8,-0.2,-0.9,-0.2)
b0.S2=c(6,5,5,6,rep(NA,11))
b1.S2=c(0.5,0.5,0.7,0.7,rep(NA,11))
b2.S2=c(0,0,0,-1.2,rep(NA,11))
b3.S2=c(-1.2,1.5,-1.2,0,rep(NA,11))
gamma.S2=c(5,6,5,5,rep(2.1,11))
tau.S2=c(rep(0.3,7),0.6,0.6,0.3,0.6)

a0.S3=c(6.5,7,6.5,8.5,3.5,3.5,3.5,6,5,3.5,5)
a1.S3=c(rep(0,15))
a2.S3=c(rep(0,15))
a3.S3=c(-0.9,-0.9,-1.0,-1.7,rep(-0.55,3),-2,-1.2,-0.55,-1.2)
b0.S3=c(4,4,4,4,1.5,1.5,3,0.1,0.9,0.9,0.9)
b1.S3=c(rep(0,15))
b2.S3=c(0.2,-0.2,-0.3,0.2,0,0,0,0,0.2,0)
b3.S3=c(-0.6,0.9,0.6,0.6,-0.2,0.2,0.2,-0.4,-0.6,0,-0.6)
gamma.S3=c(6,6,6,4.9,2.5,2.5,2.5,2.5,3.8,2.5,2.5)
tau.S3=c(0.1,0.2,0.1,0.1,0.2,0.23,0.2,0.7,0.7,0.1,0.7)


a0.S4=c(4.1,4.1,4.1,8,5,3.5,3.5,9,6,6,6)
a1.S4=c(rep(0,5),0.2,0.3,rep(0,8))
a2.S4=c(-0.4,-0.4,-0.4,-0.4,-0.4,0.3,0.4,-1.5,-0.4,-1.2,-0.7)
a3.S4=c(-0.9,-0.9,-1.98,-1.98,-0.7,-0.6,-0.7,-1.8,-1,-0.3,-1)
b0.S4=c(2.8,3.8,2.7,4,3,3,3,rep(NA,8))
b1.S4=c(0,0,0,0,0.2,0.2,0.2,rep(NA,8))
b2.S4=c(0,0,0,0,-0.35,0.35,-0.35,rep(NA,8))
b3.S4=c(-0.1,-0.1,-0.1,-1.9,0,0,0,rep(NA,8))
gamma.S4=c(7,7,7,4.9,0.5,0.4,0.5,1.1,0.5,0.9,0.5)
tau.S4=c(0.2,0.2,0.5,0.2,0.2,0.2,0.2,0.6,0.3,0.2,0.2)

a0.S5=c(4.2,4.2,5,rep(4.2,4),3.5,3.5,4.2,5)
a1.S5=c(1,1,1.2,rep(1,4),1.5,1,1,0.8)
a2.S5=c(-0.2,-0.3,-0.3,rep(-0.2,4),-0.3,rep(-0.2,7))
a3.S5=c(-0.6,-0.5,rep(-0.6,5),-0.2,rep(-0.6,7))
b0.S5=c(3.5,3.5,rep(NA,5),1.5,1.2,NA,1.9)
b1.S5=c(0.4,0.4,rep(NA,5),0.6,0.6,NA,0)
b2.S5=c(0,0,rep(NA,5),-1.1,-0.8,NA,-0.8)
b3.S5=c(rep(0,15))
gamma.S5=c(7,7,7,4.9,1.7,1.7,1.7,3,1.9,1.7,1.7)
tau.S5=c(0.3,0.35,rep(0.3,5),0.65,0.3,0.3,0.3)

# Rainfall

rain=rnorm(20)

# Densities (all but initial ones simulated): 20 sites, 15 years
# a - adults
# j - juveniles

# S1
S1.a = array(NA,c(20,15,15))
S1.a[,1,]=c(5,1,12,1,5,5,2,16,7,17,3,11,3,5,3,1,3,2,4,10)

S1.j = array(NA,c(20,15,15))
S1.j[,1,]=c(2,1,1,5,1,4,2,1,6,1,1,5,7,4,3,4,0,5,2,3)


# S2
S2.a = array(NA,c(20,15,15))
S2.a[,1,]=c(5,4,2,16,2,3,4,5,5,19,5,5,4,13,3,3,5,2,5,7)

S2.j = array(NA,c(20,15,15))
S2.j[,1,]=c(7,5,1,5,6,1,8, 7,9,12,8,29,21,4,5,13,5,7,5,9)


# S3
S3.a = array(NA,c(20,15,15))
S3.a[,1,]=c(18,4,8,24,28,14,2,3,1,3,6,1,2,11,8,2,6,3,10,2)

S3.j = array(NA,c(20,15,15))
S3.j[,1,]=c(11,10,10,21,29,47,5,4,5,1,30,12,15,34,18,1,2,2,20,5)


# S4
S4.a = array(NA,c(20,15,15))
S4.a[,1,]=c(5,8,15,4,2,14,8,12,12,8,30,22,7,1,11,14,9,8,5,15)

S4.j = array(NA,c(20,15,15))
S4.j[,1,]=c(1,3,3,1,1,1,3,1,7,1,12,9,1,2,6,7,1,4,2,2)


# S5
S5.a = array(NA,c(20,15,15))
S5.a[,1,]=c(16,2,6,1,5,5,13,5,1,2,5,9,5,1,4,12,2,4,3,10)

S5.j = array(NA,c(20,15,15))
S5.j[,1,]=c(4,9,4, 0,4,9,14,3,3,3,10,13,3,7,2,15,2,3,2,1)

# Simulate the remaining abundances

for(x in 1:10){ # number of simulations
  
  for(i in 1:20) { # Loop over sites
    
    #Intraspecific abundance
    
    Na.s1 <- S1.a[i,1,x]
    Nj.s1 <- S1.j[i,1,x]
    Na.s2 <- S2.a[i,1,x]
    Nj.s2 <- S2.j[i,1,x]
    Na.s3 <- S3.a[i,1,x]
    Nj.s3 <- S3.j[i,1,x]
    Na.s4 <- S4.a[i,1,x]
    Nj.s4 <- S4.j[i,1,x]
    Na.s5 <- S5.a[i,1,x]
    Nj.s5 <- S5.j[i,1,x]
    
  
    # Initial neighbor
    
    neigh.S1 <- log(Na.s2+Na.s3+Na.s4+Na.s5)
    neigh.S2 <- log(Na.s1+Na.s3+Na.s4+Na.s5)
    neigh.S3 <- log(Na.s2+Na.s1+Na.s4+Na.s5)
    neigh.S4 <- log(Na.s2+Na.s3+Na.s1+Na.s5)
    neigh.S5 <- log(Na.s2+Na.s3+Na.s4+Na.s1)
    
    #Specify the model for years 
    for(t in 1:14) {
    
      # S1
      
      phiA.S1 <- 1/(1+exp(-( a0.S1[x] +  a1.S1[x]*rain[t] +  a2.S1[x]* log(Na.s1+0.001)-  a3.S1[x]* neigh.S1)))
      
      if(x%in%c(8,9,11:15)){
        
        thetaJ.S1 <- 0.7
      }else{
        
        thetaJ.S1 <- 1/(1+exp(-(  b0.S1[x] + b1.S1[x]*rain[t]+  b2.S1[x]* log(Na.s1+0.001)-  b3.S1[x]* neigh.S1)))
        
      }

      G.S1 = rpois(1,gamma.S1[x])
      
      # S2
      
      phiA.S2 <- 1/(1+exp(-( a0.S2[x] +  a1.S2[x]*rain[t] +  a2.S2[x]* log(Na.s2+0.001)-  a3.S2[x]* neigh.S2)))
      
      if(x%in%c(5:15)){
        
        thetaJ.S2 <- 0.8
      }else{
        
        thetaJ.S2 <- 1/(1+exp(-(  b0.S2[x] + b1.S2[x]*rain[t]+  b2.S2[x]* log(Na.s2+0.001)-  b3.S2[x]* neigh.S2)))
        
      }
      
      G.S2 = rpois(1,gamma.S2[x])
      
      # S3
      
      phiA.S3 <- 1/(1+exp(-( a0.S3[x] +  a1.S2[x]*rain[t] +  a2.S3[x]* log(Na.s3+0.001)-  a3.S3[x]* neigh.S3)))
      thetaJ.S3 <- 1/(1+exp(-(  b0.S3[x] + b1.S3[x]*rain[t]+  b2.S3[x]* log(Na.s3+0.001)-  b3.S3[x]* neigh.S3)))
      
      G.S3 = rpois(1,gamma.S3[x])
      
      # S4
      
      phiA.S4 <- 1/(1+exp(-( a0.S4[x] +  a1.S4[x]*rain[t] +  a2.S4[x]* log(Na.s4+0.001)-  a3.S4[x]* neigh.S4)))
      
      if(x%in%c(8:15)){
        
        thetaJ.S4 <- 0.6
      }else{
        
        thetaJ.S4 <- 1/(1+exp(-(  b0.S4[x] + b1.S4[x]*rain[t]+  b2.S4[x]* log(Na.s4+0.001)-  b3.S4[x]* neigh.S4)))
        
      }
      
      
      G.S4 = rpois(1,gamma.S4[x])
      
      # S5
      
      phiA.S5 <- 1/(1+exp(-( a0.S5[x] +  a1.S5[x]*rain[t] +  a2.S5[x]* log(Na.s5+0.001)-   a3.S5[x]* neigh.S5)))
      
      if(x%in%c(3:7,10)){
        
        thetaJ.S5 <- 0.85
      }else{
        
        thetaJ.S5 <- 1/(1+exp(-(  b0.S5[x] + b1.S5[x]*rain[t]+  b2.S5[x]* log(Na.s5+0.001) -  b3.S5[x]* neigh.S5)))
        
      }

      
      G.S5 = rpois(1,gamma.S5[x])
      
      ### Update abundances
      
      Na.s1 <- phiA.S1 *  Na.s1 +  tau.S1[x] * thetaJ.S1 * Nj.s1
      Nj.s1 <- (1 - tau.S1[x])*thetaJ.S1 *  Nj.s1 + G.S1
      
      Na.s2 <- phiA.S2 *  Na.s2 +  tau.S2[x] * thetaJ.S2 * Nj.s2
      Nj.s2 <- (1 - tau.S2[x])*thetaJ.S2 *  Nj.s2 + G.S2
      
      Na.s3 <- phiA.S3 *  Na.s3 +  tau.S3[x] * thetaJ.S3 * Nj.s3
      Nj.s3 <- (1 - tau.S3[x])*thetaJ.S3 *  Nj.s3 + G.S3
      
      Na.s4 <- phiA.S4 *  Na.s4 +  tau.S4[x] * thetaJ.S4 * Nj.s4
      Nj.s4 <- (1 - tau.S4[x])*thetaJ.S4 *  Nj.s4 + G.S4
      
      Na.s5 <- phiA.S5 *  Na.s5 +  tau.S5[x] * thetaJ.S5 * Nj.s5
      Nj.s5 <- (1 - tau.S5[x])*thetaJ.S5 *  Nj.s5 + G.S5
      
      neigh.S1 <- log(Na.s2+Na.s3+Na.s4+Na.s5)
      neigh.S2 <- log(Na.s1+Na.s3+Na.s4+Na.s5)
      neigh.S3 <- log(Na.s2+Na.s1+Na.s4+Na.s5)
      neigh.S4 <- log(Na.s2+Na.s3+Na.s1+Na.s5)
      neigh.S5 <- log(Na.s2+Na.s3+Na.s4+Na.s1)
      
      # Save abundances
      
      S1.a[i,t+1,x] = rpois(1,Na.s1)
      S1.j[i,t+1,x] = rpois(1,Nj.s1)
      
      S2.a[i,t+1,x] = rpois(1,Na.s2)
      S2.j[i,t+1,x] = rpois(1,Nj.s2)
      
      S3.a[i,t+1,x] = rpois(1,Na.s3)
      S3.j[i,t+1,x] = rpois(1,Nj.s3)
      
      S4.a[i,t+1,x] = rpois(1,Na.s4)
      S4.j[i,t+1,x] = rpois(1,Nj.s4)
      
      S5.a[i,t+1,x] = rpois(1,Na.s5)
      S5.j[i,t+1,x] = rpois(1,Nj.s5)
      
    }
    
  }
  
}

par(mfrow = c(1, 1))


matplot(t(S1.a[,,x]),type = "l")
matplot(t(S1.j[,,x]),type = "l")

matplot(t(S2.a[,,x]),type = "l")
matplot(t(S2.j[,,x]),type = "l")

matplot(t(S3.a[,,x]),type = "l")
matplot(t(S3.j[,,x]),type = "l")

matplot(t(S4.a[,,x]),type = "l")
matplot(t(S4.j[,,x]),type = "l")

matplot(t(S5.a[,,x]),type = "l")
matplot(t(S5.j[,,x]),type = "l")

