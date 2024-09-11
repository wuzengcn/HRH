simu_model <- function(n.trial = 1){
  
  datastore <- array(NA, dim = c(18,5, n.trial), dimnames = list(NULL,NULL,NULL))
  
  for (ii in 1:n.trial){
    
   set.seed(ii+1) 
    sim.exp <- rbeta(1, v.exp^2*(1-v.exp)/(v.exp.sd)^2+
                       v.exp^2/(1-v.exp) - v.exp/(1-v.exp),
                     v.exp*(1-v.exp)^2/(v.exp.sd)^2 + 
                       v.exp - 1)
    sim.reduction <- rbeta(1,v.reduction^2*(1-v.reduction)/(v.reduction.sd)^2+
                            v.reduction^2/(1-v.reduction) - v.reduction/(1-v.reduction),
                           v.reduction*(1-v.reduction)^2/(v.reduction.sd)^2 + 
                             v.reduction - 1)
    
    sim.elasu5mr <- -1*rbeta(1, abs(v.elas.u5mr)^2*(1-abs(v.elas.u5mr))/(v.elas.u5mr.sd)^2+
                          abs(v.elas.u5mr)^2/(1-abs(v.elas.u5mr)) - abs(v.elas.u5mr)/(1-abs(v.elas.u5mr)),
                        abs(v.elas.u5mr)*(1-abs(v.elas.u5mr))^2/(v.elas.u5mr.sd)^2 + 
                          v.elas.u5mr - 1)
    sim.elasmmr <- -1*rbeta(1, abs(v.elas.mmr)^2*(1-abs(v.elas.mmr))/(v.elas.mmr.sd)^2+
                          abs(v.elas.mmr)^2/(1-abs(v.elas.mmr)) - abs(v.elas.mmr)/(1-abs(v.elas.mmr)),
                        abs(v.elas.mmr)*(1-abs(v.elas.mmr))^2/(v.elas.mmr.sd)^2 + 
                          abs(v.elas.mmr) - 1)
    
    sim.c11 <- rgamma(1,shape = m.uc.tr[1,1]^2/(m.uc.tr[1,1]*0.2)^2 , 
           scale= (m.uc.tr[1,1]*0.2)^2/m.uc.tr[1,1])
    
    sim.c12 <- rgamma(1,shape = m.uc.tr[1,2]^2/(m.uc.tr[1,2]*0.2)^2 , 
                      scale= (m.uc.tr[1,2]*0.2)^2/m.uc.tr[1,2])
    
    sim.c13 <- rgamma(1,shape = m.uc.tr[1,3]^2/(m.uc.tr[1,3]*0.2)^2 , 
                      scale= (m.uc.tr[1,3]*0.2)^2/m.uc.tr[1,3])
    
    sim.c14 <- rgamma(1,shape = m.uc.tr[1,4]^2/(m.uc.tr[1,4]*0.2)^2 , 
                      scale= (m.uc.tr[1,4]*0.2)^2/m.uc.tr[1,4])
    
    
    sim.c21 <- rgamma(1,shape = m.uc.tr[2,1]^2/(m.uc.tr[2,1]*0.2)^2 , 
                      scale= (m.uc.tr[2,1]*0.2)^2/m.uc.tr[2,1])
    
    sim.c22 <- rgamma(1,shape = m.uc.tr[2,2]^2/(m.uc.tr[2,2]*0.2)^2 , 
                      scale= (m.uc.tr[2,2]*0.2)^2/m.uc.tr[2,2])
    
    sim.c23 <- rgamma(1,shape = m.uc.tr[2,3]^2/(m.uc.tr[2,3]*0.2)^2 , 
                      scale= (m.uc.tr[2,3]*0.2)^2/m.uc.tr[2,3])
    
    sim.c24 <- rgamma(1,shape = m.uc.tr[2,4]^2/(m.uc.tr[2,4]*0.2)^2 , 
                      scale= (m.uc.tr[2,4]*0.2)^2/m.uc.tr[2,4])
    
    
    sim.c31 <- rgamma(1,shape = m.uc.tr[3,1]^2/(m.uc.tr[3,1]*0.2)^2 , 
                      scale= (m.uc.tr[3,1]*0.2)^2/m.uc.tr[3,1])
    
    sim.c32 <- rgamma(1,shape = m.uc.tr[3,2]^2/(m.uc.tr[3,2]*0.2)^2 , 
                      scale= (m.uc.tr[3,2]*0.2)^2/m.uc.tr[3,2])
    
    sim.c33 <- rgamma(1,shape = m.uc.tr[3,3]^2/(m.uc.tr[3,3]*0.2)^2 , 
                      scale= (m.uc.tr[3,3]*0.2)^2/m.uc.tr[3,3])
    
    sim.c34 <- rgamma(1,shape = m.uc.tr[3,4]^2/(m.uc.tr[3,4]*0.2)^2 , 
                      scale= (m.uc.tr[3,4]*0.2)^2/m.uc.tr[3,4])
    
    sim.c41 <- rgamma(1,shape = m.uc.tr[4,1]^2/(m.uc.tr[4,1]*0.2)^2 , 
                      scale= (m.uc.tr[4,1]*0.2)^2/m.uc.tr[4,1])
    
    sim.c42 <- rgamma(1,shape = m.uc.tr[4,2]^2/(m.uc.tr[4,2]*0.2)^2 , 
                      scale= (m.uc.tr[4,2]*0.2)^2/m.uc.tr[4,2])
    
    sim.c43 <- rgamma(1,shape = m.uc.tr[4,3]^2/(m.uc.tr[4,3]*0.2)^2 , 
                      scale= (m.uc.tr[4,3]*0.2)^2/m.uc.tr[4,3])
    
    sim.c44 <- rgamma(1,shape = m.uc.tr[4,4]^2/(m.uc.tr[4,4]*0.2)^2 , 
                      scale= (m.uc.tr[4,4]*0.2)^2/m.uc.tr[4,4])
    
    sim.c.tr <- matrix(c(sim.c11, sim.c12, sim.c13, sim.c14, sim.c14,
                         sim.c21, sim.c22, sim.c23, sim.c24, sim.c24,
                         sim.c31, sim.c32, sim.c33, sim.c34, sim.c34,
                         sim.c41, sim.c42, sim.c43, sim.c44, sim.c44), nrow = 4, byrow = TRUE)
    
    #need to make sure sim.elasu5mr and sim.elasmmr is a vector and change the sign of elasticity
    
    simres <- finalresult(sim.exp, sim.reduction, sim.elasu5mr, sim.elasmmr, sim.c.tr)
    
    datastore[,,ii] <- simres[[1]]
  }
  
  dtakenya <- as.data.frame(t(datastore[,1,]))
  dtaeswatini <- as.data.frame(t(datastore[,2,]))
  dtacol <- as.data.frame(t(datastore[,3,]))
  dtawc <- as.data.frame(t(datastore[,4,]))
  dtakz <- as.data.frame(t(datastore[,5,]))
  
  dtaall <- list(dtakenya, dtaeswatini, dtacol, dtawc, dtakz)
  return(dtaall)
}

