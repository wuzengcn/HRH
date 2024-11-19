
# Primary infection

coi <- function(ninfections = v.inf.tot, cfr=v.cfr.hcw, dailywage = m.inc.daily, le =v.le.hcw, annualloss = v.gdp.2020, ctr = m.uc.tr) {
  v.inf.dtype <- ninfections%*%t(v.covid.dis)
  m.dmc.dtype <- ctr * t(v.inf.dtype) 
  v.dmc <- colSums(m.dmc.dtype)
  
  v.c.travel.m <- v.uc.travel*t(v.inf.dtype)[2,]
  v.c.travel.s<- v.uc.travel*t(v.inf.dtype)[3,]
  v.c.travel.c <- v.uc.travel*t(v.inf.dtype)[4,]
  v.c.travel <- v.c.travel.m + v.c.travel.s + v.c.travel.c
  
  v.c.meal.m <- v.uc.meal*t(v.inf.dtype)[2,]*m.los[1,]
  v.c.meal.s <- v.uc.meal*t(v.inf.dtype)[3,]*m.los[2,] 
  v.c.meal.c <- v.uc.meal*t(v.inf.dtype)[4,]*m.los[3,] 
  v.c.meal <- v.c.meal.m + v.c.meal.s + v.c.meal.c
  v.dnmc <- v.c.travel + v.c.meal
  
  m.cfr.1 <- v.deathcomp*cfr[1,]/v.covid.dis.3c
  m.cfr.2 <- v.deathcomp*cfr[2,]/v.covid.dis.3c
  m.cfr.3 <- v.deathcomp*cfr[3,]/v.covid.dis.3c
  m.cfr.4 <- v.deathcomp*cfr[4,]/v.covid.dis.3c
  m.cfr.5 <- v.deathcomp*cfr[5,]/v.covid.dis.3c
  
  m.cfr <- cbind(m.cfr.1, m.cfr.2, m.cfr.3, m.cfr.4, m.cfr.5)
  
  m.sr <- 1- m.cfr
  
  v.inf.dtype.3c <- v.inf.dtype
  v.inf.dtype.3c[,1] <- v.inf.dtype[,1] + v.inf.dtype[,2]
  v.inf.dtype.3c <- v.inf.dtype.3c[,-2]
  
  m.incloss<- t(v.inf.dtype.3c)*m.sr*m.loa*dailywage
  v.incloss <- t(as.matrix(colSums(m.incloss)))
  
  v.cperlife <- annualloss * ((1-exp(-0.03*le))/0.03)
  m.cperlife <- rbind(v.cperlife,v.cperlife,v.cperlife)
  m.ic <- t(v.inf.dtype.3c)*m.cfr* m.cperlife
  v.ic <- colSums(m.ic)
  
  v.tc <- v.dmc + v.dnmc + v.incloss + v.ic
  res<-list(dmc = v.dmc, dnmc = v.dnmc, travelc= v.c.travel, mealc = v.c.meal, incloss = v.incloss, ic = v.ic, tc = v.tc)
  return(res)
}

# secondary infection 

sic <- function(share_exp) {
  n.odds <- 1.79
  v.odds <- 1+ (n.odds-1)/log(as.vector(v.ir.ratio.r))*log(v.ir.ratio.c)
  #v.odds <- t(as.matrix(c(rep(n.odds,5))))
  v.exp <- (v.admr/1000 * v.pop * share_exp + (v.hhsize-1)*v.tot.hcw)/v.pop
  v.par <- (v.odds-1)*v.exp/((v.odds-1)*v.exp+1)
  v.sinf <- t(v.inf.gp * v.par)
  
  coi(ninfections = v.sinf, cfr=v.cfr.gp, dailywage = m.uc.mwage, le =v.le.gp, annualloss = v.gdp.2020)
}

# absentism 

absenteeism <- function(share.reduction, elas.u5mr, elas.mmr) {
  
  v.rstaff <- v.inf.p*16.44/260 + v.death.p*0.5 + (1-v.inf.p-v.death.p)*share.reduction
  v.mc.u5mr <- -elas.u5mr*v.rstaff
  v.mc.mmr <- -elas.mmr*v.rstaff
  
  v.death.u5mr <- v.pop*v.br/1000*v.u5mr*v.mc.u5mr
  v.death.mmr <- v.pop*v.br/1000*v.mmr*v.mc.mmr
  
  v.c.u5mr <- v.death.u5mr*v.cperlife.u5mr
  v.c.mmr <- v.death.mmr*v.cperlife.mmr
  v.c.absenteeism <- v.c.u5mr + v.c.mmr
  abres <- list(cu5mr = v.c.u5mr, cmmr = v.c.mmr, cabs = v.c.absenteeism, nu5death = v.death.u5mr, nmmrdeath = v.death.mmr, rstaff = v.rstaff, incu5mr = v.mc.u5mr, incmmr = v.mc.mmr)
  return(abres)
}

finalresult <- function(exposure, reduction, elas.u5, elas.mm, trc) {
  a <- coi(ctr = trc)
  b <- sic(exposure)
  c <- absenteeism(reduction, elas.u5, elas.mm)
  # d <- v.c.edu
  
  #final<-rbind(a[["dmc"]],a[["dnmc"]],a[["incloss"]],a[["ic"]],a[["tc"]], b[["dmc"]],b[["dnmc"]],b[["incloss"]],b[["ic"]],b[["tc"]],c[["cu5mr"]],c[["cmmr"]], c[["cabs"]],d)
  
  final<-rbind(a[["dmc"]],a[["dnmc"]],a[["incloss"]],a[["ic"]],a[["tc"]], b[["dmc"]],b[["dnmc"]],b[["incloss"]],b[["ic"]],b[["tc"]],c[["cu5mr"]],c[["cmmr"]], c[["cabs"]], c[["nu5death"]],c[["nmmrdeath"]])
  
  # totc <- final[5,]+final[10,]+final[13,]+final[14,]
  
  totc <- final[5,]+final[10,]+final[13,]
  
  totc_the <- totc/v.the/1000000
  totc_ghee <- totc_the/v.ghee.che
  options(scipen = 100, digits = 3)
  final <- rbind(final,totc, totc_the, totc_ghee)
  
  colnames(final)<-c("Kenya","Eswatini","Colombia","SA-WC","SA-KZ")
  rownames(final)<-c("hcwdmc","hcwdnmc","hcwincloss","hcwic","hcwtc",
                     "sinfdmc","sinfdnmc","sinfincloss","sinfic","sinftc",
                     "u5mc", "mmrc","mchc","u5 death","materna death","Total_cost", "share_of_CHE",
                     "share_of_ghee")
  
  finalres <- list(final, a, b, c)
  
  return(finalres)
}
  
  
