
# Import variables
v.pop <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B3:F3", col_names = FALSE))
v.gdp.2020 <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B6:F6", col_names = FALSE))
v.u5mr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B7:F7", col_names = FALSE))
v.mmr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B8:F8", col_names = FALSE))
v.br <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B9:F9", col_names = FALSE))
v.admr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B10:F10", col_names = FALSE))
v.opdr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B11:F11", col_names = FALSE))
v.hhsize <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B12:F12", col_names = FALSE))
v.the <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B13:F13", col_names = FALSE))
m.uc.tr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B15:F18", col_names = FALSE))
m.los <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B20:F22", col_names = FALSE))
v.uc.travel <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B24:F24", col_names = FALSE))
v.uc.meal <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B25:F25", col_names = FALSE))
v.uc.mwage <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B26:F26", col_names = FALSE))
m.uc.edu <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B28:F32", col_names = FALSE))
v.tot.hcw <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B34:F34", col_names = FALSE))
v.if.gp <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B39:F39", col_names = FALSE))
v.le.hcw <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B42:F42", col_names = FALSE))
v.le.gp <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B43:F43", col_names = FALSE))
m.inf <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B45:F49", col_names = FALSE))
m.death <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B51:F55", col_names = FALSE))
m.inc <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B57:F61", col_names = FALSE))
v.inf.gp <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B63:F63", col_names = FALSE))
v.death.gp <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B64:F64", col_names = FALSE))
v.le.u5mr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B65:F65", col_names = FALSE))
v.le.mmr <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B78:F78", col_names = FALSE))
m.loa <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B75:F77", col_names = FALSE))
v.covid.dis <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B5:B8", col_names = FALSE))
v.abs <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B14:B16", col_names = FALSE))
v.deathcomp <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B22:B24", col_names = FALSE))
v.covid.dis.3c <- matrix(c(0.81,0.14,0.05), nrow = 3)
v.ir.hcw.r <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B29", col_names = FALSE))
v.ir.gp.r <- as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B30", col_names = FALSE))
v.elas.u5mr <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B26", col_names = FALSE)))
v.elas.u5mr.sd <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "C26", col_names = FALSE)))
v.elas.mmr <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B27", col_names = FALSE)))
v.elas.mmr.sd <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "C27", col_names = FALSE)))
v.elas.u5mr.l <- as.vector(v.elas.u5mr) + 1.96*0.231/3.08
v.elas.u5mr.u <- as.vector(v.elas.u5mr) - 1.96*0.231/3.08 
v.elas.mmr.l <- as.vector(v.elas.mmr) + 1.96*0.474/4.858
v.elas.mmr.u <- as.vector(v.elas.mmr ) - 1.96*0.474/4.858
v.ghee.che <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet1", range = "B70:F70", col_names = FALSE)))
v.reduction <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B16", col_names = FALSE)))
v.reduction.sd <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "C16", col_names = FALSE)))
v.exp <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "B17", col_names = FALSE)))
v.exp.sd <- as.vector(as.matrix(readxl::read_xlsx("key-parameters1.xlsx", sheet = "Sheet2", range = "C17", col_names = FALSE)))

# Generate new variables 

v.inf.tot <- as.matrix(colSums(m.inf+m.death))
v.cfr.gp <- t(v.death.gp/v.inf.gp)
v.cfr.hcw <- as.matrix(colSums(m.death)/(colSums(m.inf)+colSums(m.death)))

v.inf.tot2 <- t(as.matrix(colSums(m.inf)))
m.inf.tot2 <- rbind(v.inf.tot2,v.inf.tot2,v.inf.tot2,v.inf.tot2,v.inf.tot2)
m.inf.share <- m.inf/m.inf.tot2
v.inc.monthly <- t(as.matrix(colSums(m.inc*m.inf.share)))
v.inc.annual <- v.inc.monthly*12
m.inc.monthly <- rbind(v.inc.monthly , v.inc.monthly , v.inc.monthly)
m.inc.daily <- m.inc.monthly/22

v.ir.hcw <- t(v.inf.tot)/v.tot.hcw
v.ir.gp<- v.inf.gp/v.pop
v.ir.ratio.c <- v.ir.hcw/v.ir.gp
v.ir.ratio.r <- v.ir.hcw.r/v.ir.gp.r
v.ir.fac <- v.ir.ratio.c/as.vector(v.ir.ratio.r)
m.uc.mwage <- rbind(v.uc.mwage,v.uc.mwage,v.uc.mwage)

v.inf.p <- v.inf.tot2/v.tot.hcw
v.death.hcw <- colSums(m.death)
v.death.p <- v.death.hcw/v.tot.hcw

v.cperlife.u5mr <- v.gdp.2020 * ((1-exp(-0.03*v.le.u5mr))/0.03)
v.cperlife.mmr <- v.gdp.2020 * ((1-exp(-0.03*v.le.mmr))/0.03)

