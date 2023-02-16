#########################
#Main analysis.         #
#########################

rm(list = ls())
setwd("/Users/wz192/Library/CloudStorage/GoogleDrive-wz192@georgetown.edu/My Drive/GU/Res/WB/HRH/Dta")

# Load model and plotting functions
source("import-var.R")
source("cal-model.R")
source("sim-model.R")

res <- simu_model(10000)

quantile(res[[1]]$V16, c(0.025, 0.975))

quantile(res[[2]]$V16, c(0.025, 0.975))

quantile(res[[3]]$V16, c(0.025, 0.975))

quantile(res[[4]]$V16, c(0.025, 0.975))

quantile(res[[5]]$V16, c(0.025, 0.975))
