## Header ####
## Creator:     German Agustin Fernandez Casals
## Main:        Hyperspectral data analysis
## Version:     2.1
## Creation:    2022-II-25
## Last edited: 2022-IV-12

# Packages
library(readxl)
if(!require("lme4")) install.packages("lme4")

# Data import
data.ag <- readRDS("../Data/Excel\\Data with mean and sd.rds")
colnames(data.ag)[8:306] <- paste("sp",colnames(data.ag)[8:306], sep = "_")


## Analysis

# Shapiro-Wilk
Shap <- vector(mode = "list", length = 303)
Shap.r <- vector(mode = "list", length = 303)

for (i in 8:306){
  Shap[[i-7]] <- shapiro.test(data.ag[[i]][,1])
  Shap.r[[i-7]] <- Shap[[i-7]][["p.value"]]
}

sum(Shap.r<=0.05) #There is values that are not parametric
rm(Shap,Shap.r)

# Linear Mixed-Effects Models

lmer_list <- vector("list", length = 298)

for(i in 8:306){
  fm1 <- lmer(data.ag[[i]][,1] ~ data.ag[[2]] + data.ag[[3]] +
                (1|data.ag[[4]]))
  lmer_list[[i - 7]] <- summary(fm1)
}

lmer_list[1]
