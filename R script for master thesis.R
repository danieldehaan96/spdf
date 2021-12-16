

#R script for Master Thesis Daniel de Haan

###   CODE   ###

####          Libraries used. ####
library(xlsx)
library(tsintermittent)
library(lightgbm)
library(readxl)
library(boot)
library(expm)
library(igraph)
library(matlab)
library(markovchain)
library(Rcpp)
library(TraMineR)
library(dplyr)
library(RSNNS)
library(tidyr)
library(collapse)
library(runner)
library(ggplot2)
library(data.table)
library(greybox)
library(beepr)
library(knitr)

# Set seed for reproducibility.
set.seed(8377)

# Simulating the four simulated data sets.
# SIM1 <- simID(n=6500, obs=60, idi=1, cv2=0.75, level=10)
# SIM2 <- simID(n=6500, obs=60, idi=1.5, cv2=0.8, level=10)
# SIM3 <- simID(n=6500, obs=60, idi=1.05, cv2=0.3, level=10)
# SIM4 <- simID(n=6500, obs=60, idi=1.45, cv2=0.25, level=10)

# Saving them as Excel files for simple calculations.
# write.xlsx(SIM1, "c:/Daan/SIM1.xlsx")
# write.xlsx(SIM2, "c:/Daan/SIM2.xlsx")
# write.xlsx(SIM3, "c:/Daan/SIM3.xlsx")
# write.xlsx(SIM4, "c:/Daan/SIM4.xlsx")

####          Importing the simulated and industrial data sets. ####
SIM1 <- as.data.frame(read_excel("C:/Daan/SIM1.xlsx"))
SIM2 <- as.data.frame(read_excel("C:/Daan/SIM2.xlsx"))
SIM3 <- as.data.frame(read_excel("C:/Daan/SIM3.xlsx"))
SIM4 <- as.data.frame(read_excel("C:/Daan/SIM4.xlsx"))

MAN <- as.data.frame(read_excel("C:/Daan/MAN.xlsx", sheet = "GOEIE"))
BRAF <- as.data.frame(t(read_excel("C:/Daan/BRAF.xls", sheet = "GOEIE")))[-c(1), ]
AUTO <- as.data.frame(t(read_excel("C:/Daan/AUTO.xls", sheet = "GOEIE")))[-c(1), ]
OIL <- as.data.frame(t(read_excel("C:/Daan/OIL.xls", sheet = "GOEIE")))[-c(1), ]

# Extracting price and leadtime vectors.
pricesSIM1 <- SIM1[61,]
leadtimesSIM1 <- 1
SIM1 <- SIM1[-61,]

pricesSIM2 <- SIM2[61,]
leadtimesSIM2 <- 1
SIM2 <- SIM2[-61,]

pricesSIM3 <- SIM3[61,]
leadtimesSIM3 <- 1
SIM3 <- SIM3[-61,]

pricesSIM4 <- SIM4[61,]
leadtimesSIM4 <- 1
SIM4 <- SIM4[-61,]

pricesMAN <- MAN[151,]
leadtimesMAN <- 1
MAN <- MAN[-151,]

pricesBRAF <- BRAF[85,]
leadtimesBRAF <- 1
BRAF <- BRAF[-85,]

pricesAUTO <- AUTO[25,]
leadtimesAUTO <- 1
AUTO <- AUTO[-25,]

pricesOIL <- OIL[57,]
leadtimesOIL <- 1
OIL <- OIL[-57,]

# Coercing all negative values to zeroes.

MAN <- pmax(MAN,0)
BRAF <- pmax(BRAF,0)
AUTO <- pmax(AUTO,0)
OIL <- pmax(OIL,0)

####          Creating the test and train data sets. ####
# As the sample size is the same for all simulated data sets, the sample from SIM1 can just be used for all SIM data sets.
sampleSIM1 = round(nrow(SIM1)*.70) 
trainSIM1 <- SIM1[1:(sampleSIM1), ]
testSIM1 <- SIM1[-(1:(sampleSIM1)), ]

trainSIM2 <- SIM2[1:(sampleSIM1), ]
testSIM2 <- SIM2[-(1:(sampleSIM1)), ]

trainSIM3 <- SIM3[1:(sampleSIM1), ]
testSIM3 <- SIM3[-(1:(sampleSIM1)), ]

trainSIM4 <- SIM4[1:(sampleSIM1), ]
testSIM4 <- SIM4[-(1:(sampleSIM1)), ]

# The split point is established for the industrial data sets individually.
sampleMAN = round(nrow(MAN)*.70)
trainMAN <- MAN[1:(sampleMAN), ]
testMAN <- MAN[-(1:(sampleMAN)), ]

sampleBRAF = round(nrow(BRAF)*.70)
trainBRAF <- BRAF[1:(sampleBRAF), ]
testBRAF <- BRAF[-(1:(sampleBRAF)), ]

sampleAUTO = round(nrow(AUTO)*.70)
trainAUTO <- AUTO[1:(sampleAUTO), ]
testAUTO <- AUTO[-(1:(sampleAUTO)), ]

sampleOIL = round(nrow(OIL)*.70)
trainOIL <- OIL[1:(sampleOIL), ]
testOIL <- OIL[-(1:(sampleOIL)), ]

# Dropping the items which do not have more than one demand occurence for the industrial data sets.


cond1 <- colSums(trainMAN != 0, na.rm=T)<2
mask1 <- !(cond1)
trainMAN <- subset(trainMAN, select=mask1)
testMAN <- subset(testMAN,select=names(trainMAN))


cond2 <- colSums(trainBRAF != 0, na.rm=T)<2
mask2 <- !(cond2)
trainBRAF <- subset(trainBRAF, select=mask2)
testBRAF <- subset(testBRAF,select=names(trainBRAF))

cond3 <- colSums(trainAUTO != 0, na.rm=T)<2
mask3 <- !(cond3)
trainAUTO <- subset(trainAUTO, select=mask3)
testAUTO <- subset(testAUTO,select=names(trainAUTO))

cond4 <- colSums(trainOIL != 0, na.rm=T)<2
mask4 <- !(cond4)
trainOIL <- subset(trainOIL, select=mask4)
testOIL <- subset(testOIL,select=names(trainOIL))

# The forecast horizon is set to the amount of periods in the test sample.

####          Croston on all data sets. ####
ptm <- proc.time()

####          Croston on SIM1 data ####

h = 18
CrostonSIM1 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM1)){
  x <- trainSIM1[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out) 
    x <- rbind(x,testSIM1[j,i])
  }  
  CrostonSIM1 <- rbind(CrostonSIM1, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonSIM1 <- t(CrostonSIM1)
CrostonSIM1 <- CrostonSIM1[,-1]

#Saving the output data
save(CrostonSIM1,file="c:/Daan/CrostonSIM12.Rda")
#Reload the output data
#load("c:/Daan/CrostonSIM12.Rda")

# Forecasting accuracy measures.
CrostonMSESIM1 <- NULL
CrostonMASESIM1 <- NULL
CrostonRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  CrostonMSESIM1 <- cbind(CrostonMSESIM1,  MSE(t(testSIM1[i]),t(CrostonSIM1[,i])))
  CrostonMASESIM1 <- cbind(CrostonMASESIM1,  MASE(t(testSIM1[i]),t(CrostonSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  CrostonRMSSESIM1 <- cbind(CrostonRMSSESIM1,  RMSSE(t(testSIM1[i]),t(CrostonSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
CrostonMSESIM1 <- mean(CrostonMSESIM1)
CrostonMASESIM1 <- mean(CrostonMASESIM1)
CrostonRMSSESIM1 <- mean(CrostonRMSSESIM1)
print(c("SIM1", CrostonMSESIM1, CrostonMASESIM1, CrostonRMSSESIM1))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(CrostonSIM1)){
  leadtimedemand <- mean(t(CrostonSIM1[,i]))
  stdev <- std(t(trainSIM1[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonSIM1 <- rbind(fillrateCrostonSIM1, averagefillrateitem)
  }
CrostonholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualCrostonholdingSIM1 <- CrostonholdingSIM1/CrostonholdingSIM1[1]
achievedfillrateCrostonSIM1 <- colMeans(fillrateCrostonSIM1[-1,])
ServicelevelCrostonSIM1 <- data.frame(achievedfillrate = achievedfillrateCrostonSIM1, holding = CrostonholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")




####          Croston on SIM2 data ####

h = 18
CrostonSIM2 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM2)){
  x <- trainSIM2[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM2[j,i])
  }  
  CrostonSIM2 <- rbind(CrostonSIM2, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonSIM2 <- t(CrostonSIM2)
CrostonSIM2 <- CrostonSIM2[,-1]

#Saving the output data
save(CrostonSIM2,file="c:/Daan/CrostonSIM22.Rda")
#Reload the output data
#load("c:/Daan/CrostonSIM2.Rda")

# Forecasting accuracy measures.
CrostonMSESIM2 <- NULL
CrostonMASESIM2 <- NULL
CrostonRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  CrostonMSESIM2 <- cbind(CrostonMSESIM2,  MSE(t(testSIM2[i]),t(CrostonSIM2[,i])))
  CrostonMASESIM2 <- cbind(CrostonMASESIM2,  MASE(t(testSIM2[i]),t(CrostonSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  CrostonRMSSESIM2 <- cbind(CrostonRMSSESIM2,  RMSSE(t(testSIM2[i]),t(CrostonSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
CrostonMSESIM2 <- mean(CrostonMSESIM2)
CrostonMASESIM2 <- mean(CrostonMASESIM2)
CrostonRMSSESIM2 <- mean(CrostonRMSSESIM2)
print(c("SIM2", CrostonMSESIM2, CrostonMASESIM2, CrostonRMSSESIM2))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(CrostonSIM2)){
  leadtimedemand <- mean(t(CrostonSIM2[,i]))
  stdev <- std(t(trainSIM2[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonSIM2 <- rbind(fillrateCrostonSIM2, averagefillrateitem)
}
CrostonholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualCrostonholdingSIM2 <- CrostonholdingSIM2/CrostonholdingSIM2[1]
achievedfillrateCrostonSIM2 <- colMeans(fillrateCrostonSIM2[-1,])
ServicelevelCrostonSIM2 <- data.frame(achievedfillrate = achievedfillrateCrostonSIM2, holding = CrostonholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")


####          Croston on SIM3 data ####
h = 18
CrostonSIM3 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM3)){
  x <- trainSIM3[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM3[j,i])
  }  
  CrostonSIM3 <- rbind(CrostonSIM3, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonSIM3 <- t(CrostonSIM3)
CrostonSIM3 <- CrostonSIM3[,-1]

#Saving the output data
save(CrostonSIM3,file="c:/Daan/CrostonSIM32.Rda")
#Reload the output data
#load("c:/Daan/CrostonSIM3.Rda")


# Forecasting accuracy measures.
CrostonMSESIM3 <- NULL
CrostonMASESIM3 <- NULL
CrostonRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  CrostonMSESIM3 <- cbind(CrostonMSESIM3,  MSE(t(testSIM3[i]),t(CrostonSIM3[,i])))
  CrostonMASESIM3 <- cbind(CrostonMASESIM3,  MASE(t(testSIM3[i]),t(CrostonSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  CrostonRMSSESIM3 <- cbind(CrostonRMSSESIM3,  RMSSE(t(testSIM3[i]),t(CrostonSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
CrostonMSESIM3 <- mean(CrostonMSESIM3)
CrostonMASESIM3 <- mean(CrostonMASESIM3)
CrostonRMSSESIM3 <- mean(CrostonRMSSESIM3)
print(c("SIM3", CrostonMSESIM3, CrostonMASESIM3, CrostonRMSSESIM3))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(CrostonSIM3)){
  leadtimedemand <- mean(t(CrostonSIM3[,i]))
  stdev <- std(t(trainSIM3[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonSIM3 <- rbind(fillrateCrostonSIM3, averagefillrateitem)
}
CrostonholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualCrostonholdingSIM3 <- CrostonholdingSIM3/CrostonholdingSIM3[1]
achievedfillrateCrostonSIM3 <- colMeans(fillrateCrostonSIM3[-1,])
ServicelevelCrostonSIM3 <- data.frame(achievedfillrate = achievedfillrateCrostonSIM3, holding = CrostonholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")


####          Croston on SIM4 data ####

h = 18
CrostonSIM4 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM4)){
x <- trainSIM4[i]
prediction <- NULL
for (j in 1:h){
  prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
  x <- rbind(x,testSIM4[j,i])
}  
CrostonSIM4 <- rbind(CrostonSIM4, t(prediction))
}


# Saving the predictions without transposed and without column 1.
CrostonSIM4 <- t(CrostonSIM4)
CrostonSIM4 <- CrostonSIM4[,-1]

#Saving the output data
save(CrostonSIM4,file="c:/Daan/CrostonSIM42.Rda")
#Reload the output data
#load("c:/Daan/CrostonSIM4.Rda")

# Forecasting accuracy measures.
CrostonMSESIM4 <- NULL
CrostonMASESIM4 <- NULL
CrostonRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
CrostonMSESIM4 <- cbind(CrostonMSESIM4,  MSE(t(testSIM4[i]),t(CrostonSIM4[,i])))
CrostonMASESIM4 <- cbind(CrostonMASESIM4,  MASE(t(testSIM4[i]),t(CrostonSIM4[,i]), mean(abs(t(trainSIM4[i])))))
CrostonRMSSESIM4 <- cbind(CrostonRMSSESIM4,  RMSSE(t(testSIM4[i]),t(CrostonSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
CrostonMSESIM4 <- mean(CrostonMSESIM4)
CrostonMASESIM4 <- mean(CrostonMASESIM4)
CrostonRMSSESIM4 <- mean(CrostonRMSSESIM4)
print(c("SIM4", CrostonMSESIM4, CrostonMASESIM4, CrostonRMSSESIM4))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(CrostonSIM4)){
  leadtimedemand <- mean(t(CrostonSIM4[,i]))
  stdev <- std(t(trainSIM4[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonSIM4 <- rbind(fillrateCrostonSIM4, averagefillrateitem)
}
CrostonholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualCrostonholdingSIM4 <- CrostonholdingSIM4/CrostonholdingSIM4[1]
achievedfillrateCrostonSIM4 <- colMeans(fillrateCrostonSIM4[-1,])
ServicelevelCrostonSIM4 <- data.frame(achievedfillrate = achievedfillrateCrostonSIM4, holding = CrostonholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")


####          Croston on MAN data ####

h = nrow(testMAN)
CrostonMAN <- matrix(ncol = h)
for (i in 1:ncol(trainMAN)){
  x <- trainMAN[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testMAN[j,i])
  }  
  CrostonMAN <- rbind(CrostonMAN, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonMAN <- t(CrostonMAN)
CrostonMAN <- CrostonMAN[,-1]

#Saving the output data
save(CrostonMAN,file="c:/Daan/CrostonMAN2.Rda")
#Reload the output data
#load("c:/Daan/CrostonMAN.Rda")

# Forecasting accuracy measures.
CrostonMSEMAN <- NULL
CrostonMASEMAN <- NULL
CrostonRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  CrostonMSEMAN <- cbind(CrostonMSEMAN,  MSE(t(testMAN[i]),t(CrostonMAN[,i])))
  CrostonMASEMAN <- cbind(CrostonMASEMAN,  MASE(t(testMAN[i]),t(CrostonMAN[,i]), mean(abs(t(trainMAN[i])))))
  CrostonRMSSEMAN <- cbind(CrostonRMSSEMAN,  RMSSE(t(testMAN[i]),t(CrostonMAN[,i]), mean(abs(t(trainMAN[i])))))
}
CrostonMSEMAN <- mean(CrostonMSEMAN)
CrostonMASEMAN <- mean(CrostonMASEMAN)
CrostonRMSSEMAN <- mean(CrostonRMSSEMAN)
print(c("MAN", CrostonMSEMAN, CrostonMASEMAN, CrostonRMSSEMAN))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(CrostonMAN)){
  leadtimedemand <- mean(t(CrostonMAN[,i]))
  stdev <- std(t(trainMAN[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonMAN <- rbind(fillrateCrostonMAN, averagefillrateitem)
}
CrostonholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualCrostonholdingMAN <- CrostonholdingMAN/CrostonholdingMAN[1]
achievedfillrateCrostonMAN <- colMeans(fillrateCrostonMAN[-1,])
ServicelevelCrostonMAN <- data.frame(achievedfillrate = achievedfillrateCrostonMAN, holding = CrostonholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")

####          Croston on BRAF data ####

h = nrow(testBRAF)
CrostonBRAF <- matrix(ncol = h)
for (i in 1:ncol(trainBRAF)){
  x <- trainBRAF[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testBRAF[j,i])
  }  
  CrostonBRAF <- rbind(CrostonBRAF, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonBRAF <- t(CrostonBRAF)
CrostonBRAF <- CrostonBRAF[,-1]

#Saving the output data
save(CrostonBRAF,file="c:/Daan/CrostonBRAF2.Rda")
#Reload the output data
#load("c:/Daan/CrostonBRAF.Rda")

# Forecasting accuracy measures.
CrostonMSEBRAF <- NULL
CrostonMASEBRAF <- NULL
CrostonRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  CrostonMSEBRAF <- cbind(CrostonMSEBRAF,  MSE(t(testBRAF[i]),t(CrostonBRAF[,i])))
  CrostonMASEBRAF <- cbind(CrostonMASEBRAF,  MASE(t(testBRAF[i]),t(CrostonBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  CrostonRMSSEBRAF <- cbind(CrostonRMSSEBRAF,  RMSSE(t(testBRAF[i]),t(CrostonBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
CrostonMSEBRAF <- mean(CrostonMSEBRAF)
CrostonMASEBRAF <- mean(CrostonMASEBRAF)
CrostonRMSSEBRAF <- mean(CrostonRMSSEBRAF)
print(c("BRAF", CrostonMSEBRAF, CrostonMASEBRAF, CrostonRMSSEBRAF))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(CrostonBRAF)){
  leadtimedemand <- mean(t(CrostonBRAF[,i]))
  stdev <- std(t(trainBRAF[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonBRAF <- rbind(fillrateCrostonBRAF, averagefillrateitem)
}
CrostonholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualCrostonholdingBRAF <- CrostonholdingBRAF/CrostonholdingBRAF[1]
achievedfillrateCrostonBRAF <- colMeans(fillrateCrostonBRAF[-1,])
ServicelevelCrostonBRAF <- data.frame(achievedfillrate = achievedfillrateCrostonBRAF, holding = CrostonholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")


####          Croston on AUTO data ####
h = nrow(testAUTO)
CrostonAUTO <- matrix(ncol = h)
for (i in 1:ncol(trainAUTO)){
  x <- trainAUTO[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testAUTO[j,i])
  }  
  CrostonAUTO <- rbind(CrostonAUTO, t(prediction))
}

# Saving the predictions without transposed and without column 1.
CrostonAUTO <- t(CrostonAUTO)
CrostonAUTO <- CrostonAUTO[,-1]

#Saving the output data
save(CrostonAUTO,file="c:/Daan/CrostonAUTO2.Rda")
#Reload the output data
#load("c:/Daan/CrostonAUTO.Rda")

# Forecasting accuracy measures.
CrostonMSEAUTO <- NULL
CrostonMASEAUTO <- NULL
CrostonRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  CrostonMSEAUTO <- cbind(CrostonMSEAUTO,  MSE(t(testAUTO[i]),t(CrostonAUTO[,i])))
  CrostonMASEAUTO <- cbind(CrostonMASEAUTO,  MASE(t(testAUTO[i]),t(CrostonAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  CrostonRMSSEAUTO <- cbind(CrostonRMSSEAUTO,  RMSSE(t(testAUTO[i]),t(CrostonAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
CrostonMSEAUTO <- mean(CrostonMSEAUTO)
CrostonMASEAUTO <- mean(CrostonMASEAUTO)
CrostonRMSSEAUTO <- mean(CrostonRMSSEAUTO)
print(c("AUTO", CrostonMSEAUTO, CrostonMASEAUTO, CrostonRMSSEAUTO))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(CrostonAUTO)){
  leadtimedemand <- mean(t(CrostonAUTO[,i]))
  stdev <- std(t(trainAUTO[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonAUTO <- rbind(fillrateCrostonAUTO, averagefillrateitem)
}
CrostonholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualCrostonholdingAUTO <- CrostonholdingAUTO/CrostonholdingAUTO[1]
achievedfillrateCrostonAUTO <- colMeans(fillrateCrostonAUTO[-1,])
ServicelevelCrostonAUTO <- data.frame(achievedfillrate = achievedfillrateCrostonAUTO, holding = CrostonholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")



####          Croston on OIL data ####
h = nrow(testOIL)
CrostonOIL <- matrix(ncol = h)
for (i in 1:ncol(trainOIL)){
  x <- trainOIL[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x,h=1, w=NULL,nop=2, type="croston",cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testOIL[j,i])
  }  
  CrostonOIL <- rbind(CrostonOIL, t(prediction))
}


# Saving the predictions without transposed and without column 1.
CrostonOIL <- t(CrostonOIL)
CrostonOIL <- CrostonOIL[,-1]

#Saving the output data
save(CrostonOIL,file="c:/Daan/CrostonOIL2.Rda")
#Reload the output data
#load("c:/Daan/CrostonOIL.Rda")

# Forecasting accuracy measures.
CrostonMSEOIL <- NULL
CrostonMASEOIL <- NULL
CrostonRMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  CrostonMSEOIL <- cbind(CrostonMSEOIL,  MSE(t(testOIL[i]),t(CrostonOIL[,i])))
  CrostonMASEOIL <- cbind(CrostonMASEOIL,  MASE(t(testOIL[i]),t(CrostonOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
  CrostonRMSSEOIL <- cbind(CrostonRMSSEOIL,  RMSSE(t(testOIL[i]),t(CrostonOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
}
CrostonMSEOIL <- mean(CrostonMSEOIL,na.rm=TRUE)
CrostonMASEOIL <- mean(CrostonMASEOIL,na.rm=TRUE)
CrostonRMSSEOIL <- mean(CrostonRMSSEOIL,na.rm=TRUE)
print(c("OIL", CrostonMSEOIL, CrostonMASEOIL, CrostonRMSSEOIL))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateCrostonOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(CrostonOIL)){
  leadtimedemand <- mean(t(CrostonOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateCrostonOIL <- rbind(fillrateCrostonOIL, averagefillrateitem)
}
CrostonholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualCrostonholdingOIL <- CrostonholdingOIL/CrostonholdingOIL[1]
achievedfillrateCrostonOIL <- colMeans(fillrateCrostonOIL[-1,])
ServicelevelCrostonOIL <- data.frame(achievedfillrate = achievedfillrateCrostonOIL, holding = CrostonholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Croston")

print("time Croston total")
proc.time() - ptm


####          Simple Exponential Smoothing on all data sets. ####
ptm <- proc.time()

####          SES on SIM1 data ####
h = 18
sesSIM1 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM1)){
  x <- trainSIM1[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM1[j,i])
  }  
  sesSIM1 <- rbind(sesSIM1, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sesSIM1 <- t(sesSIM1)
sesSIM1 <- sesSIM1[,-1]

#Saving the output data
save(sesSIM1,file="c:/Daan/sesSIM12.Rda")
#Reload the output data
#load("c:/Daan/sesSIM1.Rda")

# Forecasting accuracy measures.
SESMSESIM1 <- NULL
SESMASESIM1 <- NULL
SESRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  SESMSESIM1 <- cbind(SESMSESIM1,  MSE(t(testSIM1[i]),t(sesSIM1[,i])))
  SESMASESIM1 <- cbind(SESMASESIM1,  MASE(t(testSIM1[i]),t(sesSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  SESRMSSESIM1 <- cbind(SESRMSSESIM1,  RMSSE(t(testSIM1[i]),t(sesSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
SESMSESIM1 <- mean(SESMSESIM1)
SESMASESIM1 <- mean(SESMASESIM1)
SESRMSSESIM1 <- mean(SESRMSSESIM1)
print(c("SIM1", SESMSESIM1, SESMASESIM1, SESRMSSESIM1))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(sesSIM1)){
  leadtimedemand <- mean(t(sesSIM1[,i]))
  stdev <- std(t(trainSIM1[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESSIM1 <- rbind(fillrateSESSIM1, averagefillrateitem)
}
SESholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualSESholdingSIM1 <- SESholdingSIM1/SESholdingSIM1[1]
achievedfillrateSESSIM1 <- colMeans(fillrateSESSIM1[-1,])
ServicelevelSESSIM1 <- data.frame(achievedfillrate = achievedfillrateSESSIM1, holding = SESholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



####          SES on SIM2 data ####
h = 18
sesSIM2 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM2)){
  x <- trainSIM2[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM2[j,i])
  }  
  sesSIM2 <- rbind(sesSIM2, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sesSIM2 <- t(sesSIM2)
sesSIM2 <- sesSIM2[,-1]

#Saving the output data
save(sesSIM2,file="c:/Daan/sesSIM22.Rda")
#Reload the output data
#load("c:/Daan/sesSIM2.Rda")

# Forecasting accuracy measures.
SESMSESIM2 <- NULL
SESMASESIM2 <- NULL
SESRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  SESMSESIM2 <- cbind(SESMSESIM2,  MSE(t(testSIM2[i]),t(sesSIM2[,i])))
  SESMASESIM2 <- cbind(SESMASESIM2,  MASE(t(testSIM2[i]),t(sesSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  SESRMSSESIM2 <- cbind(SESRMSSESIM2,  RMSSE(t(testSIM2[i]),t(sesSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
SESMSESIM2 <- mean(SESMSESIM2)
SESMASESIM2 <- mean(SESMASESIM2)
SESRMSSESIM2 <- mean(SESRMSSESIM2)
print(c("SIM2", SESMSESIM2, SESMASESIM2, SESRMSSESIM2))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(sesSIM2)){
  leadtimedemand <- mean(t(sesSIM2[,i]))
  stdev <- std(t(trainSIM2[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESSIM2 <- rbind(fillrateSESSIM2, averagefillrateitem)
}
SESholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualSESholdingSIM2 <- SESholdingSIM2/SESholdingSIM2[1]
achievedfillrateSESSIM2 <- colMeans(fillrateSESSIM2[-1,])
ServicelevelSESSIM2 <- data.frame(achievedfillrate = achievedfillrateSESSIM2, holding = SESholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



####          SES on SIM3 data ####
h = 18
sesSIM3 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM3)){
  x <- trainSIM3[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM3[j,i])
  }  
  sesSIM3 <- rbind(sesSIM3, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sesSIM3 <- t(sesSIM3)
sesSIM3 <- sesSIM3[,-1]

#Saving the output data
save(sesSIM3,file="c:/Daan/sesSIM32.Rda")
#Reload the output data
#load("c:/Daan/sesSIM3.Rda")

# Forecasting accuracy measures.
SESMSESIM3 <- NULL
SESMASESIM3 <- NULL
SESRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  SESMSESIM3 <- cbind(SESMSESIM3,  MSE(t(testSIM3[i]),t(sesSIM3[,i])))
  SESMASESIM3 <- cbind(SESMASESIM3,  MASE(t(testSIM3[i]),t(sesSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  SESRMSSESIM3 <- cbind(SESRMSSESIM3,  RMSSE(t(testSIM3[i]),t(sesSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
SESMSESIM3 <- mean(SESMSESIM3)
SESMASESIM3 <- mean(SESMASESIM3)
SESRMSSESIM3 <- mean(SESRMSSESIM3)
print(c("SIM3", SESMSESIM3, SESMASESIM3, SESRMSSESIM3))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(sesSIM3)){
  leadtimedemand <- mean(t(sesSIM3[,i]))
  stdev <- std(t(trainSIM3[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESSIM3 <- rbind(fillrateSESSIM3, averagefillrateitem)
}
SESholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualSESholdingSIM3 <- SESholdingSIM3/SESholdingSIM3[1]
achievedfillrateSESSIM3 <- colMeans(fillrateSESSIM3[-1,])
ServicelevelSESSIM3 <- data.frame(achievedfillrate = achievedfillrateSESSIM3, holding = SESholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



####          SES on SIM4 data ####

h = nrow(testSIM4)
sesSIM4 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM4)){
  x <- trainSIM4[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM4[j,i])
  }  
  sesSIM4 <- rbind(sesSIM4, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sesSIM4 <- t(sesSIM4)
sesSIM4 <- sesSIM4[,-1]

#Saving the output data
save(sesSIM4,file="c:/Daan/sesSIM42.Rda")
#Reload the output data
#load("c:/Daan/sesSIM4.Rda")

# Forecasting accuracy measures.
SESMSESIM4 <- NULL
SESMASESIM4 <- NULL
SESRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  SESMSESIM4 <- cbind(SESMSESIM4,  MSE(t(testSIM4[i]),t(sesSIM4[,i])))
  SESMASESIM4 <- cbind(SESMASESIM4,  MASE(t(testSIM4[i]),t(sesSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  SESRMSSESIM4 <- cbind(SESRMSSESIM4,  RMSSE(t(testSIM4[i]),t(sesSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
SESMSESIM4 <- mean(SESMSESIM4)
SESMASESIM4 <- mean(SESMASESIM4)
SESRMSSESIM4 <- mean(SESRMSSESIM4)
print(c("SIM4", SESMSESIM4, SESMASESIM4, SESRMSSESIM4))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(sesSIM4)){
  leadtimedemand <- mean(t(sesSIM4[,i]))
  stdev <- std(t(trainSIM4[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESSIM4 <- rbind(fillrateSESSIM4, averagefillrateitem)
}
SESholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualSESholdingSIM4 <- SESholdingSIM4/SESholdingSIM4[1]
achievedfillrateSESSIM4 <- colMeans(fillrateSESSIM4[-1,])
ServicelevelSESSIM4 <- data.frame(achievedfillrate = achievedfillrateSESSIM4, holding = SESholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")




####          SES on MAN data ####
h = nrow(testMAN)
sesMAN <- matrix(ncol = h)
for (i in 1:ncol(trainMAN)){
  x <- trainMAN[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testMAN[j,i])
  }  
  sesMAN <- rbind(sesMAN, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sesMAN <- t(sesMAN)
sesMAN <- sesMAN[,-1]

#Saving the output data
save(sesMAN,file="c:/Daan/sesMAN2.Rda")
#Reload the output data
#load("c:/Daan/sesMAN.Rda")

# Forecasting accuracy measures.
SESMSEMAN <- NULL
SESMASEMAN <- NULL
SESRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  SESMSEMAN <- cbind(SESMSEMAN,  MSE(t(testMAN[i]),t(sesMAN[,i])))
  SESMASEMAN <- cbind(SESMASEMAN,  MASE(t(testMAN[i]),t(sesMAN[,i]), mean(abs(t(trainMAN[i])))))
  SESRMSSEMAN <- cbind(SESRMSSEMAN,  RMSSE(t(testMAN[i]),t(sesMAN[,i]), mean(abs(t(trainMAN[i])))))
}
SESMSEMAN <- mean(SESMSEMAN)
SESMASEMAN <- mean(SESMASEMAN)
SESRMSSEMAN <- mean(SESRMSSEMAN)
print(c("MAN", SESMSEMAN, SESMASEMAN, SESRMSSEMAN))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(sesMAN)){
  leadtimedemand <- mean(t(sesMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESMAN <- rbind(fillrateSESMAN, averagefillrateitem)
}
SESholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualSESholdingMAN <- SESholdingMAN/SESholdingMAN[1]
achievedfillrateSESMAN <- colMeans(fillrateSESMAN[-1,])
ServicelevelSESMAN <- data.frame(achievedfillrate = achievedfillrateSESMAN, holding = SESholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



####          SES on BRAF data ####
h = nrow(testBRAF)
sesBRAF <- matrix(ncol = h)
for (i in 1:ncol(trainBRAF)){
  x <- trainBRAF[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testBRAF[j,i])
  }  
  sesBRAF <- rbind(sesBRAF, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sesBRAF <- t(sesBRAF)
sesBRAF <- sesBRAF[,-1]

#Saving the output data
save(sesBRAF,file="c:/Daan/sesBRAF2.Rda")
#Reload the output data
#load("c:/Daan/sesBRAF.Rda")

# Forecasting accuracy measures.
SESMSEBRAF <- NULL
SESMASEBRAF <- NULL
SESRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  SESMSEBRAF <- cbind(SESMSEBRAF,  MSE(t(testBRAF[i]),t(sesBRAF[,i])))
  SESMASEBRAF <- cbind(SESMASEBRAF,  MASE(t(testBRAF[i]),t(sesBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  SESRMSSEBRAF <- cbind(SESRMSSEBRAF,  RMSSE(t(testBRAF[i]),t(sesBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
SESMSEBRAF <- mean(SESMSEBRAF)
SESMASEBRAF <- mean(SESMASEBRAF)
SESRMSSEBRAF <- mean(SESRMSSEBRAF)
print(c("BRAF", SESMSEBRAF, SESMASEBRAF, SESRMSSEBRAF))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(sesBRAF)){
  leadtimedemand <- mean(t(sesBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESBRAF <- rbind(fillrateSESBRAF, averagefillrateitem)
}
SESholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualSESholdingBRAF <- SESholdingBRAF/SESholdingBRAF[1]
achievedfillrateSESBRAF <- colMeans(fillrateSESBRAF[-1,])
ServicelevelSESBRAF <- data.frame(achievedfillrate = achievedfillrateSESBRAF, holding = SESholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



####          SES on AUTO data ####
h = nrow(testAUTO)
sesAUTO <- matrix(ncol = h)
for (i in 1:ncol(trainAUTO)){
  x <- trainAUTO[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testAUTO[j,i])
  }  
  sesAUTO <- rbind(sesAUTO, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sesAUTO <- t(sesAUTO)
sesAUTO <- sesAUTO[,-1]

#Saving the output data
save(sesAUTO,file="c:/Daan/sesAUTO2.Rda")
#Reload the output data
#load("c:/Daan/sesAUTO.Rda")

# Forecasting accuracy measures.
SESMSEAUTO <- NULL
SESMASEAUTO <- NULL
SESRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  SESMSEAUTO <- cbind(SESMSEAUTO,  MSE(t(testAUTO[i]),t(sesAUTO[,i])))
  SESMASEAUTO <- cbind(SESMASEAUTO,  MASE(t(testAUTO[i]),t(sesAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  SESRMSSEAUTO <- cbind(SESRMSSEAUTO,  RMSSE(t(testAUTO[i]),t(sesAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
SESMSEAUTO <- mean(SESMSEAUTO)
SESMASEAUTO <- mean(SESMASEAUTO)
SESRMSSEAUTO <- mean(SESRMSSEAUTO)
print(c("AUTO", SESMSEAUTO, SESMASEAUTO, SESRMSSEAUTO))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(sesAUTO)){
  leadtimedemand <- mean(t(sesAUTO[,i]))
  stdev <- std(t(trainAUTO[i]))
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller[fillratefiller >= 1] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESAUTO <- rbind(fillrateSESAUTO, averagefillrateitem)
}
SESholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualSESholdingAUTO <- SESholdingAUTO/SESholdingAUTO[1]
achievedfillrateSESAUTO <- colMeans(fillrateSESAUTO[-1,])
ServicelevelSESAUTO <- data.frame(achievedfillrate = achievedfillrateSESAUTO, holding = SESholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")




####          SES on OIL data ####
h = nrow(testOIL)
sesOIL <- matrix(ncol = h)
for (i in 1:ncol(trainOIL)){
  x <- trainOIL[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,sexsm(x, h=1, w=NULL,cost="mar",init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testOIL[j,i])
  }  
  sesOIL <- rbind(sesOIL, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sesOIL <- t(sesOIL)
sesOIL <- sesOIL[,-1]

#Saving the output data
save(sesOIL,file="c:/Daan/sesOIL2.Rda")
#Reload the output data
#load("c:/Daan/sesOIL.Rda")

# Forecasting accuracy measures.
SESMSEOIL <- NULL
SESMASEOIL <- NULL
SESRMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  SESMSEOIL <- cbind(SESMSEOIL,  MSE(t(testOIL[i]),t(sesOIL[,i])))
  SESMASEOIL <- cbind(SESMASEOIL,  MASE(t(testOIL[i]),t(sesOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
  SESRMSSEOIL <- cbind(SESRMSSEOIL,  RMSSE(t(testOIL[i]),t(sesOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
}
SESMSEOIL <- mean(SESMSEOIL,na.rm=TRUE)
SESMASEOIL <- mean(SESMASEOIL,na.rm=TRUE)
SESRMSSEOIL <- mean(SESRMSSEOIL,na.rm=TRUE)
print(c("OIL", SESMSEOIL, SESMASEOIL, SESRMSSEOIL))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSESOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(sesOIL)){
  leadtimedemand <- mean(t(sesOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSESOIL <- rbind(fillrateSESOIL, averagefillrateitem)
}
SESholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualSESholdingOIL <- SESholdingOIL/SESholdingOIL[1]
achievedfillrateSESOIL <- colMeans(fillrateSESOIL[-1,])
ServicelevelSESOIL <- data.frame(achievedfillrate = achievedfillrateSESOIL, holding = SESholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SES")



print("time SES total")
proc.time() - ptm


####          SBA on all data sets. ####
ptm <- proc.time()

####          SBA on SIM1 data. ####
h = nrow(testSIM1)
sbaSIM1 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM1)){
  x <- trainSIM1[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM1[j,i])
  }  
  sbaSIM1 <- rbind(sbaSIM1, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sbaSIM1 <- t(sbaSIM1)
sbaSIM1 <- sbaSIM1[,-1]

#Saving the output data
save(sbaSIM1,file="c:/Daan/sbaSIM12.Rda")
#Reload the output data
#load("c:/Daan/sbaSIM1.Rda")

# Forecasting accuracy measures.
SBAMSESIM1 <- NULL
SBAMASESIM1 <- NULL
SBARMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  SBAMSESIM1 <- cbind(SBAMSESIM1,  MSE(t(testSIM1[i]),t(sbaSIM1[,i])))
  SBAMASESIM1 <- cbind(SBAMASESIM1,  MASE(t(testSIM1[i]),t(sbaSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  SBARMSSESIM1 <- cbind(SBARMSSESIM1,  RMSSE(t(testSIM1[i]),t(sbaSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
SBAMSESIM1 <- mean(SBAMSESIM1)
SBAMASESIM1 <- mean(SBAMASESIM1)
SBARMSSESIM1 <- mean(SBARMSSESIM1)
print(c("SIM1", SBAMSESIM1, SBAMASESIM1, SBARMSSESIM1))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBASIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(sbaSIM1)){
  leadtimedemand <- mean(t(sbaSIM1[,i]))
  stdev <- std(t(trainSIM1[i])[!is.na(t(trainSIM1[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM1[i])[(t(testSIM1[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBASIM1 <- rbind(fillrateSBASIM1, averagefillrateitem)
}
SBAholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualSBAholdingSIM1 <- SBAholdingSIM1/SBAholdingSIM1[1]
achievedfillrateSBASIM1 <- colMeans(fillrateSBASIM1[-1,])
ServicelevelSBASIM1 <- data.frame(achievedfillrate = achievedfillrateSBASIM1, holding = SBAholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")


####          SBA on SIM2 data. ####
h = nrow(testSIM2)
sbaSIM2 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM2)){
  x <- trainSIM2[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM2[j,i])
  }  
  sbaSIM2 <- rbind(sbaSIM2, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sbaSIM2 <- t(sbaSIM2)
sbaSIM2 <- sbaSIM2[,-1]

#Saving the output data
save(sbaSIM2,file="c:/Daan/sbaSIM22.Rda")
#Reload the output data
#load("c:/Daan/sbaSIM2.Rda")


# Forecasting accuracy measures.
SBAMSESIM2 <- NULL
SBAMASESIM2 <- NULL
SBARMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  SBAMSESIM2 <- cbind(SBAMSESIM2,  MSE(t(testSIM2[i]),t(sbaSIM2[,i])))
  SBAMASESIM2 <- cbind(SBAMASESIM2,  MASE(t(testSIM2[i]),t(sbaSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  SBARMSSESIM2 <- cbind(SBARMSSESIM2,  RMSSE(t(testSIM2[i]),t(sbaSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
SBAMSESIM2 <- mean(SBAMSESIM2)
SBAMASESIM2 <- mean(SBAMASESIM2)
SBARMSSESIM2 <- mean(SBARMSSESIM2)
print(c("SIM2", SBAMSESIM2, SBAMASESIM2, SBARMSSESIM2))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBASIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(sbaSIM2)){
  leadtimedemand <- mean(t(sbaSIM2[,i]))
  stdev <- std(t(trainSIM2[i])[!is.na(t(trainSIM2[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBASIM2 <- rbind(fillrateSBASIM2, averagefillrateitem)
}
SBAholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualSBAholdingSIM2 <- SBAholdingSIM2/SBAholdingSIM2[1]
achievedfillrateSBASIM2 <- colMeans(fillrateSBASIM2[-1,])
ServicelevelSBASIM2 <- data.frame(achievedfillrate = achievedfillrateSBASIM2, holding = SBAholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")




####          SBA on SIM3 data. ####
h = nrow(testSIM3)
sbaSIM3 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM3)){
  x <- trainSIM3[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM3[j,i])
  }  
  sbaSIM3 <- rbind(sbaSIM3, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sbaSIM3 <- t(sbaSIM3)
sbaSIM3 <- sbaSIM3[,-1]

#Saving the output data
save(sbaSIM3,file="c:/Daan/sbaSIM32.Rda")
#Reload the output data
#load("c:/Daan/sbaSIM3.Rda")


# Forecasting accuracy measures.
SBAMSESIM3 <- NULL
SBAMASESIM3 <- NULL
SBARMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  SBAMSESIM3 <- cbind(SBAMSESIM3,  MSE(t(testSIM3[i]),t(sbaSIM3[,i])))
  SBAMASESIM3 <- cbind(SBAMASESIM3,  MASE(t(testSIM3[i]),t(sbaSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  SBARMSSESIM3 <- cbind(SBARMSSESIM3,  RMSSE(t(testSIM3[i]),t(sbaSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
SBAMSESIM3 <- mean(SBAMSESIM3)
SBAMASESIM3 <- mean(SBAMASESIM3)
SBARMSSESIM3 <- mean(SBARMSSESIM3)
print(c("SIM3", SBAMSESIM3, SBAMASESIM3, SBARMSSESIM3))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBASIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(sbaSIM3)){
  leadtimedemand <- mean(t(sbaSIM3[,i]))
  stdev <- std(t(trainSIM3[i])[!is.na(t(trainSIM3[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBASIM3 <- rbind(fillrateSBASIM3, averagefillrateitem)
}
SBAholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualSBAholdingSIM3 <- SBAholdingSIM3/SBAholdingSIM3[1]
achievedfillrateSBASIM3 <- colMeans(fillrateSBASIM3[-1,])
ServicelevelSBASIM3 <- data.frame(achievedfillrate = achievedfillrateSBASIM3, holding = SBAholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")





####          SBA on SIM4 data. ####
h = 18
sbaSIM4 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM4)){
  x <- trainSIM4[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM4[j,i])
  }  
  sbaSIM4 <- rbind(sbaSIM4, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sbaSIM4 <- t(sbaSIM4)
sbaSIM4 <- sbaSIM4[,-1]

#Saving the output data
save(sbaSIM4,file="c:/Daan/sbaSIM42.Rda")
#Reload the output data
#load("c:/Daan/sbaSIM4.Rda")


# Forecasting accuracy measures.
SBAMSESIM4 <- NULL
SBAMASESIM4 <- NULL
SBARMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  SBAMSESIM4 <- cbind(SBAMSESIM4,  MSE(t(testSIM4[i]),t(sbaSIM4[,i])))
  SBAMASESIM4 <- cbind(SBAMASESIM4,  MASE(t(testSIM4[i]),t(sbaSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  SBARMSSESIM4 <- cbind(SBARMSSESIM4,  RMSSE(t(testSIM4[i]),t(sbaSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
SBAMSESIM4 <- mean(SBAMSESIM4)
SBAMASESIM4 <- mean(SBAMASESIM4)
SBARMSSESIM4 <- mean(SBARMSSESIM4)
print(c("SIM4", SBAMSESIM4, SBAMASESIM4, SBARMSSESIM4))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBASIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(sbaSIM4)){
  leadtimedemand <- mean(t(sbaSIM4[,i]))
  stdev <- std(t(trainSIM4[i])[!is.na(t(trainSIM4[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBASIM4 <- rbind(fillrateSBASIM4, averagefillrateitem)
}
SBAholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualSBAholdingSIM4 <- SBAholdingSIM4/SBAholdingSIM4[1]
achievedfillrateSBASIM4 <- colMeans(fillrateSBASIM4[-1,])
ServicelevelSBASIM4 <- data.frame(achievedfillrate = achievedfillrateSBASIM4, holding = SBAholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")






####          SBA on MAN data. ####
h = nrow(testMAN)
sbaMAN <- matrix(ncol = h)
for (i in 1:ncol(trainMAN)){
  x <- trainMAN[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testMAN[j,i])
  }  
  sbaMAN <- rbind(sbaMAN, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sbaMAN <- t(sbaMAN)
sbaMAN <- sbaMAN[,-1]

#Saving the output data
save(sbaMAN,file="c:/Daan/sbaMAN2.Rda")
#Reload the output data
#load("c:/Daan/sbaMAN.Rda")


# Forecasting accuracy measures.
SBAMSEMAN <- NULL
SBAMASEMAN <- NULL
SBARMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  SBAMSEMAN <- cbind(SBAMSEMAN,  MSE(t(testMAN[i]),t(sbaMAN[,i])))
  SBAMASEMAN <- cbind(SBAMASEMAN,  MASE(t(testMAN[i]),t(sbaMAN[,i]), mean(abs(t(trainMAN[i])))))
  SBARMSSEMAN <- cbind(SBARMSSEMAN,  RMSSE(t(testMAN[i]),t(sbaMAN[,i]), mean(abs(t(trainMAN[i])))))
}
SBAMSEMAN <- mean(SBAMSEMAN)
SBAMASEMAN <- mean(SBAMASEMAN)
SBARMSSEMAN <- mean(SBARMSSEMAN)
print(c("MAN", SBAMSEMAN, SBAMASEMAN, SBARMSSEMAN))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBAMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(sbaMAN)){
  leadtimedemand <- mean(t(sbaMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBAMAN <- rbind(fillrateSBAMAN, averagefillrateitem)
}
SBAholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualSBAholdingMAN <- SBAholdingMAN/SBAholdingMAN[1]
achievedfillrateSBAMAN <- colMeans(fillrateSBAMAN[-1,])
ServicelevelSBAMAN <- data.frame(achievedfillrate = achievedfillrateSBAMAN, holding = SBAholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")




####          SBA on BRAF data. ####
h = nrow(testBRAF)
sbaBRAF <- matrix(ncol = h)
for (i in 1:ncol(trainBRAF)){
  x <- trainBRAF[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testBRAF[j,i])
  }  
  sbaBRAF <- rbind(sbaBRAF, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sbaBRAF <- t(sbaBRAF)
sbaBRAF <- sbaBRAF[,-1]

#Saving the output data
save(sbaBRAF,file="c:/Daan/sbaBRAF2.Rda")
#Reload the output data
#load("c:/Daan/sbaBRAF.Rda")


# Forecasting accuracy measures.
SBAMSEBRAF <- NULL
SBAMASEBRAF <- NULL
SBARMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  SBAMSEBRAF <- cbind(SBAMSEBRAF,  MSE(t(testBRAF[i]),t(sbaBRAF[,i])))
  SBAMASEBRAF <- cbind(SBAMASEBRAF,  MASE(t(testBRAF[i]),t(sbaBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  SBARMSSEBRAF <- cbind(SBARMSSEBRAF,  RMSSE(t(testBRAF[i]),t(sbaBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
SBAMSEBRAF <- mean(SBAMSEBRAF)
SBAMASEBRAF <- mean(SBAMASEBRAF)
SBARMSSEBRAF <- mean(SBARMSSEBRAF)
print(c("BRAF", SBAMSEBRAF, SBAMASEBRAF, SBARMSSEBRAF))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBABRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(sbaBRAF)){
  leadtimedemand <- mean(t(sbaBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBABRAF <- rbind(fillrateSBABRAF, averagefillrateitem)
}
SBAholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualSBAholdingBRAF <- SBAholdingBRAF/SBAholdingBRAF[1]
achievedfillrateSBABRAF <- colMeans(fillrateSBABRAF[-1,])
ServicelevelSBABRAF <- data.frame(achievedfillrate = achievedfillrateSBABRAF, holding = SBAholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")





####          SBA on AUTO data. ####
h = nrow(testAUTO)
sbaAUTO <- matrix(ncol = h)
for (i in 1:ncol(trainAUTO)){
  x <- trainAUTO[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testAUTO[j,i])
  }  
  sbaAUTO <- rbind(sbaAUTO, t(prediction))
}

# Saving the predictions without transposed and without column 1.
sbaAUTO <- t(sbaAUTO)
sbaAUTO <- sbaAUTO[,-1]

#Saving the output data
save(sbaAUTO,file="c:/Daan/sbaAUTO2.Rda")
#Reload the output data
#load("c:/Daan/sbaAUTO.Rda")


# Forecasting accuracy measures.
SBAMSEAUTO <- NULL
SBAMASEAUTO <- NULL
SBARMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  SBAMSEAUTO <- cbind(SBAMSEAUTO,  MSE(t(testAUTO[i]),t(sbaAUTO[,i])))
  SBAMASEAUTO <- cbind(SBAMASEAUTO,  MASE(t(testAUTO[i]),t(sbaAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  SBARMSSEAUTO <- cbind(SBARMSSEAUTO,  RMSSE(t(testAUTO[i]),t(sbaAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
SBAMSEAUTO <- mean(SBAMSEAUTO)
SBAMASEAUTO <- mean(SBAMASEAUTO)
SBARMSSEAUTO <- mean(SBARMSSEAUTO)
print(c("AUTO", SBAMSEAUTO, SBAMASEAUTO, SBARMSSEAUTO))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBAAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(sbaAUTO)){
  leadtimedemand <- mean(t(sbaAUTO[,i]))
  stdev <- std(t(trainAUTO[i])[!is.na(t(trainAUTO[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBAAUTO <- rbind(fillrateSBAAUTO, averagefillrateitem)
}
SBAholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualSBAholdingAUTO <- SBAholdingAUTO/SBAholdingAUTO[1]
achievedfillrateSBAAUTO <- colMeans(fillrateSBAAUTO[-1,])
ServicelevelSBAAUTO <- data.frame(achievedfillrate = achievedfillrateSBAAUTO, holding = SBAholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")



####          SBA on OIL data. ####
h = nrow(testOIL)
sbaOIL <- matrix(ncol = h)
for (i in 1:ncol(trainOIL)){
  x <- trainOIL[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,crost(x, h=1, w=NULL ,nop=2 ,type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testOIL[j,i])
  }  
  sbaOIL <- rbind(sbaOIL, t(prediction))
}


# Saving the predictions without transposed and without column 1.
sbaOIL <- t(sbaOIL)
sbaOIL <- sbaOIL[,-1]

#Saving the output data
save(sbaOIL,file="c:/Daan/sbaOIL2.Rda")
#Reload the output data
#load("c:/Daan/sbaOIL.Rda")


# Forecasting accuracy measures.
SBAMSEOIL <- NULL
SBAMASEOIL <- NULL
SBARMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  SBAMSEOIL <- cbind(SBAMSEOIL,  MSE(t(testOIL[i]),t(sbaOIL[,i])))
  SBAMASEOIL <- cbind(SBAMASEOIL,  MASE(t(testOIL[i]),t(sbaOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
  SBARMSSEOIL <- cbind(SBARMSSEOIL,  RMSSE(t(testOIL[i]),t(sbaOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
}
SBAMSEOIL <- mean(SBAMSEOIL,na.rm=TRUE)
SBAMASEOIL <- mean(SBAMASEOIL,na.rm=TRUE)
SBARMSSEOIL <- mean(SBARMSSEOIL,na.rm=TRUE)
print(c("OIL", SBAMSEOIL, SBAMASEOIL, SBARMSSEOIL))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateSBAOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(sbaOIL)){
  leadtimedemand <- mean(t(sbaOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateSBAOIL <- rbind(fillrateSBAOIL, averagefillrateitem)
}
SBAholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualSBAholdingOIL <- SBAholdingOIL/SBAholdingOIL[1]
achievedfillrateSBAOIL <- colMeans(fillrateSBAOIL[-1,])
ServicelevelSBAOIL <- data.frame(achievedfillrate = achievedfillrateSBAOIL, holding = SBAholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "SBA")




print("time SBA total")
proc.time() - ptm

####          TSB on all data sets. ####
ptm <- proc.time()

####          TSB on SIM1 data. ####
h = nrow(testSIM1)
tsbSIM1 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM1)){
  x <- trainSIM1[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM1[j,i])
  }  
  tsbSIM1 <- rbind(tsbSIM1, t(prediction))
}


# Saving the predictions without transposed and without column 1.
tsbSIM1 <- t(tsbSIM1)
tsbSIM1 <- tsbSIM1[,-1]

#Saving the output data
save(tsbSIM1,file="c:/Daan/tsbSIM12.Rda")
#Reload the output data
#load("c:/Daan/tsbSIM1.Rda")


# Forecasting accuracy measures.
TSBMSESIM1 <- NULL
TSBMASESIM1 <- NULL
TSBRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  TSBMSESIM1 <- cbind(TSBMSESIM1,  MSE(t(testSIM1[i]),t(tsbSIM1[,i])))
  TSBMASESIM1 <- cbind(TSBMASESIM1,  MASE(t(testSIM1[i]),t(tsbSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  TSBRMSSESIM1 <- cbind(TSBRMSSESIM1,  RMSSE(t(testSIM1[i]),t(tsbSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
TSBMSESIM1 <- mean(TSBMSESIM1)
TSBMASESIM1 <- mean(TSBMASESIM1)
TSBRMSSESIM1 <- mean(TSBRMSSESIM1)
print(c("SIM1", TSBMSESIM1, TSBMASESIM1, TSBRMSSESIM1))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(tsbSIM1)){
  leadtimedemand <- mean(t(tsbSIM1[,i]))
  stdev <- std(t(trainSIM1[i])[!is.na(t(trainSIM1[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM1[i])[(t(testSIM1[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBSIM1 <- rbind(fillrateTSBSIM1, averagefillrateitem)
}
TSBholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualTSBholdingSIM1 <- TSBholdingSIM1/TSBholdingSIM1[1]
achievedfillrateTSBSIM1 <- colMeans(fillrateTSBSIM1[-1,])
ServicelevelTSBSIM1 <- data.frame(achievedfillrate = achievedfillrateTSBSIM1, holding = TSBholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")





####          TSB on SIM2 data. ####
h = nrow(testSIM2)
tsbSIM2 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM2)){
  x <- trainSIM2[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM2[j,i])
  }  
  tsbSIM2 <- rbind(tsbSIM2, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbSIM2 <- t(tsbSIM2)
tsbSIM2 <- tsbSIM2[,-1]

#Saving the output data
save(tsbSIM2,file="c:/Daan/tsbSIM22.Rda")
#Reload the output data
#load("c:/Daan/tsbSIM2.Rda")


# Forecasting accuracy measures.
TSBMSESIM2 <- NULL
TSBMASESIM2 <- NULL
TSBRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  TSBMSESIM2 <- cbind(TSBMSESIM2,  MSE(t(testSIM2[i]),t(tsbSIM2[,i])))
  TSBMASESIM2 <- cbind(TSBMASESIM2,  MASE(t(testSIM2[i]),t(tsbSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  TSBRMSSESIM2 <- cbind(TSBRMSSESIM2,  RMSSE(t(testSIM2[i]),t(tsbSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
TSBMSESIM2 <- mean(TSBMSESIM2)
TSBMASESIM2 <- mean(TSBMASESIM2)
TSBRMSSESIM2 <- mean(TSBRMSSESIM2)
print(c("SIM2", TSBMSESIM2, TSBMASESIM2, TSBRMSSESIM2))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(tsbSIM2)){
  leadtimedemand <- mean(t(tsbSIM2[,i]))
  stdev <- std(t(trainSIM2[i])[!is.na(t(trainSIM2[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBSIM2 <- rbind(fillrateTSBSIM2, averagefillrateitem)
}
TSBholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualTSBholdingSIM2 <- TSBholdingSIM2/TSBholdingSIM2[1]
achievedfillrateTSBSIM2 <- colMeans(fillrateTSBSIM2[-1,])
ServicelevelTSBSIM2 <- data.frame(achievedfillrate = achievedfillrateTSBSIM2, holding = TSBholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")


####          TSB on SIM3 data. ####
h = nrow(testSIM3)
tsbSIM3 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM3)){
  x <- trainSIM3[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM3[j,i])
  }  
  tsbSIM3 <- rbind(tsbSIM3, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbSIM3 <- t(tsbSIM3)
tsbSIM3 <- tsbSIM3[,-1]

#Saving the output data
save(tsbSIM3,file="c:/Daan/tsbSIM32.Rda")
#Reload the output data
#load("c:/Daan/tsbSIM3.Rda")


# Forecasting accuracy measures.
TSBMSESIM3 <- NULL
TSBMASESIM3 <- NULL
TSBRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  TSBMSESIM3 <- cbind(TSBMSESIM3,  MSE(t(testSIM3[i]),t(tsbSIM3[,i])))
  TSBMASESIM3 <- cbind(TSBMASESIM3,  MASE(t(testSIM3[i]),t(tsbSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  TSBRMSSESIM3 <- cbind(TSBRMSSESIM3,  RMSSE(t(testSIM3[i]),t(tsbSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
TSBMSESIM3 <- mean(TSBMSESIM3)
TSBMASESIM3 <- mean(TSBMASESIM3)
TSBRMSSESIM3 <- mean(TSBRMSSESIM3)
print(c("SIM3", TSBMSESIM3, TSBMASESIM3, TSBRMSSESIM3))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(tsbSIM3)){
  leadtimedemand <- mean(t(tsbSIM3[,i]))
  stdev <- std(t(trainSIM3[i])[!is.na(t(trainSIM3[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBSIM3 <- rbind(fillrateTSBSIM3, averagefillrateitem)
}
TSBholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualTSBholdingSIM3 <- TSBholdingSIM3/TSBholdingSIM3[1]
achievedfillrateTSBSIM3 <- colMeans(fillrateTSBSIM3[-1,])
ServicelevelTSBSIM3 <- data.frame(achievedfillrate = achievedfillrateTSBSIM3, holding = TSBholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")


####          TSB on SIM4 data. ####

h = nrow(testSIM4)
tsbSIM4 <- matrix(ncol = h)
for (i in 1:ncol(trainSIM4)){
  x <- trainSIM4[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testSIM4[j,i])
  }  
  tsbSIM4 <- rbind(tsbSIM4, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbSIM4 <- t(tsbSIM4)
tsbSIM4 <- tsbSIM4[,-1]

#Saving the output data
save(tsbSIM4,file="c:/Daan/tsbSIM42.Rda")
#Reload the output data
#load("c:/Daan/tsbSIM4.Rda")


# Forecasting accuracy measures.
TSBMSESIM4 <- NULL
TSBMASESIM4 <- NULL
TSBRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  TSBMSESIM4 <- cbind(TSBMSESIM4,  MSE(t(testSIM4[i]),t(tsbSIM4[,i])))
  TSBMASESIM4 <- cbind(TSBMASESIM4,  MASE(t(testSIM4[i]),t(tsbSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  TSBRMSSESIM4 <- cbind(TSBRMSSESIM4,  RMSSE(t(testSIM4[i]),t(tsbSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
TSBMSESIM4 <- mean(TSBMSESIM4)
TSBMASESIM4 <- mean(TSBMASESIM4)
TSBRMSSESIM4 <- mean(TSBRMSSESIM4)
print(c("SIM4", TSBMSESIM4, TSBMASESIM4, TSBRMSSESIM4))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(tsbSIM4)){
  leadtimedemand <- mean(t(tsbSIM4[,i]))
  stdev <- std(t(trainSIM4[i])[!is.na(t(trainSIM4[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBSIM4 <- rbind(fillrateTSBSIM4, averagefillrateitem)
}
TSBholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualTSBholdingSIM4 <- TSBholdingSIM4/TSBholdingSIM4[1]
achievedfillrateTSBSIM4 <- colMeans(fillrateTSBSIM4[-1,])
ServicelevelTSBSIM4 <- data.frame(achievedfillrate = achievedfillrateTSBSIM4, holding = TSBholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")





####          TSB on MAN data. ####
h = nrow(testMAN)
tsbMAN <- matrix(ncol = h)
for (i in 1:ncol(trainMAN)){
  x <- trainMAN[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testMAN[j,i])
  }  
  tsbMAN <- rbind(tsbMAN, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbMAN <- t(tsbMAN)
tsbMAN <- tsbMAN[,-1]

#Saving the output data
save(tsbMAN,file="c:/Daan/tsbMAN2.Rda")
#Reload the output data
#load("c:/Daan/tsbMAN.Rda")


# Forecasting accuracy measures.
TSBMSEMAN <- NULL
TSBMASEMAN <- NULL
TSBRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  TSBMSEMAN <- cbind(TSBMSEMAN,  MSE(t(testMAN[i]),t(tsbMAN[,i])))
  TSBMASEMAN <- cbind(TSBMASEMAN,  MASE(t(testMAN[i]),t(tsbMAN[,i]), mean(abs(t(trainMAN[i])))))
  TSBRMSSEMAN <- cbind(TSBRMSSEMAN,  RMSSE(t(testMAN[i]),t(tsbMAN[,i]), mean(abs(t(trainMAN[i])))))
}
TSBMSEMAN <- mean(TSBMSEMAN)
TSBMASEMAN <- mean(TSBMASEMAN)
TSBRMSSEMAN <- mean(TSBRMSSEMAN)
print(c("MAN", TSBMSEMAN, TSBMASEMAN, TSBRMSSEMAN))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(tsbMAN)){
  leadtimedemand <- mean(t(tsbMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBMAN <- rbind(fillrateTSBMAN, averagefillrateitem)
}
TSBholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualTSBholdingMAN <- TSBholdingMAN/TSBholdingMAN[1]
achievedfillrateTSBMAN <- colMeans(fillrateTSBMAN[-1,])
ServicelevelTSBMAN <- data.frame(achievedfillrate = achievedfillrateTSBMAN, holding = TSBholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")



####          TSB on BRAF data. ####
h = nrow(testBRAF)
tsbBRAF <- matrix(ncol = h)
for (i in 1:ncol(trainBRAF)){
  x <- trainBRAF[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testBRAF[j,i])
  }  
  tsbBRAF <- rbind(tsbBRAF, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbBRAF <- t(tsbBRAF)
tsbBRAF <- tsbBRAF[,-1]

#Saving the output data
save(tsbBRAF,file="c:/Daan/tsbBRAF2.Rda")
#Reload the output data
#load("c:/Daan/tsbBRAF.Rda")


# Forecasting accuracy measures.
TSBMSEBRAF <- NULL
TSBMASEBRAF <- NULL
TSBRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  TSBMSEBRAF <- cbind(TSBMSEBRAF,  MSE(t(testBRAF[i]),t(tsbBRAF[,i])))
  TSBMASEBRAF <- cbind(TSBMASEBRAF,  MASE(t(testBRAF[i]),t(tsbBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  TSBRMSSEBRAF <- cbind(TSBRMSSEBRAF,  RMSSE(t(testBRAF[i]),t(tsbBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
TSBMSEBRAF <- mean(TSBMSEBRAF)
TSBMASEBRAF <- mean(TSBMASEBRAF)
TSBRMSSEBRAF <- mean(TSBRMSSEBRAF)
print(c("BRAF", TSBMSEBRAF, TSBMASEBRAF, TSBRMSSEBRAF))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(tsbBRAF)){
  leadtimedemand <- mean(t(tsbBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBBRAF <- rbind(fillrateTSBBRAF, averagefillrateitem)
}
TSBholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualTSBholdingBRAF <- TSBholdingBRAF/TSBholdingBRAF[1]
achievedfillrateTSBBRAF <- colMeans(fillrateTSBBRAF[-1,])
ServicelevelTSBBRAF <- data.frame(achievedfillrate = achievedfillrateTSBBRAF, holding = TSBholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")



####          TSB on AUTO data. ####
h = nrow(testAUTO)
tsbAUTO <- matrix(ncol = h)
for (i in 1:ncol(trainAUTO)){
  x <- trainAUTO[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testAUTO[j,i])
  }  
  tsbAUTO <- rbind(tsbAUTO, t(prediction))
}

# Saving the predictions without transposed and without column 1.
tsbAUTO <- t(tsbAUTO)
tsbAUTO <- tsbAUTO[,-1]

#Saving the output data
save(tsbAUTO,file="c:/Daan/tsbAUTO2.Rda")
#Reload the output data
#load("c:/Daan/tsbAUTO.Rda")


# Forecasting accuracy measures.
TSBMSEAUTO <- NULL
TSBMASEAUTO <- NULL
TSBRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  TSBMSEAUTO <- cbind(TSBMSEAUTO,  MSE(t(testAUTO[i]),t(tsbAUTO[,i])))
  TSBMASEAUTO <- cbind(TSBMASEAUTO,  MASE(t(testAUTO[i]),t(tsbAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  TSBRMSSEAUTO <- cbind(TSBRMSSEAUTO,  RMSSE(t(testAUTO[i]),t(tsbAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
TSBMSEAUTO <- mean(TSBMSEAUTO)
TSBMASEAUTO <- mean(TSBMASEAUTO)
TSBRMSSEAUTO <- mean(TSBRMSSEAUTO)
print(c("AUTO", TSBMSEAUTO, TSBMASEAUTO, TSBRMSSEAUTO))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(tsbAUTO)){
  leadtimedemand <- mean(t(tsbAUTO[,i]))
  stdev <- std(t(trainAUTO[i])[!is.na(t(trainAUTO[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBAUTO <- rbind(fillrateTSBAUTO, averagefillrateitem)
}
TSBholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualTSBholdingAUTO <- TSBholdingAUTO/TSBholdingAUTO[1]
achievedfillrateTSBAUTO <- colMeans(fillrateTSBAUTO[-1,])
ServicelevelTSBAUTO <- data.frame(achievedfillrate = achievedfillrateTSBAUTO, holding = TSBholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")




####          TSB on OIL data. ####
h = nrow(testOIL)
tsbOIL <- matrix(ncol = h)
for (i in 1:ncol(trainOIL)){
  x <- trainOIL[i]
  prediction <- NULL
  for (j in 1:h){
    prediction <- rbind(prediction,tsb(x, h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out)
    x <- rbind(x,testOIL[j,i])
  }  
  tsbOIL <- rbind(tsbOIL, t(prediction))
}



# Saving the predictions without transposed and without column 1.
tsbOIL <- t(tsbOIL)
tsbOIL <- tsbOIL[,-1]

#Saving the output data
save(tsbOIL,file="c:/Daan/tsbOIL2.Rda")
#Reload the output data
#load("c:/Daan/tsbOIL.Rda")


# Forecasting accuracy measures.
TSBMSEOIL <- NULL
TSBMASEOIL <- NULL
TSBRMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  TSBMSEOIL <- cbind(TSBMSEOIL,  MSE(t(testOIL[i]),t(tsbOIL[,i])))
  TSBMASEOIL <- cbind(TSBMASEOIL,  MASE(t(testOIL[i]),t(tsbOIL[,i]), mean(abs(t(trainOIL[i])),na.rm = TRUE)))
  TSBRMSSEOIL <- cbind(TSBRMSSEOIL,  RMSSE(t(testOIL[i]),t(tsbOIL[,i]), mean(abs(t(trainOIL[i])),na.rm = TRUE)))
}
TSBMSEOIL <- mean(TSBMSEOIL,na.rm = TRUE)
TSBMASEOIL <- mean(TSBMASEOIL,na.rm = TRUE)
TSBRMSSEOIL <- mean(TSBRMSSEOIL,na.rm = TRUE)
print(c("OIL", TSBMSEOIL, TSBMASEOIL, TSBRMSSEOIL))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateTSBOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(tsbOIL)){
  leadtimedemand <- mean(t(tsbOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateTSBOIL <- rbind(fillrateTSBOIL, averagefillrateitem)
}
TSBholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualTSBholdingOIL <- TSBholdingOIL/TSBholdingOIL[1]
achievedfillrateTSBOIL <- colMeans(fillrateTSBOIL[-1,])
ServicelevelTSBOIL <- data.frame(achievedfillrate = achievedfillrateTSBOIL, holding = TSBholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "TSB")


print("time TSB total")
proc.time() - ptm

####          Willemain bootstrapping on all data sets. ####

# Defining state names for the Markov Chain.
# Defining the data sets as sequences and replacing the nonzeroes with 1's.
seqSIM1 <- seqdef(trainSIM1)
seqSIM1[trainSIM1 != 0] <- 1
seqSIM1 <- seqdef(seqSIM1)

seqSIM2 <- seqdef(trainSIM2)
seqSIM2[trainSIM2 != 0] <- 1
seqSIM2 <- seqdef(seqSIM2)

seqSIM3 <- seqdef(trainSIM3)
seqSIM3[trainSIM3 != 0] <- 1
seqSIM3 <- seqdef(seqSIM3)

seqSIM4 <- seqdef(trainSIM4)
seqSIM4[trainSIM4 != 0] <- 1
seqSIM4 <- seqdef(seqSIM4)

seqMAN <- seqdef(trainMAN)
seqMAN[trainMAN != 0] <- 1
seqMAN <- seqdef(seqMAN)

seqBRAF <- seqdef(trainBRAF)
seqBRAF[trainBRAF != 0] <- 1
seqBRAF <- seqdef(seqBRAF)

seqAUTO <- seqdef(trainAUTO)
seqAUTO[trainAUTO != 0] <- 1
seqAUTO <- seqdef(seqAUTO)

seqOIL <- seqdef(trainOIL)
seqOIL[trainOIL != 0] <- 1
seqOIL[seqOIL != 1] <- 0
seqOIL <- seqdef(seqOIL)

# Establishing output variables for each data set. 

ltdSIM1=0
WillemainSIM1=0
WillemainSDSIM1=0
predictionWillemainSIM1=matrix(ncol=nrow(testSIM1))

ltdSIM2=0
WillemainSIM2=0
WillemainSDSIM2=0
predictionWillemainSIM2=matrix(ncol=nrow(testSIM2))

ltdSIM3=0
WillemainSIM3=0
WillemainSDSIM3=0
predictionWillemainSIM3=matrix(ncol=nrow(testSIM3))

ltdSIM4=0
WillemainSIM4=0
WillemainSDSIM4=0
predictionWillemainSIM4=matrix(ncol=nrow(testSIM4))

ltdMAN=0
WillemainMAN=0
WillemainSDMAN=0
predictionWillemainMAN=matrix(ncol=nrow(testMAN))

ltdBRAF=0
WillemainBRAF=0
WillemainSDBRAF=0
predictionWillemainBRAF=matrix(ncol=nrow(testBRAF))

ltdAUTO=0
WillemainAUTO=0
WillemainSDAUTO=0
predictionWillemainAUTO=matrix(ncol=nrow(testAUTO))

ltdOIL=0
WillemainOIL=0
WillemainSDOIL=0
predictionWillemainOIL=matrix(ncol=nrow(testOIL))

# Willemain outputs for each data set.
ptm <- proc.time()

####          Willemain SIM1  ####
statesNames=c(1) # SIM1 has no zeroes, so a 1x1 matrix is used here instead of the 2x2 for the other transition probability matrices.
h=18
for(i in 1:ncol(seqSIM1)){
  leadtime <- 1
  sequence <- seqSIM1[[i]]
  for (variable in 1:h){
    statesNames=c(1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                    nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM1[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainSIM1[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdSIM1[[y]] <- sum(mcprediction)
    }
    WillemainSIM1[[variable]] <- mean(ltdSIM1)
    WillemainSDSIM1[[variable]] <- sd(ltdSIM1)
    if(testSIM1[variable,i]>0){sequence <- c(sequence,1)}else{sequence <- c(sequence,0)}
  }
predictionWillemainSIM1 <- rbind(predictionWillemainSIM1,WillemainSIM1)  
}

# Saving the predictions without transposed and without column 1.
forecastwillemainSIM1 <- t(predictionWillemainSIM1)
forecastwillemainSIM1 <- forecastwillemainSIM1[,-1]

#Saving the output data
save(forecastwillemainSIM1,file="c:/Daan/forecastwillemainSIM12.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainSIM1.Rda")

# Forecasting accuracy measures.
WillemainMSESIM1 <- NULL
WillemainMASESIM1 <- NULL
WillemainRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  WillemainMSESIM1 <- cbind(WillemainMSESIM1,  MSE(t(testSIM1[i]),(forecastwillemainSIM1[,i])))
  WillemainMASESIM1 <- cbind(WillemainMASESIM1,  MASE(t(testSIM1[i]),(forecastwillemainSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  WillemainRMSSESIM1 <- cbind(WillemainRMSSESIM1,  RMSSE(t(testSIM1[i]),(forecastwillemainSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
WillemainMSESIM1 <- mean(WillemainMSESIM1)
WillemainMASESIM1 <- mean(WillemainMASESIM1)
WillemainRMSSESIM1 <- mean(WillemainRMSSESIM1)
print(c("SIM1", WillemainMSESIM1, WillemainMASESIM1, WillemainRMSSESIM1))


# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(testSIM1)){
  leadtimedemand <- mean(t(forecastwillemainSIM1[,i]))
  stdev <- std(t(trainSIM1[i])[!is.na(t(trainSIM1[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM1[i])[(t(testSIM1[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainSIM1 <- rbind(fillrateWillemainSIM1, averagefillrateitem)
}
WillemainholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualWillemainholdingSIM1 <- WillemainholdingSIM1/WillemainholdingSIM1[1]
achievedfillrateWillemainSIM1 <- colMeans(fillrateWillemainSIM1[-1,])
ServicelevelWillemainSIM1 <- data.frame(achievedfillrate = achievedfillrateWillemainSIM1, holding = WillemainholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")


ptm <- proc.time()
####          Willemain SIM2  ####
h=18
for(i in 1:ncol(seqSIM2)){
  leadtime <- 1
  sequence <- seqSIM2[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM2[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainSIM2[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdSIM2[[y]] <- sum(mcprediction)
      }
      WillemainSIM2[[variable]] <- mean(ltdSIM2)
      WillemainSDSIM2[[variable]] <- sd(ltdSIM2)
      if(testSIM2[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM2[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainSIM2[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdSIM2[[y]] <- sum(mcprediction)
    }
    WillemainSIM2[[variable]] <- mean(ltdSIM2)
    WillemainSDSIM2[[variable]] <- sd(ltdSIM2)
    if(testSIM2[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainSIM2 <- rbind(predictionWillemainSIM2,WillemainSIM2)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainSIM2 <- t(predictionWillemainSIM2)
forecastwillemainSIM2 <- forecastwillemainSIM2[,-1]

#Saving the output data
save(forecastwillemainSIM2,file="c:/Daan/forecastwillemainSIM22.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainSIM2.Rda")

# Forecasting accuracy measures.
WillemainMSESIM2 <- NULL
WillemainMASESIM2 <- NULL
WillemainRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  WillemainMSESIM2 <- cbind(WillemainMSESIM2,  MSE(t(testSIM2[i]),(forecastwillemainSIM2[,i])))
  WillemainMASESIM2 <- cbind(WillemainMASESIM2,  MASE(t(testSIM2[i]),(forecastwillemainSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  WillemainRMSSESIM2 <- cbind(WillemainRMSSESIM2,  RMSSE(t(testSIM2[i]),(forecastwillemainSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
WillemainMSESIM2 <- mean(WillemainMSESIM2)
WillemainMASESIM2 <- mean(WillemainMASESIM2)
WillemainRMSSESIM2 <- mean(WillemainRMSSESIM2)
print(c("SIM2", WillemainMSESIM2, WillemainMASESIM2, WillemainRMSSESIM2))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(testSIM2)){
  leadtimedemand <- mean(t(forecastwillemainSIM2[,i]))
  stdev <- std(t(trainSIM2[i])[!is.na(t(trainSIM2[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainSIM2 <- rbind(fillrateWillemainSIM2, averagefillrateitem)
}
WillemainholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualWillemainholdingSIM2 <- WillemainholdingSIM2/WillemainholdingSIM2[1]
achievedfillrateWillemainSIM2 <- colMeans(fillrateWillemainSIM2[-1,])
ServicelevelWillemainSIM2 <- data.frame(achievedfillrate = achievedfillrateWillemainSIM2, holding = WillemainholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")



####          Willemain SIM3  ####

h=18
for(i in 1:ncol(seqSIM3)){
  leadtime <- 1
  sequence <- seqSIM3[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      if(sum(transprob[1,])<1){
        transprob[1,1] <- 0.5
        transprob[1,2] <- 0.5
      }
      if(sum(transprob[2,])<1){
        transprob[2,1] <- 0.5
        transprob[2,2] <- 0.5
      }
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM3[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainSIM3[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdSIM3[[y]] <- sum(mcprediction)
      }
      WillemainSIM3[[variable]] <- mean(ltdSIM3)
      WillemainSDSIM3[[variable]] <- sd(ltdSIM3)
      if(testSIM3[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM3[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainSIM3[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdSIM3[[y]] <- sum(mcprediction)
    }
    WillemainSIM3[[variable]] <- mean(ltdSIM3)
    WillemainSDSIM3[[variable]] <- sd(ltdSIM3)
    if(testSIM3[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainSIM3 <- rbind(predictionWillemainSIM3,WillemainSIM3)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainSIM3 <- t(predictionWillemainSIM3)
forecastwillemainSIM3 <- forecastwillemainSIM3[,-1]

#Saving the output data
save(forecastwillemainSIM3,file="c:/Daan/forecastwillemainSIM32.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainSIM3.Rda")

# Forecasting accuracy measures.
WillemainMSESIM3 <- NULL
WillemainMASESIM3 <- NULL
WillemainRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  WillemainMSESIM3 <- cbind(WillemainMSESIM3,  MSE(t(testSIM3[i]),(forecastwillemainSIM3[,i])))
  WillemainMASESIM3 <- cbind(WillemainMASESIM3,  MASE(t(testSIM3[i]),(forecastwillemainSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  WillemainRMSSESIM3 <- cbind(WillemainRMSSESIM3,  RMSSE(t(testSIM3[i]),(forecastwillemainSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
WillemainMSESIM3 <- mean(WillemainMSESIM3)
WillemainMASESIM3 <- mean(WillemainMASESIM3)
WillemainRMSSESIM3 <- mean(WillemainRMSSESIM3)
print(c("SIM3", WillemainMSESIM3, WillemainMASESIM3, WillemainRMSSESIM3))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(testSIM3)){
  leadtimedemand <- mean(t(forecastwillemainSIM3[,i]))
  stdev <- std(t(trainSIM3[i])[!is.na(t(trainSIM3[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainSIM3 <- rbind(fillrateWillemainSIM3, averagefillrateitem)
}
WillemainholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualWillemainholdingSIM3 <- WillemainholdingSIM3/WillemainholdingSIM3[1]
achievedfillrateWillemainSIM3 <- colMeans(fillrateWillemainSIM3[-1,])
ServicelevelWillemainSIM3 <- data.frame(achievedfillrate = achievedfillrateWillemainSIM3, holding = WillemainholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")


####          Willemain SIM4  ####

h=18
for(i in 1:ncol(seqSIM4)){
  leadtime <- 1
  sequence <- seqSIM4[[i]]
  for (variable in 1:h){
  statesNames=c(0,1)
  transprob <- createSequenceMatrix(
    sequence,
    toRowProbs = TRUE,
    sanitize = FALSE
  )
  if(length(transprob)!=1){
    mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                   nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM4[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainSIM4[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdSIM4[[y]] <- sum(mcprediction)
      }
    WillemainSIM4[[variable]] <- mean(ltdSIM4)
    WillemainSDSIM4[[variable]] <- sd(ltdSIM4)
    if(testSIM4[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqSIM4[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainSIM4[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdSIM4[[y]] <- sum(mcprediction)
    }
    WillemainSIM4[[variable]] <- mean(ltdSIM4)
    WillemainSDSIM4[[variable]] <- sd(ltdSIM4)
    if(testSIM4[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainSIM4 <- rbind(predictionWillemainSIM4,WillemainSIM4)  
  }


# Saving the predictions without transposed and without column 1.
forecastwillemainSIM4 <- t(predictionWillemainSIM4)
forecastwillemainSIM4 <- forecastwillemainSIM4[,-1]

#Saving the output data
save(forecastwillemainSIM4,file="c:/Daan/forecastwillemainSIM42.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainSIM4.Rda")

# Forecasting accuracy measures.
WillemainMSESIM4 <- NULL
WillemainMASESIM4 <- NULL
WillemainRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  WillemainMSESIM4 <- cbind(WillemainMSESIM4,  MSE(t(testSIM4[i]),(forecastwillemainSIM4[,i])))
  WillemainMASESIM4 <- cbind(WillemainMASESIM4,  MASE(t(testSIM4[i]),(forecastwillemainSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  WillemainRMSSESIM4 <- cbind(WillemainRMSSESIM4,  RMSSE(t(testSIM4[i]),(forecastwillemainSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
WillemainMSESIM4 <- mean(WillemainMSESIM4)
WillemainMASESIM4 <- mean(WillemainMASESIM4)
WillemainRMSSESIM4 <- mean(WillemainRMSSESIM4)
print(c("SIM4", WillemainMSESIM4, WillemainMASESIM4, WillemainRMSSESIM4))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(testSIM4)){
  leadtimedemand <- mean(t(forecastwillemainSIM4[,i]))
  stdev <- std(t(trainSIM4[i])[!is.na(t(trainSIM4[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainSIM4 <- rbind(fillrateWillemainSIM4, averagefillrateitem)
}
WillemainholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualWillemainholdingSIM4 <- WillemainholdingSIM4/WillemainholdingSIM4[1]
achievedfillrateWillemainSIM4 <- colMeans(fillrateWillemainSIM4[-1,])
ServicelevelWillemainSIM4 <- data.frame(achievedfillrate = achievedfillrateWillemainSIM4, holding = WillemainholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")


####          Willemain MAN  ####
h=45
for(i in 1:ncol(seqMAN)){
  leadtime <- 1
  sequence <- seqMAN[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqMAN[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainMAN[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdMAN[[y]] <- sum(mcprediction)
      }
      WillemainMAN[[variable]] <- mean(ltdMAN)
      WillemainSDMAN[[variable]] <- sd(ltdMAN)
      if(testMAN[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqMAN[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainMAN[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdMAN[[y]] <- sum(mcprediction)
    }
    WillemainMAN[[variable]] <- mean(ltdMAN)
    WillemainSDMAN[[variable]] <- sd(ltdMAN)
    if(testMAN[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainMAN <- rbind(predictionWillemainMAN,WillemainMAN)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainMAN <- t(predictionWillemainMAN)
forecastwillemainMAN <- forecastwillemainMAN[,-1]

#Saving the output data
save(forecastwillemainMAN,file="c:/Daan/forecastwillemainMAN2.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainMAN.Rda")

# Forecasting accuracy measures.
WillemainMSEMAN <- NULL
WillemainMASEMAN <- NULL
WillemainRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  WillemainMSEMAN <- cbind(WillemainMSEMAN,  MSE(t(testMAN[i]),(forecastwillemainMAN[,i])))
  WillemainMASEMAN <- cbind(WillemainMASEMAN,  MASE(t(testMAN[i]),(forecastwillemainMAN[,i]), mean(abs(t(trainMAN[i])))))
  WillemainRMSSEMAN <- cbind(WillemainRMSSEMAN,  RMSSE(t(testMAN[i]),(forecastwillemainMAN[,i]), mean(abs(t(trainMAN[i])))))
}
WillemainMSEMAN <- mean(WillemainMSEMAN)
WillemainMASEMAN <- mean(WillemainMASEMAN)
WillemainRMSSEMAN <- mean(WillemainRMSSEMAN)
print(c("MAN", WillemainMSEMAN, WillemainMASEMAN, WillemainRMSSEMAN))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(testMAN)){
  leadtimedemand <- mean(t(forecastwillemainMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainMAN <- rbind(fillrateWillemainMAN, averagefillrateitem)
}
WillemainholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualWillemainholdingMAN <- WillemainholdingMAN/WillemainholdingMAN[1]
achievedfillrateWillemainMAN <- colMeans(fillrateWillemainMAN[-1,])
ServicelevelWillemainMAN <- data.frame(achievedfillrate = achievedfillrateWillemainMAN, holding = WillemainholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")



####          Willemain BRAF  ####
h=18
for(i in 1:ncol(seqBRAF)){
  leadtime <- 1
  sequence <- seqBRAF[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqBRAF[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainBRAF[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdBRAF[[y]] <- sum(mcprediction)
      }
      WillemainBRAF[[variable]] <- mean(ltdBRAF)
      WillemainSDBRAF[[variable]] <- sd(ltdBRAF)
      if(testBRAF[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqBRAF[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainBRAF[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdBRAF[[y]] <- sum(mcprediction)
    }
    WillemainBRAF[[variable]] <- mean(ltdBRAF)
    WillemainSDBRAF[[variable]] <- sd(ltdBRAF)
    if(testBRAF[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainBRAF <- rbind(predictionWillemainBRAF,WillemainBRAF)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainBRAF <- t(predictionWillemainBRAF)
forecastwillemainBRAF <- forecastwillemainBRAF[,-1]

#Saving the output data
save(forecastwillemainBRAF,file="c:/Daan/forecastwillemainBRAF2.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainBRAF.Rda")

# Forecasting accuracy measures.
WillemainMSEBRAF <- NULL
WillemainMASEBRAF <- NULL
WillemainRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  WillemainMSEBRAF <- cbind(WillemainMSEBRAF,  MSE(t(testBRAF[i]),(forecastwillemainBRAF[,i])))
  WillemainMASEBRAF <- cbind(WillemainMASEBRAF,  MASE(t(testBRAF[i]),(forecastwillemainBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  WillemainRMSSEBRAF <- cbind(WillemainRMSSEBRAF,  RMSSE(t(testBRAF[i]),(forecastwillemainBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
WillemainMSEBRAF <- mean(WillemainMSEBRAF)
WillemainMASEBRAF <- mean(WillemainMASEBRAF)
WillemainRMSSEBRAF <- mean(WillemainRMSSEBRAF)
print(c("BRAF", WillemainMSEBRAF, WillemainMASEBRAF, WillemainRMSSEBRAF))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(testBRAF)){
  leadtimedemand <- mean(t(forecastwillemainBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainBRAF <- rbind(fillrateWillemainBRAF, averagefillrateitem)
}
WillemainholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualWillemainholdingBRAF <- WillemainholdingBRAF/WillemainholdingBRAF[1]
achievedfillrateWillemainBRAF <- colMeans(fillrateWillemainBRAF[-1,])
ServicelevelWillemainBRAF <- data.frame(achievedfillrate = achievedfillrateWillemainBRAF, holding = WillemainholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")



####          Willemain AUTO  ####

h=7
for(i in 1:ncol(seqAUTO)){
  leadtime <- 1
  sequence <- seqAUTO[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      if(sum(transprob[1,])<1){
      transprob[1,1] <- 0.5
      transprob[1,2] <- 0.5
    }
      if(sum(transprob[2,])<1){
        transprob[2,1] <- 0.5
        transprob[2,2] <- 0.5
      }
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqAUTO[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainAUTO[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdAUTO[[y]] <- sum(mcprediction)
      }
      WillemainAUTO[[variable]] <- mean(ltdAUTO)
      WillemainSDAUTO[[variable]] <- sd(ltdAUTO)
      if(testAUTO[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqAUTO[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainAUTO[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdAUTO[[y]] <- sum(mcprediction)
    }
    WillemainAUTO[[variable]] <- mean(ltdAUTO)
    WillemainSDAUTO[[variable]] <- sd(ltdAUTO)
    if(testAUTO[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainAUTO <- rbind(predictionWillemainAUTO,WillemainAUTO)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainAUTO <- t(predictionWillemainAUTO)
forecastwillemainAUTO <- forecastwillemainAUTO[,-1]

#Saving the output data
save(forecastwillemainAUTO,file="c:/Daan/forecastwillemainAUTO2.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainAUTO.Rda")

# Forecasting accuracy measures.
WillemainMSEAUTO <- NULL
WillemainMASEAUTO <- NULL
WillemainRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  WillemainMSEAUTO <- cbind(WillemainMSEAUTO,  MSE(t(testAUTO[i]),(forecastwillemainAUTO[,i])))
  WillemainMASEAUTO <- cbind(WillemainMASEAUTO,  MASE(t(testAUTO[i]),(forecastwillemainAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  WillemainRMSSEAUTO <- cbind(WillemainRMSSEAUTO,  RMSSE(t(testAUTO[i]),(forecastwillemainAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
WillemainMSEAUTO <- mean(WillemainMSEAUTO)
WillemainMASEAUTO <- mean(WillemainMASEAUTO)
WillemainRMSSEAUTO <- mean(WillemainRMSSEAUTO)
print(c("AUTO", WillemainMSEAUTO, WillemainMASEAUTO, WillemainRMSSEAUTO))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(testAUTO)){
  leadtimedemand <- mean(t(forecastwillemainAUTO[,i]))
  stdev <- std(t(trainAUTO[i])[!is.na(t(trainAUTO[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainAUTO <- rbind(fillrateWillemainAUTO, averagefillrateitem)
}
WillemainholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualWillemainholdingAUTO <- WillemainholdingAUTO/WillemainholdingAUTO[1]
achievedfillrateWillemainAUTO <- colMeans(fillrateWillemainAUTO[-1,])
ServicelevelWillemainAUTO <- data.frame(achievedfillrate = achievedfillrateWillemainAUTO, holding = WillemainholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")


####          Willemain OIL  ####

h=17
for(i in 1:ncol(seqOIL)){
  leadtime <- 1
  sequence <- seqOIL[[i]]
  for (variable in 1:h){
    statesNames=c(0,1)
    transprob <- createSequenceMatrix(
      sequence,
      toRowProbs = TRUE,
      sanitize = FALSE
    )
    if(length(transprob)!=1){
      if(sum(transprob[1,])<1){
        transprob[1,1] <- 0.5
        transprob[1,2] <- 0.5
      }
      if(sum(transprob[2,])<1){
        transprob[2,1] <- 0.5
        transprob[2,2] <- 0.5
      }
      mc<-new("markovchain", transitionMatrix=matrix((transprob),
                                                     nrow=2, dimnames=list(statesNames,statesNames)))
      for (y in 1:1000){
        mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqOIL[[i]], n=1),include.t0 = FALSE))
        nonzeroamount <- length(mcprediction[mcprediction != 0])
        nonzeroes <- trainOIL[[i]]
        mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
        mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
        ltdOIL[[y]] <- sum(mcprediction)
      }
      WillemainOIL[[variable]] <- mean(ltdOIL,na.rm=TRUE)
      WillemainSDOIL[[variable]] <- sd(ltdOIL,na.rm=TRUE)
      if(is.na(testOIL[variable,i])){testOIL[variable,i] <- 0}
      if(testOIL[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    } else {statesNames=c(1)
    mc<-new("markovchain", transitionMatrix=matrix((transprob),byrow=TRUE,
                                                   nrow=1, dimnames=list(statesNames,statesNames)))
    for (y in 1:1000){
      mcprediction <- as.numeric(markovchainSequence(n=leadtime, markovchain=mc,t0 = tail(seqOIL[[i]], n=1),include.t0 = FALSE))
      nonzeroamount <- length(mcprediction[mcprediction != 0])
      nonzeroes <- trainOIL[[i]]
      mcprediction[mcprediction != 0] <- sample(nonzeroes[nonzeroes != 0], size=nonzeroamount)
      mcprediction[mcprediction != 0] <- 1 + round(mcprediction[mcprediction != 0] + rnorm(1)*sqrt(mcprediction[mcprediction != 0]))
      ltdOIL[[y]] <- sum(mcprediction)
    }
    WillemainOIL[[variable]] <- mean(ltdOIL,na.rm=TRUE)
    WillemainSDOIL[[variable]] <- sd(ltdOIL,na.rm=TRUE)
    if(is.na(testOIL[variable,i])){testOIL[variable,i] <- 0}
    if(testOIL[variable,i]>0){sequence <- c(sequence,2)}else{sequence <- c(sequence,1)}
    }}
  predictionWillemainOIL <- rbind(predictionWillemainOIL,WillemainOIL)  
}


# Saving the predictions without transposed and without column 1.
forecastwillemainOIL <- t(predictionWillemainOIL)
forecastwillemainOIL <- forecastwillemainOIL[,-1]

#Saving the output data
save(forecastwillemainOIL,file="c:/Daan/forecastwillemainOIL2.Rda")
#Reload the output data
#load("c:/Daan/forecastwillemainOIL.Rda")

# Forecasting accuracy measures.
WillemainMSEOIL <- NULL
WillemainMASEOIL <- NULL
WillemainRMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  WillemainMSEOIL <- cbind(WillemainMSEOIL,  MSE(t(testOIL[i]),(forecastwillemainOIL[,i])))
  WillemainMASEOIL <- cbind(WillemainMASEOIL,  MASE(t(testOIL[i]),(forecastwillemainOIL[,i]), mean(abs(t(trainOIL[i])), na.rm=TRUE)))
  WillemainRMSSEOIL <- cbind(WillemainRMSSEOIL,  RMSSE(t(testOIL[i]),(forecastwillemainOIL[,i]), mean(abs(t(trainOIL[i])), na.rm=TRUE)))
}
WillemainMSEOIL <- mean(WillemainMSEOIL, na.rm=TRUE)
WillemainMASEOIL <- mean(WillemainMASEOIL, na.rm=TRUE)
WillemainRMSSEOIL <- mean(WillemainRMSSEOIL, na.rm=TRUE)
print(c("OIL", WillemainMSEOIL, WillemainMASEOIL, WillemainRMSSEOIL))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateWillemainOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(testOIL)){
  leadtimedemand <- mean(t(forecastwillemainOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateWillemainOIL <- rbind(fillrateWillemainOIL, averagefillrateitem)
}
WillemainholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualWillemainholdingOIL <- WillemainholdingOIL/WillemainholdingOIL[1]
achievedfillrateWillemainOIL <- colMeans(fillrateWillemainOIL[-1,])
ServicelevelWillemainOIL <- data.frame(achievedfillrate = achievedfillrateWillemainOIL, holding = WillemainholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "Willemain")



print("time Willemain total")
proc.time() - ptm

####          Machine learning method (Single hidden layer MLP) on all data sets. ####

ptm <- proc.time()

# Normalizing the data to 0-1 for the MLP, ignoring NA's for the OIL dataset using a custom function.
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

nndataSIM1 <- normalizeData(SIM1, type = "0_1")
nndataSIM2 <- normalizeData(SIM2, type = "0_1")
nndataSIM3 <- normalizeData(SIM3, type = "0_1")
nndataSIM4 <- normalizeData(SIM4, type = "0_1")
nndataMAN  <- normalizeData(MAN, type = "0_1")
nndataBRAF <- normalizeData(BRAF, type = "0_1")
nndataAUTO <- normalizeData(AUTO, type = "0_1")
nndataOIL <- OIL
n=1
for (n in 1:ncol(OIL)){
  nndataOIL[[n]]  <- normalize(OIL[[n]])
}

# Splitting the data again into test and train data.
sampleSIM1 = round(nrow(SIM1)*.70) 
nntrainSIM1 <- nndataSIM1[1:(sampleSIM1), ]
nntestSIM1 <- nndataSIM1[-(1:(sampleSIM1)), ]

nntrainSIM2 <- nndataSIM2[1:(sampleSIM1), ]
nntestSIM2 <- nndataSIM2[-(1:(sampleSIM1)), ]

nntrainSIM3 <- nndataSIM3[1:(sampleSIM1), ]
nntestSIM3 <- nndataSIM3[-(1:(sampleSIM1)), ]

nntrainSIM4 <- nndataSIM4[1:(sampleSIM1), ]
nntestSIM4 <- nndataSIM4[-(1:(sampleSIM1)), ]

# The split point is established for the industrial data sets individually, again.
sampleMAN = round(nrow(MAN)*.70)
nntrainMAN <- nndataMAN[1:(sampleMAN), ]
nntestMAN <- nndataMAN[-(1:(sampleMAN)), ] 

sampleBRAF = round(nrow(BRAF)*.70)
nntrainBRAF <- nndataBRAF[1:(sampleBRAF), ]
nntestBRAF <- nndataBRAF[-(1:(sampleBRAF)), ]

sampleAUTO = round(nrow(AUTO)*.70)
nntrainAUTO <- nndataAUTO[1:(sampleAUTO), ]
nntestAUTO <- nndataAUTO[-(1:(sampleAUTO)), ]

sampleOIL = round(nrow(OIL)*.70)
nntrainOIL <- nndataOIL[1:(sampleOIL), ]
nntestOIL <- nndataOIL[-(1:(sampleOIL)), ]


####          SIM1 neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM1)){
  nndatavector <- nntrainSIM1[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testSIM1)
predictionnnSIM1 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.

i=1
for (i in 1:ncol(nntrainSIM1)){
  input <- tail(nntrainSIM1[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM1)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM1[t,i])
    input <- tail(input, n=5)
  }
  predictionnnSIM1 <- rbind(predictionnnSIM1, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM1 <- getNormParameters(nndataSIM1)
predictionnnSIM1 <- t(predictionnnSIM1)
predictionnnSIM1 <- predictionnnSIM1[,-1]
# Final predictions.
predictionnnSIM1 <- denormalizeData(predictionnnSIM1, normParamsSIM1)

#Saving the output data
save(predictionnnSIM1,file="c:/Daan/predictionnnSIM12.Rda")
#Reload the output data
#load("c:/Daan/predictionnnSIM1.Rda")

# Forecasting accuracy measures.
nnMSESIM1 <- NULL
nnMASESIM1 <- NULL
nnRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  nnMSESIM1 <- cbind(nnMSESIM1,  MSE(t(testSIM1[i]),t(predictionnnSIM1[,i])))
  nnMASESIM1 <- cbind(nnMASESIM1,  MASE(t(testSIM1[i]),t(predictionnnSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  nnRMSSESIM1 <- cbind(nnRMSSESIM1,  RMSSE(t(testSIM1[i]),t(predictionnnSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
nnMSESIM1 <- mean(nnMSESIM1)
nnMASESIM1 <- mean(nnMASESIM1)
nnRMSSESIM1 <- mean(nnRMSSESIM1)
print(c("SIM1", nnMSESIM1, nnMASESIM1, nnRMSSESIM1))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(testSIM1)){
  leadtimedemand <- mean(t(predictionnnSIM1[,i]))
  stdev <- std(t(trainSIM1[i])[!is.na(t(trainSIM1[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM1[i])[(t(testSIM1[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennSIM1 <- rbind(fillratennSIM1, averagefillrateitem)
}
nnholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualnnholdingSIM1 <- nnholdingSIM1/nnholdingSIM1[1]
achievedfillratennSIM1 <- colMeans(fillratennSIM1[-1,])
ServicelevelnnSIM1 <- data.frame(achievedfillrate = achievedfillratennSIM1, holding = nnholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")

####          SIM2 neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM2)){
  nndatavector <- nntrainSIM2[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testSIM2)
predictionnnSIM2 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainSIM2)){
  input <- tail(nntrainSIM2[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM2)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM2[t,i])
    input <- tail(input, n=5)
  }
  predictionnnSIM2 <- rbind(predictionnnSIM2, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM2 <- getNormParameters(nndataSIM2)
predictionnnSIM2 <- t(predictionnnSIM2)
predictionnnSIM2 <- predictionnnSIM2[,-1]
# Final predictions.
predictionnnSIM2 <- denormalizeData(predictionnnSIM2, normParamsSIM2)

#Saving the output data
save(predictionnnSIM2,file="c:/Daan/predictionnnSIM22.Rda")
#Reload the output data
#load("c:/Daan/predictionnnSIM2.Rda")

# Forecasting accuracy measures.
nnMSESIM2 <- NULL
nnMASESIM2 <- NULL
nnRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  nnMSESIM2 <- cbind(nnMSESIM2,  MSE(t(testSIM2[i]),t(predictionnnSIM2[,i])))
  nnMASESIM2 <- cbind(nnMASESIM2,  MASE(t(testSIM2[i]),t(predictionnnSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  nnRMSSESIM2 <- cbind(nnRMSSESIM2,  RMSSE(t(testSIM2[i]),t(predictionnnSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
nnMSESIM2 <- mean(nnMSESIM2)
nnMASESIM2 <- mean(nnMASESIM2)
nnRMSSESIM2 <- mean(nnRMSSESIM2)
print(c("SIM2", nnMSESIM2, nnMASESIM2, nnRMSSESIM2))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(testSIM2)){
  leadtimedemand <- mean(t(predictionnnSIM2[,i]))
  stdev <- std(t(trainSIM2[i])[!is.na(t(trainSIM2[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennSIM2 <- rbind(fillratennSIM2, averagefillrateitem)
}
nnholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualnnholdingSIM2 <- nnholdingSIM2/nnholdingSIM2[1]
achievedfillratennSIM2 <- colMeans(fillratennSIM2[-1,])
ServicelevelnnSIM2 <- data.frame(achievedfillrate = achievedfillratennSIM2, holding = nnholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")




####          SIM3 neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM3)){
  nndatavector <- nntrainSIM3[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testSIM3)
predictionnnSIM3 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainSIM3)){
  input <- tail(nntrainSIM3[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM3)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM3[t,i])
    input <- tail(input, n=5)
  }
  predictionnnSIM3 <- rbind(predictionnnSIM3, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM3 <- getNormParameters(nndataSIM3)
predictionnnSIM3 <- t(predictionnnSIM3)
predictionnnSIM3 <- predictionnnSIM3[,-1]
# Final predictions.
predictionnnSIM3 <- denormalizeData(predictionnnSIM3, normParamsSIM3)

#Saving the output data
save(predictionnnSIM3,file="c:/Daan/predictionnnSIM32.Rda")
#Reload the output data
#load("c:/Daan/predictionnnSIM3.Rda")

# Forecasting accuracy measures.
nnMSESIM3 <- NULL
nnMASESIM3 <- NULL
nnRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  nnMSESIM3 <- cbind(nnMSESIM3,  MSE(t(testSIM3[i]),t(predictionnnSIM3[,i])))
  nnMASESIM3 <- cbind(nnMASESIM3,  MASE(t(testSIM3[i]),t(predictionnnSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  nnRMSSESIM3 <- cbind(nnRMSSESIM3,  RMSSE(t(testSIM3[i]),t(predictionnnSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
nnMSESIM3 <- mean(nnMSESIM3)
nnMASESIM3 <- mean(nnMASESIM3)
nnRMSSESIM3 <- mean(nnRMSSESIM3)
print(c("SIM3", nnMSESIM3, nnMASESIM3, nnRMSSESIM3))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(testSIM3)){
  leadtimedemand <- mean(t(predictionnnSIM3[,i]))
  stdev <- std(t(trainSIM3[i])[!is.na(t(trainSIM3[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennSIM3 <- rbind(fillratennSIM3, averagefillrateitem)
}
nnholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualnnholdingSIM3 <- nnholdingSIM3/nnholdingSIM3[1]
achievedfillratennSIM3 <- colMeans(fillratennSIM3[-1,])
ServicelevelnnSIM3 <- data.frame(achievedfillrate = achievedfillratennSIM3, holding = nnholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")



####          SIM4 neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM4)){
  nndatavector <- nntrainSIM4[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testSIM4)
predictionnnSIM4 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainSIM4)){
  input <- tail(nntrainSIM4[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM4)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM4[t,i])
    input <- tail(input, n=5)
  }
  predictionnnSIM4 <- rbind(predictionnnSIM4, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM4 <- getNormParameters(nndataSIM4)
predictionnnSIM4 <- t(predictionnnSIM4)
predictionnnSIM4 <- predictionnnSIM4[,-1]
# Final predictions.
predictionnnSIM4 <- denormalizeData(predictionnnSIM4, normParamsSIM4)

#Saving the output data
save(predictionnnSIM4,file="c:/Daan/predictionnnSIM42.Rda")
#Reload the output data
#load("c:/Daan/predictionnnSIM4.Rda")

# Forecasting accuracy measures.
nnMSESIM4 <- NULL
nnMASESIM4 <- NULL
nnRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  nnMSESIM4 <- cbind(nnMSESIM4,  MSE(t(testSIM4[i]),t(predictionnnSIM4[,i])))
  nnMASESIM4 <- cbind(nnMASESIM4,  MASE(t(testSIM4[i]),t(predictionnnSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  nnRMSSESIM4 <- cbind(nnRMSSESIM4,  RMSSE(t(testSIM4[i]),t(predictionnnSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
nnMSESIM4 <- mean(nnMSESIM4)
nnMASESIM4 <- mean(nnMASESIM4)
nnRMSSESIM4 <- mean(nnRMSSESIM4)
print(c("SIM4", nnMSESIM4, nnMASESIM4, nnRMSSESIM4))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(testSIM4)){
  leadtimedemand <- mean(t(predictionnnSIM4[,i]))
  stdev <- std(t(trainSIM4[i])[!is.na(t(trainSIM4[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennSIM4 <- rbind(fillratennSIM4, averagefillrateitem)
}
nnholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualnnholdingSIM4 <- nnholdingSIM4/nnholdingSIM4[1]
achievedfillratennSIM4 <- colMeans(fillratennSIM4[-1,])
ServicelevelnnSIM4 <- data.frame(achievedfillrate = achievedfillratennSIM4, holding = nnholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")




####          MAN neural network application.          ####


# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainMAN)){
  nndatavector <- nntrainMAN[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testMAN)
predictionnnMAN <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainMAN)){
  input <- tail(nntrainMAN[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testMAN)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestMAN[t,i])
    input <- tail(input, n=5)
  }
  predictionnnMAN <- rbind(predictionnnMAN, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsMAN <- getNormParameters(nndataMAN)
predictionnnMAN <- t(predictionnnMAN)
predictionnnMAN <- predictionnnMAN[,-1]
# Final predictions.
predictionnnMAN <- denormalizeData(predictionnnMAN, normParamsMAN)

#Saving the output data
save(predictionnnMAN,file="c:/Daan/predictionnnMAN2.Rda")
#Reload the output data
#load("c:/Daan/predictionnnMAN.Rda")

# Forecasting accuracy measures.
nnMSEMAN <- NULL
nnMASEMAN <- NULL
nnRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  nnMSEMAN <- cbind(nnMSEMAN,  MSE(t(testMAN[i]),t(predictionnnMAN[,i])))
  nnMASEMAN <- cbind(nnMASEMAN,  MASE(t(testMAN[i]),t(predictionnnMAN[,i]), mean(abs(t(trainMAN[i])))))
  nnRMSSEMAN <- cbind(nnRMSSEMAN,  RMSSE(t(testMAN[i]),t(predictionnnMAN[,i]), mean(abs(t(trainMAN[i])))))
}
nnMSEMAN <- mean(nnMSEMAN)
nnMASEMAN <- mean(nnMASEMAN)
nnRMSSEMAN <- mean(nnRMSSEMAN)
print(c("MAN", nnMSEMAN, nnMASEMAN, nnRMSSEMAN))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(testMAN)){
  leadtimedemand <- mean(t(predictionnnMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennMAN <- rbind(fillratennMAN, averagefillrateitem)
}
nnholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualnnholdingMAN <- nnholdingMAN/nnholdingMAN[1]
achievedfillratennMAN <- colMeans(fillratennMAN[-1,])
ServicelevelnnMAN <- data.frame(achievedfillrate = achievedfillratennMAN, holding = nnholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")



####          BRAF neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainBRAF)){
  nndatavector <- nntrainBRAF[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testBRAF)
predictionnnBRAF <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainBRAF)){
  input <- tail(nntrainBRAF[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testBRAF)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestBRAF[t,i])
    input <- tail(input, n=5)
  }
  predictionnnBRAF <- rbind(predictionnnBRAF, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsBRAF <- getNormParameters(nndataBRAF)
predictionnnBRAF <- t(predictionnnBRAF)
predictionnnBRAF <- predictionnnBRAF[,-1]
# Final predictions.
predictionnnBRAF <- denormalizeData(predictionnnBRAF, normParamsBRAF)

#Saving the output data
save(predictionnnBRAF,file="c:/Daan/predictionnnBRAF2.Rda")
#Reload the output data
#load("c:/Daan/predictionnnBRAF.Rda")

# Forecasting accuracy measures.
nnMSEBRAF <- NULL
nnMASEBRAF <- NULL
nnRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  nnMSEBRAF <- cbind(nnMSEBRAF,  MSE(t(testBRAF[i]),t(predictionnnBRAF[,i])))
  nnMASEBRAF <- cbind(nnMASEBRAF,  MASE(t(testBRAF[i]),t(predictionnnBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  nnRMSSEBRAF <- cbind(nnRMSSEBRAF,  RMSSE(t(testBRAF[i]),t(predictionnnBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
nnMSEBRAF <- mean(nnMSEBRAF)
nnMASEBRAF <- mean(nnMASEBRAF)
nnRMSSEBRAF <- mean(nnRMSSEBRAF)
print(c("BRAF", nnMSEBRAF, nnMASEBRAF, nnRMSSEBRAF))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(testBRAF)){
  leadtimedemand <- mean(t(predictionnnBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennBRAF <- rbind(fillratennBRAF, averagefillrateitem)
}
nnholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualnnholdingBRAF <- nnholdingBRAF/nnholdingBRAF[1]
achievedfillratennBRAF <- colMeans(fillratennBRAF[-1,])
ServicelevelnnBRAF <- data.frame(achievedfillrate = achievedfillratennBRAF, holding = nnholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")




####          AUTO neural network application.          ####

# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainAUTO)){
  nndatavector <- nntrainAUTO[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testAUTO)
predictionnnAUTO <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
for (i in 1:ncol(nntrainAUTO)){
  input <- tail(nntrainAUTO[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testAUTO)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestAUTO[t,i])
    input <- tail(input, n=5)
  }
  predictionnnAUTO <- rbind(predictionnnAUTO, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsAUTO <- getNormParameters(nndataAUTO)
predictionnnAUTO <- t(predictionnnAUTO)
predictionnnAUTO <- predictionnnAUTO[,-1]
# Final predictions.
predictionnnAUTO <- denormalizeData(predictionnnAUTO, normParamsAUTO)

#Saving the output data
save(predictionnnAUTO,file="c:/Daan/predictionnnAUTO2.Rda")
#Reload the output data
#load("c:/Daan/predictionnnAUTO.Rda")

# Forecasting accuracy measures.
nnMSEAUTO <- NULL
nnMASEAUTO <- NULL
nnRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  nnMSEAUTO <- cbind(nnMSEAUTO,  MSE(t(testAUTO[i]),t(predictionnnAUTO[,i])))
  nnMASEAUTO <- cbind(nnMASEAUTO,  MASE(t(testAUTO[i]),t(predictionnnAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  nnRMSSEAUTO <- cbind(nnRMSSEAUTO,  RMSSE(t(testAUTO[i]),t(predictionnnAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
nnMSEAUTO <- mean(nnMSEAUTO)
nnMASEAUTO <- mean(nnMASEAUTO)
nnRMSSEAUTO <- mean(nnRMSSEAUTO)
print(c("AUTO", nnMSEAUTO, nnMASEAUTO, nnRMSSEAUTO))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(testAUTO)){
  leadtimedemand <- mean(t(predictionnnAUTO[,i]))
  stdev <- std(t(trainAUTO[i])[!is.na(t(trainAUTO[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennAUTO <- rbind(fillratennAUTO, averagefillrateitem)
}
nnholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualnnholdingAUTO <- nnholdingAUTO/nnholdingAUTO[1]
achievedfillratennAUTO <- colMeans(fillratennAUTO[-1,])
ServicelevelnnAUTO <- data.frame(achievedfillrate = achievedfillratennAUTO, holding = nnholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")





####          OIL neural network application.          ####


# nndata variable, length 6 with 5 input and 1 output.
nndata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainOIL)){
  nndatavector <- nntrainOIL[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    nndata <- rbind(nndata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
nndata <- na.omit(nndata)

# Training the Neural Network.
model <- mlp(nndata[,1:5], nndata[,6], size=6, learnFuncParams=c(0.1),
             maxit=200)

# Creating the prediction matrix.
h <- nrow(testOIL)
predictionnnOIL <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictionsnndata.
t=1
i=1
for (i in 1:ncol(nntrainOIL)){
  input <- tail(nntrainOIL[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testOIL)) {
    input[is.na(input)] <- 0
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestOIL[t,i])
    input <- tail(input, n=5)
  }
  predictionnnOIL <- rbind(predictionnnOIL, outputs)
}

# Saving the predictions without transposed and without column 1.
predictionnnOIL <- t(predictionnnOIL)
predictionnnOIL <- predictionnnOIL[,-1]

# Denormalizing the data for evaluation of forecasts.
for (i in 1:ncol(predictionnnOIL)) {
  predictionnnOIL[,i] <- predictionnnOIL[,i] * (max(OIL[i], na.rm=TRUE) - min(OIL[i], na.rm=TRUE)) + min(OIL[i], na.rm=TRUE)
}

#Saving the output data
save(predictionnnOIL,file="c:/Daan/predictionnnOIL2.Rda")
#Reload the output data
#load("c:/Daan/predictionnnOIL.Rda")

# Forecasting accuracy measures.
nnMSEOIL <- NULL
nnMASEOIL <- NULL
nnRMSSEOIL <- NULL
i=1
for (i in 1:ncol(testOIL)){
  nnMSEOIL <- cbind(nnMSEOIL,  MSE(t(testOIL[i]),t(predictionnnOIL[,i])))
  nnMASEOIL <- cbind(nnMASEOIL,  MASE(t(testOIL[i]),t(predictionnnOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
  nnRMSSEOIL <- cbind(nnRMSSEOIL,  RMSSE(t(testOIL[i]),t(predictionnnOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
}
nnMSEOIL <- mean(nnMSEOIL,na.rm=TRUE)
nnMASEOIL <- mean(nnMASEOIL,na.rm=TRUE)
nnRMSSEOIL <- mean(nnRMSSEOIL,na.rm=TRUE)
print(c("OIL", nnMSEOIL, nnMASEOIL, nnRMSSEOIL))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillratennOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(testOIL)){
  leadtimedemand <- mean(t(predictionnnOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillratennOIL <- rbind(fillratennOIL, averagefillrateitem)
}
nnholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualnnholdingOIL <- nnholdingOIL/nnholdingOIL[1]
achievedfillratennOIL <- colMeans(fillratennOIL[-1,])
ServicelevelnnOIL <- data.frame(achievedfillrate = achievedfillratennOIL, holding = nnholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "MLP")




print("time MLP total")
proc.time() - ptm

####          SIM1 LightGBM method application.         ####

ptm <- proc.time()

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM1)){
  nndatavector <- nntrainSIM1[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testSIM1)
predictionLightGBMSIM1 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainSIM1)){
  input <- tail(nntrainSIM1[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM1)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM1[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMSIM1 <- rbind(predictionLightGBMSIM1, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM1 <- getNormParameters(nndataSIM1)
predictionLightGBMSIM1 <- t(predictionLightGBMSIM1)
predictionLightGBMSIM1 <- predictionLightGBMSIM1[,-1]
# Final predictions.
predictionLightGBMSIM1 <- denormalizeData(predictionLightGBMSIM1, normParamsSIM1)

#Saving the output data
save(predictionLightGBMSIM1,file="c:/Daan/predictionLightGBMSIM12.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMSIM1.Rda")

# Forecasting accuracy measures.
LightGBMMSESIM1 <- NULL
LightGBMMASESIM1 <- NULL
LightGBMRMSSESIM1 <- NULL
for (i in 1:ncol(testSIM1)){
  LightGBMMSESIM1 <- cbind(LightGBMMSESIM1,  MSE(t(testSIM1[i]),t(predictionLightGBMSIM1[,i])))
  LightGBMMASESIM1 <- cbind(LightGBMMASESIM1,  MASE(t(testSIM1[i]),t(predictionLightGBMSIM1[,i]), mean(abs(t(trainSIM1[i])))))
  LightGBMRMSSESIM1 <- cbind(LightGBMRMSSESIM1,  RMSSE(t(testSIM1[i]),t(predictionLightGBMSIM1[,i]), mean(abs(t(trainSIM1[i])))))
}
LightGBMMSESIM1 <- mean(LightGBMMSESIM1)
LightGBMMASESIM1 <- mean(LightGBMMASESIM1)
LightGBMRMSSESIM1 <- mean(LightGBMRMSSESIM1)
print(c("SIM1", LightGBMMSESIM1, LightGBMMASESIM1, LightGBMRMSSESIM1))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM1)
prices <- as.data.frame(pricesSIM1)

holdingcostsSIM1 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMSIM1 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM1)),ncol=nrow(testSIM1))
for (i in 1:ncol(testSIM1)){
  leadtimedemand <- mean(t(predictionLightGBMSIM1[,i]))
  stdev <- std(t(trainSIM1[i])[!is.na(t(trainSIM1[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM1 <- rbind(holdingcostsSIM1,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM1[i])[(t(testSIM1[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM1[i])[(t(testSIM1[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMSIM1 <- rbind(fillrateLightGBMSIM1, averagefillrateitem)
}
LightGBMholdingSIM1 <- colSums(holdingcostsSIM1[-1,])
ProcentualLightGBMholdingSIM1 <- LightGBMholdingSIM1/LightGBMholdingSIM1[1]
achievedfillrateLightGBMSIM1 <- colMeans(fillrateLightGBMSIM1[-1,])
ServicelevelLightGBMSIM1 <- data.frame(achievedfillrate = achievedfillrateLightGBMSIM1, holding = LightGBMholdingSIM1, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")


####          SIM2 LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM2)){
  nndatavector <- nntrainSIM2[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testSIM2)
predictionLightGBMSIM2 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainSIM2)){
  input <- tail(nntrainSIM2[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM2)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM2[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMSIM2 <- rbind(predictionLightGBMSIM2, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM2 <- getNormParameters(nndataSIM2)
predictionLightGBMSIM2 <- t(predictionLightGBMSIM2)
predictionLightGBMSIM2 <- predictionLightGBMSIM2[,-1]
# Final predictions.
predictionLightGBMSIM2 <- denormalizeData(predictionLightGBMSIM2, normParamsSIM2)

#Saving the output data
save(predictionLightGBMSIM2,file="c:/Daan/predictionLightGBMSIM22.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMSIM2.Rda")

# Forecasting accuracy measures.
LightGBMMSESIM2 <- NULL
LightGBMMASESIM2 <- NULL
LightGBMRMSSESIM2 <- NULL
for (i in 1:ncol(testSIM2)){
  LightGBMMSESIM2 <- cbind(LightGBMMSESIM2,  MSE(t(testSIM2[i]),t(predictionLightGBMSIM2[,i])))
  LightGBMMASESIM2 <- cbind(LightGBMMASESIM2,  MASE(t(testSIM2[i]),t(predictionLightGBMSIM2[,i]), mean(abs(t(trainSIM2[i])))))
  LightGBMRMSSESIM2 <- cbind(LightGBMRMSSESIM2,  RMSSE(t(testSIM2[i]),t(predictionLightGBMSIM2[,i]), mean(abs(t(trainSIM2[i])))))
}
LightGBMMSESIM2 <- mean(LightGBMMSESIM2)
LightGBMMASESIM2 <- mean(LightGBMMASESIM2)
LightGBMRMSSESIM2 <- mean(LightGBMRMSSESIM2)
print(c("SIM2", LightGBMMSESIM2, LightGBMMASESIM2, LightGBMRMSSESIM2))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM2)
prices <- as.data.frame(pricesSIM2)

holdingcostsSIM2 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMSIM2 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM2)),ncol=nrow(testSIM2))
for (i in 1:ncol(testSIM2)){
  leadtimedemand <- mean(t(predictionLightGBMSIM2[,i]))
  stdev <- std(t(trainSIM2[i])[!is.na(t(trainSIM2[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM2 <- rbind(holdingcostsSIM2,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM2[i])[(t(testSIM2[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM2[i])[(t(testSIM2[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMSIM2 <- rbind(fillrateLightGBMSIM2, averagefillrateitem)
}
LightGBMholdingSIM2 <- colSums(holdingcostsSIM2[-1,])
ProcentualLightGBMholdingSIM2 <- LightGBMholdingSIM2/LightGBMholdingSIM2[1]
achievedfillrateLightGBMSIM2 <- colMeans(fillrateLightGBMSIM2[-1,])
ServicelevelLightGBMSIM2 <- data.frame(achievedfillrate = achievedfillrateLightGBMSIM2, holding = LightGBMholdingSIM2, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")




####          SIM3 LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM3)){
  nndatavector <- nntrainSIM3[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testSIM3)
predictionLightGBMSIM3 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainSIM3)){
  input <- tail(nntrainSIM3[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM3)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM3[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMSIM3 <- rbind(predictionLightGBMSIM3, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM3 <- getNormParameters(nndataSIM3)
predictionLightGBMSIM3 <- t(predictionLightGBMSIM3)
predictionLightGBMSIM3 <- predictionLightGBMSIM3[,-1]
# Final predictions.
predictionLightGBMSIM3 <- denormalizeData(predictionLightGBMSIM3, normParamsSIM3)

#Saving the output data
save(predictionLightGBMSIM3,file="c:/Daan/predictionLightGBMSIM32.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMSIM3.Rda")

# Forecasting accuracy measures.
LightGBMMSESIM3 <- NULL
LightGBMMASESIM3 <- NULL
LightGBMRMSSESIM3 <- NULL
for (i in 1:ncol(testSIM3)){
  LightGBMMSESIM3 <- cbind(LightGBMMSESIM3,  MSE(t(testSIM3[i]),t(predictionLightGBMSIM3[,i])))
  LightGBMMASESIM3 <- cbind(LightGBMMASESIM3,  MASE(t(testSIM3[i]),t(predictionLightGBMSIM3[,i]), mean(abs(t(trainSIM3[i])))))
  LightGBMRMSSESIM3 <- cbind(LightGBMRMSSESIM3,  RMSSE(t(testSIM3[i]),t(predictionLightGBMSIM3[,i]), mean(abs(t(trainSIM3[i])))))
}
LightGBMMSESIM3 <- mean(LightGBMMSESIM3)
LightGBMMASESIM3 <- mean(LightGBMMASESIM3)
LightGBMRMSSESIM3 <- mean(LightGBMRMSSESIM3)
print(c("SIM3", LightGBMMSESIM3, LightGBMMASESIM3, LightGBMRMSSESIM3))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM3)
prices <- as.data.frame(pricesSIM3)

holdingcostsSIM3 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMSIM3 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM3)),ncol=nrow(testSIM3))
for (i in 1:ncol(testSIM3)){
  leadtimedemand <- mean(t(predictionLightGBMSIM3[,i]))
  stdev <- std(t(trainSIM3[i])[!is.na(t(trainSIM3[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM3 <- rbind(holdingcostsSIM3,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM3[i])[(t(testSIM3[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM3[i])[(t(testSIM3[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMSIM3 <- rbind(fillrateLightGBMSIM3, averagefillrateitem)
}
LightGBMholdingSIM3 <- colSums(holdingcostsSIM3[-1,])
ProcentualLightGBMholdingSIM3 <- LightGBMholdingSIM3/LightGBMholdingSIM3[1]
achievedfillrateLightGBMSIM3 <- colMeans(fillrateLightGBMSIM3[-1,])
ServicelevelLightGBMSIM3 <- data.frame(achievedfillrate = achievedfillrateLightGBMSIM3, holding = LightGBMholdingSIM3, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")



####          SIM4 LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainSIM4)){
  nndatavector <- nntrainSIM4[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testSIM4)
predictionLightGBMSIM4 <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainSIM4)){
  input <- tail(nntrainSIM4[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testSIM4)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestSIM4[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMSIM4 <- rbind(predictionLightGBMSIM4, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsSIM4 <- getNormParameters(nndataSIM4)
predictionLightGBMSIM4 <- t(predictionLightGBMSIM4)
predictionLightGBMSIM4 <- predictionLightGBMSIM4[,-1]
# Final predictions.
predictionLightGBMSIM4 <- denormalizeData(predictionLightGBMSIM4, normParamsSIM4)

#Saving the output data
save(predictionLightGBMSIM4,file="c:/Daan/predictionLightGBMSIM42.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMSIM4.Rda")

# Forecasting accuracy measures.
LightGBMMSESIM4 <- NULL
LightGBMMASESIM4 <- NULL
LightGBMRMSSESIM4 <- NULL
for (i in 1:ncol(testSIM4)){
  LightGBMMSESIM4 <- cbind(LightGBMMSESIM4,  MSE(t(testSIM4[i]),t(predictionLightGBMSIM4[,i])))
  LightGBMMASESIM4 <- cbind(LightGBMMASESIM4,  MASE(t(testSIM4[i]),t(predictionLightGBMSIM4[,i]), mean(abs(t(trainSIM4[i])))))
  LightGBMRMSSESIM4 <- cbind(LightGBMRMSSESIM4,  RMSSE(t(testSIM4[i]),t(predictionLightGBMSIM4[,i]), mean(abs(t(trainSIM4[i])))))
}
LightGBMMSESIM4 <- mean(LightGBMMSESIM4)
LightGBMMASESIM4 <- mean(LightGBMMASESIM4)
LightGBMRMSSESIM4 <- mean(LightGBMRMSSESIM4)
print(c("SIM4", LightGBMMSESIM4, LightGBMMASESIM4, LightGBMRMSSESIM4))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesSIM4)
prices <- as.data.frame(pricesSIM4)

holdingcostsSIM4 <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMSIM4 <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testSIM4)),ncol=nrow(testSIM4))
for (i in 1:ncol(testSIM4)){
  leadtimedemand <- mean(t(predictionLightGBMSIM4[,i]))
  stdev <- std(t(trainSIM4[i])[!is.na(t(trainSIM4[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsSIM4 <- rbind(holdingcostsSIM4,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testSIM4[i])[(t(testSIM4[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testSIM4[i])[(t(testSIM4[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMSIM4 <- rbind(fillrateLightGBMSIM4, averagefillrateitem)
}
LightGBMholdingSIM4 <- colSums(holdingcostsSIM4[-1,])
ProcentualLightGBMholdingSIM4 <- LightGBMholdingSIM4/LightGBMholdingSIM4[1]
achievedfillrateLightGBMSIM4 <- colMeans(fillrateLightGBMSIM4[-1,])
ServicelevelLightGBMSIM4 <- data.frame(achievedfillrate = achievedfillrateLightGBMSIM4, holding = LightGBMholdingSIM4, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")




####          MAN LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainMAN)){
  nndatavector <- nntrainMAN[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testMAN)
predictionLightGBMMAN <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainMAN)){
  input <- tail(nntrainMAN[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testMAN)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestMAN[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMMAN <- rbind(predictionLightGBMMAN, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsMAN <- getNormParameters(nndataMAN)
predictionLightGBMMAN <- t(predictionLightGBMMAN)
predictionLightGBMMAN <- predictionLightGBMMAN[,-1]
# Final predictions.
predictionLightGBMMAN <- denormalizeData(predictionLightGBMMAN, normParamsMAN)

#Saving the output data
save(predictionLightGBMMAN,file="c:/Daan/predictionLightGBMMAN2.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMMAN.Rda")

# Forecasting accuracy measures.
LightGBMMSEMAN <- NULL
LightGBMMASEMAN <- NULL
LightGBMRMSSEMAN <- NULL
for (i in 1:ncol(testMAN)){
  LightGBMMSEMAN <- cbind(LightGBMMSEMAN,  MSE(t(testMAN[i]),t(predictionLightGBMMAN[,i])))
  LightGBMMASEMAN <- cbind(LightGBMMASEMAN,  MASE(t(testMAN[i]),t(predictionLightGBMMAN[,i]), mean(abs(t(trainMAN[i])))))
  LightGBMRMSSEMAN <- cbind(LightGBMRMSSEMAN,  RMSSE(t(testMAN[i]),t(predictionLightGBMMAN[,i]), mean(abs(t(trainMAN[i])))))
}
LightGBMMSEMAN <- mean(LightGBMMSEMAN)
LightGBMMASEMAN <- mean(LightGBMMASEMAN)
LightGBMRMSSEMAN <- mean(LightGBMRMSSEMAN)
print(c("MAN", LightGBMMSEMAN, LightGBMMASEMAN, LightGBMRMSSEMAN))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesMAN)
prices <- as.data.frame(pricesMAN)

holdingcostsMAN <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMMAN <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testMAN)),ncol=nrow(testMAN))
for (i in 1:ncol(testMAN)){
  leadtimedemand <- mean(t(predictionLightGBMMAN[,i]))
  stdev <- std(t(trainMAN[i])[!is.na(t(trainMAN[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsMAN <- rbind(holdingcostsMAN,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testMAN[i])[(t(testMAN[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testMAN[i])[(t(testMAN[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMMAN <- rbind(fillrateLightGBMMAN, averagefillrateitem)
}
LightGBMholdingMAN <- colSums(holdingcostsMAN[-1,])
ProcentualLightGBMholdingMAN <- LightGBMholdingMAN/LightGBMholdingMAN[1]
achievedfillrateLightGBMMAN <- colMeans(fillrateLightGBMMAN[-1,])
ServicelevelLightGBMMAN <- data.frame(achievedfillrate = achievedfillrateLightGBMMAN, holding = LightGBMholdingMAN, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")



####          BRAF LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainBRAF)){
  nndatavector <- nntrainBRAF[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testBRAF)
predictionLightGBMBRAF <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainBRAF)){
  input <- tail(nntrainBRAF[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testBRAF)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestBRAF[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMBRAF <- rbind(predictionLightGBMBRAF, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsBRAF <- getNormParameters(nndataBRAF)
predictionLightGBMBRAF <- t(predictionLightGBMBRAF)
predictionLightGBMBRAF <- predictionLightGBMBRAF[,-1]
# Final predictions.
predictionLightGBMBRAF <- denormalizeData(predictionLightGBMBRAF, normParamsBRAF)

#Saving the output data
save(predictionLightGBMBRAF,file="c:/Daan/predictionLightGBMBRAF2.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMBRAF.Rda")

# Forecasting accuracy measures.
LightGBMMSEBRAF <- NULL
LightGBMMASEBRAF <- NULL
LightGBMRMSSEBRAF <- NULL
for (i in 1:ncol(testBRAF)){
  LightGBMMSEBRAF <- cbind(LightGBMMSEBRAF,  MSE(t(testBRAF[i]),t(predictionLightGBMBRAF[,i])))
  LightGBMMASEBRAF <- cbind(LightGBMMASEBRAF,  MASE(t(testBRAF[i]),t(predictionLightGBMBRAF[,i]), mean(abs(t(trainBRAF[i])))))
  LightGBMRMSSEBRAF <- cbind(LightGBMRMSSEBRAF,  RMSSE(t(testBRAF[i]),t(predictionLightGBMBRAF[,i]), mean(abs(t(trainBRAF[i])))))
}
LightGBMMSEBRAF <- mean(LightGBMMSEBRAF)
LightGBMMASEBRAF <- mean(LightGBMMASEBRAF)
LightGBMRMSSEBRAF <- mean(LightGBMRMSSEBRAF)
print(c("BRAF", LightGBMMSEBRAF, LightGBMMASEBRAF, LightGBMRMSSEBRAF))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesBRAF)
prices <- as.data.frame(pricesBRAF)

holdingcostsBRAF <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMBRAF <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testBRAF)),ncol=nrow(testBRAF))
for (i in 1:ncol(testBRAF)){
  leadtimedemand <- mean(t(predictionLightGBMBRAF[,i]))
  stdev <- std(t(trainBRAF[i])[!is.na(t(trainBRAF[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsBRAF <- rbind(holdingcostsBRAF,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testBRAF[i])[(t(testBRAF[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testBRAF[i])[(t(testBRAF[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMBRAF <- rbind(fillrateLightGBMBRAF, averagefillrateitem)
}
LightGBMholdingBRAF <- colSums(holdingcostsBRAF[-1,])
ProcentualLightGBMholdingBRAF <- LightGBMholdingBRAF/LightGBMholdingBRAF[1]
achievedfillrateLightGBMBRAF <- colMeans(fillrateLightGBMBRAF[-1,])
ServicelevelLightGBMBRAF <- data.frame(achievedfillrate = achievedfillrateLightGBMBRAF, holding = LightGBMholdingBRAF, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")





####          AUTO LightGBM method application.         ####

# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainAUTO)){
  nndatavector <- nntrainAUTO[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testAUTO)
predictionLightGBMAUTO <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainAUTO)){
  input <- tail(nntrainAUTO[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testAUTO)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestAUTO[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMAUTO <- rbind(predictionLightGBMAUTO, outputs)
}

# Denormalizing the data for evaluation of forecasts.
normParamsAUTO <- getNormParameters(nndataAUTO)
predictionLightGBMAUTO <- t(predictionLightGBMAUTO)
predictionLightGBMAUTO <- predictionLightGBMAUTO[,-1]
# Final predictions.
predictionLightGBMAUTO <- denormalizeData(predictionLightGBMAUTO, normParamsAUTO)

#Saving the output data
save(predictionLightGBMAUTO,file="c:/Daan/predictionLightGBMAUTO2.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMAUTO.Rda")

# Forecasting accuracy measures.
LightGBMMSEAUTO <- NULL
LightGBMMASEAUTO <- NULL
LightGBMRMSSEAUTO <- NULL
for (i in 1:ncol(testAUTO)){
  LightGBMMSEAUTO <- cbind(LightGBMMSEAUTO,  MSE(t(testAUTO[i]),t(predictionLightGBMAUTO[,i])))
  LightGBMMASEAUTO <- cbind(LightGBMMASEAUTO,  MASE(t(testAUTO[i]),t(predictionLightGBMAUTO[,i]), mean(abs(t(trainAUTO[i])))))
  LightGBMRMSSEAUTO <- cbind(LightGBMRMSSEAUTO,  RMSSE(t(testAUTO[i]),t(predictionLightGBMAUTO[,i]), mean(abs(t(trainAUTO[i])))))
}
LightGBMMSEAUTO <- mean(LightGBMMSEAUTO)
LightGBMMASEAUTO <- mean(LightGBMMASEAUTO)
LightGBMRMSSEAUTO <- mean(LightGBMRMSSEAUTO)
print(c("AUTO", LightGBMMSEAUTO, LightGBMMASEAUTO, LightGBMRMSSEAUTO))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesAUTO)
prices <- as.data.frame(pricesAUTO)

holdingcostsAUTO <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMAUTO <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testAUTO)),ncol=nrow(testAUTO))
for (i in 1:ncol(testAUTO)){
  leadtimedemand <- mean(t(predictionLightGBMAUTO[,i]))
  stdev <- std(t(trainAUTO[i])[!is.na(t(trainAUTO[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsAUTO <- rbind(holdingcostsAUTO,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testAUTO[i])[(t(testAUTO[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testAUTO[i])[(t(testAUTO[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMAUTO <- rbind(fillrateLightGBMAUTO, averagefillrateitem)
}
LightGBMholdingAUTO <- colSums(holdingcostsAUTO[-1,])
ProcentualLightGBMholdingAUTO <- LightGBMholdingAUTO/LightGBMholdingAUTO[1]
achievedfillrateLightGBMAUTO <- colMeans(fillrateLightGBMAUTO[-1,])
ServicelevelLightGBMAUTO <- data.frame(achievedfillrate = achievedfillrateLightGBMAUTO, holding = LightGBMholdingAUTO, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")


####          OIL LightGBM method application.         ####


# Creating running indices and new variables.
lightgbmdata <- matrix(ncol = 6)

# For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
for (i in 1:ncol(nntrainOIL)){
  nndatavector <- nntrainOIL[,i]
  for (t in 1:length(nndatavector)){
    inputs <- nndatavector[(0+t):(4+t)]
    outputs <- nndatavector[5+t]
    lightgbmdata <- rbind(lightgbmdata, c(inputs,outputs))  
  }}

# Removing the rows with NA's. Since the data is large enough already to learn the characteristics of the data, this should not decrease accuracy.
lightgbmdata <- na.omit(lightgbmdata)
lightgbmdata <- as.data.frame(lightgbmdata)

# Setting the hyperparameters for the LightGBM model, based on the kaggle method.
p <- list(objective = "regression",
          metric ="rmse",
          boosting = "gbdt",
          force_row_wise = TRUE,
          learning_rate = 0.075,
          num_leaves = 128,
          min_data = 100,
          sub_feature = 0.8,
          sub_row = 0.75,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          nthread = 4)

# Training the LightGBM algorithm.
model <- lightgbm(
  data = as.matrix(lightgbmdata[,(1:5)])
  , params = p
  , label = lightgbmdata$V6
  , nrounds = 12000,
  early_stopping_rounds = 400,
  eval_freq = 400)

# Creating the output variable.
h <- nrow(testOIL)
predictionLightGBMOIL <- matrix(ncol = h)

# Predicting the forecast horizon with as input the last five values of a series and saving it as the predictiondata.
for (i in 1:ncol(nntrainOIL)){
  input <- tail(nntrainOIL[,i],n=5)
  outputs <- vector()
  for (t in 1:nrow(testOIL)) {
    output <- predict(model, t(input))
    outputs <- c(outputs, output)
    input <- c(input,nntestOIL[t,i])
    input <- tail(input, n=5)
  }
  predictionLightGBMOIL <- rbind(predictionLightGBMOIL, outputs)
}


# Saving the predictions without transposed and without column 1.
predictionLightGBMOIL <- t(predictionLightGBMOIL)
predictionLightGBMOIL <- predictionLightGBMOIL[,-1]

# Denormalizing the data for evaluation of forecasts.
for (i in 1:ncol(predictionLightGBMOIL)) {
  predictionLightGBMOIL[,i] <- predictionLightGBMOIL[,i] * (max(OIL[i], na.rm=TRUE) - min(OIL[i], na.rm=TRUE)) + min(OIL[i], na.rm=TRUE)
}

#Saving the output data
save(predictionLightGBMOIL,file="c:/Daan/predictionLightGBMOIL2.Rda")
#Reload the output data
#load("c:/Daan/predictionLightGBMOIL.Rda")

# Forecasting accuracy measures.
LightGBMMSEOIL <- NULL
LightGBMMASEOIL <- NULL
LightGBMRMSSEOIL <- NULL
for (i in 1:ncol(testOIL)){
  LightGBMMSEOIL <- cbind(LightGBMMSEOIL,  MSE(t(testOIL[i]),t(predictionLightGBMOIL[,i])))
  LightGBMMASEOIL <- cbind(LightGBMMASEOIL,  MASE(t(testOIL[i]),t(predictionLightGBMOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
  LightGBMRMSSEOIL <- cbind(LightGBMRMSSEOIL,  RMSSE(t(testOIL[i]),t(predictionLightGBMOIL[,i]), mean(abs(t(trainOIL[i])),na.rm=TRUE)))
}
LightGBMMSEOIL <- mean(LightGBMMSEOIL,na.rm=TRUE)
LightGBMMASEOIL <- mean(LightGBMMASEOIL,na.rm=TRUE)
LightGBMRMSSEOIL <- mean(LightGBMRMSSEOIL,na.rm=TRUE)
print(c("OIL", LightGBMMSEOIL, LightGBMMASEOIL, LightGBMRMSSEOIL))

# Inventory performance measures.
targetfillrates <- c(qnorm(0.75),qnorm(0.8),qnorm(0.85),qnorm(0.9),qnorm(0.95),qnorm(0.99),qnorm(0.999999))
leadtimes <- as.data.frame(leadtimesOIL)
prices <- as.data.frame(pricesOIL)

holdingcostsOIL <- matrix(c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999),ncol = 7)
fillrateLightGBMOIL <- matrix(ncol=7)
averagefillrateitem <- matrix(ncol=7)
fillratefiller <- matrix(rep(NA,nrow(testOIL)),ncol=nrow(testOIL))
for (i in 1:ncol(testOIL)){
  leadtimedemand <- mean(t(predictionLightGBMOIL[,i]))
  stdev <- std(t(trainOIL[i])[!is.na(t(trainOIL[i]))])
  stocklevelR <- (targetfillrates * stdev + leadtimedemand)
  holdingcostsOIL <- rbind(holdingcostsOIL,(0.25 * (stocklevelR * prices[,i])))
  for (j in 1:length(targetfillrates)){
    fillratefiller <- stocklevelR[j] / t(testOIL[i])[(t(testOIL[i]) > 0)]
    fillratefiller <- c(fillratefiller,t(testOIL[i])[(t(testOIL[i]) == 0)])
    fillratefiller[fillratefiller >= 1] <- 1
    fillratefiller[fillratefiller == 0] <- 1
    averagefillrateitem[j] <- mean(fillratefiller)
  }
  fillrateLightGBMOIL <- rbind(fillrateLightGBMOIL, averagefillrateitem)
}
LightGBMholdingOIL <- colSums(holdingcostsOIL[-1,])
ProcentualLightGBMholdingOIL <- LightGBMholdingOIL/LightGBMholdingOIL[1]
achievedfillrateLightGBMOIL <- colMeans(fillrateLightGBMOIL[-1,])
ServicelevelLightGBMOIL <- data.frame(achievedfillrate = achievedfillrateLightGBMOIL, holding = LightGBMholdingOIL, targetfillrates = c(0.75,0.8,0.85,0.9,0.95,0.99,0.9999999), method = "LightGBM")



print("time LightGBM total")
proc.time() - ptm

####          Tradeoff curves ####


# Creating the tradeoff curves for SIM1.
servicelevelsSIM1 <- rbind(ServicelevelCrostonSIM1, ServicelevelSESSIM1,ServicelevelSBASIM1,ServicelevelTSBSIM1,ServicelevelnnSIM1,ServicelevelLightGBMSIM1,ServicelevelWillemainSIM1)
# Saving the service level data.
save(servicelevelsSIM1,file="c:/Daan/servicelevelsSIM1.Rda")
kable(servicelevelsSIM1, "latex")

ggplot(servicelevelsSIM1, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedSIM1.png")

ggplot(servicelevelsSIM1, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingSIM1.png")


# Creating the tradeoff curves for SIM2.
servicelevelsSIM2 <- rbind(ServicelevelCrostonSIM2, ServicelevelSESSIM2,ServicelevelSBASIM2,ServicelevelTSBSIM2,ServicelevelnnSIM2,ServicelevelLightGBMSIM2,ServicelevelWillemainSIM2)
# Saving the service level data.
save(servicelevelsSIM2,file="c:/Daan/servicelevelsSIM2.Rda")
kable(servicelevelsSIM2, "latex")

ggplot(servicelevelsSIM2, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedSIM2.png")

ggplot(servicelevelsSIM2, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingSIM2.png")


# Creating the tradeoff curves for SIM3.
servicelevelsSIM3 <- rbind(ServicelevelCrostonSIM3, ServicelevelSESSIM3,ServicelevelSBASIM3,ServicelevelTSBSIM3,ServicelevelnnSIM3,ServicelevelLightGBMSIM3,ServicelevelWillemainSIM3)
# Saving the service level data.
save(servicelevelsSIM3,file="c:/Daan/servicelevelsSIM3.Rda")
kable(servicelevelsSIM3, "latex")

ggplot(servicelevelsSIM3, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedSIM3.png")

ggplot(servicelevelsSIM3, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingSIM3.png")


# Creating the tradeoff curves for SIM4.
servicelevelsSIM4 <- rbind(ServicelevelCrostonSIM4, ServicelevelSESSIM4,ServicelevelSBASIM4,ServicelevelTSBSIM4,ServicelevelnnSIM4,ServicelevelLightGBMSIM4,ServicelevelWillemainSIM4)
# Saving the service level data.
save(servicelevelsSIM4,file="c:/Daan/servicelevelsSIM4.Rda")
kable(servicelevelsSIM4, "latex")

ggplot(servicelevelsSIM4, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedSIM4.png")

ggplot(servicelevelsSIM4, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingSIM4.png")


# Creating the tradeoff curves for MAN.
servicelevelsMAN <- rbind(ServicelevelCrostonMAN, ServicelevelSESMAN,ServicelevelSBAMAN,ServicelevelTSBMAN,ServicelevelnnMAN,ServicelevelLightGBMMAN,ServicelevelWillemainMAN)
# Saving the service level data.
save(servicelevelsMAN,file="c:/Daan/servicelevelsMAN.Rda")
kable(servicelevelsMAN, "latex")

ggplot(servicelevelsMAN, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedMAN.png")

ggplot(servicelevelsMAN, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingMAN.png")


# Creating the tradeoff curves for BRAF.
servicelevelsBRAF <- rbind(ServicelevelCrostonBRAF, ServicelevelSESBRAF,ServicelevelSBABRAF,ServicelevelTSBBRAF,ServicelevelnnBRAF,ServicelevelLightGBMBRAF,ServicelevelWillemainBRAF)
# Saving the service level data.
save(servicelevelsBRAF,file="c:/Daan/servicelevelsBRAF.Rda")
kable(servicelevelsMAN, "latex")

ggplot(servicelevelsBRAF, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedBRAF.png")

ggplot(servicelevelsBRAF, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingBRAF.png")


# Creating the tradeoff curves for AUTO.
servicelevelsAUTO <- rbind(ServicelevelCrostonAUTO, ServicelevelSESAUTO,ServicelevelSBAAUTO,ServicelevelTSBAUTO,ServicelevelnnAUTO,ServicelevelLightGBMAUTO,ServicelevelWillemainAUTO)
# Saving the service level data.
save(servicelevelsAUTO,file="c:/Daan/servicelevelsAUTO.Rda")
kable(servicelevelsAUTO, "latex")

ggplot(servicelevelsAUTO, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedAUTO.png")

ggplot(servicelevelsAUTO, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingAUTO.png")


# Creating the tradeoff curves for OIL.
servicelevelsOIL <- rbind(ServicelevelCrostonOIL, ServicelevelSESOIL,ServicelevelSBAOIL,ServicelevelTSBOIL,ServicelevelnnOIL,ServicelevelLightGBMOIL,ServicelevelWillemainOIL)
# Saving the service level data.
save(servicelevelsOIL,file="c:/Daan/servicelevelsOIL.Rda")
kable(servicelevelsOIL, "latex")

ggplot(servicelevelsOIL, aes(x = targetfillrates, y = achievedfillrate, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Target fill rate vs. Achieved fill rate")+ xlab("Target fill rate") + ylab("Achieved fill rate")
ggsave("targetvsachievedOIL.png")

ggplot(servicelevelsOIL, aes(x = achievedfillrate, y = holding, group = method)) +
  geom_line(aes(color=method)) +
  geom_point(size = 0.5) +
  ggtitle("Achieved fill rate vs. Inventory holding costs")+ xlab("Achieved fill rate") + ylab("Inventory holding costs")
ggsave("achievedvsholdingOIL.png")




####          time per method  ####


#time Croston   : 18660s  =     311   minutes -> 0.433s
#time SES       : 18540s  =     309   minutes -> 0,430s
#time SBA       : 18540s  =     309   minutes -> 0,430s
#time TSB       : 2871s   =     48    minutes -> 0,067s
#time Willemain : 71880s  =     1198  minutes -> 1,669s
#time MLP       : 10485s  =     175   minutes -> 0,243s
#time LightGBM  : 14400s  =     240   minutes -> 0,334s

#total items is 43068





