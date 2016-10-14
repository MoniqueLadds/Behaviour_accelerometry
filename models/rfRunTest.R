rm(list = ls())
setwd("I:/Behaviour accelerometry/behaviour_accelerometry-master/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
library("parallel")
library("vegan")

load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))

featureData<-outputData[!outputData$SealName=="mav"&!outputData$SealName=="miri",]
testData<-outputData[outputData$SealName=="mav"|outputData$SealName=="miri",]


###Testing### 
#read in the testing file
source('models/accRun_test.r')

Hz=25
epochs=13


rfModel <- accRun(featureData,
                  testData,
                  Model = "RF",
                  codeTest = TRUE,
                  Parallel = TRUE,
                  K = 10,
                  SAVE = TRUE,
                  Dummies = FALSE,
                  Cores = 1,
                  printSummary = TRUE,
                  classMax = 3000)

