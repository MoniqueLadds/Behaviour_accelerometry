##### xgboost test
rm(list = ls())
setwd("I:/Behaviour accelerometry/behaviour_accelerometry-master/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))

featureData<-outputData[!outputData$SealName=="mav"&!outputData$SealName=="miri",]
testData<-outputData[outputData$SealName=="mav"|outputData$SealName=="miri",]
    

    #read in the testing file
    source('models/accRun.r')


Hz=25
epochs=13

rlrModel <- accRun(featureData,
                  Model = "RLR",
                  codeTest = TRUE,
                  Parallel = TRUE,
                  K = 10,
                  SAVE = TRUE,
                  Dummies = TRUE,
                  Cores = 1,
                  printSummary = TRUE,
                  classMax = 3000)


##Add the best model parameters to the data and run

load("model_outputs/testing-leavetwoout/Dummies/RLR Model, mav miri, CM = 3000, Hz = 25, epochs = 13.RData")

outputData$bestParams

  #read in the testing file
  source('models/accRun_test.r')


Hz=25
epochs=13

rlrModel <- accRun(featureData,
                   testData,
                   randSeal="mav miri",
                   Model = "RLR",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 3000)


load("model_outputs/cross-validation/Dummies/RLR Test, mav miri, Hz = 25, epochs = 13.RData")

testData <- testData[complete.cases(testData),]
##### remove the dodgey variables
testData <- testData[, !(names(testData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
testData <- testData[testData$EventIds!="Other",]
testData <- testData[!is.na(testData$EventIds),]
testData <- testData[testData$EventIds != "NA",]

testData$finalPreds<-(outputData$finalPreds)
conf.mat<-table(testData$EventIds,testData$finalPreds)

prop.table(conf.mat,1)  
