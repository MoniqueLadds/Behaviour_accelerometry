##### xgboost test
rm(list = ls())
setwd("I:/Behaviour accelerometry/behaviour_accelerometry-master/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

source("models/behaviourCodeLoop.r")
#behaviourCodeLoop()

source("models/captiveProcessing.R")
#outputData<-featureProcessing(epochs=15)

##### load the data
load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))

#If random animals:

##split the data
#table(outputData$Place,outputData$EventIds)
#SealNames<-unique(outputData$SealName)
#randSeal<-sample(SealNames,2)
#featureData<-outputData[!outputData$SealName==randSeal,]
#testData<-outputData[outputData$SealName==randSeal,]

##If animals are selected.
featureData<-outputData[!outputData$SealName=="mav"&!outputData$SealName=="miri",]
testData<-outputData[outputData$SealName=="mav"|outputData$SealName=="miri",]

###########################################################################
##Training
###Test the model to find the best parameters

    #read in the testing file
    source('models/accRun.r')
    #code for GBM
    source("models/parallelCVxgb.r")


Hz=25
epochs=13

xgbModel <- accRun(outputData,
                   Model = "XGB",
                   codeTest = TRUE,
                   Parallel = TRUE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 4,
                   printSummary = TRUE,
                   classMax = 3000)


###########################################################################
##Testing

##Add the best model parameters to the data and run

  #read in the testing file
  source('models/accRun_test.r')
  #code for GBM
  source("models/parallelCVxgb.r")

Hz=25
epochs=15

xgbModel <- accRun(featureData,
                   testData,
                   randSeal = "mav miri",
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 3000)

testData <- testData[complete.cases(testData),]
##### remove the dodgey variables
testData <- testData[, !(names(testData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
testData <- testData[testData$EventIds!="Other",]
# browser()
testData <- testData[!is.na(testData$EventIds),]
testData <- testData[testData$EventIds != "NA",]
testDataELevels <- unique(as.character(testData$EventIds))

testData$finalPred<-outputData$finalPreds
table(testData$EventIds,testData$finalPred)
