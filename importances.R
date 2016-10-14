##### xgboost test
rm(list = ls())
setwd("I:/Behaviour accelerometry/behaviour_accelerometry-master/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

##### load the data
load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))
##If animals are selected.
featureData<-outputData[!outputData$SealName=="mav"&!outputData$SealName=="miri",]
testData<-outputData[outputData$SealName=="mav"|outputData$SealName=="miri",]

featureData <- featureData[complete.cases(featureData),]
##### remove the dodgey variables
featureData <- featureData[, !(names(featureData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
featureData <- featureData[featureData$EventIds!="Other",]
# browser()
featureData <- featureData[!is.na(featureData$EventIds),]
featureData <- featureData[featureData$EventIds != "NA",]
featureDataELevels <- unique(as.character(featureData$EventIds))
featureData <- featureData[, !(names(featureData) %in% c("FileDate", "SealName", "nRows", "X"))]

sealCharacteristic <- read.csv("I:/Juvenile fur seal energetics/animal_details2.csv",
                               stringsAsFactors=FALSE)

featureData <- merge(featureData, sealCharacteristic[,1:6], by = "SealName")

featureData$Place <- as.numeric(as.factor(featureData$Place))           
featureData$Gender <- as.numeric(as.factor(featureData$Gender))
featureData$species <- as.numeric(as.factor(featureData$species))
featureData$harsness <- factor(featureData$harsness)  

inputData<-featureData[,1:50]
inputData$Place<-as.numeric(as.factor(inputData$Place))
inputLabels<-featureData[,51]

dtrain <- xgb.DMatrix(data = data.matrix(inputData),
                      label = as.numeric(factor(inputLabels)) - 1)

bst <- xgboost(data = dtrain, max.depth = 5,
               eta = 0.01, nthread = 2, nround = 5000)

importance <- xgb.importance(feature_names = colnames(inputData), model = bst)
