##### xgboost test
rm(list = ls())
setwd("I:/Behaviour accelerometry/behaviour_accelerometry-master/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
require("parallel")

source("models/behaviourCodeLoop.r")
#behaviourCodeLoop()

source("models/captiveProcessing.R")
#outputData<-featureProcessing(epochs=15)

source('models/accRun.r')

##### load the data
load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))

featureData <- outputData[!outputData$SealName == "mav" & !outputData$SealName == "miri",]
testData <- outputData[outputData$SealName == "mav" | outputData$SealName == "miri",]
print(table(testData$EventIds))


####### radial
Hz = 25
epochs = 13
classMax <- 3000
folds <- 10
codeTest <- TRUE
Parallel <- FALSE
SAVE <- TRUE
Cores <- 1
PrintSummary = TRUE
codeTest <- FALSE
Dummies <- FALSE

source("models/accRun_test.r")
startTime <- Sys.time()
cat("\n##### starting SVM radial model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData = featureData,
                      testData = testData,
                      Kernel = "radial",
                      Model = "SVM",
                      codeTest = codeTest,
                      Parallel = Parallel,
                      K = folds,
                      SAVE = SAVE,
                      Dummies = Dummies,
                      Cores = 1,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM radial model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")

source("models/accRun_test.r")
startTime <- Sys.time()
cat("\n##### starting SVM polynomial model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData = featureData,
                      testData = testData,
                      Kernel = "polynomial",
                      Model = "SVM",
                      codeTest = codeTest,
                      Parallel = Parallel,
                      K = folds,
                      SAVE = SAVE,
                      Dummies = Dummies,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM polynomial model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")


startTime <- Sys.time()
cat("\n##### starting SVM sigmoid model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData = featureData,
                      testData = testData,
                      Kernel = "sigmoid",
                      Model = "SVM",
                      codeTest = codeTest,
                      Parallel = Parallel,
                      K = folds,
                      SAVE = SAVE,
                      Dummies = Dummies,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM sigmoid model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")

