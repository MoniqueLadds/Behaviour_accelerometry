##### xgboost test
rm(list = ls())
setwd("~/Documents/repos/behaviour_accelerometry/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
require("multicore")

source("models/behaviourCodeLoop.r")
#behaviourCodeLoop()

source("models/captiveProcessing.R")
#outputData<-featureProcessing(epochs=15)

source('models/accRunsvm.r')

##### load the data
load("featureData/outputData_4f_e13.RData")
is.na(outputData) <- do.call(cbind,lapply(outputData, is.infinite))

featureData <- outputData[!outputData$SealName == "mav" & !outputData$SealName == "miri",]
testData <- outputData[outputData$SealName == "mav" | outputData$SealName == "miri",]


###########################################################################
##Training
###Test the model to find the best parameters

#read in the testing file

Hz = 25
epochs = 15
classMax <- 3000
folds <- 10
codeTest <- FALSE
Parallel <- TRUE
SAVE <- TRUE
Cores <- 4
PrintSummary = TRUE
Dummies <- FALSE

# Kernels to iterate over
# "linear" 
# "polynomial"
# "radial" 
# "sigmoid"

startTime <- Sys.time()
cat("\n##### starting SVM Linear model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData,
                      Kernel = "linear",
                      Model = "SVM",
                      codeTest = codeTest,
                      Parallel = Parallel,
                      K = folds,
                      SAVE = SAVE,
                      Dummies = Dummies,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM Linear model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")

startTime <- Sys.time()
cat("\n##### starting SVM polynomial model at:", as.character.POSIXt(startTime), "#####\n")
svmPolyModel <- accRun(featureData,
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
cat("\n##### starting SVM radial model at:", as.character.POSIXt(startTime), "#####\n")
svmRadialModel <- accRun(featureData,
                         Kernel = "radial",
                         Model = "SVM",
                         codeTest = codeTest,
                         Parallel = Parallel,
                         K = folds,
                         SAVE = SAVE,
                         Dummies = Dummies,
                         Cores = Cores,
                         printSummary = PrintSummary,
                         classMax = classMax)
cat("\n##### finished SVM radial model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")

startTime <- Sys.time()
cat("\n##### starting SVM sigmoid model at:", as.character.POSIXt(startTime), "#####\n")
svmSigModel <- accRun(featureData,
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

###########################################################################
##Testing
