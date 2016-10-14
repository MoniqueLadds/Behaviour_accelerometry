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


###########################################################################
##Training
###Test the model to find the best parameters

#read in the testing file

Hz = 25
epochs = 13
classMax <- 3000
folds <- 10
codeTest <- TRUE
Parallel <- TRUE
SAVE <- TRUE
Cores <- 4
PrintSummary = TRUE
Dummies <- TRUE

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
codeTest <- FALSE

source("models/svmmodels/accRun_test.r")
startTime <- Sys.time()
cat("\n##### starting SVM Linear model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData = featureData,
                      testData = testData,
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

# Travelling   Foraging    Resting   Grooming 
# 0.3238808  0.7354513  0.7079716  0.7560137


source("models/accRun_test.r")
startTime <- Sys.time()
cat("\n##### starting SVM Linear model at:", as.character.POSIXt(startTime), "#####\n")
svmLinModel <- accRun(featureData = featureData,
                      testData = testData,
                      Kernel = "linear",
                      Model = "SVM",
                      codeTest = T,
                      Parallel = Parallel,
                      K = folds,
                      SAVE = SAVE,
                      Dummies = TRUE,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM Linear model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")

# Travelling   Foraging    Resting   Grooming 
# 0.6152685  0.8399644  0.6531176  0.7285223 
# [1] 0.6815084

####### radial
codeTest <- FALSE

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

# Travelling   Foraging    Resting   Grooming 
# 0.5441775  0.6889846  0.6700868  0.6013746 
# [1] 0.5970559


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
                      Dummies = TRUE,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM radial model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")
# Travelling   Foraging    Resting   Grooming 
# 0.6746094  0.7727138  0.5730071  0.4879725 
# [1] 0.6886872

####### poly
codeTest <- FALSE

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

# Travelling   Foraging    Resting   Grooming 
# 0.5087305  0.7228325  0.6397001  0.7147766 
# [1] 0.5826981


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
                      Dummies = TRUE,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM polynomial model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")
# Travelling   Foraging    Resting   Grooming 
# 0.7071025  0.8693587  0.4104183  0.6460481 
# [1] 0.7201452

# sigmoid
codeTest <- FALSE

source("models/accRun_test.r")
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

# Travelling   Foraging    Resting   Grooming 
# 0.3026126  0.7219418  0.7020521  0.7731959 
# [1] 0.4628756


source("models/accRun_test.r")
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
                      Dummies = TRUE,
                      Cores = Cores,
                      printSummary = PrintSummary,
                      classMax = classMax)
cat("\n##### finished SVM sigmoid model at:", as.character.POSIXt(startTime), "#####\n")
cat("##### execution time took:", as.character.POSIXt(difftime(Sys.time() , startTime)), "#####\n")
# Travelling   Foraging    Resting   Grooming 
# 0.4002232  0.8338777  0.7146803  0.7766323 
# [1] 0.5545876
