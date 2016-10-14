##Add the best model parameters to the data and run

load("model_outputs/testing-leavetwoout/SVM Model with linear kernel, 10-fold CV, on 2016-10-01.RData")
outputData$bestParams

#read in the testing file
source('models/accRunsvm.r')


Hz=25
epochs=13

svmLinearModel <- accRun(featureData,
                   testData,
                   #randSeal="miri mav",
                   Model = "SVM",
                   codeTest = TRUE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 3000,
                   Kernel = "linear")


testData <- testData[complete.cases(testData),]
##### remove the dodgey variables
testData <- testData[, !(names(testData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
testData <- testData[testData$EventIds!="Other",]
testData <- testData[!is.na(testData$EventIds),]
testData <- testData[testData$EventIds != "NA",]

testData$finalPreds<-(rlrModel$finalPreds)
conf.mat<-table(testData$EventIds,testData$finalPreds)

prop.table(conf.mat,1)  


load("model_outputs/testing-leavetwoout/SVM Model with linear kernel, 10-fold CV, on 2016-10-01.RData")
outputData$bestParams

#read in the testing file
source('models/accRun_test.r')


Hz=25
epochs=13

svmLinearModel <- accRun(featureData,
                         testData,
                         randSeal="miri mav",
                         Model = "SVM",
                         codeTest = TRUE,
                         Parallel = FALSE,
                         K = 10,
                         SAVE = TRUE,
                         Dummies = FALSE,
                         Cores = 1,
                         printSummary = TRUE,
                         classMax = 3000,
                         Kernel = "linear")
