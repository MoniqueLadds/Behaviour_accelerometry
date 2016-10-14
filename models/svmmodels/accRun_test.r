accRun <- function(featureData,
                   testData,
                   Model = "RF",
                   codeTest = TRUE,
                   Parallel = TRUE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 4,
                   printSummary = FALSE,
                   classMax = 500,
                   Kernel = "linear"){
  library(caret)
  library(randomForest)
  library(glmnet)
  library(e1071)
  require(devtools)
  
  ###### remove NA's from the data
  # browser()
  featureData <- featureData[complete.cases(featureData),]
  ##### remove the dodgey variables
  featureData <- featureData[, !(names(featureData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
  
  testData <- testData[complete.cases(testData),]
  ##### remove the dodgey variables
  testData <- testData[, !(names(testData) %in% c("InvCovar.x","InvCovar.y","InvCovar.z"))]
  
  ##### feature imputation for missing values
  # mean imputation, nope no missing values! yay!
  
  if(printSummary){
    cat("##### raw data summary:\n")
    print(table(featureData$EventIds))
  }
  
  
  ##### remove class other  
  featureData <- featureData[featureData$EventIds!="Other",]
  # browser()
  featureData <- featureData[!is.na(featureData$EventIds),]
  featureData <- featureData[featureData$EventIds != "NA",]
  
  testDataELevels <- levels(testData$EventIds)
  testData$EventIds <- testDataELevels[testData$EventIds]
  testData <- testData[testData$EventIds!="Other",]
  
  #   testData <- testData[testData$EventIds %in% c("Foraging", "Travelling", "Resting", Grooming)),]
  # browser()
  testData <- testData[!is.na(testData$EventIds),]
  testData <- testData[testData$EventIds != "NA",]
  testDataELevels <- unique(as.character(testData$EventIds))
  
  testData$EventIds <- factor(x = testData$EventIds, levels = testDataELevels)
  
  ##### remove low length behaviours
  #featureData <- featureData[featureData$nRows > 5,]
  #testData <- testData[testData$nRows > 5,]
  
  if(Dummies){
    ##### add in the seal specific features
    sealCharacteristic <- read.csv("animal_details2.csv",
                                   stringsAsFactors=FALSE)
    
    featureData <- merge(featureData, sealCharacteristic[,1:5], by = "SealName")
    
    featureData$Place <- as.numeric(as.factor(featureData$Place))           
    featureData$Gender <- as.numeric(as.factor(featureData$Gender))
    featureData$species <- as.numeric(as.factor(featureData$species))
    #featureData$harsness <- factor(featureData$harsness)  
    
    testData <- merge(testData, sealCharacteristic[,1:5], by = "SealName")
    
    testData$Place <- as.numeric(as.factor(testData$Place))           
    testData$Gender <- as.numeric(as.factor(testData$Gender))
    testData$species <- as.numeric(as.factor(testData$species))
  }
  else{
    # browser()
    PlaceVector <- as.factor(featureData$Place)
    featureData$Place <- as.numeric(PlaceVector)
    testData$Place <- as.numeric(factor(x = testData$Place, levels = levels(PlaceVector)))
  }
  
  ##### Down sample the large classes
  set.seed(123, "L'Ecuyer")
  featureData$EventIds <- as.character(featureData$EventIds)
  uEventIds <- unique(featureData$EventIds)
  if(codeTest){
    classMax <- 200
    K <- 2
  }else{
    classMax <- classMax
  }
  
  sampledData <- NULL
  for(i in 1:length(uEventIds)){
    
    tempData <- featureData[featureData$EventIds==uEventIds[i],]
    nr <- nrow(tempData)
    
    if(nr>classMax){
      sampleIdx <- sample.int(n = nr, size = classMax)
      tempData <- tempData[sampleIdx,]
    }
    sampledData <- rbind(sampledData, tempData)
  }
  
  if(printSummary){
    cat("##### full training and testing data summary:\n")
    print(table(sampledData$EventIds))
  }
  
  testDataSplit=testData
  
  ##### remove the indetifier variables
  trainDataSplit <- sampledData[, !(names(sampledData) %in% c("FileDate", "SealName", "nRows"))]
  
  testDataSplit <- testDataSplit[,!(names(testDataSplit) %in%  c("FileDate", "SealName", "nRows"))]
  
  if(printSummary){
    cat("##### training split data summary:\n")
    print(table(trainDataSplit$EventIds))
    cat("##### testing split data summary:\n")
    print(table(testDataSplit$EventIds))
  }
  
  ##### creaete a function to evaluate the final fittness
  fittnessFun <- function(obs,pred){
    ACC <- sum(obs==pred)/length(pred)
  }
  
  switch(Model,
         "XGB" = {
           #source("parallelCVxgb.r")
           if(codeTest){
             K = K
             paramList = expand.grid(eta = 0.01,
                                     max.depth = 5,
                                     nrounds = 5000,
                                     subsample = c(0.8),
                                     colsample_bytree = 1,
                                     # lambda = seq(from = 0.5, to = 1, by = 0.5),
                                     # alpha = seq(from = 0.5, to = 1, by = 0.5), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
             
           }
           else{
             
             # paramList <- NULL
             paramList = expand.grid(eta = 10 ^ - seq(from = 2, to = 4, by = 1),
                                     max.depth = 1:5,
                                     nrounds = 5000,
                                     subsample = c(0.7, 0.8),
                                     colsample_bytree = 1,
                                     # lambda = seq(from = 0, to = 1, by = 0.2),
                                     # alpha = seq(from = 0, to = 1, by = 0.2), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
           }
           outputData <- parallelCVxgb(inputData = trainDataSplit,
                                       k = K,
                                       trainTargets = "EventIds",
                                       paramList = paramList,
                                       #Model= "XGB",
                                       testDataSplit = testDataSplit)
           # browser()
           finalPreds <- outputData$testDataSplit$predsTest
           finalModel <- outputData$trainData$predsTrain
           fittnessCV <- outputData
         },
         "RF" = {
           source("models/parallelCV6.R")
           
           if(codeTest){
             paramList <-  expand.grid(mtry = seq(from = 1, to = 2, by = 1),
                                       ntree = seq(from = 1000, to = 1200, by = 100),
                                       nodesize = seq(from = 1, to = 2, by = 1))#,
           }
           else{
             paramList <-  expand.grid(mtry = seq(from = 1, to = 12, by = 1),
                                       ntree = seq(from = 1000, to = 2400, by = 200),
                                       nodesize = seq(from = 1, to = 10, by = 1))
             
           }
           fittnessCV <- parallelCV(trainDataSplit,
                                    k = K,
                                    paramList = paramList,
                                    Model="RF",
                                    Parallel = Parallel,
                                    Cores = Cores,
                                    printSummary = printSummary)
           finalModel <- do.call("randomForest",
                                 c(list(x = trainDataSplit[, !names(trainDataSplit) == "EventIds"],
                                        y = factor(trainDataSplit$EventIds)),
                                   fittnessCV$bestParams,
                                   importance = TRUE))
           
           #            print(finalModel$confusion)
           if(printSummary){
             plot(finalModel)
           }
           finalPreds <- predict(finalModel,
                                 newdata=testDataSplit[, !names(testDataSplit) == "EventIds"],
                                 type="response")
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           if(printSummary){
             varImpPlot(finalModel)
           }
           
         },"RLR"={
           source("models/parallelCvRlr.r")
           
           if(codeTest){
             paramList = list(param1=seq(from = 0, to = 1, by = 0.5),
                              param2=10^seq(from = 0, to = -3, length.out = 5))
             
           }
           else{
             
             paramList = list(param1=seq(from = 0, to = 1, by = 0.01),
                              param2=10^seq(from = 0, to = -3, length.out = 50))
           }
           ##### train the model to find the best parameters
           fittnessCV <- parallelCvRlr(trainDataSplit,
                                       k = K,
                                       paramList = paramList,
                                       Model="RLR",
                                       Parallel = FALSE,
                                       Cores = Cores,
                                       Dummies = Dummies,
                                       printSummary = printSummary)
           
           xx <- trainDataSplit[, !names(trainDataSplit) == "EventIds"]
           
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species + harsness -1, data=xx)))
           }else
           {
             xx <- as.matrix(xx)
           }
           ##### re-train the overall model
           finalModel <- glmnet(x=xx,
                                y=factor(x = trainDataSplit$EventIds, levels = testDataELevels),
                                family = "multinomial",
                                alpha = fittnessCV$bestParams[1],
                                lambda = paramList$param2)
           
           #            print(finalModel)
           
           plot(finalModel)
           
           xxTest <- testDataSplit[, !names(testDataSplit) == "EventIds"]
           if(Dummies){
             xxTest <- as.matrix(cbind(xxTest[, !(names(xxTest) %in% c("Place", "Gender", "species", "harsness"))],
                                       model.matrix(~Place + Gender + species + harsness -1, data=xxTest)))
           }else
           {
             xxTest <- as.matrix(xxTest)
           }
           finalPreds <- predict(finalModel,
                                 newx=xxTest,
                                 type="class",
                                 s = fittnessCV$bestParams[2])
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           
           
           coef(finalModel, s=fittnessCV$bestParams[2])
           
           
         },"SVM" = {
           source("models/parallelCVSvm.r")
           
           if(codeTest){
             #              degree  
             #              parameter needed for kernel of type polynomial (default: 3)
             #              
             #              gamma	
             #              parameter needed for all kernels except linear (default: 1/(data dimension))
             #              
             #              coef0	
             #              parameter needed for kernels of type polynomial and sigmoid (default: 0)
             #              
             #              cost	
             #              cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation.
             
             switch(Kernel,
                    "linear" = {
                      paramList <-  data.frame(cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "polynomial" = {
                      paramList <-  expand.grid(degree = seq(from = 0, to = 2, by = 1),
                                                gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                coef0 = seq(from = 0, to = 1, by = 1),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "radial" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "sigmoid" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                coef0 = seq(from = 0, to = 1, by = 1),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    }
             )
             
           }else{
             switch(Kernel,
                    "linear" = {
                      paramList <-  data.frame(cost = 10 ^ 2)
                    },
                    "polynomial" = {
                      paramList <-  expand.grid(degree = 4,
                                                gamma = 10 ^ -2,
                                                coef0 = 4,
                                                cost = 10 ^ -1)
                    },
                    "radial" = {
                      paramList <-  expand.grid(gamma = 10 ^ -3,
                                                cost = 10 ^ 5)
                    },
                    "sigmoid" = {
                      paramList <-  expand.grid(gamma = 10 ^ -4,
                                                coef0 = 0,
                                                cost = 10 ^ 2)                    }
             )             
           }
           #            fittnessCV <- parallelCVSvm(trainDataSplit, 
           #                                        k = K,
           #                                        paramList = paramList,
           #                                        Model="SVM",
           #                                        Parallel = Parallel,
           #                                        Cores = Cores,
           #                                        Dummies = Dummies,
           #                                        Kernel = Kernel)
           # browser()
           xx <- trainDataSplit[, !names(trainDataSplit) == "EventIds"]
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species -1, data=xx))) 
           }else
           {
             xx <- as.matrix(xx)
           }
           
           
           finalModel <- do.call("svm",
                                 c(list(x = xx,
                                        y=factor(x = trainDataSplit$EventIds, levels = testDataELevels)),
                                   # fittnessCV$bestParams,
                                   paramList,
                                   kernel = Kernel,
                                   type = "C-classification"))
           
           xxTest <- testDataSplit[, !names(testDataSplit) == "EventIds"]
           if(Dummies){
             xxTest <- as.matrix(cbind(xxTest[, !(names(xxTest) %in% c("Place", "Gender", "species", "harsness"))],
                                       model.matrix(~Place + Gender + species -1, data=xxTest)))  
           }else
           {
             xxTest <- as.matrix(xxTest)
           }
           finalPreds <- predict(finalModel,
                                 newdata=xxTest,
                                 type="response")
           # browser()
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           
           cat("\n##### test metrics \n")
           confusionMatrixTest <- confusionMatrix(data = finalPreds,
                                                  reference = testDataSplit$EventIds)
           print(confusionMatrixTest)
           print(diag(confusionMatrixTest$table)/colSums(confusionMatrixTest$table))
           print(finalFittness)
           cat("Sensitivity == Recall, PPV == Precision\n")
           
         })
  
  
  ######print out the results#####
  
  propTable <- table(finalPreds)
  if(printSummary){
    print(propTable)
    cat("Sensitivity == Recall, PPV == Precision\n")
  }
  
  outputData <- list(finalModel = finalModel,
                     finalFittness = finalFittness,
                     #confusionMatrixTest = confusionMatrixTest,
                     #fittnessCV = fittnessCV$meanfittnessCV,
                     finalPreds = finalPreds,
                     # trainDataSplit = fittnessCV$trainData,
                     # testDataSplit = fittnessCV$testDataSplit,
                     # trainTestIdx = trainTestIdx,
                     # bestParams = fittnessCV$bestParams,
                     #fScore = fScore,
                     Dummies = Dummies,
                     Model = Model)
  if(Model == "SVM"){
    outputData$kernel <- Kernel
  }
  
  if(SAVE){
    if(Model == "SVM"){
      fName <- paste0(Model,Dummies," Model with ",Kernel," kernel, ", K, "-fold CV, on ", Sys.Date(),".RData")
    }else{
      fName <- paste0(Model,Dummies, "Test Model, ","Hz = ", Hz,", epochs = ", epochs,".RData")
    }
    cat("##### saving", fName, "\n")
    # browser()
    mainDir <- getwd()
    if(codeTest){
      subDir <- "model_outputs/test"
    }
    else{
      subDir <- "model_outputs"
    }
    if (file.exists(file.path(mainDir, subDir))){
      # setwd(file.path(mainDir, subDir))
    } else {
      dir.create(file.path(mainDir, subDir))
      # setwd(file.path(mainDir, subDir))
    }
    save(outputData, file = file.path(subDir,fName))
    # setwd(mainDir)   
  }
  
  return(outputData)  
}
