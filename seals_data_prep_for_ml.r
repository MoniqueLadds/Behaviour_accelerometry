##### down sample and get the folds for the data
##### returns a list with components:
# 1. train_data
# 2. test data
# 3. a list of the folds for cv
seals_data_prep_for_ml <- function(featureData,
                                   classMaxTrain = 500,
                                   classMaxTest = 500,
                                   printSummary = TRUE,
                                   codeTest = TRUE,
                                   test_animal_names = c("abbey", "bella")) # NULL for no test data
{
  
  library(dplyr)
  ###### remove NA's from the data
  featureData <- featureData[complete.cases(featureData),]
  
  ##### feature imputation for missing values
  # mean imputation, nope no missing values! yay!
  
  if(printSummary){
    cat("##### raw data summary:")
    print(table(featureData$EventIds, featureData$SealName))
  }
  
  featureData$EventIds <- as.character(featureData$EventIds)
  
  ##### remove class other  
  featureData <- featureData[featureData$EventIds!="Other",]
  featureData <- featureData[featureData$EventIds!="other",]
  featureData <- featureData[featureData$EventIds!="high_freq",]
  featureData <- featureData[!is.na(featureData$EventIds),]
  featureData <- featureData[featureData$EventIds != "NA",]
  
  ##### remove low length behaviours
  featureData <- featureData[featureData$nRows > 5,]
  
  uEventIds <- unique(featureData$EventIds)
  
  ##### remove testing animals
  if(!is.null(test_animal_names)){
    testDataSplitFull <- featureData %>%
      dplyr::filter(SealName %in% test_animal_names)
    
    trainDataSplit <- featureData %>%
      dplyr::filter(!(SealName %in% test_animal_names))
  }
  ##### Down sample the large classes
  set.seed(123, "L'Ecuyer")
  
  if(codeTest){
    classMaxTrain = 20
    classMaxTest = 20
  }
  
  train_animal_names <- unique(trainDataSplit$SealName)
  
  ##### sample the data by animal
  trainDataSplit <- lapply(train_animal_names, sample_by_animal_worker,
                           trainDataSplit,
                           classMaxTrain,
                           uEventIds,
                           printSummary)
  
  trainDataSplit <- do.call(what = rbind, args = trainDataSplit)
  
  ##### grab row ids for each fold
  folds_list <- lapply(train_animal_names, function(i){
    which(trainDataSplit$SealName == i)
  })
  
  ##### sample the test data
  testDataSplit <- lapply(test_animal_names, sample_by_animal_worker,
                          testDataSplitFull,
                          classMaxTest,
                          uEventIds,
                          printSummary)
  
  testDataSplit <- do.call(what = rbind, args = testDataSplit)
  
  if(printSummary){
    cat("\n##### training split data summary:")
    print(table(trainDataSplit$EventIds, trainDataSplit$SealName))
    cat("\n##### testing split data summary:")
    print(table(testDataSplit$EventIds, testDataSplit$SealName))
  }
  
  ##### remove the indetifier variables
  trainDataSplit <- trainDataSplit[, !(names(trainDataSplit) %in% c("FileDate", "SealName", "nRows", 
                                                                    "Acf.x", "Acf.y", "Acf.z", "Corr.xy", "Corr.yz", "Corr.xz"))]
  
  testDataSplit <- testDataSplit[,!(names(testDataSplit) %in%  c("FileDate", "SealName", "nRows", 
                                                                 "Acf.x", "Acf.y", "Acf.z", "Corr.xy", "Corr.yz", "Corr.xz"))]
  
  testDataSplitFull <- testDataSplitFull[,!(names(testDataSplit) %in%  c("FileDate", "SealName", "nRows", 
                                                                         "Acf.x", "Acf.y", "Acf.z", "Corr.xy", "Corr.yz", "Corr.xz"))]
  
  
  if(printSummary){
    cat("\n##### final test data summary:")
    print(table(trainDataSplit$EventIds))
    
    cat("\n##### final test data summary:")
    print(table(testDataSplit$EventIds))
  }
  
  return(list(trainDataSplit = trainDataSplit,
              testDataSplit = testDataSplit,
              folds_list = folds_list))
}



sample_by_animal_worker <- function(animal_name,
                                    inputData,
                                    classMax,
                                    uEventIds,
                                    printSummary){
  
  inputData <- inputData %>%
    dplyr::filter(SealName %in% animal_name)
  
  sampledData <- NULL
  for(i in 1:length(uEventIds)){
    
    tempData <- inputData[inputData$EventIds == uEventIds[i],]
    nr <- nrow(tempData)
    
    if(is.data.frame(classMax)){
      if(nr > classMax$classMax[classMax$behaviour == uEventIds[i]]){
        sampleIdx <- sample.int(n = nr,
                                size = classMax$classMax[classMax$behaviour == uEventIds[i]])
        tempData <- tempData[sampleIdx,]
      }
      
    }
    else if(nr>classMax){
      sampleIdx <- sample.int(n = nr, size = classMax)
      tempData <- tempData[sampleIdx,]
    }
    sampledData <- rbind(sampledData, tempData)
  }
  
  if(printSummary){
    cat(paste0("\n##### ", animal_name, " training data summary:"))
    print(table(sampledData$EventIds))
  }
  return(sampledData)
}

