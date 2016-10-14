rm(list=ls())

files<-list.files("model_outputs/testing-leavetwoout/Dummies/")
files<-files[!files=="desktop.ini"]
files<-files[!files=="importance"]
sl<-NULL
super<-NULL

for (i in 1:length(files)){
  load(paste0("model_outputs/testing-leavetwoout/Dummies/",files[i]))
  
  model<-outputData$Model
  dummies<-outputData$Dummies
    sensitivty<-outputData$confusionMat$byClass[1:4]
    specificity<-outputData$confusionMat$byClass[5:8]
    n<-sum(outputData$confusionMat$table)
    
    results<-cbind(model,dummies,sensitivty)
    results<-data.frame(cbind(results,specificity))
    results$behaviour<-c("foraging","grooming","resting","travelling")
    
    sl<-rbind(sl,results)
  
}
write.csv(sl,"conf.matrix_training.csv")
