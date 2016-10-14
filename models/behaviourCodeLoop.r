behaviourCodeLoop <- function(PlotBeahviour=FALSE){
  
  source("R/models/behaviourCode.r")
  
  behaviourLoc <-"data/raw_data/"
  

  ###### run behaviourCode on the raw data to encode the behaviours, this should overwrite any previously encoded files

  folderNames <- list.files(behaviourLoc)
  folderNames <- folderNames[!(folderNames %in% c("desktop.ini","Icon\r"))]
  
  ##### loop over the different folders 
  for (i in 1:length(folderNames)){
    rawfiles <- list.files(
      paste0(behaviourLoc, folderNames[i]) ) 
    rawfiles <- rawfiles[!(rawfiles %in% c("desktop.ini","Icon\r"))]
    
    ##### loop over the different files in each folder
    for (k in 1:length(rawfiles)){
      tempFileNameLoc <- paste0(behaviourLoc,
                                folderNames[i],
                                "/",
                                rawfiles[k])
      cat(tempFileNameLoc, "\n")
      
      rawData <- read.csv(tempFileNameLoc,
                          header=TRUE,
                          stringsAsFactors=FALSE)
      rawData$doe<-substr(rawfiles[k],1,8)
      
      options(digits.secs = 3)
      a<-nchar(rawData$time[1])
      if(a<=20){
      rawData$date<-paste(rawData$doe," ",rawData$time)  
      rawData$time<-as.POSIXct(rawData$date,format="%d-%m-%y %H:%M:%OS")
      #rawData$time<-as.factor(rawData$date)
      }else{
        rawData$date<-as.POSIXct(rawData$time,format="%Y-%m-%d %H:%M:%OS")
        rawData$time<-as.factor(rawData$date)
      }
      
      ##### encode the behaviours in the files
      codedData <- behaviourCode(rawData)
      write.csv(codedData, file=tempFileNameLoc, row.names = FALSE)
      
      
    }
  }
}