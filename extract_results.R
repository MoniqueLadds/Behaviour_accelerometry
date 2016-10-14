rm(list=ls())

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)

fileLoc<-"model_outputs/cross-validation/"

output<-NULL

files<-list.files(fileLoc)
for(i in 1:length(files)){
  load(paste0("model_outputs/cross-validation/",files[i]))
  testData<-outputData$testDataSplit
  confMat<-table(testData$EventIds,testData$predsTest)
  props<-prop.table(confMat,1)
  forage<-props[1,1]
  groom<-props[4,2]
  rest<-props[3,3]
  travel<-props[2,4]
  
  ACC<- (confMat[1,1]+confMat[4,2]+confMat[3,3]+confMat[2,4])/sum(confMat)
  catAcc<-cbind(forage,groom,travel,rest)
  output<-(output, catAcc)
  
  }




totals<-NULL
Travels<-NULL
Forages<-NULL
Grooms<-NULL
Rests<-NULL

for(l in 1:length(files)){
  
  load(paste0("model_outputs/cross-validation/",files[l]))
  
  testData<-outputData$testDataSplit
  
  EventProbs<-testData$prob
  EventProbs$Event <- colnames(EventProbs)[max.col(EventProbs, ties.method = 'first')]
  EventProbs$EventValue <- apply(EventProbs[,1:4],1,max)
  
  Travel<-EventProbs[EventProbs$EventValue<0.75&EventProbs$Event=="Travelling",]
  Travel$DiffEvent <- colnames(Travel)[max.col(Travel[,1:3], ties.method = 'first')]
  Travel$DiffEventValue <-   apply(Travel[,1:3],1,max)
  Travel$Diff<-Travel$EventValue-Travel$DiffEventValue
  
  Travels<-rbind(Travels,Travel)
  
  Forage<-EventProbs[EventProbs$EventValue<0.75&EventProbs$Event=="Foraging",]
  Forage$DiffEvent <- colnames(Forage[,2:4])[max.col(Forage[,2:4], ties.method = 'first')]
  Forage$DiffEventValue <-   apply(Forage[,2:4],1,max)
  Forage$Diff<-Forage$EventValue-Forage$DiffEventValue
  
  Forages<-rbind(Forages,Forage)
  
  Groom<-EventProbs[EventProbs$EventValue<0.75&EventProbs$Event=="Grooming",]
  Groom$DiffEvent <- colnames(Groom[,c(1,3:4)])[max.col(Groom[,c(1,3:4)], ties.method = 'first')]
  Groom$DiffEventValue <-   apply(Groom[,c(1,3:4)],1,max)
  Groom$Diff<-Groom$EventValue-Groom$DiffEventValue
  
  Grooms<-rbind(Grooms,Groom)
  
  Rest<-EventProbs[EventProbs$EventValue<0.75&EventProbs$Event=="Resting",]
  Rest$DiffEvent <- colnames(Rest[,c(1:2,4)])[max.col(Rest[,c(1:2,4)], ties.method = 'first')]
  Rest$DiffEventValue <-   apply(Rest[,c(1:2,4)],1,max)
  Rest$Diff<-Rest$EventValue-Rest$DiffEventValue
  
  Rests<-rbind(Rests, Rest)
  
  
  total<-nrow(EventProbs)
  totals<-rbind(totals,total)
}

Groom<-Grooms %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Grooms))
Groom$Var<-'Groom'

Travel<-Travels %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Travels))
Travel$Var<-'Travel'
Forage<-Forages %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Forages))
Forage$Var<-'Forage'
Rest<-Rests %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Rests))
Rest$Var<-'Rest'

ConfMat<-rbind(Groom, Rest, Travel, Forage)
write.csv(ConfMat, "model_outputs/leavevoneout_confusion_matrix.csv")

(sum(totals)-sum(nrow(Groom),nrow(Rest),nrow(Travel),nrow(Forage)))/sum(totals)


rownames(output)<-names

output<-data.frame(output)

output$names<-rownames(output)

output$names[output$names=="RF Model TRUE"]<-"Random forest"
output$names[output$names=="RLR Model TRUE"]<-"Logistic regression"
output$names[output$names=="SVM Model with linear kernel TRUE"]<-"SVM (linear)"
output$names[output$names== "SVM Model with polynomial kernel TRUE"] = "SVM (polynomial)"
output$names[output$names== "SVM Model with radial kernel TRUE" ] = "SVM (radial)"
output$names[output$names== "SVM Model with sigmoid kernel TRUE" ] = "SVM (sigmoid)"
output$names[output$names== "XGB Model TRUE"] = "Stochastic gradient boosting"


output$names<-factor(output$names,levels(factor(output$names))[c(4,7,5,6,1,2,3)])
  
write.csv(output,"output/model.results.csv")



limits<-aes(xmax=output$AccuracyUpper, xmin = output$AccuracyLower)

##Figure 1 paper
#export at 650 x 350

ggplot(output, aes(x=Accuracy,y=names))+
  geom_point()+
  geom_errorbarh(limits)+
  theme_bw()+
  ylab("Model")+
  scale_x_continuous(labels = scales::percent,limits=c(.5,1))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16))


conf.mat<-outputData$confusionMat$table

importance<-data.frame(outputData$varImp)

importance$names<-rownames(importance)

## Re-level the variables by importance

#export at 800 x 1000
importance$names <-factor(importance$Feature, 
                          levels=importance[order(importance$Gain), 
                                            "Feature"])

x <-ggplot(importance, aes(y=names, x=Gain)) + 
  geom_point(stat="identity")+
  ylab("Variable")+
  xlab("Mean decrease accuracy")+
  scale_x_continuous(labels = scales::percent,limits=c(0,0.045))+
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill="NA"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"))

y <-ggplot(importance, aes(x=names, y=Gain)) + 
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Summary statistic")+
  ylab("Mean decrease accuracy")+
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill="NA"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"))

grid.arrange(x, y, ncol=2)