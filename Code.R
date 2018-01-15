rm(list=ls(all=TRUE))
gc(reset=T)
setwd("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click")


lib<-c('ggplot2','dplyr','caret','mice','missForest')
lapply(lib,FUN = require,character.only=T)

#setTrain <- read.csv.sql(file = "C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\train.csv", sql = "select * from file order by random() limit 200000")
setTrain=read.csv("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\train3.csv",na.strings = c(""," ","NAN",NaN,NA,'NA','<NA>'),nrows = 50000)
#test=read.csv("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\train.csv",na.strings = c(""," ","NAN",NaN,NA,'NA','<NA>'))
head(setTrain)
sum(is.na(setTrain))
setTrain=setTrain[,-1]


setTrain$date=gsub(" ?[0-9]{2}:(.*)","",setTrain$datetime)
setTrain$date=gsub(" ?[0-9]{4}-[0-2]{2}-","",setTrain$date)
setTrain$date=as.numeric(setTrain$date)
setTrain$time= gsub("(.*)-[0-9]{2} ?","",setTrain$datetime)
setTrain$time=gsub(":(.*)","",setTrain$time)
setTrain$time=as.numeric(setTrain$time)
setTrain$date=as.numeric(as.character(setTrain$date))
#setTrain   %>% filter(click==1) 

setTrain$day=weekdays(as.Date(paste("2017-01-",setTrain$date,sep = "")))
#summary(setTrain)
head(setTrain)
setTrain$weekend=ifelse(setTrain$day=='Saturday' | setTrain$day=='Sunday',T,F)

setTrain=setTrain[,-1]

ggplot(data = setTrain   %>% filter(click==1) ,aes(day))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(click)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')

ggplot(data = setTrain  %>% filter(click==1)  ,aes(countrycode))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(click)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')

ggplot(data = setTrain   %>% filter(click==1) ,aes(browserid))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(click)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')


unique(setTrain$browserid)
setTrain$browserid=ifelse(test = setTrain$browserid=='Firefox' | setTrain$browserid=='Mozilla Firefox' |
         setTrain$browserid=='Mozilla' 
       ,yes = 'Mozilla',no = as.character(setTrain$browserid))

setTrain$browserid=ifelse(test = setTrain$browserid=='Google Chrome' | setTrain$browserid=='Chrome' 
                          ,yes = 'Chrome',no = as.character(setTrain$browserid))

setTrain$browserid=ifelse(test = setTrain$browserid=='IE' | setTrain$browserid=='InternetExplorer' 
       ,yes = 'Internet Explorer',no = as.character(setTrain$browserid))
setTrain$browserid=as.factor(setTrain$browserid)
unique(setTrain$browserid)


ggplot(data = setTrain   %>% filter(click==1) ,aes(browserid))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(click)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')


unique(setTrain$devid)
str(setTrain)
names(setTrain)

ggplot(data = setTrain   %>% filter(click==1) ,aes(time))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(countrycode)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')

# Using cut
setTrain$activeSurfing=cut(setTrain$time, breaks = c(0,7), right = F)  

setTrain$activeSurfing=ifelse(setTrain$activeSurfing=='[0,7)',F,T)
setTrain$activeSurfing=is.na(setTrain$activeSurfing)

ggplot(data = setTrain   %>% filter(click==1) ,aes(activeSurfing))+
  geom_bar(aes(colour=as.factor(click),fill=as.factor(click)))+
  theme_bw()+scale_fill_brewer(palette = 'Accent')

setTrain$click=as.factor(setTrain$click)
#setTrain$date=as.factor(setTrain$date)
setTrain$day=as.factor(setTrain$day)
setTrain$weekend=as.factor(setTrain$weekend)

#amelia=Amelia::amelia(x = setTrain,m=5,parallel='multicore')
require(Amelia)
rofl=amelia(x = setTrain,m = 1,idvars =c(5,8,11,12), noms = c(6,7))
colSums(is.na(setTrain))

rofl$imputations$imp1$devid

sum(is.na(rofl$imputations$imp1))


write.amelia(rofl, file.stem = "imputed_data_set")


x_vars=setdiff(names(setTrain),c("click"))
?setdiff
setTrain$click=as.factor(setTrain$click)
#setTrain$date=as.factor(setTrain$date)
setTrain$day=as.factor(setTrain$day)
setTrain$weekend=as.factor(setTrain$weekend)

Grid <-  expand.grid(
  n.trees = c(100),
  interaction.depth = c(8) ,
  shrinkage = 0.2,n.minobsinnode=5)

# Define the parameters for cross validation
fitControl <- trainControl(method = "none")

setTrain=read.csv("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\imputed_data_set1.csv",na.strings = c(""," ","NAN",NaN,NA,'NA','<NA>'))
setTrain=setTrain[,-1]
setTrain$click=as.factor(setTrain$click)
setTrain$weekend=as.factor(setTrain$weekend)
setTrain=setTrain[,-1]
setTrain=setTrain[,-1]
#setTrain=setTrain[,-9]
names(setTrain)

GBMmodel <- train(click ~ .,
                  data = setTrain,
                  method = "gbm",
                  tuneGrid = Grid,
                  trControl = fitControl
)

confusionMatrix(predict(object = GBMmodel,newdata = setTrain),setTrain$click)

pred=predict(GBMmodel,newdata = setTrain)

require(MLmetrics)
AUC(y_pred = pred,y_true = setTrain$click)

