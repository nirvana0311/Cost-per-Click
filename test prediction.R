#test=read.csv("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\test.csv",na.strings = c(""," ","NAN",NaN,NA,'NA','<NA>'),nrows = 100000)
test=test[,-1]
head(test)

test$date=gsub(" ?[0-9]{2}:(.*)","",test$datetime)
test$date=gsub(" ?[0-9]{4}-[0-2]{2}-","",test$date)
test$date=as.numeric(test$date)
test$time= gsub("(.*)-[0-9]{2} ?","",test$datetime)
test$time=gsub(":(.*)","",test$time)
test$time=as.numeric(test$time)
test$date=as.numeric(as.character(test$date))
#test   %>% filter(click==1) 

test$day=weekdays(as.Date(paste("2017-01-",test$date,sep = "")))
#summary(test)
head(test)
test$weekend=ifelse(test$day=='Saturday' | test$day=='Sunday',T,F)

test=test[,-1]

unique(test$browserid)
test$browserid=ifelse(test = test$browserid=='Firefox' | test$browserid=='Mozilla Firefox' |
                            test$browserid=='Mozilla' 
                          ,yes = 'Mozilla',no = as.character(test$browserid))

test$browserid=ifelse(test = test$browserid=='Google Chrome' | test$browserid=='Chrome' 
                          ,yes = 'Chrome',no = as.character(test$browserid))

test$browserid=ifelse(test = test$browserid=='IE' | test$browserid=='InternetExplorer' 
                          ,yes = 'Internet Explorer',no = as.character(test$browserid))
test$browserid=as.factor(test$browserid)
unique(test$browserid)
unique(test$devid)

# Using cut
test$activeSurfing=cut(test$time, breaks = c(0,7), right = F)  

test$activeSurfing=ifelse(test$activeSurfing=='[0,7)',F,T)
test$activeSurfing=is.na(test$activeSurfing)

#test$click=as.factor(test$click)
#test$date=as.factor(test$date)
test$day=as.factor(test$day)
test$weekend=as.factor(test$weekend)
test$devid=as.factor(test$devid)
test$countrycode=as.factor(test$countrycode)
colSums(is.na(test))
test=test[,-1]
test=test[,-7]
rofl_test=amelia(x = test,m = 1,idvars =c('countrycode','day','weekend'), noms = c('browserid','devid'))




sum(is.na(rofl$imputations$imp1))
write.amelia(rofl_test, file.stem = "imputed_data_set_tests")


#x_vars=setdiff(names(test),c("click"))
#test$date=as.factor(test$date)
test$day=as.factor(test$day)
test$weekend=as.factor(test$weekend)

test=read.csv("C:\\Users\\nirva\\IdeaProjects\\Hacker Earth\\Ad click\\imputed_data_set_tests1.csv",na.strings = c(""," ","NAN",NaN,NA,'NA','<NA>'))
test=test[,-1]
test$day=as.factor(test$day)
test$weekend=as.factor(test$weekend)
pred_test=predict(GBMmodel,newdata = test)
length(pred_test)
