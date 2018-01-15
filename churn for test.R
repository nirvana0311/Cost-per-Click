test=read.csv('retail_testset.csv')
head(test)
test$DOB=as.Date(test$DOB)
test$Card_registration_date=as.Date(test$Card_registration_date)
test$Last_visit=as.Date(test$Last_visit)

hist(test$Amt_spent_in_last_year)
hist(test$Total_amt_spent)
require(lubridate)

test$Age=(max(test$Last_visit)-test$DOB)/365
test$Age=round(test$Age)
test=test[,-c(which(names(test) %in% c('DOB')))]

test$LastVisitDay=max(test$Last_visit) - test$Last_visit
test=test[,-c(which(names(test) %in% c('DOB','Last_visit')))]
#test=test[,-c(which(names(test) %in% c('DOB','Last_visit')))]
test$LoyaltyAgeInDays=as.Date('2015-01-31')-test$Card_registration_date
test=test[,-c(which(names(test) %in% c('DOB','Last_visit','Card_registration_date')))]

table(test$Churn)

modGlm=glm(formula = Churn~.,data = test,family = 'binomial')
pred1=predict(modGlm,type = 'response')
pred1=ifelse(test = pred1>0.5,yes = 'Yes',no = 'No')
confusionMatrix(pred1,test$Churn)

tr_cntrl=trainControl(method = 'cv',number = 3,repeats = 1)
statGrid <-  expand.grid(cost = matrix(c(0, 2,1, 0), 2, 2, byrow=TRUE))
tr_cntrl$sampling='down'
modGbm=train(Churn~.,data = test,method='rpart',trControl = tr_cntrl,weights =model_weights )
pred=predict(modGbm)
summary(pred)


model_weights <- ifelse(test$Churn == "Yes",
                        (1/table(test$Churn)[1]) * 0.2,
                        (1/table(test$Churn)[2]) * 0.8)

unique(model_weights)

install.packages('ROSE')
require(ROSE)
test1=ovun.sample(Churn~.,data = test,method = 'under',N = 7000,seed = 1)$data
modRpart=train(Churn~.,data = test1,method='rpart')
pred=predict(modRpart)
table(pred)
