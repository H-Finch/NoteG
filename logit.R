data=read.delim("logit.txt",header=T,sep='\t')
attach(data)
head(data)
library(lubridate)
datime=dmy_hms(paste(date, time),tz="Asia/Kolkata")
datime
library(data.table)
dt=data.table(data)
data=data[with(data,order(pan,datime)),]
head(dt)
#Count of ATM withdrawals per day
dt[ , nper := 1:.N , by = c("pan" , "date") ]

#average of each card
df=dt[,avg:=(cumsum(amt)/(1:.N)),by = c("pan")]
dt$avg=paste(df$avg)
dt=dt[,avg:=as.numeric(avg)]
#calculation of avg1
dt$a1=paste((dt$amt-dt$avg)/dt$avg*100)

head(dt)
write.csv(dt,"logit1.csv")
#############
data=read.delim("l1.txt",header=T,sep='\t')
head(data)
data$tod=factor(data$tod)
data$dv=factor(data$dv)
mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

library(plyr)
library(ggplot2)
library(ggrepel)
library(caret) 
library(plyr) 
#library(Hmisc) 
library(pROC) 
library(DMwR) 
#library(missForest) 
#library(mi) 
#library(mice) 
#library(car) 
#library(xgboost) 
library(Amelia) 
library(caret)
library(ROCR) 
library(unbalanced) 
library(plotly)
library(InformationValue)
library(car)
library(reshape2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(plotmo)
library(ModelGood)
library(d3heatmap)
library(ggthemes)
library(rpivotTable)
library(lubridate)
library(InformationValue)###FOR LOGISTIC's METRICS!!!!

##### CUTTING TO THE CHASE
### Data Splitting to training & test datasets
## 75% of the sample size
smp_size = floor(0.75 * nrow(data))
## set the seed to make the partition reproductible
set.seed(123)
train_ind = sample(seq_len(nrow(data)), size = smp_size)
train = data[train_ind, ]
test = data[-train_ind, ]
dim(train)
dim(test)
str(train)
str(test)
pie(table(train$dv))
t3=ggplot(train,aes(x=factor(dv),fill=factor(dv)))+geom_bar()+ coord_polar()+ggtitle("Number of transactions by type")
t3+scale_fill_discrete("Type",breaks=c("0","1"),labels=c("Genuine","Fraudulent"))+xlab("Transaction")
## Count
ytrain=count(train,dv)
ytrain
ytest=count(test,dv)
ytest

###Building the model
###Logit
##Sinking output
sink("L1-All IVs sans Smote.txt",append=F)
logit_fit = glm(dv~tod+amt+avg+a1+nper,
                data = train,
                family = 'binomial')
vif(logit_fit)
summary(logit_fit)
varImp(logit_fit,scale=T)

## Train
y_pred = predict(logit_fit, train, type='response')
ROCcurv=Roc(logit_fit)
plot(ROCcurv)
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.6)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)

sink()#Stop writing output

##Model 2
##Sinking output
sink("L1-All IVs sans Smote and avg.txt",append=F)
logit_fit = glm(dv~tod+amt+a1+nper,
                data = train,
                family = 'binomial')
vif(logit_fit)
summary(logit_fit)
varImp(logit_fit,scale=T)

## Train
y_pred = predict(logit_fit, train, type='response')
ROCcurv=Roc(logit_fit)
plot(ROCcurv)
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)
ks_stat(train$dv,as.numeric(train$ypred))
ks_plot(train$dv,as.numeric(train$ypred))
## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.6)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
ks_stat(test$dv,as.numeric(test$ypred))
ks_plot(test$dv,as.numeric(test$ypred))
sink()#Stop writing output

###Model 2(a) 10-fold cross-validation with caret
sink("L1 All IVs sans Smote,avg with 10f CV.txt",append=F)
trainCtrl=trainControl(method="cv",number=10,savePredictions = T)
logit_cv=caret::train(dv~tod+amt+a1+nper,data=train,method="glm",family="binomial",trControl=trainCtrl)
#logit_cv$pred

summary(logit_cv)
varImp(logit_cv)
#logit_cv$pred
## Train
y_pred = predict(logit_cv, train, type='raw')
#y_pred = floor(y_pred+0.5)
train$ypred = y_pred

ks_stat(train$dv,as.numeric(train$ypred))
ks_plot(train$dv,as.numeric(train$ypred))
optimalCutoff(train$dv,as.numeric(train$ypred),optimiseFor = "Both",returnDiagnostics = T)
optimalCutoff(train$dv,as.numeric(train$ypred))
youdensIndex(train$dv,as.numeric(train$ypred))


sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, as.numeric(train$ypred))

## Test
y_pred = predict(logit_cv, test, type='raw')
#table(train$dv,y_pred>0.5)
#y_pred = floor(y_pred+0.5)
test$ypred = y_pred

ks_stat(test$dv,as.numeric(test$ypred))
ks_plot(test$dv,as.numeric(test$ypred))
optimalCutoff(test$dv,as.numeric(test$ypred),optimiseFor = "Both",returnDiagnostics = T)
optimalCutoff(test$dv,as.numeric(test$ypred))
youdensIndex(test$dv,as.numeric(test$ypred))

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, as.numeric(test$ypred))

logit_cv$results
logit_cv$finalModel

sink()
