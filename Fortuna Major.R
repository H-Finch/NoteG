### Reading Data
#setwd("C:/Users/v7184/Documents")
data=read.delim("DTREE.txt",header=T,sep='\t')
attach(data)
head(data)
#fix(data)
data$avg1=avg1*100
data$avg2=avg2*100
data$timeofday=factor(data$timeofday)
data$prev=factor(data$prev)
data$dv=factor(data$dv)
mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
hist(data)

#Previous usage(prev): 1-Yes, 0-No
#Timeofday: Morning-1, Evening-2,Night-3
#DV: 0-Genuine,1-Fraud

### Loading packages
library(dplyr)
library(ggthemes)
library(plyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
require(caret) 
require(plyr) 
#require(Hmisc) 
require(pROC) 
require(DMwR) 
#require(missForest) 
#require(mi) 
#require(mice) 
#require(car) 
#require(xgboost) 
require(Amelia) 
require(ROCR) 
require(unbalanced) 
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

missmap(data)

### EDA
##Time of day vs DV
datime=dmy_hms(paste(date, time),tz="Asia/Kolkata")
#plot_ly(data,x=factor(dv),y=amount,type='box')
p1=ggplot(data,aes(x=factor(dv),y=amount))+ geom_boxplot()+scale_x_discrete(name ="Transaction Type",labels=c("0" = "Genuine", "1" = "Fraudulent"))+ggtitle("Distribution of transactions by type")
ggplotly(p1)
p=ggplot(data,aes(amount,color=dv))+geom_freqpoly()
ggplotly(p)
dg=(subset(data,mark=="Genuine"))
summary(dg$amount)
mode(dg$amount)
#y=count(dg,c("hour","mark"))
t1=ggplot(dg,aes(x=factor(hour),fill=factor(timeofday)))+geom_bar()+ coord_polar()+ theme_hc() + scale_colour_hc()+geom_text_repel(stat="Count",aes(label=..count..))+ggtitle("Distribution of transactions by time of day")
t1+ scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )+ylab("Count")+xlab("Hour")

p=ggplot(dg,aes(x=factor(hour),y=amount))+ geom_boxplot(aes(fill=timeofday))+ theme_economist() + scale_colour_hc()+xlab("Hour")+scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )
ggplotly(p)
p2=ggplot(dg,aes(amount,fill=timeofday))+geom_histogram()
ggplotly(p2)
#(aes(fill = ..count..))+
  #scale_fill_gradient("Count", low = "green", high = "red")


#plot_ly(y,x=hour,y=freq,type='bar')

df=(subset(data,mark=="Fraud"))
summary(df$amount)
mode(df$amount)
t2=ggplot(df,aes(x=factor(hour),fill=factor(timeofday)))+geom_bar() + coord_polar()+ theme_hc() + scale_colour_hc()+ geom_text_repel(stat="Count",aes(label=..count..))+ggtitle("Distribution of transactions by time of day")+ 
  scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )+ylab("Count")+xlab("Hour")
t2
#y=count(df,c("hour","mark"))
p=ggplot(df,aes(x=factor(hour),y=amount))+ geom_boxplot(aes(fill=timeofday))+theme_economist()+xlab("Hour")+scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )
ggplotly(p)


### Data Splitting to training & test datasets
## 75% of the sample size
smp_size = floor(0.75 * nrow(data))
## set the seed to make the partition reproductible
set.seed(123)
train_ind = sample(seq_len(nrow(data)), size = smp_size)
train = data[train_ind, ]
train= train[,-16]#removing column 'tie' as it has blanks
test = data[-train_ind, ]
test=test[,-16]#removing column 'tie' as it has blanks
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
###Model 1
##Sinking output
sink("All IVs sans Smote.txt",append=F)
logit_fit = glm(dv~timeofday+prev+amount+avg1+avg2,
                 data = train,
                 family = 'binomial')
vif(logit_fit)
summary(logit_fit)
#varImp(logit_fit,scale=T)

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
with(test, table(dv, ypred))

auc(test$dv, test$ypred)

sink()#Stop writing output

###Model 2 
sink("All IVs sans Smote,avg2.txt",append=F)
logit_fit = glm(dv~timeofday+prev+amount+avg1,
                data = train,
                family = 'binomial')
summary(logit_fit)

## Train
y_pred = predict(logit_fit, train, type='response')
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()

###Model 2(a) 10-fold cross-validation with caret
sink("All IVs sans Smote,avg2 with 10f CV.txt",append=F)
trainCtrl=trainControl(method="cv",number=10,savePredictions = T)
logit_cv=caret::train(dv~timeofday+prev+amount+avg1,data=train,method="glm",family="binomial",trControl=trainCtrl)
#logit_cv$pred

summary(logit_cv)
varImp(logit_cv)

## Train
y_pred = predict(logit_cv, train, type='raw')
#y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_cv, test, type='raw')
#table(train$dv,y_pred>0.5)
#y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()


###Model 3
sink("All IVs sans Smote,amt.txt",append=F)
logit_fit = glm(dv~timeofday+prev+avg2+avg1,
                data = train,
                family = 'binomial')
summary(logit_fit)

## Train
y_pred = predict(logit_fit, train, type='response')
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()

###Model 4
sink("All IVs sans Smote,Timeofday.txt",append=F)
logit_fit = glm(dv~prev+avg2+avg1+amount,
                data = train,
                family = 'binomial')
summary(logit_fit)

## Train
y_pred = predict(logit_fit, train, type='response')
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()

###Model 5
sink("All IVs sans Smote,prev.txt",append=F)
logit_fit = glm(dv~timeofday+avg2+avg1+amount,
                data = train,
                family = 'binomial')
summary(logit_fit)

## Train
y_pred = predict(logit_fit, train, type='response')
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()

###Model 6
sink("All IVs sans Smote,avg2,amt.txt",append=F)
logit_fit = glm(dv~timeofday+prev+avg1,
                data = train,
                family = 'binomial')
summary(logit_fit)

## Train
y_pred = predict(logit_fit, train, type='response')
y_pred = floor(y_pred+0.5)
train$ypred = y_pred

sum(with(train, table(ypred, dv)))
with(train, table(ypred, dv))

auc(train$dv, train$ypred)

## Test
y_pred = predict(logit_fit, test, type='response')
y_pred = floor(y_pred+0.5)
test$ypred = y_pred

sum(with(test, table(ypred, dv)))
with(test, table(ypred, dv))

auc(test$dv, test$ypred)
sink()

######### For RMD, all Logits #########
logit_fit1 = glm(dv~timeofday+prev+amount+avg1+avg2,data = train,family = 'binomial')
roc_fit1=Roc(logit_fit1)
logit_fit2= glm(dv~timeofday+prev+amount+avg2,data = train,family = 'binomial')
roc_fit2=Roc(logit_fit2)
logit_fit3=glm(dv~timeofday+prev+avg1+avg2,data = train,family = 'binomial')
roc_fit3=Roc(logit_fit3)
logit_fit4=glm(dv~prev+amount+avg1+avg2,data = train,family = 'binomial')
roc_fit4=Roc(logit_fit4)
logit_fit5=glm(dv~timeofday+amount+avg1+avg2,data = train,family = 'binomial')
roc_fit5=Roc(logit_fit5)
logit_fit6=glm(dv~timeofday+prev+avg1,data = train,family = 'binomial')
roc_fit6=Roc(logit_fit6)
plot(roc_fit1)
plot(roc_fit2,add=TRUE,col=2)
plot(roc_fit3,add=T,col=3)
plot(roc_fit4,add=T,col=4)
plot(roc_fit5,add=T,col=5)
plot(roc_fit6,add=T,col=6)

legend(x="bottomright",col=c(1,2,3,4,5,6),lwd=7,legend=c("Model 1","Model 2","Model 3","Model 4","Model 5","Model 6"),bty="n")


######### Valinor #########
###Model 7 - Random Forest

sink("RF sans SMOTE.txt",append=F)
rf_fit = randomForest(dv~timeofday+amount+avg1+avg2+prev,
                     data = train,ntree=500,mtry=5,proximity=T)
rf_fit
plot(rf_fit)
summary(rf_fit)
varImpPlot(rf_fit, sort = T,main="Variable Importance")
#MDSplot(rf_fit,train$dv)
partialPlot(rf_fit,train,prev)

rf_pred = predict(rf_fit, test)
table(rf_pred,test$dv)

sink()
###Model 8 - Classification Tree
sink("Classification Tree sans SMOTE.txt",append=F)
fit=rpart(dv~timeofday+amount+avg1+prev+avg2,
          data = train,method="class",parms = list(split = 'information'))
confusionMatrix(fit,train$dv)
with(fit, table(fit, train$dv))
summary(fit)
plotcp(fit)
plotmo(fit)
printcp(fit)

rpart.plot(fit,cex=0.7,extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

prune(fit, cp=0.010000)#prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

pred=predict(fit,test,type="class")
with(test, table(pred, test$dv))



rpart.plot(pred,cex=0.6)
sink()
#rm(list=ls())

###
dat=read.csv("metrics.csv",header=T)
d3heatmap(dat[,-1],scale="column",dendrogram = "none",color="Blues")
###
plot_ly(dat,x=rownames(dat),y=colnames(dat),type="heatmap")
g=ggplot(dat1,aes(x=metric,y=model,fill=value))+  geom_tile()+ coord_equal()
ggplotly(g)
