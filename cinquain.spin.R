library(lubridate)
library(dplyr)
data=read.csv("v.csv",header=T)
attach(data)
head(data)
#fix(data)

datime=dmy_hms(paste(date, time),tz="Asia/Kolkata")
datime
data=data[with(data,order(card,datime)),]#sort by card, datime
conc=paste(data$card,data$mcc,data$date)
conc1=paste(data$card,data$tid)
conc2=paste(data$card,data$tid,data$date)
data$conc=conc
data$conc1=conc1
data$conc2=conc2
#add datime and conc to data
library(data.table)
dt=data.table(data)
dt[order(card,mcc,date),]

#no. of times card used @ mcc
dt[ , mccount := 1:.N , by = c("card" , "mcc") ]

#avg per MCc
dt$Conc=paste(dt$card,dt$mcc)
df=dt[,Mean:=(cumsum(amt)/(1:.N)),(Conc)]
dt$Mean=paste(df$Mean)
#dt[,Mean:=cumsum(amt)/1:length(amt),Conc]
#df=dt[,.(Mean=cumsum(amt)/1:length(amt)),(Conc)]

#no of distinct mccs per day
dt[, number := 1:.N, by = conc]

#no. of times card used at this TID
dt[, prev := 1:.N, by = conc1]

#no. of times card used at this TID per day (Run)
dt[, run := 1:.N, by = conc2]

head(dt)
write.csv(dt,"v1.csv")
#############
data=read.delim("cinq.txt",header=T,sep='\t')
head(data)
data$tod=factor(data$tod)
data$dv=factor(data$dv)
data$txn=factor(data$txn)
data$resp=factor(data$resp)
data$mcc=factor(data$mcc)
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

datime=dmy_hms(paste(date, time),tz="Asia/Kolkata")
d1=subset(data,(amt<300000))
ggplot(d1,aes(x=factor(dv),y=amt))+ geom_boxplot()+scale_x_discrete(name ="Transaction Type",labels=c("0" = "Genuine", "1" = "Fraudulent"))+ggtitle("Distribution of transactions by mark")
ggplot(d1,aes(x=factor(txn),y=amt))+ geom_boxplot()+scale_x_discrete(name ="Transaction Type")+ggtitle("Distribution of transactions by type")

dg=(subset(data,fraud=="G"&amt<=300000))
y=count(dg,c("txn"))
ggplot(y,aes(x=reorder(factor(txn),-freq),y=freq))+ geom_bar(stat='identity')#+scale_x_discrete(name ="Transaction Type")+ggtitle("Distribution of transactions by type")
summary(dg$amt)
mode(dg$amt)
y1=count(dg,c("mcc"))
t1=ggplot(dg,aes(x=factor(hour),fill=factor(tod)))+geom_bar()+ coord_polar()+ theme_hc() + scale_colour_hc()+geom_text_repel(stat="Count",aes(label=..count..))+ggtitle("Distribution of genuine transactions by time of day")
t1+ scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )+ylab("Count")+xlab("Hour")
p=ggplot(dg,aes(x=factor(hour),y=amt))+ geom_boxplot(aes(fill=tod))+ theme_economist() + scale_colour_hc()+xlab("Hour")+scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )
p


df=(subset(data,fraud=="F"))
y=count(df,c("txn"))
ggplot(y,aes(x=reorder(factor(txn),-freq),y=freq))+ geom_bar(stat='identity')#+scale_x_discrete(name ="Transaction Type")+ggtitle("Distribution of transactions by type")
summary(df$amt)
mode(df$amt)
y2=count(df,c("mcc"))
t1=ggplot(df,aes(x=factor(hour),fill=factor(tod)))+geom_bar()+ coord_polar()+ theme_hc() + scale_colour_hc()+geom_text_repel(stat="Count",aes(label=..count..))+ggtitle("Distribution of genuine transactions by time of day")
t1+ scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )+ylab("Count")+xlab("Hour")
p=ggplot(df,aes(x=factor(hour),y=amt))+ geom_boxplot(aes(fill=tod))+ theme_economist() + scale_colour_hc()+xlab("Hour")+scale_fill_discrete("Time of Day",breaks=c("1","2","3"),labels=c("Morning","Evening","Night") )
p

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
sink("Cinq-All IVs sans Smote.txt",append=F)
logit_fit = glm(dv~tod+amt+resp+txn+mcc+mccnt+mean+number+prev+run,
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
         

##Classification tree
sink("Cinq-Classification Tree sans SMOTE.txt",append=F)
fit=rpart(dv~tod+amt+resp+txn+mcc+mccnt+mean+number+prev+run,
          data = train,method="class",parms = list(split = 'information'))
#confusionMatrix(fit,train$dv)
with(fit, table(fit, train$dv))
summary(fit)
plotcp(fit)
plotmo(fit)
printcp(fit)

rpart.plot(fit,cex=0.7,extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)
prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pred=predict(fit,test,type="class")
with(test, table(pred, test$dv))

rpart.plot(pred,cex=0.6)


##Random Forest
sink("Cinq-RF sans SMOTE.txt",append=F)
rf_fit = randomForest(dv~tod+amt+resp+txn+mcc+mccnt+mean+number+prev+run,
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
