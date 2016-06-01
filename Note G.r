getwd()
Sys.timezone= "Asia/Kolkata"
Sys.setenv(TZ='Asia/Kolkata')
library(plotly)
library(psych)
library(plyr)
require(shiny)
require(lubridate)
require(treemap)
library(wordcloud)
require(ggplot2)
data=read.delim(file='C:\\Users\\v7184\\Documents\\R-wd\\CC ATM 1.1.txt',header=TRUE,sep='\t')# CC ATM txns for the first 13 days of Nov'15
#sink('C:\\Users\\v7184\\Documents\\R-wd\\music.pdf') #save output to a PDF
attach(data)
summary(data)
fix(data)
head(data)
#library(ggplot)
boxplot(amount~mark,data=data, xlab='Mark type',ylab='Amount',col=c('Red','Green'))
boxplot(amount~state,data=data, xlab='state',ylab='Amount')
#rm(data)
t3=format(as.POSIXct(time,format="%H:%M:%S"),format="%H:%M:%S")
h=substring(t3,1,2)#break up data hour-wise
#df=table(cut(time,breaks='hour'))
boxplot(amount~h,data=data, xlab='Hour',ylab='Amount')
#not counting enquiries, PIN changes & other non-withdrawals
d2=(subset(data,trancode<16))
head(d2)
#describeBy(d2,mark,mat=FALSE)

summary(d2)
boxplot(amount~mark,data=d2, xlab='Mark type',ylab='Amount',col=c('Red','Green'))
boxplot(amount~state,data=d2, xlab='state',ylab='Amount')
#boxplot(amount~h,data=d2, xlab='Hour',ylab='Amount')->Error:variable lengths differ (found for 'h')
# plot(h,amount,type="h",col=mark)
# ggplot(aes(x = time, y = city,color=amount), data = d2) + geom_line()+ scale_colour_gradient(low="blue",high="red")
# # library(reshape2)
# # xyplot(amount~h,group=mark,data=d2,jitter.x=TRUE,jitter.y=TRUE)
# # qplot(x=h,y=amount,data=d2,color=mark)
# # boxplot(amount~city,data=d2, xlab='state',ylab='Amount')
#  plot_ly(d2, x = amount, color = city, type = "box", horizontal=FALSE)
#  plot_ly(d2, x = amount, y=city,color = mark, type = "scatter", horizontal=FALSE)
# # plot_ly(d2, x = amount, y=t3,color = city, type = "scatter", horizontal=FALSE)
head(d2)
datime=dmy_hms(paste(d2$date, d2$time),tz="Asia/Kolkata")#timestamp generation
dia <- strptime(time,"%H:%M:%S") 
dia[dia$hour < 20] <- dia[dia2$hour < 20]+ 24*60*60 

rm(dia)
head(date)
head(time)
time
d2$time <- as.POSIXct(d2$time, format="%H:%M:%S")
dg=(subset(data,mark=="Genuine"))
dg$datime=dmy_hms(paste(dg$date, dg$time),tz="Asia/Kolkata")
dc=(subset(data,mark=="Counterfeit"))
p=ggplot(data=d2, aes(x=datime,y=amount),environment = environment()) +geom_point(color='Blue') +facet_grid(.~city )

ggplotly(p)

#subset = .(mark == 'Counterfeit'), colour = 'Red') 
dg$datime
ggplot(d2, aes(x=h,y=amount)) +  geom_point(aes(mark="Genuine", color="Blue"), size=2) +  geom_point(aes(mark="Genuine", colour ="Red"), size=4) +scale_colour_manual(name="",  values = c("Blue"="blue","Red"="red"))
write.table(d2, "d2.txt", sep="\t", row.names=FALSE, col.names=TRUE)
cardata=unique(card)
cardata=substr(unique(card),1,6)
summary(cardata)
c2=unique(cardata)
nrow(c2)
plot_ly(d2, x = datime, y=amount, type = "scatter")
dc=(subset(d2,mark=="Counterfeit"))#subsetting only counterfeit txns
summary(dc)
nrow(dc)


count(uni2,"bin")
#####treemap
png(filename="tree.png",width=800, height=800)
treemap(univ, 
        index="bin", 
        vSize="freq", 
        type="value")
treemap(univ2, 
        index="bin", 
        vSize="freq", 
        type="value")
itreemap(univ, 
        index="bin", 
        vSize="freq", 
        type="value",height=800)
itreemap(univ2, 
         index="bin", 
         vSize="freq", 
         type="value",height=800)
tree=gvisTreeMap(data,  idvar="bin", parentvar="SD_FIID",
            sizevar=nrow(data))
plot(tree)
#####treemap
png("wordcloud_packages.png", width=12,height=8, units='in', res=600)
lord=t(lord)
words=names(lords)
wordcloud(words=data$bin,freq=data$freq,min.freq = 1)#wordcloud
plot(wordcloud(univ$bin,univ$freq,max.words=200))
wordcloud(words[1:100000], frequency[1:100000])
wordcloud(rownames(univ),min.freq=1)
plot(wordcloud(data$bin, scale=c(5,0.5), max.words=1000, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2")))
###rough work
require(data.table)
dt =select(d2,card,mark)
df[,c("card","bin","mark")]
dt[, count := length(unique(card)), by=mark]



uni=unique(data[c("card","bin")])
univ=count(uni,"bin")
univ=univ[with(univ, order(-freq)), ]
univ


uni2=unique(d2[c("card","bin")])
univ2=count(uni2,"bin")
univ2=univ[with(univ2, order(-freq)), ]
univ2
# library(plotly)
# library(plyr)
# require(shiny)
# require(lubridate)
# require(treemap)
# require(ggplot2)
# data=read.delim(file='C:\\Users\\v7184\\Documents\\R-wd\\CC ATM 1.1.txt',header=TRUE,sep='\t')
# attach(data)
# datime=dmy_hms(paste(date, time),tz="Asia/Kolkata")
# 
# d2=(subset(data,trancode<16))
# datime=dmy_hms(paste(d2$date, d2$time),tz="Asia/Kolkata")
# dg=(subset(data,mark=="Genuine"))
# dg$datime=dmy_hms(paste(dg$date, dg$time),tz="Asia/Kolkata")
# dc=(subset(data,mark=="Counterfeit"))
# dc$datime=dmy_hms(paste(dc$date, dc$time),tz="Asia/Kolkata")
# 
# p1=ggplot(data=dg, aes(x=datime,y=amount),environment = environment()) +geom_point(color='Blue')+ggtitle("Transaction spread over time-Genuine")
# p2=ggplot(data=dc, aes(x=datime,y=amount),environment = environment()) +geom_point(color='Red')+ggtitle("Transaction spread over time-Counterfeit")
# ggplotly(p1)
# gplotly(p2)
# 
# dg1=(subset(d2,mark=="Genuine"))
# dg1$datime=dmy_hms(paste(dg1$date, dg1$time),tz="Asia/Kolkata")
# dc1=(subset(d2,mark=="Counterfeit"))
# dc1$datime=dmy_hms(paste(dc1$date, dc1$time),tz="Asia/Kolkata")
# 
# p3=ggplot(data=dg1, aes(x=datime,y=amount),environment = environment()) +geom_point(color='Blue')+ggtitle("Withdrawal spread over time-Genuine")
# p4=ggplot(data=dc1, aes(x=datime,y=amount),environment = environment()) +geom_point(color='Red')+ggtitle("Withdrawal spread over time-Counterfeit")
# ggplotly(p3)
# ggplotly(p4)
# 
# cols <- RColorBrewer::brewer.pal(nlevels(d2$city), "Set1")
# p5=ggplot(data=d2, aes(x=datime,y=amount,color=city,colors="Set1"),environment = environment()) +geom_point(aes(size=amount))
# ggplotly(p5)
# 
# box1=ggplot(data,aes(amount,mark))+geom_boxplot()+ggtitle("Transaction summaries ")
# box2=ggplot(d2,aes(datime,amount,color=mark))+geom_boxplot()+ggtitle("Withdrawal summaries over time")
# box3=ggplot(dg,aes(datime,amount,color=city))+geom_boxplot()+ggtitle("Withdrawal summaries over time by city")
# box4=ggplot(dc,aes(datime,amount,color=city))+geom_boxplot()+ggtitle("Withdrawal summaries over time by city")
# ggplotly(box1)
# box1=boxplot(amount~mark,data=data, xlab='Mark type',ylab='Amount',col=c('Red','Green'))
# x=midwest
# library(plotly)
# p <- plot_ly(midwest, x = percollege, color = state, type = "line", horizontal=FALSE)
# p
# p <- plot_ly(midwest, x = percollege, color = state, type = "box",horizontal=FALSE)
# install.packages("plyr")
# ?itreemap
# require('snippets')
# library(tm)
# ?ddply
# nrow(data)
# rmarkdown::run('Note G.Rmd') 
# 
# head(d2)
# treemap(bin, nrow(d2), nrow(data), d2$amount, 
#         lab = c(TRUE, TRUE), main="London", pal="Oranges", linecol= "dark gray", textcol="white")
# devtools::install_github("ropensci/plotly")
# 
# data(business)
# itreemap(business)
# 
# lords <-d2$bin
# lord=read.csv(file="lords.csv")
# wordcloud(words=univ$bin,freq=univ$freq,min.freq = 1)
# 
# nrow(data)
# write.csv(lords,file="lords.csv")
# res<-rquery.wordcloud(data$bin, type ="file", lang = "english")
# rm(lord,lords)
