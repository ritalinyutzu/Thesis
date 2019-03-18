rm(list=ls());gc()
setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout.csv")
set.seed(100)
n<-15761

summary(pollstar$Change) #mean=0.05898 #sd=0.4442019
sd(pollstar$Change)
summary(log(pollstar$USGorss.TicketSold)) #mean=4.148 #sd=0.5338415
sd(log(pollstar$USGorss.TicketSold))
c1<-matrix(rnorm(n,mean=5.898,sd=0.4442019),ncol=2)
c2<-matrix(rnorm(n,mean=4.148,sd=0.5338415),ncol=2)

data1<-rbind(c1,c2)
data1<-as.data.frame(data1)
names(data1)<- c("Change","Gross.TicketSold")
smoothScatter(data1$Change,data1$Gross.TicketSold,main="Scatterplot Colored  by Smoothed Densities")

c3<- matrix(rnorm(n,mean=7.677,sd=0.2662374),ncol=2)
data2<-rbind(c3,c2)
data2<-as.data.frame(data2)
names(data2)<-c("Promoter","Gross.TicketSold")
smoothScatter(data2$Promoter,data2$Gross.TicketSold,main="Scatterplot Colored  by Smoothed Densities")


log(mean(pollstar$Last)) #7.887958
log(sd(pollstar$Last)) #8.800904
c4<-matrix(rnorm(n,mean=7.887958,sd=8.800904),ncol=2)
data3<-rbind(c4,c2)
data3<-as.data.frame(data3)
names(data3)<-c("Last","USGross.TicketSold")
smoothScatter(data3$Last,data3$USGross.TicketSold,main="Scatterplot Colored  by Smoothed Densities")

mean(pollstar$new_song)
sd(pollstar$new_song)
c5<-matrix(rnorm(n,mean=4.132352,sd=0.4924299),ncol=2)
data4<-rbind(c5,c2)
data4<-as.data.frame(data4)
names(data4)<-c("New_Song","USGross.TicketSold")
smoothScatter(data4$New_Song,data4$USGross.TicketSold,main="Scatterplot Colored  by Smoothed Densities")

summary(pollstar)

