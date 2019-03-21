rm(list=ls());gc()
setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout_new.csv")

#檢查欄位資料型態
str(pollstar)
#SVM Library
library('e1071')
ncol(pollstar)

######轉換各欄位分類

#USGross_TicketSold
tmp=0
for(i in 1:15761){
  if(pollstar[i,10]>92)tmp[i]="5"
  else if(pollstar[i,10]>71)tmp[i]="4"
  else if(pollstar[i,10]>65)tmp[i]="3"
  else if(pollstar[i,10]>45)tmp[i]="2"
  else tmp[i]="1"
}
pollstar[,10]=factor(tmp)

#轉換欄位格式
str(pollstar)
pollstar$Shows<-as.factor(pollstar$Shows)
pollstar$Year<-as.factor(pollstar$Year)
pollstar$Month<-as.factor(pollstar$Month)
pollstar$Tickets_Sold<-as.numeric(pollstar$Tickets_Sold)
pollstar$Capacity<-as.numeric(pollstar$Capacity)
pollstar$SoldOut<-as.factor(pollstar$SoldOut)
pollstar$US_Gross<-as.numeric(pollstar$US_Gross)
pollstar$Supporting_Act_Dummy<-as.factor(pollstar$Supporting_Act_Dummy)
pollstar$Global_Concert_Pulse_Dummy<-as.factor(pollstar$Global_Concert_Pulse_Dummy)
pollstar$Global_Concert_Pulse_Times<-as.factor(pollstar$Global_Concert_Pulse_Times)
pollstar$InhousePromotion<-as.factor(pollstar$InhousePromotion)
pollstar$no_promoter<-as.factor(pollstar$no_promoter)
pollstar$US<-as.factor(pollstar$US)
pollstar$Canada<-as.factor(pollstar$Canada)
pollstar$Europe<-as.factor(pollstar$Europe)
pollstar$UK<-as.factor(pollstar$UK)
pollstar$SouthUS<-as.factor(pollstar$SouthUS)
pollstar$Oceania<-as.factor(pollstar$Oceania)
pollstar$Date_1_Whichday<-as.factor(pollstar$Date_1_Whichday)
pollstar$Live.Nation<-as.factor(pollstar$Live.Nation)
pollstar$Holiday<-as.factor(pollstar$Holiday)
pollstar$Volume.x<-as.numeric(pollstar$Volume.x)
pollstar$Volume.y<-as.numeric(pollstar$Volume.y)
pollstar$Last<-as.factor(pollstar$Last)
pollstar$This<-as.factor(pollstar$This)
pollstar$new_song<-as.factor(pollstar$new_song)
str(pollstar)


#ticket.sold
summary(pollstar$Tickets_Sold)
ticket=0
for(i in 1:15761){
  if(pollstar[i,4]>18757)ticket[i]="5"
  else if(pollstar[i,4]>17801)ticket[i]="4"
  else if(pollstar[i,4]>12397)ticket[i]="3"
  else if(pollstar[i,4]>5916)ticket[i]="2"
  else ticket[i]="1"
}
pollstar[,4]=factor(ticket)

#capacity
summary(pollstar$Capacity)
capacity=0
for(i in 1:15761){
  if(pollstar[i,5]>17302)capacity[i]="5"
  else if(pollstar[i,5]>15151)capacity[i]="4"
  else if(pollstar[i,5]>12582)capacity[i]="3"
  else if(pollstar[i,5]>6029)capacity[i]="2"
  else capacity[i]="1"
}
pollstar[,5]=factor(capacity)

#percentage
summary(pollstar$Percentage)
per=0
for(i in 1:15761){
  if(pollstar[i,6]>0.99)per[i]="5"
  else if(pollstar[i,6]>0.9312)per[i]="4"
  else per[i]="3"
}
pollstar[,6]=factor(per)

#US.Gross
summary(pollstar$US_Gross)
gross=0
for(i in 1:15761){
  if(pollstar[i,8]>1558827)gross[i]="5"
  else if(pollstar[i,8]>1583539)gross[i]="4"
  else if(pollstar[i,8]>786888)gross[i]="3"
  else if(pollstar[i,8]>342061)gross[i]="2"
  else ticket[i]="1"
}
pollstar[,8]=factor(gross)

#min_price
summary(pollstar$min_price)
minp=0
for(i in 1:15761){
  if(pollstar[i,9]>409)minp[i]="5"
  else if(pollstar[i,9]>49)minp[i]="4"
  else if(pollstar[i,9]>39)minp[i]="3"
  else if(pollstar[i,9]>29)minp[i]="2"
  else minp[i]="1"
}
pollstar[,9]=factor(minp)

#open_x
summary(pollstar$Open.x)
ox=0
for(i in 1:15761){
  if(pollstar[i,25]>1122)ox[i]="5"
  else if(pollstar[i,25]>1039)ox[i]="4"
  else if(pollstar[i,25]>1035)ox[i]="3"
  else if(pollstar[i,25]>941)ox[i]="2"
  else ox[i]="1"
}
pollstar[,25]=factor(ox)

#high.x
summary(pollstar$High.x)
hx=0
for(i in 1:15761){
  if(pollstar[i,26]>1133)hx[i]="5"
  else if(pollstar[i,26]>1048)hx[i]="4"
  else if(pollstar[i,26]>1047)hx[i]="3"
  else if(pollstar[i,26]>948)hx[i]="2"
  else hx[i]="1"
}
pollstar[,26]=factor(hx)

#close.x
summary(pollstar$Close.x)
cx=0
for(i in 1:15761){
  if(pollstar[i,27]>1120)cx[i]="5"
  else if(pollstar[i,27]>1039)cx[i]="4"
  else if(pollstar[i,27]>1033)cx[i]="3"
  else if(pollstar[i,27]>942)cx[i]="2"
  else cx[i]="1"
}
pollstar[,27]=factor(cx)

#volume.x
summary(pollstar$Volume.x)
vx=0
for(i in 1:15761){
  if(pollstar[i,28]>1823999)vx[i]="5"
  else if(pollstar[i,28]>1595454)vx[i]="4"
  else if(pollstar[i,28]>1389599)vx[i]="3"
  else if(pollstar[i,28]>1169799)vx[i]="2"
  else vx[i]="1"
}
pollstar[,28]=factor(vx)

#low.y
summary(pollstar$Low.y)
ly=0
for(i in 1:15761){
  if(pollstar[i,29]>48)ly[i]="5"
  else if(pollstar[i,29]>42.5)ly[i]="4"
  else if(pollstar[i,29]>42.7)ly[i]="3"
  else if(pollstar[i,29]>36)ly[i]="2"
  else ly[i]="1"
}
pollstar[,29]=factor(ly)

#volum.y
summary(pollstar$Volume.y)
vy=0
for(i in 1:15761){
  if(pollstar[i,30]>1737399)vy[i]="5"
  else if(pollstar[i,30]>1535017)vy[i]="4"
  else if(pollstar[i,30]>1259499)vy[i]="3"
  else if(pollstar[i,30]>961699)vy[i]="2"
  else vy[i]="1"
}
pollstar[,30]=factor(vy)

#change
summary(pollstar$Change)
c=0
for(i in 1:15761){
  if(pollstar[i,33]>0.099)c[i]="5"
  else if(pollstar[i,33]>0.05897)c[i]="4"
  else c[i]="3"
}
pollstar[,33]=factor(c)


str(pollstar)








pollstar = pollstar[complete.cases(pollstar),]
set.seed(200)
n<-nrow(pollstar)
train_indices<-1:round(0.8*n+1)
traindata<-pollstar[train_indices,]
test_indices<-(round(0.8*n)+1):n
testdata<-pollstar[test_indices,]

svm_model <- svm(USGorss.TicketSold~Last+Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
                   Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
                   Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data = traindata)
results <- predict(svm_model,testdata)
print(results)
conf_matrix<-table(results,testdata$USGorss.TicketSold)
accuracy<-sum(diag(conf_matrix))/sum(conf_matrix)
accuracy

