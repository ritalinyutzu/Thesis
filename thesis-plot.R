setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout_new.csv")
library('ggplot2')
library('GGally')

ggpairs(pollstar)
#售票率與場地可納人數對每票收益的散佈圖
qplot(x=Tickets_Sold,y=Capacity,data=pollstar,color=USGorss.TicketSold)

#上週點擊率、本週點擊率對每票收益的散佈圖
qplot(x=Last,y=This,data=pollstar,color=USGorss.TicketSold)

#本資料完售率表現
table(pollstar$SoldOut)
df<- data.frame(SoldOut=c("SoldOut","Left"),perc=c(9018,6743))
ggplot(data=df)+geom_bar(aes(x=factor(1),y=perc,fill=SoldOut),stat="identity")+coord_polar("y",start=0)

#本資料共演團體機制表現
table(pollstar$Supporting_Act_Dummy)
df1<- data.frame(supporting=c("Yes","No"),perc=c(10308,5453))
ggplot(data=df1)+geom_bar(aes(x=factor(1),y=perc,fill=supporting),stat="identity")+coord_polar("y",start=0)

#本資料策展人是否為場地擁有者
table(pollstar$InhousePromotion)
df2<- data.frame(In_House_Promotion=c("Yes","No"),perc=c(1210,14551))
ggplot(data=df2)+geom_bar(aes(x=factor(1),y=perc,fill=In_House_Promotion),stat="identity")+coord_polar("y",start=0)

#是否辦在美國
table(pollstar$US)
df3<- data.frame(US=c("Yes","No"),perc=c(12308,3453))
ggplot(data=df3)+geom_bar(aes(x=factor(1),y=perc,fill=US),stat="identity")+coord_polar("y",start=0)

#演唱會是否為假期
table(pollstar$Holiday)
df4<- data.frame(Holiday=c("Yes","No"),perc=c(3159,12602))
ggplot(data=df4)+geom_bar(aes(x=factor(1),y=perc,fill=Holiday),stat="identity")+coord_polar("y",start=0)
#問題：週末並未計入為假期，且所有連假加總日數本來就比週休二日的總數少！

#策展人是否為Live Nation
table(pollstar$Live.Nation)
df5<- data.frame(Live.Nation=c("Yes","No"),perc=c(9190,6571))
ggplot(data=df5)+geom_bar(aes(x=factor(1),y=perc,fill=Live.Nation),stat="identity")+coord_polar("y",start=0)

#演唱會前是否發表新歌
table(pollstar$new_song)
df6<- data.frame(new_song=c("Yes","No"),perc=c(6513,9248))
ggplot(data=df6)+geom_bar(aes(x=factor(1),y=perc,fill=new_song),stat="identity")+coord_polar("y",start=0)

#是否登過百大排行，來區分其上週點擊率與本週點擊率來觀察對每票收益的影響
library('lattice')
cloud(x=USGorss.TicketSold~This+Last | Global_Concert_Pulse_Dummy,data=pollstar)
