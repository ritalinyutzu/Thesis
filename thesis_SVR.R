rm(list=ls());gc()
setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout_new.csv")
#
library('e1071')
model<-lm(USGorss.TicketSold~Last+Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
            Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
            Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data=pollstar)
predicteddY<-predict(model,pollstar)
rmse<-function(error)
{
  sqrt(mean(error^2))
}
error<-model$residuals #same as  data$Y - predictedY
predictionRMSE<-rmse(error)
svm_model<-svm(USGorss.TicketSold~Last+Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
                 Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
                 Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data=pollstar)
predictedY<- predict(model,pollstar)
svm_error<-svm_model$residuals
svrPredictionRMSE<-rmse(svm_error)
svrPredictionRMSE
