setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout_new.csv")
library('AER')


tsls2 <- ivreg(formula = USGorss.TicketSold ~ Last+Shows+Tickets_Sold+Capacity+Percentage+SoldOut+min_price+Supporting_Act_Dummy+
                      Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
                      Date_1_Whichday+Live.Nation+Holiday+This+Change+new_song|Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+Shows+Tickets_Sold+Capacity+Percentage+SoldOut+min_price+Supporting_Act_Dummy+
                 Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
                 Date_1_Whichday+Holiday+new_song,data=pollstar)

summary(tsls2, diagnostics = TRUE)


summary(tsls2)
rmse<-function(error)
{
  sqrt(mean(error^2))
}
error<-tsls2$residuals
tsls2RMSE<-rmse(error)
tsls2RMSE