rm(list=ls());gc()
setwd("~/Desktop/")
pollstar <-read.csv("Rita_2017+2018_clean_garbageout.csv")

#lasso regression
library('ISLR')
library('caret')
library('ggplot2')
library('lattice')
sampling_size <- floor(0.8*nrow(pollstar))
set.seed(100)
training_ind <- sample(seq_len(nrow(pollstar)), size = sampling_size)
training_set <- pollstar[training_ind, ]
testing_set <- pollstar[-training_ind, ]

library('leaps')
library('glmnet')
def_plan = caret::preProcess(training_set, method=c("scale"))
def_plan$method
default_train = predict(def_plan, training_set)
default_test = predict(def_plan, testing_set)

# 畫出常態機率分佈圖
training_set_plot <- density(log(training_set$USGorss.TicketSold))
testing_set_plot <- density(log(testing_set$USGorss.TicketSold))
plot(training_set_plot)
plot(testing_set_plot)

# 畫Q-Q plot
ggplot(data.frame(x=training_set$USGorss.TicketSold), aes(sample=x)) + stat_qq() + stat_qq_line()
ggplot(data.frame(x=testing_set$USGorss.TicketSold), aes(sample=x)) + stat_qq() + stat_qq_line()
# Lilliefors(kolmogoroc-smirnov) normality test 常態性檢定
nortest::lillie.test(training_set$USGorss.TicketSold)
# Ans：Training和Testing之P-Value < 0.1 符合常態性假設
pollstar_fit  <- lm(USGorss.TicketSold~.,data=pollstar)
resid = pollstar$USGorss.TicketSold - predict(pollstar_fit,pollstar)
qqnorm(resid);qqline(resid)
#畫三維可視圖
library('scatterplot3d')
last<-pollstar$Last
change<-pollstar$Change
gross<-pollstar$USGorss.TicketSold
scatterplot3d(change,gross,last,main="Basic 3D Scatter  Plot")
summary(pollstar_fit)

# 建立Lasso Model
x <- model.matrix(USGorss.TicketSold~.,default_train)
y <- default_train$USGorss.TicketSold
x <- as.matrix(x)
y <- as.numeric(y)
summary(pollstar$USGorss.TicketSold)

# 使用 Lasso Regression 來挑選變數
lasso.tr <- glmnet(x,y)
pred <- predict(lasso.tr,x)
dim(pred)
rmse <- sqrt(apply((y-pred)^2,2,mean))
summary(rmse)
# 找出最適lambda並繪圖
plot(log(lasso.tr$lambda),rmse,type="b",xlab="log(lambda")
lam.best <- lasso.tr$lambda[order(rmse)[1]]
coef(lasso.tr,s=lam.best)
lam.best

# 建立最佳lambda 的Lasso Regression模型
lasso_model <- glmnet(x,y,alpha = 1,lambda = 0.0004707585)
coef(lasso_model,s=lam.best)
cv.lasso <- cv.glmnet(x,y,alpha=1,parallel=TRUE,standardize=TRUE,type.measure = 'class')
plot(cv.lasso)
cv.lasso
summary(lasso_model)
coef(lasso_model,s=c(lasso_model$lambda[33],0.0004707585))
plot(lasso_model,xvar="lambda",label=T)

#交叉驗證
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso_model,s=bestlam,newx = x)
mean((lasso.pred-y)^2)
lasso.coef=predict(lasso_model,type="coefficients",s=lam.best)
length(lasso_model)
lambda_min <- cv.out$lambda.min
lamba_1se <- cv.out$lambda.1se
coef(cv.out,s=lamba_1se)
c(cv.out$lambda.min,cv.out$lambda.1se)
cv.predict <-table(predict(cv.out,newx=x,type="response",s="lambda.1se"))

#SVM Machine Learning
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

#兩階段最小平方法2SLS
#Stage1
tsls1<-lm(Last~ Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
            Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
            Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data=pollstar)
summary(tsls1)
pollstar = pollstar[complete.cases(pollstar),]
d.hat<-fitted.values(tsls1)
pollstar<-cbind(pollstar,d.hat)
summary(d.hat)
#Stage2
tsls2<-lm(USGorss.TicketSold~ Last+Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
            Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
            Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data=pollstar)
summary(tsls2)
rmse<-function(error)
{
  sqrt(mean(error^2))
}
error<-tsls2$residuals
tsls2RMSE<-rmse(error)
tsls2RMSE

#tuning SVM better
tuneResult<-tune(svm,USGorss.TicketSold~Last+Shows+Year+Month+Tickets_Sold+Capacity+Percentage+SoldOut+US_Gross+min_price+Supporting_Act_Dummy+
                   Global_Concert_Pulse_Dummy+Global_Concert_Pulse_Times+InhousePromotion+no_promoter+US+Canada+Europe+UK+SouthUS+Oceania+
                   Date_1_Whichday+Live.Nation+Holiday+Open.x+High.x+Close.x+Volume.x+Low.y+Volume.y+This+Change+new_song,data=pollstar,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:9)))
print(tuneResult)
plot(tuneResult)
tuneModel<-tuneResult$best.model
tuneModelY<-predict(tuneModel,pollstar)
tune_error<-pollstar$USGorss.TicketSold - tuneModelY
tuneResultRMSE<-rmse(tune_error)
