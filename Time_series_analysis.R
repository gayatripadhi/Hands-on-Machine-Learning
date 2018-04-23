setwd("F:/")
data<-read.csv("final_data_11.04.18.csv",header=T)
head(data)
str(data)
summary(data)
library(psych)
describe(data)
na.rm=TRUE

library(zoo)
data$Period<-as.yearmon(as.character(data$Period), "%Y%m")
head(data)
dataf<-as.data.frame(data)

library(stringr)
ID<-str_extract(dataf$Pharmacy,"[[:digit:]]+")
dataf<-cbind(ID,dataf)
dataf$Pharmacy<-str_sub(dataf$Pharmacy, 1, str_length(dataf$Pharmacy)-9)
dataf$Pharmacy<-gsub("/", ",", dataf$Pharmacy) 
library(tidyr)
data1<-separate(dataf,Pharmacy,c('Pharmacy Name','Address','Location','Location1'), sep = ",", remove = TRUE)
data1$Location1<-NULL
data1<-as.data.frame(data1)
head(data1)

pharmfact<-levels(factor(data1$`Pharmacy Name`))
print(pharmfact)
packfact<-levels(factor(data1$`Pack`))
print(packfact)
AP_PHARMACY=subset(data1, data1$`Pharmacy Name`== 'A & P PHARMACY')
ABDULLAH_MS=subset(data1, data1$`Pharmacy Name`== 'ABDULLAH M.S')
ABDULLAH_SONS=subset(data1, data1$`Pharmacy Name`== 'ABDULLAH SONS')
AP_PHARMACY_AMOXILCAPS250=subset(AP_PHARMACY, AP_PHARMACY$Pack== 'AMOXIL CAPS 0250MG 000000 00100')
AP_PHARMACY_AMOXILCAPS500=subset(AP_PHARMACY, AP_PHARMACY$Pack== 'AMOXIL CAPS 0500MG 000000 00020')
AP_PHARMACY_AMOXILDROPS125=subset(AP_PHARMACY, AP_PHARMACY$Pack== 'AMOXIL DROPS 0125MG 0020ML 00001')
ABDULLAH_MS_AMOXILCAPS250=subset(ABDULLAH_MS, ABDULLAH_MS$Pack== 'AMOXIL CAPS 0250MG 000000 00100')
ABDULLAH_MS_AMOXILCAPS500=subset(ABDULLAH_MS, ABDULLAH_MS$Pack== 'AMOXIL CAPS 0500MG 000000 00020')
ABDULLAH_MS_AMOXILDROPS125=subset(ABDULLAH_MS, ABDULLAH_MS$Pack== 'AMOXIL DROPS 0125MG 0020ML 00001')
ABDULLAH_SONS_AMOXILCAPS250=subset(ABDULLAH_SONS, ABDULLAH_SONS$Pack== 'AMOXIL CAPS 0250MG 000000 00100')
ABDULLAH_SONS_AMOXILCAPS500=subset(ABDULLAH_SONS, ABDULLAH_SONS$Pack== 'AMOXIL CAPS 0500MG 000000 00020')
ABDULLAH_SONS_AMOXILDROPS125=subset(ABDULLAH_SONS, ABDULLAH_SONS$Pack== 'AMOXIL DROPS 0125MG 0020ML 00001')

library(forecast)
library(tseries)
library(ggplot2)
library(xts)

#Value
ts_AP_PHARMACY_AMOXILCAPS250_Value<-xts(x = AP_PHARMACY_AMOXILCAPS250$Value, order.by = AP_PHARMACY_AMOXILCAPS250$Period)
mAP_PHARMACY_AMOXILCAPS250_Value<-as.matrix(ts_AP_PHARMACY_AMOXILCAPS250_Value)
train_Value<-ts(data=mAP_PHARMACY_AMOXILCAPS250_Value,start=c(2015,7),end=c(2017,6),frequency=12)
test_Value<-ts(mAP_PHARMACY_AMOXILCAPS250_Value,start=c(2017,7),end=c(2017,12),frequency=12)
summary(mAP_PHARMACY_AMOXILCAPS250_Value)
class(train_Value)
class(test_Value)
adf.test(train_Value, alternative="stationary")#p=0.037,lag=2
acf(train_Value,plot=TRUE)
pacf(train_Value,plot=TRUE)

library(ggfortify)
ggfreqplot(train_Value,frequency=12)
autoplot(acf(train_Value, plot = FALSE))
autoplot(pacf(train_Value, plot = FALSE))

#Units
ts_AP_PHARMACY_AMOXILCAPS250_Units<-xts(x = AP_PHARMACY_AMOXILCAPS250$Units, order.by = AP_PHARMACY_AMOXILCAPS250$Period)
mAP_PHARMACY_AMOXILCAPS250_Units<-as.matrix(ts_AP_PHARMACY_AMOXILCAPS250_Units)
train_Units<-ts(data=mAP_PHARMACY_AMOXILCAPS250_Units,start=c(2015,7),end=c(2017,6),frequency=12)
test_Units<-ts(mAP_PHARMACY_AMOXILCAPS250_Units,start=c(2017,7),end=c(2017,12),frequency=12)
summary(mAP_PHARMACY_AMOXILCAPS250_Units)
Units<-window()
class(train_Units)
class(test_Units)
adf.test(train_Units, alternative="stationary")#p-value=0.037<0.5
acf(train_Units,plot=TRUE)#p=1
pacf(train_Units,plot=TRUE)#q=0

ggfreqplot(train_Units,frequency=12)
autoplot(acf(train_Units, plot = FALSE))
autoplot(pacf(train_Units, plot = FALSE))

plot(train_Value, xlab='Period', ylab = 'Value')
plot(train_Units, xlab='Period', ylab = 'Units')

plot(log10(train_Value),ylab='Log (Value)')
plot(log10(train_Units),ylab='Log (Units)')

#azfinal.aic <- Inf
#azfinal.order <- c(0,0,0)
#for (p in 1:4) for (d in 0) for (q in 1:4) {
#     azcurrent.aic <- AIC(arima(train_Units, order=c(p, d, q)))
#     if (azcurrent.aic < azfinal.aic) {
#         azfinal.aic <- azcurrent.aic
#         azfinal.order <- c(p, d, q)
#         azfinal.arima <- arima(train_Units, order=azfinal.order)
#       }
#   }
#azfinal.order

#for(d in 0:1){
#  for(p in 0:9){
#    for(q in 0:9){
#      fit=Arima(train_Units,order=c(p,d,q))
#      print(paste0("AIC is ", AIC(fit), " for d = ", d, ", p = ", p, " and q = ", q))
#    }
# }
#}

modelAIC <- data.frame()
for(d in 0:1){
  for(p in 0:3){
    for(q in 0:3){
      fit=arima(train_Units,order=c(p,d,q))
      modelAIC <- rbind(modelAIC, c(d,p,q,AIC(fit))) 
    }
  }
}
names(modelAIC) <- c("d", "p", "q",  "AIC")
rowNum <- which(modelAIC$AIC==max(modelAIC$AIC))
modelAIC[rowNum,]#Required model parameters


fit_Value<-arima(train_Value,order=c(2,2,0))
summary(fit_Value)

fit_Units<-arima(train_Units,order=c(2,2,0))
summary(fit_Units)

predict(fit_Value,test_Value)
predict(fit_Units,test_Units)


autoplot(ts(cbind(train_Units, test_Units), start = c(2015,7), frequency = 12 ),facets = FALSE)


plot(fit_Units$residuals,col="red")
lines(fitted(fit_Units),col="blue")

accuracy(fit_Units)

train_Units.stl <- stl(ts(train_Units,frequency = 12), t.window=15, s.window="periodic", robust=TRUE)
plot(train_Units.stl)

