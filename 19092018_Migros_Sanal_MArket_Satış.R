require(readxl)
require(zoo)
require(xts)
mydf<-read_xlsx(file.choose())
str(mydf)
#mydf$dummy_deep<-if_else(mydf$Date %in% c("2014-08-03","2015-09-27","2016-07-10","2016-09-18","2017-09-03"),1,0)
#table(mydf$dummy_deep)
colnames(mydf)[c(1:3)]<-c("Date","Sales","Exclude")
my_xts <- xts(mydf[, -1], order.by = mydf$Date, tzone = Sys.getenv("TZ"))
# rollup to weekly. Dates shown are the last day in the weekperiod.


my_xts_weekly <- period.apply(my_xts, endpoints(my_xts, on = "weeks"), colSums)
nrow(my_xts_weekly)
head(my_xts_weekly)
#İlk olarak kurban satışlarını sanal markt cirosundan düşürüyoruz.
my_xts_weekly$Sales<-(my_xts_weekly$Sales-my_xts_weekly$Exclude)
my_xts_weekly<-my_xts_weekly[,c(1)]
my_xts_weekly$Sales<-log(my_xts_weekly$Sales)
#Şimdi grafiğimize bakalım, grafik üzerindeki noktaları daha rahat incelemek için interactive grafik çizen bir paket yükleyeceğim
library(dygraphs)
p<-dygraph(my_xts_weekly,xlab="Haftalar",ylab="Satıs")
my_ts_weekly<-ts(my_xts_weekly$Sales,frequency=7)
decompose_Sales<-decompose(my_ts_weekly,"additive")
plot(decompose_Sales$seasonal)
plot(decompose_Sales$trend)
plot(decompose_Sales$random)
library(forecast)
library(dplyr)

my_xts_weekly$Sales_deep<-if_else(index(my_xts_weekly)=="2014-08-03 03:00:00"|index(my_xts_weekly)=="2015-09-27 03:00:00"|index(my_xts_weekly)=="2016-07-10 03:00:00" |index(my_xts_weekly)=="2016-09-18 03:00:00"|index(my_xts_weekly)=="2017-09-03 03:00:00",1,0)
my_xts_weekly$Sales_peak<-if_else(index(my_xts_weekly)=="2017-05-28 03:00:00"|index(my_xts_weekly)=="2017-12-31 03:00:00"|index(my_xts_weekly)=="2016-06-05 03:00:00" |index(my_xts_weekly)=="2015-06-21 03:00:00"|index(my_xts_weekly)=="2014-06-29 03:00:00",1,0)
head(my_xts_weekly)
#*******************************************************************************************************************************************************
#* Unit Root testleri buradan itibaren başlıyor 
#*                                                                                                                                                     *
#*                                                                                                                                                     *
#*                                                                                                                                                     *
#*******************************************************************************************************************************************************
library(urca)
#Satış üzerindeki şokları içermeyen düz ADF testi 
ADF.out<-ur.df(my_xts_weekly$Sales,type="trend",selectlags="AIC")
summary(ADF.out)

#KPSS 
KPSS.out<-ur.kpss(my_xts_weekly$Sales,type="tau",lags="long")
summary(KPSS.out)

#ERS Point Optimal 
ERS.out<-ur.ers(my_xts_weekly$Sales,type="DF-GLS",model="trend",lag.max=5)
summary(ERS.out)

#Philips-Perron1988 
PP.out<-ur.pp(my_xts_weekly$Sales,type="Z-tau",model="trend",lags="long")
summary(PP.out)

#Zivot
ZA.out<-ur.za(my_xts_weekly$Sales,model="both")
summary(ZA.out)

#Forecast kuruluyor 
require(forecast)
Exo.Vars<-cbind(my_xts_weekly$Sales_deep,my_xts_weekly$Sales_peak)
Arima.out<-forecast::auto.arima(my_xts_weekly$Sales,xreg=Exo.Vars)
summary(Arima.out)
checkresiduals(Arima.out)

?auto.arima