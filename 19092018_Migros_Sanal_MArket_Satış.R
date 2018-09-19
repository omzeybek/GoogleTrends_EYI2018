Sanal_ciro<-read_xlsx(file.choose())
colnames(Sanal_ciro)[c(1,2)]<-c("Tarih","Ciro")
str(Sanal_ciro)
Sanal_ciro$Tarih<-as.POSIXlt(Sanal_ciro$Tarih)
install.packages('forecast', dependencies = TRUE)
require(forecast)
require(ggplot2)
summary(Sanal_ciro)
p<-ggplot(Sanal_ciro,aes(x=Tarih,y=Ciro)) +
geom_line()

View(Sanal_ciro)

rm(Sanal_ciro)
  
?strptime
  
  ggplot(data=Sanal_ciro,aes(x="Tarih",y="Ciro"))

names(Sanal_ciro)