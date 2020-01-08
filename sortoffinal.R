library(Rcmdr)
library(rmgarch)
library(timeSeries)
library(fAssets)
library(tseries)
library(dplyr)
library(excel.link)
library(readxl)
library(moments)
library(parma)
library(forecast)
library(fAssets)
library(aTSA)

exchangeratefinalforsure <- read_excel("dissertation/exchangeratefinalforsure.xlsx", 
                                       col_types = c("date", "numeric", "numeric", 
                                                     "numeric", "numeric"))
View(exchangeratefinalforsure)
growthratesfinal <- read_excel("dissertation/growthratesfinal.xlsx", 
                          col_names = FALSE,col_types = c("numeric","numeric","numeric","numeric"))
View(growthratesfinal)


# download GROWTH RATE DATA##################################33
#For USD
USD <- growthratefinal[,1]
usdactual <- as.numeric(unlist(USD))
usdq1 <- usdactual[1:100]
usdq2 <- usdactual[101:200]
usdq3 <- usdactual[201:300]
usdq4 <- usdactual[301:400]

#FOr GBP
GBP <- growthratefinal[,2]
gbpactual<-as.numeric(unlist(GBP))
gbpq1<- gbpactual[1:100]
gbpq2<- gbpactual[101:200]
gbpq3<- gbpactual[201:300]
gbpq4<- gbpactual[301:400]

#For Euro
EUR <- growthratefinal[,3]
euractual<-as.numeric(unlist(EUR))
eurq1<- euractual[1:100]
eurq2<- euractual[101:200]
eurq3<- euractual[201:300]
eurq4<- euractual[301:400]

#For Yen
YEN <- growthratefinal[,4]
yenactual<-as.numeric(unlist(YEN))
yenq1<- yenactual[1:100]
yenq2<- yenactual[101:200]
yenq3<- yenactual[201:300]
yenq4<- yenactual[301:400]
date<- exchangeratefinalforsure[,1]
#################################################PlottingERData
usdplot<- cbind(date,exchangeratefinalforsure[,2])
gbpplot<- cbind(date,exchangeratefinalforsure[,3])
eurplot<-cbind(date,exchangeratefinalforsure[,4])
yenplot<-cbind(date,exchangeratefinalforsure[,5])


plot(usdplot,main="Graphical Analysis USD Data", xlab="Dates", ylab="ER", col ="Blue")
plot(gbpplot,main="Graphical Analysis GBP Data", xlab="Dates",ylab="ER",col = "Blue")
plot(eurplot,main="Graphical Analysis EUR Data",xlab="Dates",ylab="ER",col="Blue")
plot(yenplot,main="Graphical Analysis YEN Data",xlab="Dates",ylab="ER",col="Blue")
usdactualdata<-as.numeric(unlist(exchangeratefinalforsure[,2]))
usdqtr1<-usdactualdata[1:100]
usdqtr2<-usdactualdata[101:200]
usdqtr3<-usdactualdata[201:300]
usdqtr4<-usdactualdata[301:400]
gbpactualdata<-as.numeric(unlist(exchangeratefinalforsure[,3]))
gbpqtr1<-gbpactualdata[1:100]
gbpqtr2<-gbpactualdata[101:200]
gbpqtr3<-gbpactualdata[201:300]
gbpqtr4<-gbpactualdata[301:400]
euractualdata<-as.numeric(unlist(exchangeratefinalforsure[,4]))
eurqtr1<-euractualdata[1:100]
eurqtr2<-euractualdata[101:200]
eurqtr3<-euractualdata[201:300]
eurqtr4<-euractualdata[301:400]
yenactualdata<-as.numeric(unlist(exchangeratefinalforsure[,5]))
yenqtr1<-yenactualdata[1:100]
yenqtr2<-yenactualdata[101:200]
yenqtr3<-yenactualdata[201:300]
yenqtr4<-yenactualdata[301:400]
mu1<-mean(usdqtr1)
mu2<-mean(usdqtr2)
mu3<-mean(usdqtr3)
mu4<-mean(usdqtr4)
sduq1<-sd(usdqtr1)
sduq2<-sd(usdqtr2)
sduq3<-sd(usdqtr3)
sduq4<-sd(usdqtr4)
mp1<-mean(gbpqtr1)
mp2<-mean(gbpqtr2)
mp3<-mean(gbpqtr3)
mp4<-mean(gbpqtr4)
sdpq1<-sd(gbpqtr1)
sdpq2<-sd(gbpqtr2)
sdpq3<-sd(gbpqtr3)
sdpq4<-sd(gbpqtr4)
me1<-mean(eurqtr1)
me2<-mean(eurqtr2)
me3<-mean(eurqtr3)
me4<-mean(eurqtr4)
sdeq1<-sd(eurqtr1)
sdeq2<-sd(eurqtr2)
sdeq3<-sd(eurqtr3)
sdeq4<-sd(eurqtr4)
my1<-mean(yenqtr1)
my2<-mean(yenqtr2)
my3<-mean(yenqtr3)
my4<-mean(yenqtr4)
sdyq1<-sd(yenqtr1)
sdyq2<-sd(yenqtr2)
sdyq3<-sd(yenqtr3)
sdyq4<-sd(yenqtr4)
                       
#Making Tseries
ertseries1<-cbind(usdactual,gbpactual,euractual,yenactual)
ertseries2<-cbind(date,usdactual,gbpactual,euractual,yenactual)
erdata1<-cbind(usdq1,gbpq1,eurq1,yenq1)
erdata2<-cbind(usdq2,gbpq2,eurq2,yenq2)
erdata3<-cbind(usdq3,gbpq3,eurq3,yenq3)
erdata4<-cbind(usdq4,gbpq4,eurq4,yenq4)


#Stationarity Testing
sttest<-as.numeric(unlist(growthrates))
adf.test(sttest)
adf.test(usdactual)
adf.test(yenactual)
adf.test(euractual)
adf.test(gbpactual)
 



ertsgraph<-ts(ertseries1,start=c(2016,11),end=c(2018,7) ,frequency = 400)
plot(ertsgraph,main="Graphical Analysis of Exchange Rate Data", xlab="Dates", ylab="ER", col ="red")

#x1<-ertseries2[,"usdactual"]
#plot(x1)

#x2<-ertseries2[,"gbpactual"]
#plot(x2)

#x3<-ertseries2[,"euractual"]
#plot(x3)

#x4<-ertseries2[,"yenactual"]
#plot(x4)
#x5<-usdq1
#adf.test(x5)





##1.QUARTER 1 specification of DCC with multivariate normal distribution

uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1), distribution ='mvnorm')
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1),model="aDCC", distribution ='mvnorm')
fit.1 <- dccfit(spec.dccn, data = erdata1, solver = 'solnp',fit.control = list(eval.se = TRUE))
fit.1

ts.plot(rcor(fit.1)[1,2,])
ts.plot(rcor(fit.1)[3,2,])
ts.plot(rcor(fit.1)[4,2,])
ts.plot(rcor(fit.1)[2,1,])
ts.plot(rcor(fit.1)[3,1,])
ts.plot(rcor(fit.1)[4,1,])
ts.plot(rcor(fit.1)[1,3,])
ts.plot(rcor(fit.1)[2,3,])
ts.plot(rcor(fit.1)[4,3,])
ts.plot(rcor(fit.1)[1,4,])
ts.plot(rcor(fit.1)[2,4,])


resi1=residuals(fit.1)

##2. QUARTER 2 specification of DCC with multivariate normal distribution

uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1), distribution ='mvnorm')
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1),model="aDCC", distribution ='mvnorm')
fit.2 <- dccfit(spec.dccn, data = erdata2, solver = 'solnp',fit.control = list(eval.se = TRUE))
fit.2

ts.plot(rcor(fit.2)[1,2,])
ts.plot(rcor(fit.2)[3,2,])
ts.plot(rcor(fit.2)[4,2,])
ts.plot(rcor(fit.2)[2,1,])
ts.plot(rcor(fit.2)[3,1,])
ts.plot(rcor(fit.2)[4,1,])
ts.plot(rcor(fit.2)[1,3,])
ts.plot(rcor(fit.2)[2,3,])
ts.plot(rcor(fit.2)[4,3,])
ts.plot(rcor(fit.2)[1,4,])
ts.plot(rcor(fit.2)[2,4,])


resi2=residuals(fit.2)


##3. QUARTER 3specification of DCC with multivariate normal distribution

uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1), distribution ='mvnorm')
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1),model="aDCC", distribution ='mvnorm')
fit.3 <- dccfit(spec.dccn, data = erdata3, solver = 'solnp',fit.control = list(eval.se = TRUE))
fit.3

ts.plot(rcor(fit.3)[1,2,])
ts.plot(rcor(fit.3)[3,2,])
ts.plot(rcor(fit.3)[4,2,])
ts.plot(rcor(fit.3)[2,1,])
ts.plot(rcor(fit.3)[3,1,])
ts.plot(rcor(fit.3)[4,1,])
ts.plot(rcor(fit.3)[1,3,])
ts.plot(rcor(fit.3)[2,3,])
ts.plot(rcor(fit.3)[4,3,])
ts.plot(rcor(fit.3)[1,4,])
ts.plot(rcor(fit.3)[2,4,])


resi3=residuals(fit.3)

##4. QUARTER 4specification of DCC with multivariate normal distribution

uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1), distribution ='mvnorm')
spec.dccn <- dccspec(uspec.n, dccOrder = c(1, 1),model="aDCC", distribution ='mvnorm')
fit.4 <- dccfit(spec.dccn, data = erdata4, solver = 'solnp',fit.control = list(eval.se = TRUE))
fit.4

ts.plot(rcor(fit.4)[1,2,])
ts.plot(rcor(fit.4)[3,2,])
ts.plot(rcor(fit.4)[4,2,])
ts.plot(rcor(fit.4)[2,1,])
ts.plot(rcor(fit.4)[3,1,])
ts.plot(rcor(fit.4)[4,1,])
ts.plot(rcor(fit.4)[1,3,])
ts.plot(rcor(fit.4)[2,3,])
ts.plot(rcor(fit.4)[4,3,])
ts.plot(rcor(fit.4)[1,4,])
ts.plot(rcor(fit.4)[2,4,])


resi4=residuals(fit.4)


#Engle and Sheppard Test of Dynamic Correlation


DCCtest(erdata1, garchOrder = c(1,1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
DCCtest(erdata2, garchOrder = c(1,1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
DCCtest(erdata3, garchOrder = c(1,1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
DCCtest(erdata4, garchOrder = c(1,1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)



sttest<-as.numeric(unlist(growthrates))
adf.test(sttest)
adf.test(usdactual)
adf.test(yenactual)
adf.test(euractual)
adf.test(gbpactual)
adf.test(usdqtr1)
kpss.test(usdactual)
adf.test(resi1)

stationary.test(usdq1, method = c("adf", "pp", "kpss"), nlag = NULL, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
tempdir()
library(ggplot2)
datefinal<-date[2:401,]
usdactualplot<-cbind(datefinal,usdactual)
ggplot(usdactualplot,aes(y=usdactual,x=Date))+geom_line()