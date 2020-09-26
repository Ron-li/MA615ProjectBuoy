library(ggplot2)
library(stringr)
library(rstanarm)
library(lubridate)
library(tseries)
library(forecast)
library(urca)

MRC$factor <- as.factor(MRC$YYYY)

#plot the boxplot of atem&wtem
ggplot(MRC, aes(x = factor, y = ATMP)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "atem")+
  scale_x_discrete(name = "year") +
  ggtitle("Boxplot of atem")

ggplot(MRC, aes(x = factor, y = WTMP)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "wtem")+
  scale_x_discrete(name = "year") +
  ggtitle("Boxplot of wtem")
#result we can't find trend on the graphes

ameans <- array()
wmeans <- array()
z <- 1
for (i in 1999:2018){
  for (j in 1:12){
    ameans[z] <- mean(subset(MRC, YYYY == i)$ATMP[which(subset(MRC, YYYY == i)$MM == j)])
    wmeans[z] <- mean(subset(MRC, YYYY == i)$WTMP[which(subset(MRC, YYYY == i)$MM == j)])
    z <- z+1
    if (z > 5){  
      if (is.na(ameans[z-2])){
        ameans[z-2] <- (ameans[z-3]+ameans[z-1])/2 
      }
      if (is.na(wmeans[z-2])){
        wmeans[z-2] <- (wmeans[z-3]+wmeans[z-1])/2 
      }
    }
  }
}
atmpseries <- ts(ameans, frequency=12, start=c(1999,1))
wtmpseries <- ts(wmeans, frequency=12, start=c(1999,1)) 
plot(atmpseries, main = "The Time Series of ATMP", xlab = "Date")
plot(wtmpseries, main = "The Time Series of WTMP", xlab = "Date")


tsdisplay(atmpseries)
dca <- decompose(atmpseries)
plot(dca)
seasona<-dca$figure
plot(seasona,type = "b",xaxt="n",xlab = "")
a<-auto.arima(atmpseries)
print(a)
fit<-arima(atmpseries,order = c(0, 0, 1),seasonal = list(order=c(2, 1, 0),period=12))
print(fit)
fore=predict(fit)
ts.plot(atmpseries, fore$pred, col=c(1,2,4,4), lty=c(1,1,2,2))
ur.df(atmpseries)
#Value of test-statistic is: -0.8381
#<5%, we can consider the series stable








