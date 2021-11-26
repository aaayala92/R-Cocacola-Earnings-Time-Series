# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# ----------------------------------------------
library(fBasics)
library(forecast) 
# ----------------------------------------------

datos<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
head(datos)
nrow(datos) 

y<-datos[,2]
ts.plot(y)

#y<-datos[60:107,2]
#y<-log(y)
ts.plot(y)

par(mar=c(4,4,4,4))
par(mfrow=c(3,1))

nlags=24     # play with this parameter..
ts.plot(y)
acf(y,nlags)
pacf(y,nlags)  

s=4       # seasonal parameter FOR THIS DATA SET


# formal unit root test (Augmented Dickey Fuller test). Testing for stationarity.
# Ho: the process is not stationary. We need, at least, a unit root
# H1: the process is stationary. We have to check different models (lags)
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?

# Ho: uncorrelated data
# H1: correlated data

Box.test(y,lag=nlags) # white noise residuals?

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
#fit<-arima(y,order=c(5,0,0),seasonal=list(order=c(2,1,0),period=s)) 
y<-log(datos[,2])
fit<-arima(y,order=c(5,0,2),seasonal=list(order=c(0,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

# Ho: uncorrelated data
# H1: correlated data

Box.test(fit$residuals,lag=nlags) # white noise residuals?

auto.arima(y, seasonal = TRUE, allowdrift = FALSE)

########################## model 1 ##########################
y<-datos[,2]
fit<-arima(y,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)

acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    
ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?


########################## model 2 ##########################
y<-datos[,2]
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

########################## model 3 ##########################
y<-datos[,2]
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    



########################## model 4 ##########################
y<-datos[,2]
fit<-arima(y,order=c(4,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)


########################## model 5 ##########################
y<-datos[,2]
fit<-arima(y,order=c(3,0,2),seasonal=list(order=c(0,1,0),period=s)) 
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)




########################## model 6 ##########################
y<-datos[,2]
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)


########################## model 7 ##########################
y<-datos[,2]
fit<-arima(y,order=c(1,0,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

########################## model 8 ##########################
y<-datos[,2]
fit<-arima(y,order=c(3,0,1),seasonal=list(order=c(0,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

###############################LOG MODELS###############################

auto.arima(log(datos[,2]), seasonal = TRUE,allowdrift = FALSE)
########################## model 9 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)


########################## model 10 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(5,0,2),seasonal=list(order=c(0,1,0),period=s))  
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

########################## model 11 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

########################## model 12 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)


########################## model 13 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(3,0,1),seasonal=list(order=c(0,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)


########################## model 14 ##########################
y<-log(datos[,2])
fit<-arima(y,order=c(3,0,1),seasonal=list(order=c(3,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

#######################BoxTest###################
#-------------------------------------------------------------------------
# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data

Box.test(fit$residuals,lag=nlags) # white noise residuals?


# formal normality test
# Ho: the data is normally distributed
# H1: the data is not normally distributed
shapiro.test(fit$residuals)  # normality test

# normality test graphically
par(mfrow=c(1,1))
hist(fit$residuals,prob=T,xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")


# C.	Testing for STRICT WHITE NOISE

par(mar=c(4,4,4,4)) # to adjust graphic size

par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(fit$residuals^2)   
acf(fit$residuals^2,nlags)
pacf(fit$residuals^2,nlags)
# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test(y^2,lag=20, type="Ljung")    # Null: ro1=.=ro20=0

# D.	Testing for GAUSSIAN WHITE NOISE
shapiro.test(y)
# GWN ??? SWN

######################### P-Values #################################

(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))/2 

 #Coefficients that are significant
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))/2 < 0.05




#--------------------------Difference Transformation-----------------
#y<-diff(y) #for 2 transformations
z<-diff(y)  
ts.plot(z)

par(mfrow=c(3,1))
ts.plot(z)   
acf(z)
pacf(z)
mean(z)

ndiffs(z, alpha=0.05, test=c("adf"))

Box.test (z, lag = 20, type="Ljung") 

Box.test (z^2, lag = 20, type="Ljung") 

#Checking for normality

shapiro.test(z)

hist(z,prob=T,ylim=c(0,0.6),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")






#-----------plotting real data with point predictions--------
########### PLOTTING ################

par(mar=c(4,4,4,4))
par(mfrow=c(2,1))

nrow(datos)-24
y<-datos[1:83,2]
y
y.pred<-predict(fit,n.ahead=25)
y.pred$pred   # point predictions
y.pred$se    # standard errors

a<-datos[,2]


predictions<-y.pred$pred
predictions<-exp(y.pred$pred)   #This is for log transformation

new <- c(y,predictions) # real data + predicted values

plot.ts(new,main="Predictions",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(a,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)


real<-datos[,2][83:107]
c=1:25
plot(c,real,type="b",col="red", ylim=c(0.4,1.2))
lines(c,predictions,col="blue",type="b")
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,10),bty ="n")


#plot(forecast(fit))

######################## Confidence Intervals ##############################

y.pred<-predict(fit,n.ahead=25)


y.pred$pred   # point predictions
y.pred$se    # standard errors

y<-datos[,2]
y[83]

  #One period ahead
CI_upper1 <-y.pred$pred[1]+(1.96*y.pred$se[1])
CI_lower1 <-y.pred$pred[1]-(1.96*y.pred$se[1])

  #eight periods ahead

y[90]

CI_upper8 <-y.pred$pred[8]+(1.96*y.pred$se[5])
CI_lower8 <-y.pred$pred[8]-(1.96*y.pred$se[5])

CI_upper1
CI_lower1

CI_upper8
CI_lower8

 #Log Transformation
 exp(CI_upper1)
 exp(CI_lower1)
 
 exp(CI_upper8)
 exp(CI_lower8)

######## Errors ######

y<-datos[,2]
n<-length(y)
n

n.estimation<-83 # 
n.forecasting<-n-n.estimation 
horizontes<-8 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)];
    #fit<-arima(aux.y,order=c(5,1,0),seasonal=list(order=c(2,1,0),period=s));
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- (y.pred$pred[Periods_ahead]);
    #predicc[i,Periods_ahead]<- (exp(y.pred$pred[Periods_ahead]));   #for log transformations
  }
  error<-real-predicc[,Periods_ahead];         

  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}

MSFE
MAPE

Errors_Model<-data.frame(MSFE,MAPE)
Errors_Model




















########################## model 8 ##########################
y<-datos[1:83,2]
fit<-arima(y,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=s)) 
fit

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals,m=s,test=c("ocsb"))  # seasonal differences?

mean(fit$residuals)

