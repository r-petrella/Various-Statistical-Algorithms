# sample

x=c(94,197,16,38,99,141,23)
n=7

mean(x)

library(boot)

#we consider the following two estimators
#sX=mean(x)
#sY=log(mean(x))

## nonparametric bootstrap 
B=10000
Xb=matrix(0,B,n)
sXb=matrix(0,B)
sYb=matrix(0,B)
set.seed(1)
for (i in 1:B) {
  Xb[i,]=sample(x,n,replace=TRUE)
  sXb[i]=mean(Xb[i,])
  sYb[i]=log(sXb[i])
}


hist(sXb)
hist(sYb)

### normal intervals normali for Xb and Yb

## standard error estimate 
std.X=sd(c(sXb))
## interval
mean(x)-1.96*std.X
mean(x)+1.96*std.X


## standard error estimate 
std.Y=sd(c(sYb))
## interval
log(mean(x))-1.96*std.Y
log(mean(x))+1.96*std.Y


### percentile intervals
q1<-quantile(sXb,c(0.025,0.975))
q2<-quantile(sYb,c(0.025,0.975))


## check of the TRANSFORMING RESPECTING property
exp(quantile(sYb,c(0.025,0.975)))

##basic intervals
B=10000
sXb2=matrix(0,B)
sYb2=matrix(0,B)
for (i in 1:B){
  sXb2[i]=mean(Xb[i,])-mean(x)
  sYb2[i]=log(sXb[i])-log(mean(x))
}
q3<-quantile(sXb2,c(0.025,0.975))
mean(x)-q3[2]
mean(x)-q3[1]
#or
mean(x)*2-q1[2]
mean(x)*2-q1[1]

##second estimator
q4<-quantile(sYb2,c(0.025,0.975))
log(mean(x))-q4[2]
log(mean(x))-q4[1]
#or
log(mean(x))*2-q2[2]
log(mean(x))*2-q2[1]

###t bootstrap intervals
stima.1=function(data)
{
  out=mean(data)
  return(out)
}


stima.2=function(data)
{
  out=log(mean(data))
  return(out)
}

library(bootstrap)
?boott
# intervals t bootstrap
int.conf.stima1.tboot<-boott(x,stima.1,nbootsd=200, nboott=1000)
int.conf.stima2.tboot<-boott(x,stima.2,nbootsd=200, nboott=1000)


##confidence intervals with the function boot.ci
### non parametric BOOTSTRAP 
library(boot)
theta1<-function(data,i){
  mean(data[i])
}

theta2<-function(data,i){
  log(mean(data[i]))
}


set.seed(1)
stima1.boot<-boot(x,theta1,R=10000)
set.seed(1)
stima2.boot<-boot(x,theta2,R=10000)

plot(stima1.boot)
plot(stima2.boot)

### confidence intervals

int.conf.stima1<-boot.ci(stima1.boot,conf=0.95,type=c("norm","perc","basic","bca"))
int.conf.stima2<-boot.ci(stima2.boot,conf=0.95,type=c("norm","perc","basic","bca"))
#transforming property
exp(int.conf.stima2$normal[-1])
exp(int.conf.stima2$basic[-c(1,2,3)])
exp(int.conf.stima2$percent[-c(1,2,3)])
exp(int.conf.stima2$bca[-c(1,2,3)])
