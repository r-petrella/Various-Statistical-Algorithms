
theta=5
set.seed(1)
n=15
x=rexp(n,theta)
Fn=ecdf(x)

plot(Fn,lwd=3,col=2,main="Empirical DF vs True DF")
curve(pexp(x,theta),add=TRUE,lwd=3,lty=2,col=3)


### distance
xx=seq(0,1,length.out=1000)

distance1=function(theta) return(max(abs(Fn(xx)-pexp(xx,theta))))
distance2=function(theta) return(mean(abs(Fn(xx)-pexp(xx,theta))))
distance3=function(theta) return(sqrt(mean(Fn(xx)-pexp(xx,theta))^2))

theta.val=seq(0,30,length.out=1000)
out1=out2=out3=0
for (i in 1:1000) {out1[i]=distance1(theta.val[i])
out2[i]=distance2(theta.val[i])    
out3[i]=distance3(theta.val[i])}


plot(theta.val,out1,type='l',col=1,ylim=c(0,2))
lines(theta.val,out2,type='l',col=2)
lines(theta.val,out3,type='l',col=3)

theta.val[which.min(out1)]
theta.val[which.min(out2)]
theta.val[which.min(out3)]

optim(1,distance1,method="BFGS")$par
optim(1,distance2,method="BFGS")$par
optim(1,distance3,method="BFGS")$par


#### the sample with n=1000

theta=5
set.seed(123)
n=1000
x=rexp(n,theta)
Fn=ecdf(x)

plot(Fn,lwd=3,col=2,main="Empirical DF vs True DF")
curve(pexp(x,theta),add=TRUE,lwd=3,lty=2,col=3)


### distance
xx=seq(0,1,length.out=1000)

distance1=function(theta) return(max(abs(Fn(xx)-pexp(xx,theta))))
distance2=function(theta) return(mean(abs(Fn(xx)-pexp(xx,theta))))
distance3=function(theta) return(sqrt(mean(Fn(xx)-pexp(xx,theta))^2))

theta.val=seq(0,30,length.out=1000)
out1=out2=out3=0
for (i in 1:1000) {out1[i]=distance1(theta.val[i])
out2[i]=distance2(theta.val[i])    
out3[i]=distance3(theta.val[i])}


plot(theta.val,out1,type='l',col=1,ylim=c(0,2))
lines(theta.val,out2,type='l',col=2)
lines(theta.val,out3,type='l',col=3)

theta.val[which.min(out1)]
theta.val[which.min(out2)]
theta.val[which.min(out3)]

optim(1,distance1,method="BFGS")$par
optim(1,distance2,method="BFGS")$par
optim(1,distance3,method="BFGS")$par