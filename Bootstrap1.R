x= c(94,197,16,38,99,141,23)
plot(ecdf(x))

#approach M
# vector with all the positions
unit<- 1:length(x)
campioni <- expand.grid(unit,unit,unit,unit,unit,unit,unit)
campioni <- as.matrix(campioni)
M=dim(campioni)[1]
7^7

media.M <- rep(0,M)
  for(i in 1:M) media.M[i] <- mean(x[campioni[i,]])
media.M

#expected value of the mean in the bootstrap world
EB.media <- mean(media.M)

#parameter of interest  in the bootstrap world
TF.media <- media(x)

bias.media <- EB.media - TF.media
varianza.media <- 1/M*sum((media.M-EB.media)^2)
MSE.media <- 1

######Monte Carlo approach
V=10000
media.B <- rep(0,B)
for (i in 1:B) media.B[i] <- mean(x[sample(1:7,7, replace=T)])

##expected value of mean in the bootstrap world
EB.media <- mean(media.B)

TF.media <- mean(x)
bias.media2 <- EB.media -TF.media

varianza.media2 <- 1/(B-1)*sum((media.B-EB.media)^2)
varianza.media
MSE.media2 <- 1/B*sum((media.B-TF.media)^2)
MSE.media



######parametric bootstrap
x=c(94,197,16,38,99,141,23)
plot(ecdf(x), lwd=3)
curve(pnorm(x,mean=mean(x),sd=sd(x)),add=T,lwd=3,lty=3,lty=2,col=2)


##non parametric analysis

library(boot)
?boot
media.f.np <- function(data,i){
  mean(data[i])
}
bootstrap.np <- boot(x,media.fr=5000)


media=mean(x)
varianza=var(x)
plot(density(bootstrap.np$t),main="non parametric analysis", lwd=2,xlab="",ylim=c(0,0.02))
curve(dnorm(x,mean=media,sd=sqrt(varianza/7)),add=T, col=2, lty=2)

##parametric approach
plot(ecdf(x))
curve(pgamma(x,shape=1,scale=mean(x)),add=T, lwd=3, lty=2, col=3)
curve(pnorm(x,mean=mean(x),sd=sd(x)),add=T, lwd=3, lty=2, col=2)


#parametric analysis
?boot 

media.f.p <- function(data){
  mean(data)
}
rg <- function(data, mle){
  out <- rgamma(length(data), shape=1, scale=mle)
}
bootstrap.p <- boot(x,media.f.p, R=5000, sim='parametric', ran.gen=rg.mle=mean(x))

plot(density(bootstrap.p$t),main="parametric analysis", lwd=2, xlab="", ylim=c(0,0.016))
curve(dnorm(x,mean=media,sd=sqrt(media^2/7)))
curve(dgamma(x,shape=7, scale=media/7), add=T, col=4, lwd=2,lty=3)

#comparison between parametric bootstrap (esp)
plot(density(bootstrap.p$t), main="comparison bootstrap", )
