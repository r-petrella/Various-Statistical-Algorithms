library(boot)



B=100000
Stomach<- c(216, 338, 42, 40, 125, 828, 352, 154, 274, 42, 398, 218, 354)
Breast<- c(1120, 9761, 30, 5868, 3439, 1191, 119, 1791, 653, 206, 1120, 1165, 1603)


mean(Stomach)
var(Stomach)
median(Stomach)
sd(Stomach)

############################### Point 1


stat_boot<-function(data){
  median(data)
}

rg <- function(data, mle){
  out <- rexp(length(data), rate=1/mle)
  out
}
set.seed(1)
bootstrap.p.exp <- boot(Stomach, stat_boot, R=B, sim="parametric",ran.gen=rg, mle=mean(Stomach))
bootstrap.p.exp


########################### Point 2
### Non parametric analysis
theta<-function(data,i){
  mean(data[i])
}

set.seed(1)

np_bootstrap<-boot(Breast,theta,R=B)
np_bootstrap

### confidence intervals
set.seed(1)
CI_boot<-boot.ci(np_bootstrap, conf=0.95, type=c("perc", "bca"))
CI_boot

################################## Point 3
mu0 <- 2*365
Stomach2 <- Stomach - mean(Stomach)+ mu0  
Stomach2


#bootstrap with the R function

t_stat<-function(data){
  (mean(data) - mu0) / (sd(data) / sqrt(length(data)))
}

statistics <- function(data,i){
  data <- data[i]
  t_stat(data)
}
set.seed(1)

bootstr.np.H0 <- boot(Stomach2, statistics, R=B)
t_star <- bootstr.np.H0$t

t_oss <- t_stat(Stomach)

p_value <- (sum(abs(t_star)>= abs(t_oss))) /B

bootstr.np.H0
p_value







