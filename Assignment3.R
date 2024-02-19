# Riccardo Petrella

# ATPC6 on healthy and leukemia

library(boot)

healthy <- c(835, 935, 1665, 764, 1323, 1030, 1482, 1306, 593, 2375, 542, 
             809, 2474, 1514, 1977, 1235, 933, 1114, 3072, 608, 499, 1740, 
             1189, 1870, 892, 677)

leukemia <- c(1237, 1125, 1655, 1807, 1593, 2201, 1737, 1552, 2255, 1249, 1670) 

bone_marrow <- c(healthy, leukemia)
healthy <- bone_marrow[1:26]
leukemia <- bone_marrow[27:37]

#evaluate the normality of our data
plot(density(healthy))
plot(density(leukemia))

hist(bone_marrow, breaks=8)
plot(density(bone_marrow), type="l")
qqnorm(bone_marrow)
qqline(bone_marrow)

shapiro.test(healthy)
shapiro.test(leukemia)
shapiro.test(bone_marrow)

######################################### Point b
#t test
t.test(healthy, leukemia, alternative = "less", mu=0, conf.level = 0.95)


##### second method: parametric bootstrap

statistics_test<-function(data){
  mean_HEA <- mean(data[1:26])
  mean_LEU<- mean(data[27:37])
  var_HEA <- var(data[1:26])
  var_LEU <- var(data[27:37])
  
  (mean_LEU-mean_HEA)/sqrt(var_LEU/11 + var_HEA/26)
}

theta<-function(data, i){
  statistics_test(data[i])
}


rg <- function(data, mle){
  out <- rpois(length(data), lambda=mle)
  out
}

set.seed(1)

p_bootstrap <- boot(bone_marrow, theta, R=100000, sim="parametric",
                       ran.gen = rg, mle=mean(bone_marrow))
p_bootstrap

t_star <- p_bootstrap$t
t_obs<-statistics_test(bone_marrow)

B=100000


p_value_2<-(sum(t_star>=t_obs) )/B
p_value_2


####### third method: Wilks

mu1 <- mean(healthy)
mu2 <- mean(leukemia)
mu <- mean(bone_marrow)
n1 <- length(healthy)
n2 <- length(leukemia)
n <- length(bone_marrow)

w <- -2*(sum(bone_marrow)*log(mu)-n*mu - sum(healthy)*log(mu1)+ n1*mu1
         - sum(leukemia)*log(mu2) + n2*mu2)
w

p_value_3 <- 1- pchisq(w, 1)
p_value_3

##################point c

wilcox.test(healthy,leukemia, alternative = "less",
            mu=0, conf.level = 0.95)
