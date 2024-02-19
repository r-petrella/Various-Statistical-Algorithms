###### Petrella Riccardo ----- Assignment 1

x <- c(5,1,5,14,3,19,1,1,4,22)
t <- c(94.32,15.72,62.88,125.76,5.24,31.44,1.05,1.05,2.10,10.48)
data <- data.frame(x,t)
x <- data[,1]
t <- data[,2]


theta <- seq(0,15, length.out=1000)

logL = function(x,t,theta)  #theta is a vector
{
  alpha = theta[1]
  beta = theta[2]
  lambda = alpha + t/beta
  lik = sum(- lambda + x*log(lambda) - log(factorial(x)) )
  return(lik)
}

#### score function

S.theta = function(x,t,theta)
{
  alpha = theta[1]
  beta = theta[2]
  
  out = c(0,0)
  out[1] = sum(x/(alpha + t/beta)) - 10
  out[2] = sum((t/beta^2)- (x*t)/(alpha*beta^2+t*beta))
  return(out)  
}



## Gradient descent

theta.init = c(5,5)   #alpha and beta
tol = 10^-4
lrate = 0.1


gd.alg = function(theta.init,x,t,lrate,tol)
{
  it = 1
  p = length(theta.init)
  theta.it = matrix(theta.init,1,p)
  l.it = logL(x,t,theta.init)  
  print(c(it,l.it[it],theta))
  delta = 100   
  
  while ( delta>tol )  {
    
    S.it = S.theta(x,t,theta.it[it,])   
    theta = theta.it[it,]+lrate*S.it  
    theta.it = rbind(theta.it,theta)   
    l.it = c(l.it,logL(x,t,theta))
    delta = abs((l.it[it+1]-l.it[it])/l.it[it])
    it = it+1   
    print(c(it,l.it[it],theta))
  }
  out = list(likelihood=l.it,theta=theta.it)
  return(invisible(out))
}
gd.alg(theta.init, x, t, lrate, tol)   #test of GD 


### Hessian

H.theta = function(x,t,theta)
{ 
  alpha = theta[1]
  beta = theta[2]
  
  out = matrix(0,2,2)
  out[1,1] = -sum(x*beta^2/(alpha*beta+t)^2)
  out[1,2] = out[2,1] = sum(x*t/(alpha*beta+t)^2)
  out[2,2] = sum( (2*alpha*beta*x*t + x*t^2)/(alpha*beta^2+t*beta)^2 - (2*t)/beta^3 )
  return(out)  
}

H.theta(x,t,c(0.5,3))   #test of the H matrix

#### newton raphson
theta.init = c(5,5)

nr.alg = function(theta.init,x,t,tol){
  it = 1
  p = length(theta.init)
  theta.it = matrix(theta.init,1,p)
  l.it = logL(x,t,theta.init)
  
  delta = 100
  
  while (delta>tol)  {
    
    S.it=S.theta(x,t,theta.it[it,])
    H.it=H.theta(x,t,theta.it[it,])
    
    theta = c(theta.it[it,] - solve(H.it)%*%S.it)
    theta.it=rbind(theta.it,theta)
    l.it=c(l.it,logL(x,t,theta))
    delta=abs((l.it[it+1]-l.it[it])/l.it[it])
    it=it+1
    print(c(it,l.it[it],theta))
    }
  out=list(likelihood=l.it,theta=theta.it)
  
  return(invisible(out))
}

nr.alg(theta.init,x,t,tol)   #test of NR

