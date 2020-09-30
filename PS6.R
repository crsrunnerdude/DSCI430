#Problem Set 6
#Cole Streich, Akber Sakib, Devon Braun

#1.) Fertilizer Profits

#b.)
library(Rsolnp)

profit = function(x)
{
  return(-(30*x[1] - 2*x[1]^2 + 25*x[2] - .5*x[2]^2)) 
  
}

g = function(x)
{
  return(3*x[1] + 6*x[2])
}  

max = solnp(pars=c(1,1),fun=profit, eqfun=g,eqB=300,LB=c(0,0))

#Profit
max$values


#2.) Plant Choice

#d.)

tc = function(x)
{
  return(10*x[1] + .4*x[1]^2 + 20*x[2] + .25*x[2]^2 + 50*x[3] + .2*x[3]^3)
}

con = function(x)
{
  return(x[1] + x[2] + x[3])
}

solnp(pars=c(1,1,1), fun=tc, eqfun=con, eqB=1280, LB= c(0,0,0))


#3.) Cobb Douglas Function

pd = read.csv('productiondata.csv', header=TRUE)

attach(pd)
pd$logQ = log(Q)
pd$logK = log(K)
pd$logL = log(L)
cobbdoug = lm(logQ ~ logK + logL, data = pd)
cobbdoug
exp(cobbdoug$coefficients[1])
summary(cobbdoug)

#c)
obj = function(x)
{
  return((5.803*(15*x[1]^.696)*(10*(x[2]^.2454))))
}
constraints = function(x)
{
  return((5.803*(x[1]^.696)*(x[2]^.2454)))
}
start = c(5,5)
bound = 1200
lower = c(0,0)
library(Rsolnp)
solnp(pars = start,fun = obj, eqfun = constraints, eqB = bound, LB = lower)



