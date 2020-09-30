#Problem Set 1

#2a
x2a = seq(30,40)
pr2a = dpois(x2a,35)
sum(pr2a)

#2b
sim = rpois(14,35)
sum(sim)

#2c
rev = c(4,2,2.5,4,3,0)
pr = c(.3,.4,.3,.35,.4,.25)
sum(rev*pr)

#2d
x1c = seq(0,200)
pbinom(x1c,200,.8)
qbinom(.95,200,.8)

#3a
car = read.csv('cars.csv',header=TRUE)
attach(car)
pbar = mean(price)
sp = sd(price)
hpbar = mean(horsepower)
shp = sd(horsepower)

#3b
N = 107
t3b = (pbar - 19000)/(sp/sqrt(N))
cv90 = qt(.1/2,N-1)
cv95 = qt(.05/2,N-1)
cv99 = qt(.01/2,N-1)

#3c
upper = pbar - cv99*sp/sqrt(N)
lower = pbar + cv99*sp/sqrt(N)

#3d
t3d = (hpbar - 130)/(shp/sqrt(N))

#3e
pt(-abs(t3d),N-1)*2
