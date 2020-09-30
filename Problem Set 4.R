#Problem Set 4
#Cole Streich, Akber Sakib, Devon Braun

#1.) Stock Returns
pricedata = read.csv("prices.csv", header = TRUE)
attach(pricedata)

#a. 
pricedata$t = seq(1,length(pricedata$time))
price = ts(pricedata$sp500)
price1 = lag(pricedata$price, -1)

return = ((price - price1)/price1)
avgreturn = mean(return)
pricedata1 = ts.union(pricedata, price1, return)
#This number shows that the average stock price of fortune 500 companies, increased by .16% between years. 


#b.
#remove the N/A
plot(pricedata$return)
#c.



#2.) Gas Prices
attach(pricedata)

#a.
plot(pricedata$brent, type = 'l')



#b.
gasreg = lm(gasoline ~ brent)
summary(gasreg)

#c.
brentc = ts(pricedata$brent)
gas = ts(pricedata$gasoline)
brent1 = lag(brentc, -1)
gas1 = lag(gas, -1)
pricets = ts.union(brentc, brent1, gas, gas1)
ar1gas = lm(gas ~ gas1, data = pricets)
ar1brent = lm(brentc ~ brent1, data = pricets)
summary(ar1gas)
summary(ar1brent)
#ar1gas is .998 meaning there is a unit root
#ar1brent is .998 meaning there is a unit root 

#d.
diffgas = gas - gas1
diffbrent = brentc - brent1
pricediff = ts.union(gas, gas1, diffgas, brentc, brent1, diffbrent)
summary(lm(diffgas ~ diffbrent, data = pricediff))

#e.


#3.) Approval Rating
bushdata = read.csv("bush.csv", header = TRUE)
attach(bushdata)

#a.
bushdata$t = ts(seq(1,length(bushdata$month)))
plot(bushdata$approve, type = "l", col = "red", 
     xlab = "Months Passed (2001-2007)", ylab = "Approval Rating")

#b.
bushreg = lm(approve ~ gas)
summary(bushreg)

approvetrend = lm(approve ~ t, data = bushdata)
bushdata$approvefit = approvetrend$fitted.values
bushdata$approvedt = approvetrend$residuals

gastrend = lm(gas ~ t, data = bushdata)
bushdata$gasfit = gastrend$fitted.values
bushdata$gasdt = gastrend$residuals

detrend = lm(approvedt ~ gasdt, data = bushdata)
summary(detrend)

#c.
bushreg2 = lm(approve ~ gas + cpifood + unem + sep11 + iraq, data = bushdata)
summary(bushreg2)



#d.
#Omitted Variable 