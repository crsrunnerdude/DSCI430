#Problem Set 5
#Cole Streich, Akber Sakib, Devon Braun

minwagedata = read.csv("minwage.csv", header =TRUE)


#a.) Total Employment

#Total Employment Variable
minwagedata$totalemployment = minwagedata$fulltime + (minwagedata$parttime * .5) +minwagedata$managers

#DD Estimate
minwagedata$nj = as.numeric(minwagedata$state == "NJ")
minwagedata$november = as.numeric(minwagedata$time == "November")
minwagedata$njnov = minwagedata$nj*minwagedata$november
ddreg = lm(totalemployment ~ nj + november + njnov, data=minwagedata)
summary(ddreg)

totemploymean = summaryBy(totalemployment ~ nj + time, na..rm =TRUE, data = minwagedata)
dd = (totemploymean$totalemployment.mean[4] - totemploymean$totalemployment.mean[3]) - (totemploymean$totalemployment.mean[2] - totemploymean$totalemployment.mean[1])

#Total Employment Graph
cf1 = c(totemploymean$totalemployment.mean[3],totemploymean$totalemployment.mean[3]+totemploymean$totalemployment.mean[2]-totemploymean$totalemployment.mean[1])
month = c(0,1)
plot(month, subset(totemploymean$totalemployment.mean,totemploymean$nj==0),type="l",col="blue3", ylim=c(16,25),xaxp=c(0,1,1), ylab="Total Employees", xlab = "Month")
lines(month,subset(totemploymean$totalemployment.mean,totemploymean$nj==1),type="l",col="red3")
lines(month,cf1,col="green3")
segments("February",cf1[2],"November",totemploymean$totalemployment.mean[4])

#text(x=.5,y=24,label="No Minimum Wage Change",col="blue3")
#text(x=.5,y=20,label="Counterfactual",col="green3")
#text(x=.5,y=22,label="Minimum Wage Changed",col="red3")
#text(x=.5,y=22,label="DD Estimator")



#b.) Full Meal Price
#DD Estimate
minwagedata$mealprice = minwagedata$pfry + minwagedata$psoda + minwagedata$pentree
ddreg2 = lm(mealprice ~ nj + november + njnov, data = minwagedata)
mealpricemean = summaryBy(mealprice ~ nj + time, data = minwagedata, na.rm = TRUE)
means1 = mealpricemean$mealprice.mean
dd1 = (means1[4] - means1[3]) - (means1[2] - means1[1])
summary(ddreg2)

#Meal Price Graph
cf = c(mealpricemean$mealprice.mean[3],mealpricemean$mealprice.mean[3]+mealpricemean$mealprice.mean[2]-mealpricemean$mealprice.mean[1])
Month = c(0,1)
plot(Month, subset(mealpricemean$mealprice.mean, mealpricemean$nj==0),type="l",col="blue3", ylim=c(3,3.5),xaxp=c(0, 1, 1),ylab="Meal Price")
lines(Month, subset(mealpricemean$mealprice.mean, mealpricemean$nj==1),type="l",col="red3")
lines(Month,cf,col="green3")
segments(1,cf[2],1,mealpricemean$mealprice.mean[4])
text(x=0.5,y=3.10,label="New Jersey",col="blue3")
text(x=0.5,y=3.30,label="Counterfactual",col="green3")
text(x=0.5,y=3.45,label="Pennsylvania",col="red3")






