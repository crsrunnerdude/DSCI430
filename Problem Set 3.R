#Cole Streich, Akber Sakib, Devon Braun
#Problem Set 3

#2.)

vote.data = read.csv("vote.csv", header = TRUE)

attach(vote.data)

#a.) 
vote.reg = lm(voteA ~ expendA)
vote.reg
summary(vote.reg)
length(vote.data$state)

#b.)

vote.reg2 = lm(voteA ~ expendA + expendB)
vote.reg2
summary(vote.reg2)
length(vote.data$state)

#c.)
vote.reg3 = lm(voteA ~ expendA + expendB + democA + prtystrA)
vote.reg3
summary(vote.reg3)
length(vote.data$state)


#3.)
subdata = read.csv("subscription.csv", header = TRUE)
attach(subdata)

#a.)

subreg = lm(Subscribers ~ Price + Income)
subreg
summary(subreg)
length(subdata$Subscribers)
#c.)
sub1 = lm(Price ~ Labor + Income)
sub1
pricehat = sub1$fitted.values
sub2 = lm(Subscribers ~ pricehat + Income)
sub2

#d.)
library(sem)
tsls(Subscribers ~ Price + Income, ~ Labor + Income)
