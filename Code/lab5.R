#Section 1

#a)
# p0 = the proportion of dangerous lead levels 
#One-sided test
#H0: p0 = 10% vs H1: p0 > 10%
flint <-read.csv('~/UCLA Coursework/STATS 10/flint_2015.csv', header=TRUE)
#b)
n <- nrow(flint)
dangerous_lead_indicator <- (flint$Pb >= 15)
p_hat <- mean(dangerous_lead_indicator)
sd_sample <-sqrt(p_hat*(1-p_hat)/n)

#c)
p_null <- 0.10
se_null <- sqrt(p_null*(1-p_null)/n)
z_stat <- (p_hat-p_null)/se_null
print(z_stat)

#d)
#H1: p > 0.10
p_value <- 1-pnorm(z_stat,sd=1,mean=0)
print(p_value)
#e)
#reject

#f)
#to take remediation action

#g)
library(mosaic)
prop.test(x=sum(dangerous_lead_indicator), n=n, p=0.10, alternative="greater")
c(p_hat,p_value)

#h)
prop.test(x=sum(dangerous_lead_indicator), n=n, p=0.10, alternative="greater", conf.level = 0.99)

#Section 2

#a)
#H0: phat1 - phat2 = 0 vs H1: phat1 - phat2 != 0
#Two sided test

#b)
flint_north <- flint[flint$Region =="North",]
n_north <- nrow(flint_north)
flint_south <- flint[flint$Region == "South",]
n_south <- nrow(flint_south)

p_hat_north <- mean(flint_north$Pb>=15)
p_hat_south <- mean(flint_south$Pb>=15)

p_hat_pooled <- mean(flint$Pb >= 15)

SE <- sqrt(p_hat_pooled*(1-p_hat_pooled) * (1/n_north + 1/n_south))
z_stat <- (p_hat_north-p_hat_south-0)/SE
print(z_stat)

#c)
p_value <- (1-pnorm(abs(z_stat), mean=0,sd=1)) * 2
print(p_value)

#d)
#Reject

#e)
library(mosaic)
x_north <- sum(flint_north$Pb>=15)
x_south <- sum(flint_south$Pb>=15)
prop.test(x=c(x_north,x_south), n=c(n_north,n_south), alternative="two.sided")


#Section 3

#a)
# Ho: mu =  40 Ha mu != 40, Two sided Test

#b)
xbar <- mean(flint$Cu)
s <- sd(flint$Cu)

#c)
n <- nrow(flint)
SE = s/sqrt(n)

#d)
t_stat <- (xbar-40)/SE
p_value <- (1-pt(abs(t_stat),df=n-1))*2
print(p_value)

#e)
#Do not reject

#f)
library(mosaic)
t.test(flint$Cu, mu=40, alt="two.sided")

#Section BONUS CREDIT

#a)

#b)

#c)
summary(flint)
