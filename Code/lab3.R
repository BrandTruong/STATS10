#Section 1
soil<-read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt", header=TRUE)
#a
linear_model <- lm(soil$lead ~ soil$zinc)
summary(linear_model)
#b
plot(soil$lead ~ soil$zinc, xlab = "Zinc Concentration (ppm)", ylab = "Lead Concentration (ppm)", main = "Regression of Lead Concentration on Zinc Concentration", )
abline(linear_model, col = "red", lwd = 2)
#c
plot(linear_model$residuals ~ soil$zinc, main = "Residuals plot")
abline(a = 0, b = 0, col = "red", lwd = 2)
abline(h = 0, col = "red", lwd = 2)
#d
[predicted] lead ppm = 17.37 +0.29 * zinc ppm
#e
linear_model$coefficients[1] + linear_model$coefficients[2] * 1000
17.37 + 0.29 * 1000
#f
linear_model$coefficients[2] * 100
0.29 * 100
#g
0.9114
91.14% of the variability in lead is explained by the variability in zinc
#h
Linearity okay
Symmetry okay
Equal Variance: Variance is not constant
#Section 2
ice <- read.csv('~/UCLA Coursework/STATS 10/sea_ice.csv', header = T)
colnames(ice)
ice$Date <-as.Date(ice$Date, "%m/%d/%Y")
#a
linear_model <-lm(ice$Extent~ice$Date)
summary(linear_model)
#b
plot(ice$Extent~ice$Date, ylab = "Sea Ice Extent", xlab = "Date", main = "Regression of Sea Ice Extent on Time")
abline(linear_model, col = "red", lwd = 2)
#no trend
#c
plot(linear_model$residuals ~ ice$Date, main = "residuals plot")
abline(a = 0, b = 0, col ="red", lwd = 2)
#linearity, data not related
#Section 3
#a
doubles money : (1,6),(6,1),(2.5),(5,2),(3,4),(4,3),(5,6),(6,5)
loses all : (1,1),(1,2),(2,1),(6,6)
#b
set.seed(123)
numbers= 1:6
rand_draws = replicate(5000, sample(numbers, 2, replace = TRUE))
results = colSums(rand_draws)
table(results)
barplot(table(results), main="Barplot of 5,000 Simulated Craps games")
#c
#P(doubles money)
sum(table(results)[c(7,11)-1]/5000)
#P(loses all)
sum(table(results)[c(2,3,12)-1]/5000)
#d

#e

#Section 4
#a
n=365
p=.40
#b
n*p #mean
sqrt(n*p*(1-p)) #standard deviation
#c
dbinom(145,size=n,prob=p)
#d
pbinom(175,size=n,prob=p)-pbinom(124,size=n,prob=p)
#We subtract 124 instead of 125 because we want to include the probability of 125, since we are assuming it is inclusive
#e
1-pnorm(230,mean=200,sd=20)
pnorm(230,mean=200,sd=20,lower.tail=F)
