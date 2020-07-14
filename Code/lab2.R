setwd("~/UCLA Coursework/STATS 10") #Restart
library(mosaic)
library(maps)
NCbirths <- read.csv('births.csv')

?Comparison
4 > 3
c(3,8) >=3
c(3,8) <= 3
c(1,4,9) == 9
c(1,4,9) != 9
c(3,8) >= c(3,10)

sum(NCbirths$weight > 100) #the number of babies that weighed more than 100 ounces
sum(NCbirths$weight > 100)/1992
mean(NCbirths$weight > 100) #the proportion of babies that weighed more than 100 ounces
mean(NCbirths$Gender == "Female") #the proportion of female babies
mean(NCbirths$Gender != "Male") #gives the proportion of babies not assigned male

NCbirths$weight[c(1,2,3)] #last class
fem_weights <-NCbirths$weight[NCbirths$Gender == "Female"]
fem_weights = NCbirths$weight[NCbirths$Gender == "Female"]
sum(fem_weights)

sub1_weights <- NCbirths$weight[NCbirths$Gender == "Female" & NCbirths$Premie == "No"]
sub2_weights <- NCbirths$weight[NCbirths$Gender == "Female" | NCbirths$Premie == "No"]

#Create an object with the baby weights from NCbirths
baby_weight <- NCbirths$weight
#Create an object with the baby genders from NCbirths
baby_gender <- NCbirths$Gender
#Create a logical vector to describe if the gender is female
is_female <-baby_gender == "Female"
# Create the vector of weights containing only females
fem_weights <-baby_weight[is_female]


#Exercise 1
#a
flint <- read.csv('flint.csv')
flint <- read.csv('~/UCLA Coursework/STATS 10/flint.csv')
head(flint) #testing to see if read properly
class(flint)
#b
library(mosaic)
dangerousPb_indicator = (flint$Pb >= 15)
tally(~dangerousPb_indicator, format="proportion")
mean(flint$Pb>=15)
#c
north_flint <- flint[flint$Region=="North",]
mean(north_flint$Cu)
#d
dangerousPb_flint <- flint[flint$Pb>=15,]
mean(dangerousPb_flint$Cu)
#e
mean(flint$Pb)
mean(flint$Cu)
#f
boxplot(x = flint$Pb, main="Lead Levels(PPB) in Flint", xlab="Flint Locations",ylab="PPB")
boxplot(flint$Pb~flint$Region, main="Lead Levels(PPB) between Flint Regions")
boxplot(NCbirths$Fage, NCbirths$Mage, names=c("Fage","Mage"), main="Insert Title")
#g
mean(flint$Pb)
median(flint$Pb)

#Exercise 2
#1
life<-read.table("http://www.stat.ucla.edu/~nchristo/statistics12/countries_life.txt", header=TRUE)
#a
plot(x=life$Income, y=life$Life,xlab="Income (per capita)",ylab="Life Expectancy (years)",main="Life Expectancy vs Income")
#Discussion of positive association, reference to being impportant up to a point, mention of inability to identify causal relationship from data
#b
hist(life$Income,xlab="Income",ylab="Frequency",main="Histogram of Income")
boxplot(life$Income,xlab="Countries",ylab="Income",main="Boxplot of Income")
summary(life$Income)
#Rubric they mention potential outliers
#c
below1k <- life[life$Income<1000,]
above1k  <- life[life$Income>=1000,]
#d
plot(x=below1k$Income, y=below1k$Life,xlab="Income (below 1k)",ylab="Life Expectancy",main="Life Expectancy vs Income (below 1k)")
cor(below1k$Income, below1k$Life)
#Exercise 3
maas<-read.table("http://www.stat.ucla.edu/~nchristo/statistics12/soil.txt", header=TRUE)
#a
summary(maas$lead)
summary(maas$zinc)
#b
hist(maas$lead, xlab="Lead (ppm)", ylab="Frequency",main="Histogram of Lead")
hist(log(maas$lead),xlab="Lead (ppm)",main="Histogram of log(Lead)")
#c
plot(x=log(maas$zinc),y=log(maas$lead),ylab="log(lead)", xlab="log(zinc)", main="Log(lead) vs Log(zinc)")
#include discussion of linearity, symmetry, and equal variance
#d
lead_colors <- c("green","orange","red")
lead_levels <- cut(maas$lead, c(0,150,400,1000))
plot(maas$x,maas$y,xlab="x-coordinates",ylab="y-coordinates",main="Lead Concentrations along the Maas River","n")
points(maas$x,maas$y,cex=maas$lead/mean(maas$lead)/1.5,col=lead_colors[as.numeric(lead_levels)],pch=19)
#Exercise 4
LA <-read.table("http://www.stat.ucla.edu/~nchristo/statistics12/la_data.txt", header=TRUE)
library(maps)
#a
plot(x=LA$Longitude,y=LA$Latitude,ylab="Latitude",xlab="Longitude",main="LA School Locations",pch=19,col="red")
map("county", "california", add = TRUE)
#b
LA.subset <- LA[LA$Schools>0,]
cor(LA.subset$Schools,LA.subset$Income)
plot(x=LA.subset$Income, y=LA.subset$Schools, xlab="Income",ylab="School Performance",main="School Performance vs Income",pch=21,col="black")
#relationship linear moderate positive no implication of causation