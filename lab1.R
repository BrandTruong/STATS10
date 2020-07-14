a <- c(1,2,3)
b <- c("a", "b")
print(a)

setwd("~")

numbers <-c(1,2,3,4,5)
schools <-c("UCLA", "UC Berkeley", "USC")
numbers * 2
numbers
numbers[5]

#Section 1

#Exercise 1
heights <- c(68,65,72) #Height in inches
print(heights)
names <- c("Brandon", "Samuel", "Bryan")
print(names)
cbind(heights,names)
class(cbind(heights,names))
#Exercise 2
setwd("~UCLA Coursework/Stats 10/") #Directory containing births.csv
NCbirths <- read.csv(file="births.csv")
head(NCbirths)
#Exercise 3
?map
install.packages("maps") # package installation
find.package("maps")
library(maps) #package loading
map("state")
#Exercise 4
weights<-NCbirths$weight
head(weights)
weights_in_pounds<-weights/16
weights_in_pounds[1:20]
#Section 2

install.packages("mosaic")
devtools::install_github("ProjectMOSAIC/mosaic", build_vignettes = TRUE)
find.package("mosaic")
library(mosaic)

#Exercise 1
mean(NCbirths$weight)
#Exercise 2
tally(NCbirths$Habit, format = c("percent"))
#Exercise 3

#Section 3

#Exercise 1
dotPlot(weights_in_pounds)
#Exercise 2
histogram(weights_in_pounds, nint=3)
histogram(weights_in_pounds, nint=20)
histogram(weights_in_pounds, nint=100)
#Exercise 3
boxplot(NCbirths$Fage, NCbirths$Mage)
#Exercise 4
histogram(~weight|Habit,data=NCbirths,layout=c(1,2))

#Section 4
tally(~Habit | MomPriorCond, data = NCbirths, format = "proportion")
tally(~Habit | Premie, data = NCbirths, format = "proportion")
tally(~BirthDef | Habit, data = NCbirths, format = "proportion")
tally(~Premie | Habit, data = NCbirths, format = "proportion")

#Section 5
plot(y, ~x)
plot(x, y)
plot(NCbirths$weight ~ NCbirths$Gained) 
#Exercise 1
plot(NCbirths$weight ~ NCbirths$Mage, col = "blue", cex = 1.5, pch = 3, xlab= "Mother's Age (years)", ylab = "Baby weight (oz.)", main = "Baby Weight vs. Mother's Age")

#Section 6
setwd()
a <- read.table(file = "ozone.txt", header=TRUE)
AQI_colors<-c("pink","blue","orange","cyan","mahogany")
AQI_levels<-cut(a$o3, c(0,0.06,0.075,0.104,0.115,0.374))
as.numeric(AQI_levels)

library(maps)
plot(a$x,a$y, xlim=c(-125,-114),ylim=c(32,43), xlab="longitude", ylab="latitude", main="California ozone bubble plot", "n")
map("county", "ca",add=TRUE)
points(a$x,a$y, cex=a$o3/mean(a$o3), col=AQI_colors[as.numeric(AQI_levels)], pch=2)
