#Section 1
pawnee <-read.csv('~/UCLA Coursework/STATS 10/pawnee.csv', header=TRUE)
#a)
head(pawnee)
dim(pawnee)
#b)
set.seed(1337)
rowsToSample <- sample(x=1:nrow(pawnee),size=30,replace = F)
pawnee_sample <- pawnee[rowsToSample,]

head(pawnee_sample)
#c)
mean(pawnee_sample$Arsenic)
p.hat <- mean(pawnee_sample$New_hlth_issue == "Y")
print(p.hat)
 
#d)
#See Lab Manual
#x_bar
#p_hat

#e)
se <- sqrt(p.hat*(1-p.hat)/30) # Standard Error
#Critical values
z1 <-qnorm(p=0.95)
z2 <-qnorm(p=0.975)
z3 <-qnorm(p=0.995)

p.hat+c(-1,1)*z1*se
p.hat+c(-1,1)*z2*se
p.hat+c(-1,1)*z3*se

#f)

# [0,1]

#g)

mean(pawnee$New_hlth_issue=="Y")

#h)
hist(pawnee$Arsenic,breaks=42,xaxt='n',prob = T,xlab="Levels of Arsenic (ppm)",main="Distribution of Arsenic in Pawnee Households")
axis(side=1,at=seq(0,210,l=43),labels=seq(0,210,l=43))


dotPlot(pawnee$Arsenic,col="black",cex=5,xlab="Levels of Arsenic (ppm)",main="Distribution of Arsenic in Pawnee Households")


#Section 2

#a)
# We first create objects for common quantities we will use for this exercise.
n <-30 
# The sample size
N <-541
# The population size
M <-1000 # Number of samples/repetitions
# Create vectors to store the simulated proportions from each repetition.
phats <-numeric(M)
# for sample proportions
# Set the seed for reproduceability
set.seed(123)
# Always set the seed OUTSIDE the for loop.
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times).
for(i in seq_len(M)){
# The i-th iteration of the for loop represents a single repetition.
  # Take a simple random sample of size n from the population of size N.
  index <-sample(N,size=n)
  # Save the random sample in the sample_i vector.
  sample_i <-pawnee[index,]
  # Compute the proportion of the i-th sample of households with a new health issue.
  phats[i] <-mean(sample_i$New_hlth_issue == "Y")
}

hist(phats,prob=T)
curve(dnorm(x,mean(phats),sd(phats)),add=TRUE,col="red")

library(mosaic)
histogram(phats, fit="normal")

#b)
mean(phats)
sd(phats)


#c)
#See lab manual

#d)

(p_true = mean(pawnee$New_hlth_issue=="Y"))
(se_true = sqrt(p_true*(1-p_true)/n))

p_true = mean(phats)
se_true = sd(phats)

#Section 3

#a)
# We first create objects for common quantities we will use for this exercise.
n <-30 
# The sample size
N <-541
# The population size
M <-1000 # Number of samples/repetitions
# Create vectors to store the simulated proportions from each repetition.
xbars <- c()
# for sample proportions
# Set the seed for reproduceability
set.seed(123)
# Always set the seed OUTSIDE the for loop.
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times).
for(i in 1:M){
  # The i-th iteration of the for loop represents a single repetition.
  # Take a simple random sample of size n from the population of size N.
  index <-sample(N,size=n)
  # Save the random sample in the sample_i vector.
  sample_i <- pawnee[index,]
  # Compute the proportion of the i-th sample of households with a new health issue.
  xbars[i] <- mean(sample_i$Arsenic)
}

#b)
hist(xbars,prob=T)
curve(dnorm(x,mean(xbars),sd(xbars)),add=TRUE)

#c)
#See lab manual

#e)

#f)