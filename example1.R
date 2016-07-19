# Plotting central limit theorem in R
# simulation of normal distribution data
set.seed(2015)
lambda <- 0.2 # the rate parameter lambda as instructed
n <- 40 # number of exponentials
sim <- 1000 # a thousand simulations

# the exponential distribution
plot(rexp(10000,lambda), pch=20, cex=0.6, main="The exponential distribution with rate 0.2 and 10.000 observations")

means <- NULL
for (i in 1 : sim) means <- c(means, mean(rexp(n,lambda)))
hist(means, col="blue", main="rexp mean distribution", breaks=40)
rug(means)

hist(means, col="darkblue", main="Theoretical vs actual mean for rexp()", breaks=20)
abline(v=mean(means), lwd="4", col="red")
text(3.6, 90, paste("Actual mean = ", round(mean(means),2), "\n Theoretical mean = 5" ), col="red")

hist(means, prob=TRUE, col="lightblue", main="mean distribution for rexp()", breaks=20)
lines(density(means), lwd=3, col="blue")