#------------3.1-------------
#Theoretical with emphirical distribution

mu <- 0
sigma <- 1
x <- seq(mu-3*sigma, mu+3*sigma,length.out = 100) # empirical rule 3 sigma rule

plot(ecdf(sample_1))
lines(x,pnorm(x), xlab = "X", ylab = "density")


#sample_2

plot(ecdf(sample_2))
lines(x,pnorm(x), xlab = "X", ylab = "density")


#-------------------Comments

set.seed(1234)
library(fitdistrplus)
(sample_1 <- rnorm(20))
sample_2 <- rnorm(100)

#Emphirical

plot(sort(sample_1, decreasing=F))
#lines(pnorm(1:20), type="l", add=T)


plotdist(sample_1)

#with function ecdf
plot(ecdf(sample_1))
#lines(pnorm(seq(-2,2,0.01)), type="l", add=T) #it doesn't work good
#?ecdf


?integrate





