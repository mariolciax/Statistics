#------------3.1-------------
set.seed(1234)
library(fitdistrplus)
(sample_1 <- rnorm(20))
sample_2 <- rnorm(100)

plot(sort(sample_1, decreasing=F))
lines(pnorm(1:20), type="l", add=T)
plotdist(sample_1)

#with function ecdf
plot(ecdf(sample_1))
lines(pnorm(seq(-2,2,0.01)), type="l", add=T) #it doesn't work good
?ecdf

#sample_2
plot(sort(sample_2, decreasing=F))
lines(pnorm(0:100), type="l")
plotdist(sample_2)

#with function ecdf
plot(ecdf(sample_2))