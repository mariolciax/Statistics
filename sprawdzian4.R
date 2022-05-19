n<-50
theta1<- c()
for (i in 1:1000){
  MEAN<-mean(runif(n,0,1))
  theta1<-c(theta1, 2*MEAN)
}
srednia_probkowa_thety<-mean(theta1)
(var(theta1))^1/2

hist(theta1,freq = F)
curve(dnorm(x,srednia_probkowa_thety,(var(theta1))^(1/2)),add=T)
?dnorm

