n<-50
theta1<- c()
theta2<-c()

for (i in 1:1000){
  X<-runif(n,0,1)
  MEAN<-mean(X)
  theta1<-c(theta1, 2*MEAN)
  theta2<-c(theta2, max(X))
}

theta3<-(n+1)/n*theta2

#-sprawdzian
srednia_probkowa_thety<-mean(theta1)
(var(theta1))^1/2

hist(theta1,freq = F)
curve(dnorm(x,srednia_probkowa_thety,(var(theta1))^(1/2)),add=T)
?dnorm
#-koniec sprawdzianu

bias<-c(mean(theta1)-1,mean(theta2)-1,mean(theta3)-1 )
v<-c(var(theta1), var(theta2), var(theta3))
mse<-c(v[1]+bias[1]^2,v[2]+bias[2]^2,v[3]+bias[3]^2)

hist(theta2, freq=F)



#-----------zadanie 3.5
n<-50
alpha<-0.10
x<-rnorm(n,0,1)
#---------przedzial ufnosci


a<-mean(x)-qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
b<-mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
print(list(a,b))

#--------funkcja t.test
t.test(x,conf.level=1-alpha)$conf.int
test<-t.test(x,conf.level=1-alpha)
c(test$conf.int)

inside<-c()
for (i in 1:1000){
  X<-rnorm(n,0,1)
  przedzial<-c(t.test(X,conf.level=1-alpha)$conf.int)
  b<-0>=przedzial[1] & 0<=przedzial[2]
  inside<-c(inside,b)
}
inside<-inside*1
inside
mean(inside)


#---
M<-100
plot(1:M,rep(0,M),ylim=c(-0.8,0.8),lwd=2,type="l")

inside2<-c()
for (i in 1:M){
  X<-rnorm(n,0,1)
  przedzial<-c(t.test(X,conf.level=1-alpha)$conf.int)
  polygon(c(i,i),c(przedzial[1],przedzial[2]))
  b<-0>=przedzial[1] & 0<=przedzial[2]
  inside2<-c(inside2,b)
}
inside2<-inside2*1
inside2
mean(inside2)


#----------Zadanie-4------
respondenci<-578
n<-1014
?prop.test()
?binom.test()

prop.test(578,1014)
binom.test(578,1014)
