samochody <- read.csv2("samochody.csv")
producent<-samochody$producent
prod<-factor(producent)
levels(prod)<-c("America","Europe","Japan")
barplot(table(prod))


#2.8)
a<-samochody$mpg*1.609/100/3.785
zuzycie<- 1/a
samochody$zuzycie<-zuzycie
#prod kateryzacja
# important! tylda~
boxplot(zuzycie~prod, horizontal = T, main="Spalanie paliwa", ylab="Kontynenty", xlab="l/100km")
?boxplot

#zuzycie z podzialem na producenta
?aggregate()
aggregate(zuzycie, by=list(samochody$producent),FUN="mean", na.rm=T, trim=0.04)


### very important, useful to write arictle!!!
library(dplyr)
by_prod<-samochody %>% 
  group_by(producent)
by_prod %>% summarize_at(vars(zuzycie), list(sr=~mean(.,na.rm=T), sd=~sd(.,na.rm = T)))



#-------------------Estymacja-punktowa----------------------
#1)
sample1<-rnorm(20)
sample2<-rnorm(100)

#more plots, one figure
par(mfrow=c(1,2))

plot(ecdf(sample1))
curve(pnorm(x), add=T,lwd=2, col=2)


plot(ecdf(sample2))
curve(pnorm(x), add=T,lwd=2,col=2)


#2)a  cauchy distribution

C<-rcauchy(500)
?rcauchy

srednia<-vector(length = 500)
M<-vector(length = 500)

for (i in 1:500){
  srednia[i]<-mean(C[1:i])
  M[i]<-median(C[1:i])
} 

plot(srednia,main="Cauchy")
points(M, col=2)
abline(h=0)
#-------------normalny
#3)a
N<-rnorm(500)

srednia_N<-vector(length = 500)
M_N<-vector(length = 500)

for (i in 1:500){
  srednia_N[i]<-mean(N[1:i])
  M_N[i]<-median(N[1:i])
} 

plot(srednia_N, main="Normalny")
points(M_N, col=2)
abline(h=0)



#3.4
n<-100
runif(n)
?runif
duzo<-1000
M<-list(length=length(duzo))
for (i in 1:1000){
}

#suma estymatorow podzielona przez M

