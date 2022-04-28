samochody <-read.csv2("samochody.csv")
head(samochody)
?read.csv2

#it doesn't work
#samochody$spalanie<-(1/samochody$mpg*1.609/3.785)*100
#summary(samochody$spalanie)
#boxplot(samochody$spalanie)


a<-samochody$mpg*1.609/100/3.785
zuzycie<- 1/a
summary(zuzycie)
samochody$spalanie <-zuzycie
summary(samochody)

summary(factor(samochody$producent))
# 1  2  3    America=1  Europe=2   Japan =3
#85 26 44 

boxplot(samochody$spalanie, horizontal = T) #do prawej

quantile(samochody$spalanie, probs=c(0.05,0.95), na.rm = T)
?quantile
auto<-samochody[complete.cases(samochody),]

hist(samochody$spalanie, freq = F, ylim=c(0,0.35), main="Spalanie")
dens <- density(samochody$spalanie,na.rm = TRUE)
?density
lines(dens, lwd=2)



library(e1071)
skewness(samochody$spalanie, na.rm=TRUE)
?kurtosis #czy jestesmy bliko rozkladu normalnego
kurtosis(samochody$spalanie, na.rm=TRUE)

x <- rnorm(1000)
kurtosis(x)


z.f<-character(length(samochody$spalanie))
z.f
z.f[which(samochody$spalanie<7)] <-"malo"
z.f[which(samochody$spalanie>=7 & (samochody$spalanie<=10))] <-"srednio"
z.f[which(samochody$spalanie>10)]<-"duzo"
zp.f<-factor(z.f, levels=c("malo", "srednio","duzo"), ordered=T)
table(zp.f)
barplot(table(zp.f), ylim=c(0,70))


#important!! table 2x2
table(zp.f, samochody$producent)
barplot(table(zp.f, samochody$producent), legend=T, ylim=c(0,90), col=c("green","blue","red"), names.arg=c("America","Europe","Japan"))
?barplot
