chisq.test(c(73,74,34,19), p=c(1/4,1/4,1/4,1/4))

chisq.test(c(73,74,34,19),p=c(.367,.371,.186,.076))


p_value<-1-pchisq(1.228,3)


#do badania zaleznosci

#5.10
#test na niezaleznosc

n<-450
wyzsze<-c(80,115,55)
podst<-c(95,70,35)
macierz<-rbind(wyzsze,podst)
ncol(macierz)
nrow(macierz)
chisq.test(macierz)


x<-rexp(10,1)
shapiro.test(x)
lambda<-1/mean(x)
ks.test(x,"pexp",lambda)

?ks.test()

#graficzne przedstawienie funkcji mocy


?aov
?anova



sigma<-1
miu=1.2

kwantyl<-qnorm(0.95)
n<-20
curve(1-pnorm(kwantyl-(x-miu)/sigma*sqrt(n)), xlim=c(miu,4) )
abline(h=0.05)
