#-----zadanie 3.12-----

dane<-chickwts[which(chickwts$feed=="soybean"),]
waga<-dane$weight
#malo danych n=14
n<-14
qqnorm(waga)
qqline(waga)

#czy odrzucamy hipoteze, H_0=normalny 
shapiro.test(waga)
#nie mamy podstaw p-value wyszlo wieksze 

#bootstrap
#replace=T, wybieramy z powtorzeniami
#sami tworzymy probke - 
#ALTERNATYWA JAK NIE MAMY NORMALNYCH DANYCH 
#zawsze warto zrobic porownanie z bootrsapem
B=1000
srednia_boot<-c()
for (i in 1:B){
ind<-sample(1:n,n,replace=T)
srednia_boot<- c(srednia_boot, mean(waga[ind]))
}
srednia_boot
alpha<-0.07
#kwantyle
quantile(srednia_boot,c(alpha/2,1-alpha/2))


# na przyszlosc
# library(boot)


#wariancja
przedzial_war<-c((n-1)*var(waga)/qchisq(1-alpha/2,n-1), (n-1)*var(waga)/qchisq(alpha/2,n-1))
przedzial_war

przedzial_sd<-sqrt(przedzial_war)
przedzial_sd



#porownanie bootstrapowe - odchylenie
srednia_boot<-c()
for (i in 1:B){
  ind<-sample(1:n,n,replace=T)
  srednia_boot<- c(srednia_boot, sd(waga[ind]))
}
alpha<-0.07
#kwantyle
quantile(srednia_boot,c(alpha/2,1-alpha/2))


#porownanie bootstrapowe - wariancja
srednia_boot<-c()
for (i in 1:B){
  ind<-sample(1:n,n,replace=T)
  srednia_boot<- c(srednia_boot, var(waga[ind]))
}
alpha<-0.07
#kwantyle
quantile(srednia_boot,c(alpha/2,1-alpha/2))

#testowanie NADAL t.test
#H_1: mu=270, H_1: mu!=270
t.test(waga, mu=270,conf.level = 0.9)

#nie mamy podstaw aby odrzucic bo p-value=0.1272 wieksze niz alfa
#sobie sprawdzimy ze to 270 lezy w przedziale
t.test(waga, conf.level = 0.9)

t.test(waga,mu=270,conf.level = 0.9, alternative = "less")
#odrzucamy hipoteze zerowa na rzecz alternatywy, wartosc p-value to 1/2 poprzedniej


#uwaga na jednostronne
#trzeba miec uzasadnienie


#----zad 3.10 kwiatki
data(iris)
iris$Petal.Length
iris$Species

dane<-iris[which(iris$Species=="virginica"),]
head(dane)
t.test(dane$Petal.Length,conf.level = 0.99)$conf.int
platki<-dane$Petal.Length
n<-length(platki)

alpha<-0.05
przedzial_war<-c((n-1)*var(platki)/qchisq(1-alpha/2,n-1),
                 (n-1)*var(platki)/qchisq(alpha/2,n-1))
sqrt(przedzial_war)


#----zad 3.9-------

binom.test(x=3,n=12)$conf.int

#test hipotezy
binom.test(x=3,n=12, p=1/6)