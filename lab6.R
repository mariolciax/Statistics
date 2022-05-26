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
alpha<-0.1
#kwantyle
quantile(srednia_boot,c(alpha/2,1-alpha/2))


# na przyszlosc
# library(boot)


#wariancja

przedzial_war<-c((n-1)*var(waga)/qchisq(1-alpha/2,n-1), (n-1)*var(waga)/qchisq(alpha/2,n-1))
przedzial_war


przedzial_sd<-sqrt(przedzial_war)
przedzial_sd
