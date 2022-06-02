#-----------------4.4-----------
#dwie niezalezne probki na podstawie dlugoisci mozna wywnioskowa
m1<-c(145, 150, 153 ,148 ,141 ,152, 146, 154, 139, 148)
m2<-c(152, 150, 147, 155, 140, 146, 158, 152, 151, 143, 153)
#H_0:mu1=mu2
#H_1: jednostronan

sr1<-mean(m1)
sr2<-mean(m2)
#sr1<sr2
#potwierdzamy H_1

t.test(x=m1,y=m2,var.equal=TRUE,alternative = "less")
#nie ma podstaw do odrzucenia H_0

#df=19=10+11-2
#H_0 sigma_1^2,sigma_2^2
var.test(m1,m2)
#dobre zalozenie bo wartosc p-value jest duza
#f-snedecora wyszlo 

var(m1)/var(m2)


#---------4.5

p1<-c(14, 17, 7 ,33, 2, 24, 26, 22, 12)
p2<-c(13, 15, 3, 2, 25, 4, 1, 18, 6, 9, 20, 11, 5, 1, 7)
mean(p1)
mean(p2)
#sr(p1)>sr(p2)
var.test(p1,p2)
#p-value = 0.3557 >0.05

t.test(x=p1,y=p2,var.equal=TRUE,alternative = "greater")
#p.value =0.01587<0.05
#Przyjmujemy H_1

t.test(x=p1,y=p2,var.equal=F,alternative = "greater")

shapiro.test(p1)
shapiro.test(p2)
wilcox.test(p1,p2, paired=F, exact=F,alternative = "greater")
#qqnorm(p1)
#-------4.6
#TO NIE SA PROBKI NIEZALEZNE
#Dane sparowane
d1<-c(27, 21, 34, 24, 30, 27, 33, 31, 22, 27)
d2<-c(29, 32, 29, 27, 31, 26, 35, 30, 29, 28)
#H_0: E(X-Y)=0
#H_1: <0

D<-d1-d2
mean(D)
#ujemna
#H_0: mu_D=0
#H_1: <0

t.test(x=D, alternative = "less")

t.test(d1,d2,paired = T,var.equal = T, alternative = "less")
#to samo co przed chwila

shapiro.test(D)
?wilcox.test()
wilcox.test(d1,d2, paired=T,exact=F, alternative = "less")


t_n<-455
t<-700
p1<-455/700


l<-1320
l_n<-517
p2<-l_n/l

#H_0 p1=p2
#H_1 p2<p1

prop.test(x=c(455,517),n=c(700,1320),alternative = "greater")
