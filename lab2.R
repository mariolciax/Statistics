library(graphics)
dane <- c(17364000, 56128000, 11239000, 8170000)
#a)
pie(dane, labels=c("panny", "mezatki", "wdowy", "rozwodki"), col=rainbow(4))
#b)
barplot(dane, width=0.5, names.arg=c("panny", "mezatki", "wdowy", "rozwodki"))
?barplot


dane_ <- c(17364000, 56128000, 11239000, 8170000)
nazwy<-c("panny", "mezatki", "wdowy", "rozwodki")
nazwy_s<-nazwy[order(dane_s, decreasing=T)]
dane_s<-sort(dane_, decreasing = T)
barplot(dane_s, width=0.5, names.arg=c("mezatki","panny",  "wdowy", "rozwodki"))



dane_p<-prop.table(dane_s)
barplot(dane_p, width=0.5, names.arg=c("mezatki","panny",  "wdowy", "rozwodki"), col="lightblue", main="Badania demograficzne kobiet przeprowadzone w 1988 roku w USA ")

?barplot




#2---------------------------------------------

stacje<- read.csv(file='h:/Windows7/Desktop/statystyka/Lab2/stacje.csv')
stacje_<-read.csv("stacje.csv")
table(stacje)
barplot(table(stacje), main = "Preferowana lokalizacja", ylim=c(0,400))

        
#4----------------------------------------

butelki<-read.csv("butelki.csv")
head(butelki)
butelki$presure<-butelki$strength*0.0068947

?hist
h <- hist(butelki$presure, freq=FALSE, main="Wytrzymalosc butelek", col="lightblue", ylim=c(0,2))
#c)z gestoscia
lines(h$mids, h$density, lwd=2)

?stem
#lodyga lisc
stem(butelki$presure)

?boxplot
boxplot(butelki$presure, horizontal =TRUE)
?IQR
iqr<-IQR(butelki$presure)
?quantile()
q2<-mean(butelki$presure)
?mean
x_<-mean(butelki$presure, trim=0.05)

med<-median(butelki$presure)

summary(butelki$presure)

#sd var IQR diff(range())

diff(range(butelki$presure))

#quantile
q1<-quantile(butelki$presure, probs=0.25)
q3<-quantile(butelki$presure, probs=0.75)
a<-q1-1.5 * iqr
b<-q3+1.5 *iqr

b_p<-butelki$presure[(butelki$presure>=a)&(butelki$presure<=b)]

boxplot(b_p, horizontal =TRUE)

pr<- butelki$presure[which((butelki$presure<=a)|(butelki$presure>=b))]
