#The most imortant
# functions : sort , order, quantile, IQR, mean(trim), median, summary, diff(range())
# df$col_name<-df$col_name*value
# pie, barplot, hist, stem, boxplot
#hist!!!- we can save hist to variable and next use its elements to draw line or other
#lines(x,y, lwd=2)



library(graphics)
dane <- c(17364000, 56128000, 11239000, 8170000)
#a)
pie(dane, labels=c("panny", "mezatki", "wdowy", "rozwodki"), col=rainbow(4))
#b)
barplot(dane, width=0.5, names.arg=c("panny", "mezatki", "wdowy", "rozwodki"))
?barplot


dane_ <- c(17364000, 56128000, 11239000, 8170000)
nazwy<-c("panny", "mezatki", "wdowy", "rozwodki")
dane_s<-sort(dane_, decreasing = T)
dane_s
order(dane_, decreasing=T)
nazwy_s<-nazwy[order(dane_, decreasing=T)]
?order
barplot(dane_s, width=0.5, names.arg=nazwy_s)
barplot(dane_s, width=0.5, names.arg=c("mezatki","panny",  "wdowy", "rozwodki"))



dane_p<-prop.table(dane_s)
dane_p
barplot(dane_p, width=0.5, names.arg=c("mezatki","panny",  "wdowy", "rozwodki"), col="lightblue", main="Badania demograficzne kobiet przeprowadzone w 1988 roku w USA ")

?barplot




#2---------------------------------------------

stacje<- read.csv(file='h:/Windows7/Desktop/statystyka/Lab2/stacje.csv')
stacje_<-read.csv("stacje.csv")
df<- sort(table(stacje_), decreasing = T)
barplot(df, main = "Preferowana lokalizacja", ylim=c(0,400))


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

aa<-diff(range(butelki$presure))

#quantile
q1<-quantile(butelki$presure, probs=0.25)
q3<-quantile(butelki$presure, probs=0.75)
a<-q1-1.5 * iqr
b<-q3+1.5 *iqr

b_p<-butelki$presure[(butelki$presure>=a)&(butelki$presure<=b)]

boxplot(b_p, horizontal =TRUE)
