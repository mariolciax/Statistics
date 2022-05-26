

#moje
data(chickwts)
head(chickwts)

waga<-chickwts$weight
t.test(waga,conf.level = 0.9)$conf.int
feed<-(chickwts$feed=="soybean")
t.test(waga*feed,conf.level = 0.9)$conf.int


#-na to nie wpadlam
dane<-chickwts[which(chickwts$feed=="soybean"),]
waga<-dane$weight
