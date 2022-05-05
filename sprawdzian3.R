
samochody <- read.csv2("samochody.csv")
head(samochody)
producent<-samochody$producent
s<- character(length = length(samochody$producent))
s
s[which(samochody$producent ==1)] <-"America"
s[which(samochody$producent ==2)] <-"Europe"
s[which(samochody$producent ==3)] <-"Japan"

prod<- factor(s, levels=c("America","Europe","Japan"))

barplot(table(prod))


#2)
barplot(summary(factor(samochody$producent)),names.arg=c("America","Europe","Japan"),col="lightblue", ylim=c(0,100))

#3)
prod<-factor(producent)
levels(prod)<-c("America","Europe","Japan")
barplot(table(prod))



