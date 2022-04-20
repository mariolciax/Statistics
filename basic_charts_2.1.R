library(graphics)
dane <- c(17364000, 56128000, 11239000, 8170000)
#a)
pie(dane, labels=c("panny", "mezatki", "wdowy", "rozwodki"))
#b)
barplot(dane, width=0.5, names.arg=c("panny", "mezatki", "wdowy", "rozwodki"))
?barplot
