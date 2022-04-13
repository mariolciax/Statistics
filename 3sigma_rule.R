#Regu³a Trzech Sigm dla danego rozk³adu normalnego N(µ,??) oznacza 
#¿e w przedziale [µ–3??,µ+3??] znajduje siê 99.7 % wszystkich obserwacji. 
#Oznacza to ¿e obserwacje, które nie nale¿¹ do tego przedzia³u 
#bêd¹ siê zdarza³ybardzo rzadko. 
#Dziêki tej regule w ³atwy sposób mo¿na te¿ zlokalizowaæ obserwacje odstaj¹ce.

sigma3_rule <- function(mean_ = 0, sd_ = 1) {
  lb<-(-sd_)
  ub<-sd_
  lb2<--2*sd_
  ub2<-2* sd_
  lb3<--3*sd_
  ub3<-3* sd_
  x <- seq(mean_ - 3 * sd_, mean_ + 3 * sd_, length = 100) 
  plot(x, dnorm(x, mean_, sd_), type = "n", ylab = "")
  x2 <- seq(lb, ub, length = 100)  
  y <- dnorm(x2, mean_, sd_)
  polygon(c(lb, x2, ub), c(0, y, 0), col ="lightgray")
 # polygon(c(lb2, x2, ub2), c(0, y, 0), col ="gray")
 # polygon(c(lb3, x2, ub3), c(0, y, 0), col ="lightblue")
  lines(x, dnorm(x, mean_, sd_), type = "l", ...)
}
normal_area()
