#Utworzyæ wykresy gêstoœci, dystrybuanty i funkcji prze¿ycia dla zmiennych losowych o rozk³adzie normalnym 
#z parametrami: N(0,1) , N(1,1) , N(2,1) N(0,1) , N(0,0.5) , N(0,2) . 


plot_normal_density<- function(mean_, sd_){
  x <- seq(-5, 5, length=1000)
  y <- dnorm(x, mean=mean_, sd=sd_)
  plot(x, y, type="l", lwd=1)
}

plot_normal_distribution<- function(mean_, sd_){
  x <- seq(-5, 5, length=1000)
  y <- pnorm(x, mean=mean_, sd=sd_)
  plot(x, y, type="l", lwd=1)
}
plot_normal_dist<- function(mean_, sd_){
  x <- seq(-5, 5, length=1000)
  y <- 1- pnorm(x, mean=mean_, sd=sd_)
  plot(x, y, type="l", lwd=1)
}

plot_normal_density(2,1)
plot_normal_distribution(2,1)
plot_normal_dist(2,1)

