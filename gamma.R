#plots gamma distribution
#ex5
plot_gamma_density<- function(alpha_, beta_){
  x <- seq(0, 5, length=1000)
  y <- dgamma(x, shape = alpha_, scale = beta_)
  plot(x, y, type="l", lwd=1)
}
#a)
plot_gamma_density(1,1)
plot_gamma_density(0.5,1)
#b)
plot_gamma_density(2,1)
