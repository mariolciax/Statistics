x <- seq(-5, 5, length=10)
y <- pnorm(x, mean=0, sd=2)
pnorm(0.5, mean=0, sd=2)
dnorm(0.5, mean=0, sd=2)
#-------------------------------


plot_normal_density<- function(){
  x <- seq(-5, 5, length=1000)
  y <- dnorm(x, mean=0, sd=1)
  y1 <- dnorm(x, mean=1, sd=1)
  y2 <- dnorm(x, mean=2, sd=1)
  plot(x, y, type="l",lwd=1, col="blue")
  lines(x, y1,type="l", lwd=1,col="green")
  lines(x, y2, type="l",lwd=1)
  
}
plot_normal_density()



dens_fun <- function(x,mean_, sd_){
  return(dnorm(x, mean_,sd_))
}

curves_plot_density<-function(m1,sd1,m2,sd2,m3,sd3){
  curve(dens_fun(x, m1, sd1), xlim=c(-5,5), col="blue")
  curve(dens_fun(x,m2,sd2),add=TRUE, col="green")
  curve(dens_fun(x,m3,sd3),add=TRUE)
}
curves_plot_density(0,1,1,1,2,1)


#3sigma
p1<-pnorm(3,0,1)
p2<-pnorm(-3,0,1)
p<-p1-p2
p #0.9973002

p1<-pnorm(2,0,1)
p2<-pnorm(-2,0,1)
p<-p1-p2
p #0.9544997
#

?rnorm()
ilosc<-rnorm(1000)
sum((ilosc<=3)&(ilosc>=-3))/1000



licz =0
for (i in 1:1000){
  if (abs(ilosc[i])<3){
    licz =licz+1
  }
}
licz/1000

?qnorm

qnorm(0.6,173,6)



quantile_student<- function(q,df_){
  return(qt(q/100,df_))
  
}
quantile_student(95,10)


quantile_f <- function(q,df1,df2){
  return(qf(q,df1,df2))
}

quantile_f(0.99,3,18) 


dens_fun_t <- function(x,df_){
  return(dt(x, df_))
}

curves_plot_density_t<-function(df1,df2,df3){
  curve(dens_fun_t(x, df3), xlim=c(-5,5), col="green")
  curve(dens_fun_t(x,df2),add=TRUE, col="blue")
  curve(dens_fun_t(x,df1),add=TRUE, col="red")
  curve(dens_fun(x,0,1), add=TRUE, col="black")
}
curves_plot_density_t(1,5,30)

dens_fun_gamma <- function(x,alpha_,beta_){
  return(dgamma(x, shape=alpha_,rate =beta_))
}

curves_plot_density_t<-function(alpha1,beta1,alpha2,beta2,alpha3,beta3){
  curve(dens_fun_gamma(x,alpha1,beta1 ), xlim=c(0,10), col="green")
  curve(dens_fun_gamma(x,alpha2,beta2),add=TRUE, col="blue")
  curve(dens_fun_gamma(x,alpha3,beta3),add=TRUE, col="red")
  legend("topright", c("gamma1","gamma2","gamma3"),col=c("green", "blue", "red"), lty=1,lwd=2)
}
curves_plot_density_t(1,1,0.5,1,2,1)

curves_plot_density_t(2,1,2,2,2,3)
?legend


#gdy wyszla Martyna

rbinom(1,1,0.5)

barplot(dbinom(0:100,10,0.2),names.arg=0:100)
curve(dens_fun(x,2,1.6),add=TRUE, col="red")

plot(dbinom(0:100,10,0.2), type="h")
curve(dens_fun(x,2,1.6),xlim=c(0,10),add=TRUE, col="red")
