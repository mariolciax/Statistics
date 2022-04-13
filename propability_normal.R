#looking for propability from normal distribution

propability_normal_lower <- function(value, mean_, sd_){
  return(pnorm(value,mean= mean_,sd = sd_,lower.tail=TRUE))
  #upper tail, lower tail?
}
#a)
propability_normal_lower(179,173,6)

#P(a < Z < b) = P(Z < b) - P(Z < a)
propability_normal_between <- function(value_lower,value_upper, mean_, sd_){
  p1<-pnorm(value_lower,mean= mean_,sd = sd_ ,lower.tail=TRUE)
  p2<-pnorm(value_upper,mean= mean_,sd = sd_ ,lower.tail=TRUE)
  return(p2-p1)
}
#b)
propability_normal_between(167,180,173,6)

propability_normal_upper <- function(value_upper, mean_, sd_){
  p1<-pnorm(value_upper,mean= mean_,sd = sd_ ,lower.tail=FALSE)
  return(p1)
}
#c)
propability_normal_upper(181,173,6)


quantile_normal<- function(q, mean_, sd_){
  #q as % between 1 and 100
  return(qnorm(q/100, mean_, sd_))
  
}
#d)
quantile_normal(60,173,6)
