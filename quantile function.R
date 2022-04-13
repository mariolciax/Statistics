#quantile functions 

quantile_normal<- function(q, mean_, sd_){
  #q as % between 1 and 100
  return(qnorm(q/100, mean_, sd_))
  
}
#1.4
#a)
quantile_normal(95,0,1)
#b)
quantile_normal(97.5,0,1)


quantile_student<- function(q,df_){
  #df degress of freedom
  #q as % between 1 and 100
  return(qt(q/100,df_))
  
}

#c)
quantile_student(95,10)
#d)
quantile_student(99,20)

quantile_chisq<- function(q,df_){
  #df degress of freedom
  #q as % between 1 and 100
  return(qchisq(q/100,df_))
  
}
#e)
quantile_chisq(9,4)
#f)
quantile_chisq(95,10)
