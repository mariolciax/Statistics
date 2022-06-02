drzewka<-Orange
head(Orange)
#mu=120
sr<-mean(drzewka$circumference)
#sr=115<120
?t.test()
t.test(drzewka$circumference,alternative = "less",mu=120)
##p-value = 0.3363
#p-value>0.05.
#brak podstaw do odrzucenia H_0