
first_vec = 1:5

d = data.frame(x = 1:2, y = 3:4) 
class(d)

m = as.matrix(d)
fact_groups = sample(letters,10,replace=T)
as.integer(fact_groups)
as.character(d)

lm(d$y~d$x)

d2 = data.frame(x = rnorm(10), y = runif(10))

lm(y~x,data=d)

lm(y~x,data=d2)
summary(lm(y~x,data=d2))
#TRUE = 1

pi = function(q,p,mc){
  q*p - q*mc
}

rm(pi)

c(1,2,3)
c = 2

library(dplyr)
library(data.table)
library(ggplot2)

filter = stats::filter




