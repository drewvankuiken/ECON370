library(mvtnorm)
library(pracma)
library(data.table)
library(tidyverse)
library(fixest) # in case you'd like to run a normal Poisson regression
library(numDeriv)

LL_pois = function(theta){
  y = as.matrix(HW5_data[,1])
  X = as.matrix(HW5_data[,-1])
  Xtheta = X %*% matrix(theta)
  -sum(-exp(Xtheta)+y*Xtheta-log(factorial(y)))
}

NLS_pois = function(theta){
  y = as.matrix(HW5_data[,1])
  X = as.matrix(HW5_data[,-1])
  Xtheta = X %*% matrix(theta)
  sum((y-exp(Xtheta))^2)
}

mean_wgt = function(x,ws) sum(ws*x)/sum(ws)
var_wgt = function(x,ws) sum(ws*(x-mean_wgt(x,ws))^2)*(1/(sum(ws)-1))
cov_wgt = function(x1,x2,ws) (sum(ws*(x1-mean_wgt(x1,ws))*(x2-mean_wgt(x2,ws))))/(sum(ws)-1)
prop_wgt = function(ws) sum(ws)/length(ws)
