d = data.frame(x=runif(6),y=rnorm(6),z=rchisq(6,1))
d$x = (d$x - min(d$x,na.rm=TRUE))/(max(d$x, na.rm=TRUE) - min(d$x, na.rm=TRUE))
d$y = (d$y - min(d$x,na.rm=TRUE))/(max(d$y, na.rm=TRUE) - min(d$y, na.rm=TRUE))
d$z = (d$z - min(d$z,na.rm=TRUE))/(max(d$z, na.rm=TRUE) - min(d$z, na.rm=TRUE))















rescale <- function(v) {
  outvec = (v - min(v,na.rm=TRUE))/(max(v,na.rm=TRUE) - min(v,na.rm=TRUE))
  return(outvec)
}

# frankly, with the tools at our disposal, the utility of this function is limited:
d$x <- rescale(d$x)
d$y <- rescale(d$y)
d$z <- rescale(d$z)

# a bunch of stuff we haven't learned yet - but it's cool!
#d2 = as.data.frame(lapply(d,rescale))




return_input = function (x) {
  x #return the input as output
}

return_input = function (x) {
  stop("hey")
  return(y)
}


hypotenuse <- function(a, b) {
  length = (a**2 + b**2) ** (1/2)
  return(length)
}

hypotenuse(3, 4)




hypotenuse = function(a,b){
  # a^2 + b^2 = c^2
  # sqrt(a^c+b^2) = c
  sqrt(a^2+b^2)
}

wt_mean = function(x,w){
  sum(x*w)/sum(w)
}


wt_mean = function(x,w){
  if(length(x)!=length(w)){
    
  }
  elseif(length(x)>length(w)){
    
  }
  else{
    
  }
  sum(x*w)/sum(w)
}


wt_mean(1:20,rep(1/20,20))
mean(1:20)

wt_mean(1:20,c(0.1,0.2,0.3,0.3))

1:5
2:6

hypotenuse(1:5,2:6)
hypotenuse(1,2)
hypotenuse(2,3)

wt_mean = function(x,w){
  if(length(x)!=length(w)){
    stop("x and w must be the same length")
  }
  sum(x*w)/sum(w)
}

wt_mean(1:20,c(2,3))


test_fun = function(x,y=2){
  x+y
}

normalize = function(x, m = mean(x),s = sd(x)){
  (x - m)/s
}

normalize(c(1:20))
normalize(c(1:20,NA),m=mean(c(1:20,NA),na.rm=T),s=sd(c(1:20,NA),na.rm=T))

normalize = function(x, m = mean(x,na.rm=na.rm),s = sd(x,na.rm=na.rm),na.rm=FALSE){
  (x - m)/s
}

normalize(c(1:20,NA),na.rm=T)

my_mean = function(x){
  x_sum = sum(x)
  x_sum/length(x)
}

plus_delta = function(x,delta=1){
  print(paste0("We are adding ", delta, " to ", x, "!"))
  x + delta
}

test_out = plus_delta(5,0.123)

say_hello = function(){
  "Hello! :)"
}
say_hello()

y = 2
add_xy = function(x,y){
  x+y
}

add_xy(2,4)
# Will this return 4 or 6 since "y" is defined in 
# the global and function environment 

