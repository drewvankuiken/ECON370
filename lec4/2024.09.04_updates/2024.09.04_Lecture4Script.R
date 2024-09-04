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

### Discussion of if-else logic flow
x = -2
if ( x < 0 ) {
  print("x is less than 0")
}

x = runif(1,-1,1)
if(x > 0){
  print("x is positive!")
} else if(x < 0){
  print("x is negative!")
} else{
  print("x is 0!")
}

grade = 85 + rnorm(1,sd=5)
if (grade >= 90) {
  print(paste0("A ",round(grade)," is an A"))
} else if (grade >= 80) {
  print(paste0("An ",round(grade)," is a B"))
} else {
  print("The grade is not an A nor a B")
}

statement = F
if(statement){
  print("It's True!")
} else{ 
  print("It's False!")
}


#vals = c(TRUE,FALSE)
vals = c(T,F)
if(vals){
  print("TRUE!")
}

# original
x = 1:10
ifelse(x %% 2 == 0, "Even","Odd")

# GOAL, print out "the remainder is r"
# for the integers 1 to 10 where r is 
# the remainder when dividing by 3













# GOAL, print out "the remainder is r"
# for the integers 1 to 10 where r is 
# the remainder when dividing by 3
# There are only 3 possible values for
# r which are 0, 1, and 2
x = 1:10
ifelse(x %% 3 == 0, "Remainder is 0",
       ifelse(x %% 3 == 1, "Remainder is 1",
              "Remainder is 2"))

y = rep("",length(x))
y[x %% 3 == 0] = "Remainder is 0"
y[x %% 3 == 1] = "Remainder is 1"
y[x %% 3 == 2] = "Remainder is 2"
y

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

# big function
is_prime = function(x){
  if ( x %% 1 != 0 ) stop("x must be an integer!")
  if ( length(x)!=1 ) stop("x can only be length 1!")
  if ( x %in% 1:2 ){ # if x is 1 or 2, return FALSE or TRUE
    return(x == 2)
  } else { # otherwise, loop through numbers 3 to x
    num_vec    = 3:x
    prime_list = 2
    i          = 1
    for(n in num_vec){
      if(sum((n %% prime_list) == 0) == 0){
        i             = i + 1
        prime_list[i] = n
      }
    }
    if(x %in% prime_list){ # if x is in list of primes, return TRUE
      return(TRUE)
    }else{ # otherwise, return FALSE
      return(FALSE)
    }
  }
}


