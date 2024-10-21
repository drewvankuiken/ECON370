########
# Solutions for HW2
########

## Logic questions
## Subsetting/Index a data.frame
library(openxlsx)
desk_fp = "/Users/drewvankuiken/Dropbox/econ370/local/hw2/"
HT_data = read.xlsx(paste0(desk_fp,"HTdata_labels.xlsx"),
                    sheet=1)

# Q1
HT_data_Zone2 = HT_data[HT_data$Zone.2 == 1,]
HT_data_Zone2 = HT_data[HT_data["Zone.2"] == 1,]

# Q2
HT_data_Delta45 = HT_data[HT_data$Delta > 0.04 & HT_data$Delta < 0.05,]

# Q3
HT_data_Con = HT_data[HT_data$Species.Concentration>0.5,]

# Q4
HT_data_Bidders23 = HT_data[HT_data$Bidders %in% 2:3,]

# Q5
HT_data_4Bidders1982 = HT_data[HT_data$Bidders==4 & HT_data$Year == 82,]

## --- If Statements and Functions

# Q1
my_abs = function(x){
  if(x<0){
    return(-1*x)
  }else{
    return(x)
  }
}

my_abs(-1)
my_abs = function(x){
  if(x<0){
    return(-x)
  }else{
    return(x)
  }
}

my_abs(-1)
#my_abs(c(-1,-2))

# Q2 
my_abs_vec = function(x){
  out = rep(0,length(x))
  out[x<0] = -x[x<0]
  out[x>0] = x[x>0]
  # out[x==0] = 0 # last case but not really needed because init 0
  return(out)
}

test_func <- function(x){
  x[x<0] = -x[x<0]
}

# Q3
my_sign = function(x){
  if(x > 0){
    return(1)
  }else if(x <0){
    return(-1)
  }else{
    return(0)
  }
}

# Q4 
my_sign_vec = function(x){
  out = rep(0,length(x))
  out[x<0] = -1
  out[x>0] = 1
  #out[x==0] = 0 # last case but not really needed because init 0
  return(out)
}

# Q5
CRRA = function(x,n){
  if(n >= 0 & n != 1){
    out = (x^(1-n)-1)/(1-n)
  }else if(n == 1){
    out = log(x)
  }else{
    stop("n must be non-negative!")
  }
  return(out)
}

# Q6
my_funct = function(x){
  out = rep(0,length(x))
  out[x<0] = (x^2+2*x+abs(x))[x < 0]
  out[x>=0 & x<2] = (x^2+3+log(abs(x)+1))[x>=0 & x<2]
  out[x>=2] = (x^2+4*x-14)[x>=2]
  return(out)
}

# Q7
my_mat = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)

mean_row    = apply(my_mat,1,mean)
mean_column = apply(my_mat,2,mean)

min_row    = apply(my_mat,1,min)
min_column = apply(my_mat,2,min)

max_row    = apply(my_mat,1,max)
max_column = apply(my_mat,2,max)

median_row    = apply(my_mat,1,median)
median_column = apply(my_mat,2,median)

sd_row    = apply(my_mat,1,sd)
sd_column = apply(my_mat,2,sd)

## Bad Controls

# q1
# Data is generated for this process in two steps. First, nature randomly selects 
# a population of 10,000 men and women with equal probability. Then, nature allocates
# one of two jobs to each individual. Men are allocated both job types with equal 
# probability, while women are allocated the high-type job a quarter of the time.
# Our outcome variable in this case is a person's wage, which is a direct function
# of what job they have. The high-type job pays $30 and the low-type job pays $15. 
# The data consist of 3 variables: wages, jobs, and sexes. 

# q2
# MC simulation
N = 10000
wl = 15
wh = 30
Nrep = 100

start.time <- Sys.time()
coef = matrix(nrow = Nrep, ncol = 7)
for(i in 1:Nrep){
  # sample populations
  # determine sex of agents
  s = sample(c(T,F), size=N, replace=TRUE)
  # determine wage of agents
  w = rep(0,N)
  w[s==0] = sample(c(wl,wh), size=length(s[s==0]), replace = TRUE)
  w[s==1] = sample(c(wl,wh), size=length(s[s==1]), replace = TRUE, 
                   prob = c(.75, .25))
  # determine job of agents - j==0 --> low wage, j==1 --> high wage
  j = w == 30
  
  mc_data <- data.frame(s,w,j)
  
  ws <- lm(w ~ as.factor(s), mc_data)
  wj <- lm(w ~ as.factor(j), mc_data)
  wsj <- lm(w ~ as.factor(s) + as.factor(j), mc_data)
  coef[i,] <- c(ws[[1]], wj[[1]],wsj[[1]])
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

apply(coef,2,mean, na.rm=T)

# q3
# Note: I think selection bias is the correct answer here, but if someone has a 
# reasonably compelling answer about adding bad controls, that's fine too. 
# You should have found the following results: 
# sex on wages: intercept of 22, beta_sex = -4
# job on wages: intercept of 15, beta_job = 15
# sex and job on wages: intercept of 15, beta_sex = ~0, beta_job = 15

# as you can see, including job in our regression sends the sex coefficient to 0. 
# this makes sense: jobs perfectly predict wages in this example, so sex doesn't 
# add any explanatory power to the regression. think back to our discussion of 
# selection bias. we defined selection bias as the bias that arises when individuals 
# are selected into our sample differently based on their characteristics. this is 
# exactly what's happening here. individuals are selected into the high and low 
# wage jobs differently based on what their sex is. to understand the determinants
# of wages, we need to look at the stage at which individuals are randomized. in
# this case, that's the stage when nature chooses the sex of an individual. 




## Auction Question
## --- First-Price Sealed Bid Optimal Bidding Function

# arugments are v (valuations) and n (number of bidders)
# note that v CAN be a vector of length greater than 1
b_i = function(v,n,Fv=function(x){pexp(x,rate=1/10)},
               xs=seq(0,max(v),0.00001),...){
  
  f = function(x){Fv(x,...)^(n-1)} # define function to integrate
  
  ## ---- set up objects needed for integration
  xs  = sort(xs)   # guarantee x grid is sorted
  dxs = diff(xs)   # create dxs 
  xN  = length(xs) # set number of points in x grid
  fx  = f(xs)      # calculate f(x) for all xs in grid
  
  ## ---- estimate F(x) = int_{x0}^{x} f(t) dt
  Fxs = (fx[1:(xN-1)]+fx[2:xN]) # create the terms in the trap rule
  Fxs = cumsum(Fxs*dxs/2)       # sum up each value of integral
  Fxs = c(0,Fxs)                # add that int_{x0}^{x0} f(t) dt is 0
  
  ## ---- interpolate F(x) and create numerator 
  xids  = findInterval(v,xs)             # get ids for which interval v is in 
  alpha = (v - xs[xids])/dxs[xids]       # weights between interval points
  numer = Fxs[xids]*alpha                # weight F(x) on the LHS
  numer = numer + Fxs[xids+1]*(1-alpha)  # weight F(x) on the RHS
  numer[is.na(numer)] = Fxs[length(Fxs)] # fix points out of bounds
  
  denom    = f(v)         # create denominator 
  bidshave = numer/denom  # create bid shave 
  
  # if the denominator is essentially 0, return v, otherwise shave bid
  ifelse(denom < .Machine$double.eps,v,v - bidshave)
}

SimulateAuction = function(Nsim){
  N = 5                         # set number of bidders
  sims = rexp(N*Nsim,rate=1/10) # draw valuations
  Vs = matrix(sims,ncol=N)      # make valuations into matrix
  Vs_s = apply(Vs,1,sort)       # sort Vs: remember the dimensions are flipped
  rev_SP = Vs_s[N-1,]           # get second price auction revenue
  rev_FP = b_i(Vs_s[N,],N)      # get first price auction revenue
  return(list(rev_FP=rev_FP,rev_SP=rev_SP)) # return revenues from each auction
}

## Q1

revs_Nsim5 = SimulateAuction(5)
sapply(revs_Nsim5,mean)


## Q2
# Nsim=10
revs_Nsim10 = SimulateAuction(10)
sapply(revs_Nsim10,mean)

# Nsim=100
revs_Nsim100 = SimulateAuction(100)
sapply(revs_Nsim100,mean)

# Nsim=1000
revs_Nsim1000 = SimulateAuction(1000)
sapply(revs_Nsim1000,mean)

# Nsim=10000
revs_Nsim10000 = SimulateAuction(10000)
sapply(revs_Nsim10000,mean)


## Q3

SimulateAuction_Loop = function(Nsim){
  N = 5                # set number of bidders
  rev_FP = rep(0,Nsim) # initialize revenue first price auction
  rev_SP = rep(0,Nsim) # initialize revenue first price auction
  
  for(sim in 1:Nsim){
    Vs = rexp(N,rate=1/10)       # draw valuations
    Vs_s = sort(Vs)              # sort valuations
    rev_SP[sim] = Vs_s[N-1]      # store second price revenue
    rev_FP[sim] = b_i(Vs_s[N],N) # store first price revenue
  }
  return(list(rev_FP=rev_FP,rev_SP=rev_SP)) # return revenues from each auction
}

VecTime = system.time(SimulateAuction(100))
LoopTime = system.time(SimulateAuction_Loop(100))
as.numeric(LoopTime[3]/VecTime[3])


# Q5

# The answers are the number of bidders (N=5), the rate parameter 1/10, and
# the distribution of valuations Exp. The first two are easy to implement,
# see code below. The other is a bit more challanging but very doable when
# you're more experienced.

# EXAMPLE CODE: NOT NEEDED
SimulateAuction = function(Nsim,N,rate){
  # Arguments:
  # Nsim - Number of auction simulations to run
  # N    - Number of bidders in the auction
  # rate - The rate parameter for the exponential distribution
  
  sims   = rexp(N*Nsim,rate=rate) # draw valuations
  Vs     = matrix(sims,ncol=N)    # make valuations into matrix
  Vs_s   = apply(Vs,1,sort)       # sort Vs: remember the dimensions are flipped
  rev_SP = Vs_s[N-1,]             # get second price auction revenue
  rev_FP = b_i(Vs_s[N,],N)        # get first price auction revenue
  
  return(list(rev_FP=rev_FP,rev_SP=rev_SP)) # return revenues from each auction
}




