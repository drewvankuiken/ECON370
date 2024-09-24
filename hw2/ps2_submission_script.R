#-------------------------------------------------------------------------------
# Drew Van Kuiken
# 09.24.2024
# PS2 Submission Script Shell
# Goal: Provide an example script that students can use to submit their homework. 
# Demonstrate how to write a script in general. 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Prep 
#-------------------------------------------------------------------------------
# functions
## --- First-Price Sealed Bid Optimal Bidding Function

# arguments are v (valuations) and n (number of bidders)
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
# note: normally, we would store all of the functions we need in a separate 
# script (in this case, ps2_funcs.R). we could then use the source command to 
# run that script from top to bottom. that way, we would have the functions saved
# in our global environment. since this homework is designed around writing functions,
# it will be easier for the grader if you don't use the source framework here. 
# for this homework, include functions you write in response to a question in the 
# body of your script, and not up here

# load packages

# load datasets

#-------------------------------------------------------------------------------
# Logic
#-------------------------------------------------------------------------------

## Subsetting Data

# 1

# 2

# 3...

## If Statements and Functions

# 1

# 2...

#-------------------------------------------------------------------------------
# Wage Gap
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Auction Simulation
#-------------------------------------------------------------------------------

