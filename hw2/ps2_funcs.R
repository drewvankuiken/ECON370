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
