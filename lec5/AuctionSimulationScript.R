N     = 2          # set number of bidders
vs    = c(1,2) # set valuations
Delta = 0.5       # set bid increment 

bidder = sample(1:N,1) # set initial bidder
pn     = Delta         # set initial prices
n      = 1             # initialize counter for number of rounds
print(bidder)          # show who gets to bid first

while(vs[bidder] >= pn + Delta){
  # print out round, current price, and current bidder
  print(c("round"=n,"Current Price"=pn,"Current Bidder"=bidder))
  
  pn = pn + Delta                # increase price from bidding
  bidder = ifelse(bidder==1,2,1) # switch who is now bidding
  n = n + 1
}

pn
