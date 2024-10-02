# supply and demand
a_d = 10
a_s = 1 
b_d = -3
b_s = 2

Nmkts = 1000                 # set number of markets
Sigma = c(2,1,1,3)           # set vcov for epsilon dist
Sigma = matrix(Sigma,ncol=2) # make Sigma a 2 by 2 matrix
print(Sigma)

## --- Form market supply and demand "shocks"
e_data = mvtnorm::rmvnorm(Nmkts,sigma = Sigma) # generate epsilons
colnames(e_data) = paste0("e_",c("d","s"))     # name columns
e_data = as.data.frame(e_data)                 # make data.frame
e_d = e_data$e_d
e_s = e_data$e_s                               # store epsilons

mean(e_data$e_s)
var(e_data$e_s)
mean(e_data$e_s[e_data$e_d>0]) # what should the sign be? 



## --- Create Supply and Demand data.frame
SD_data = data.frame(mkt_id = 1:Nmkts) # add a "market id"
## --- Add (observed) equilibrium price and quantity
SD_data$price    = (a_d+e_d-a_s-e_s)/(b_s-b_d)   # price
SD_data$quantity = a_s + b_s*SD_data$price + e_s # quantity
## --- Trim markets with negative price or negative demand
SD_data = SD_data[SD_data$price>0 & SD_data$quantity>0,]
head(SD_data)


ggplot(SD_data,aes(x=quantity,y=price)) + 
  geom_point() + geom_smooth(method="lm",se=F)+
  xlab("Quantity")+ylab("Price")+
  theme_minimal()
# supply?





# recovering parameters based on generated data
coef(lm(quantity~price,data=SD_data))












# flipping a coin
set.seed(123) 
runif(n=1,min=0,max=1)



ubs          = seq(1/6,1,1/6)
lbs          = seq(0,1-1/6,1/6)


draw = runif(1)

n = 6
ubs  = seq(1/n,1,1/n)
lbs  = seq(0,1-1/n,1/n)




which(ubs >= draw & lbs < draw)

n=20
findInterval(0.023,seq(0,1,1/n)) 

# how does findInterval work? 
x = seq(0.01,1,0.01)
y = findInterval(x,seq(0,1,1/3))
y2 = findInterval(x,seq(0,1,1/3), rightmost.closed = T)


roll_dice = function(k,n){
  draws  = runif(k)                #draw simulations
  findInterval(draws,seq(0,1,1/n)) #find interval
}


Nrolls    = 50000                    # set number of rolls
Nsides    = 20                       # set number of sides
rolls     = roll_dice(Nrolls,Nsides) # simulate rolls
roll_data = data.frame(rolls=rolls)  # store rolls in data.table

mean(roll_data[,"rolls"] == x)

# Approximating probabilities
Ndraw       = 3000                                   #set number of draws (sims)
ub          = 3                                     #set upper bound of the interval
lb          = -1                                    #set lower bound of the interval
mu          = 1                                     #set mean of the dist
sigma       = sqrt(4)                               #set st dev of the dist
vi          = rnorm(Ndraw,mu,sigma)                 #draw simulations
sim_prob    = mean(vi < ub &  vi > lb)              #estimate sim prob
theory_prob = pnorm(ub,mu,sigma)-pnorm(lb,mu,sigma) #calc theory prob
c("Simulated Prob"=sim_prob,"Actual Prob"=theory_prob)


# Harder
Nsim       = 1000                            # set number of sims
Mu         = c(2,3)                          # store means
Sigma      = matrix(c(4,0.5,0.5,1),ncol=2)   # store covariance matrix
vws        = mvtnorm::rmvnorm(Nsim,Mu,Sigma) # draw vs and ws
test1      = vws[,1] >= -1 & vws[,1] <= 3    # test if vs are in test range
names(vws) = c("v_i","w_i")                  # set names of draws
test2      = vws[,2] >= -1 & vws[,2] <= 3    # test if ws are in test range
sim_prob   = mean(test1 & test2)             # calc simulated probability
sim_prob

