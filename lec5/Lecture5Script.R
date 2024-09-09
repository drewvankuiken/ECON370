1:10

sum_val = 0
for(i in 1:10){
  sum_val = sum_val + i
  print(c(sum_val,i))
}
sum_val
11*10/2

Nsim       = 100         #set number of simulations/draws
norm_draws = rnorm(Nsim) #draw N(0,1) random variables
out1       = rep(0,Nsim) #initialize output 1: MORE ON THIS LATER
out2       = rep(0,Nsim) #initialize output 2: MORE ON THIS LATER
n          = 1           #initialize counter

for(draw in norm_draws){
  print(draw)
} #NOTE THAT A COUNTER IS NEEDED

for(draw in norm_draws){
  out1[n] = draw^2
  n = n + 1
}

for(i in 1:Nsim){
  out2[i] = norm_draws[i]^2
}

all.equal(out1,out2)


# THIS IS NOT PREALLOCATION
out1 = c(0) #initialize output 1: MORE ON THIS LATER
out2 = c(0) #initialize output 2: MORE ON THIS LATER

N          = 100000
my_vec     = c(0)
my_vec_pre = rep(0,N)

for(i in 1:N){
  my_vec[i] = i^2
}
for(i in 1:N){
  my_vec_pre[i] = i^2
}

# Auction Example is in other script

### Fixed Point Example


eps   = .Machine$double.eps #set tolerance 
x_n   = 1e-12                #starting guess
x_np1 = sqrt(x_n)           #apply function

while(abs(x_n - x_np1) >= eps){
  print(c("distance"=abs(x_n - x_np1),"current x"=x_n))
  x_n   = x_np1     #update guess
  x_np1 = sqrt(x_n) #apply function
}
x_np1




