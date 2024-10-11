monty_hall <- function(strategy="stay", guess_draw = runif(1)) {
  # simulate monty hall in steps
  # Step 1: 
  # allocate goats, car, and guess to doors
  car_draw <- runif(1)
  doors <- seq(0,2/3,1/3)
  
  car <- doors[findInterval(car_draw, doors, rightmost.closed = T)] 
  goats <- doors[doors != car]
  guess_1 <- doors[findInterval(guess_draw, doors, rightmost.closed = T)]

# Step 2: 
# monty opens a door with a goat behind it
  
# Step 3:
# player chooses strategy, we resolve who wins
  
  
  
  
  
  
  
  
  
  
  
  
# Step 2: 
open_draw <- runif(1)
num_doors_to_open <- length(goats[goats != guess_1])
open_door <- goats[goats != guess_1][findInterval(open_draw,
                                                  c(0,1/num_doors_to_open),
                                                  rightmost.closed = T)] # double indexing!


# resolve who wins
win <- guess_2 == car
return(win)
}

switch_res <- sapply(1:10000, function(x) monty_hall("switch"))
stay_res <- sapply(1:10000, function(x) monty_hall("stay"))
print(cbind(mean(switch_res), mean(stay_res)))



### solutions from class: 
guess_2 <- if(strategy=="stay"){
  guess_1
}else {
  doors[doors != guess_1 & doors != open_door]
}













first_guess <- findInterval(guess_draw,seq(0,1,1/3))
goat_door_1 <- 1
goat_door_2 <- 2
car_door <- 3
door_1 == goat
door_2 == car
door_3 == goat

guess = function(g) {
  if(g = 1) {
    return(door_1)
  }
  if(g = 2) {
    return(door_2)
  }
  if(g = 3) {
    return(door_3)
  }
}
