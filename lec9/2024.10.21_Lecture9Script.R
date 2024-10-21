library(tidyverse)
library(nycflights13)

tidyverse_packages()

mean_hwy_audi = mpg |>  
  filter(manufacturer=="audi") |> 
  group_by(model) |> 
  mutate(hwy_mean = mean(hwy)) |> 
  ungroup() |> 
  mutate(hwy_1 = hwy+1)

summarize(group_by(filter(mpg, manufacturer=="audi"), model), 
          hwy_mean = mean(hwy))

class(mean_hwy_audi)

glimpse(starwars)

starwars |> 
  filter( 
    species == "Human", height >= 190
  )

# base R version:
starbool <- starwars$species == "Human" & starwars$height >= 190
starbool

starwars |> 
  filter( 
    species == "Human" & height >= 190
  )

starwars |> 
  arrange(desc(birth_year))

starwars |> 
  select(name:skin_color)

starwars |> 
  select(name, height, mass, hair_color, skin_color)



starwars |> 
  select(name:skin_color,species)

starwars |> 
  select(name:skin_color,species,-height)

starwars |> 
  select(name,mass,hair_color,skin_color,species)

starwars |> 
  rename(alias=name,crib=homeworld)

starwars2 = starwars
names(starwars2) <- toupper(names(starwars2))

starwars_upper <- starwars |> 
  mutate(across(everything(), toupper)) 

starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE), .groups = "drop")




starwars2 = starwars
starwars2 = starwars2 |> select(height,mass,contains("color"),everything())

starwars2 |>
  select(name,everything())


starwars |> 
  select(name, birth_year) |>
  mutate(dog_years = birth_year * 7) |>
  mutate(comment = paste0(name, " is ", dog_years, " in dog years."))


# Mutate commands "stick around." DON'T have to do it like this!
starwars_dogyears = starwars |> 
  select(name, birth_year) |>
  mutate(dog_years = birth_year * 7) 
starwars_dogyears |>
  mutate(comment = paste0(name, " is ", dog_years, " in dog years."))

starwars |> 
  select(name:eye_color) |> 
  mutate(across(where(is.character), toupper)) |>
  head(5)

starwars |> 
  head(5)

starwars |> 
  mutate(across(where(is.numeric), function(x){x+1})) |>
  head(5)

starwars |> 
  mutate(across(where(is.numeric), function(x){
    x+1
    }
    )) |>
  head(5)

add_one <- function(x) x+1
starwars |> 
  mutate(across(where(is.numeric), add_one)) |>
  head(5)

starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE),
            min_height = min(height, na.rm=TRUE),
            max_height = max(height, na.rm=TRUE))


starwars |> 
  group_by(species, gender) |> 
  summarize(mean_height = mean(height, na.rm = TRUE),
            min_height = min(height, na.rm=TRUE),
            max_height = max(height, na.rm=TRUE)) |>
  filter(species=="Human")

starwars |> 
  group_by(species) |> 
  summarize(across(where(is.numeric), function(x) mean(x, na.rm=T))) |> #<<
  head(5)

# different way of writing same thing!
starwars |> 
  group_by(species) |> 
  summarize(across(where(is.numeric), \(x) mean(x, na.rm=T))) |> #<<
  head(5)

starwars |> slice(c(1, 5))

starwars |> 
  filter(gender=="feminine") |> 
  pull(height)

unique(starwars$gender)

starwars |>
  distinct(gender)

starwars |>
  count(gender)

library(nycflights13)
data(flights)
data(planes)
data(airlines)
data(airports)

flights |>
  distinct(dest)

head(flights)
head(planes)
head(airlines)
head(airports)

# first combine flights and planes using a left join
firstjoin = left_join(flights, planes)

firstjoin |>
  filter(is.na(engines))

left_join(flights, planes,by="tailnum") |>
  filter(is.na(engines))

left_join(flights, planes,by=join_by(tailnum)) |>
  filter(is.na(engines))

left_join(flights, planes |> rename(year_manuf = year),
          by=join_by(tailnum)) |>
  filter(is.na(engines))

# For Tampa Intl airport on Jan 1, 2013, figure out: 
# which carrier flew the most flights into Tampa Intl? 
# how many Airbus planes did JetBlue fly into Tampa Intl? 
# What manufacturers did Delta fly into Tampa?
tpa <- airports |> 
  filter(name == "Tampa Intl")

tampa_flights <- flights |> 
  left_join(tpa, by = join_by(dest==faa)) |> 
  filter(name=="Tampa Intl")

tampa_flights <- flights |> 
  left_join(tpa |> rename(dest=faa), by = join_by(dest)) |> 
  filter(name=="Tampa Intl")







# answer: 
# join planes to flights: 
pf <- left_join(flights, planes  |> select(-year) ,by="tailnum")
pfa <- pf |> 
  left_join(airports, by = join_by(dest==faa)) |> 
  left_join(airlines |> rename(carrier_name = name), by = "carrier")

# subset to airport and day of interest
pfa_s <- pfa |> 
  filter(year==2013 & day == 1 & month == 1 & name == "Tampa Intl")

# q1: carrier flew the most flights into tampa? 
pfa_s |> group_by(carrier_name) |> count() |> arrange(desc(n))

# q2: which plane type flown into Tampa most frequently?
pfa_s |> group_by(manufacturer) |> count() |> arrange(desc(n))





secondjoin = left_join(flights, planes,by="tailnum")

thirdjoin = left_join(flights |> rename(flight_year = year), 
          planes |> rename(year_built = year),
          by="tailnum") 


FlightsNoPlanes = left_join(flights,planes,by="tailnum") |>
  filter(is.na(engines))

temp = left_join(
  flights,
  airports |> rename(origin=faa) |> select(origin,lat,lon),
  by="origin") |> 
  rename(orig_lat = lat, orig_lon = lon)

temp=left_join(temp,
          airports |> rename(dest=faa) |> select(dest,lat,lon),
          by="dest") |>
  rename(dest_lat = lat, dest_lon = lon)


stocks = data.frame( ## Could use "tibble" instead of "data.frame" if you prefer
  time = as.Date('2009-01-01') + 0:1,
  X = rnorm(2, 0, 1),
  Y = rnorm(2, 0, 2),
  Z = rnorm(2, 0, 4)
)

tidy_stocks = stocks |> 
  pivot_longer(-time,names_to = "stock",values_to = "returns")

tidy_stocks = stocks |> 
  pivot_longer(X:Z,names_to = "stock",values_to = "returns")

tidy_stocks |> 
  pivot_wider(names_from = "stock",values_from = "returns")

stock_vol = data.frame(
  X_vol = rep(1,2),
  Y_vol = rep(2,2),
  Z_vol = rep(4,2)
)

stocks2 <- cbind(stocks, stock_vol) |> 
  rename(X_price = X, Y_price=Y, Z_price=Z)

stocks2_long <- stocks2 |> 
  pivot_longer(-time, 
               names_sep = "_",
               names_to = c("stock",".value"))
# more info: https://stackoverflow.com/questions/69798752/pivot-longer-for-multiple-sets-having-the-same-names-to

economists = data.frame(name = c("Adam.David.Smith", "Paul.David.Samuelson", "Milton.David.Friedman"))
economists

economists |> separate(name, c("first_name","middle_name", "last_name"))
# gives warning
economists |> separate(name, c("first_name", "last_name"))


gdp = data.frame(
  yr = rep(2016, times = 4),
  mnth = rep(1, times = 4),
  dy = 1:4,
  gdp = rnorm(4, mean = 100, sd = 2)
)
as_tibble(gdp |> unite(date, c("yr", "mnth", "dy"), sep = "-"))

# alternatives
crossing(grade = c("Freshmen","Sophomore","Junior","Senior"), subject = c("Econ","Math","English"))
expand.grid(c("Freshmen","Sophomore","Junior","Senior"),c("Econ","Math","English"))




