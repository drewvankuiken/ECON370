library(tidyverse)

tidyverse_packages()

mean_hwy_audi = mpg |> 
  filter(manufacturer=="audi") |> 
  group_by(model) |> 
  summarize(hwy_mean = mean(hwy))

class(mean_hwy_audi)

summarize(group_by(filter(mpg, manufacturer=="audi"), model), hwy_mean = mean(hwy))

starwars |> 
  filter( 
    species == "Human", height >= 190
  )

starwars |> 
  filter( 
    species == "Human" & height >= 190
  )

starwars |> 
  select(name:skin_color)

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

starwars |> 
  rename(across(everything(), toupper)) 


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


# first combine flights and planes using a left join
firstjoin = left_join(flights, planes)

firstjoin |>
  filter(is.na(engines))

left_join(flights, planes,by="tailnum") |>
  filter(is.na(engines))

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

tidy_stocks |> 
  pivot_wider(names_from = "stock",values_from = "returns")


economists = data.frame(name = c("Adam.David.Smith", "Paul.David.Samuelson", "Milton.David.Friedman"))
economists

economists |> separate(name, c("first_name","middle_name", "last_name"))
# gives warning
economists |> separate(name, c("first_name", "last_name"))


expand.grid(c("Freshmen","Sophomore","Junior","Senior"),c("Econ","Math","English"))




