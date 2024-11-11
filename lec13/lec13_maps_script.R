## Load and install the packages that we'll be using today
library(httr)
library(jsonlite)
library(tidycensus)
library(tigris)
library(sf)
library(tidyverse)

### class notes
file_loc = system.file("shape/nc.shp", package="sf")
nc = st_read(file_loc, quiet = TRUE)
head(nc)

ggplot(nc) + 
  geom_sf(aes(geometry=geometry, fill=AREA), col="white") + 
  scale_fill_viridis_c(name = "Area") + 
  ggtitle("Counties of North Carolina")

nc_moll <- nc |> 
  st_transform(crs = "+proj=moll") # using the Mollweide projection
head(nc_moll)

ggplot(nc_moll) + 
  geom_sf(aes(geometry=geometry, fill=AREA), col="white") + 
  scale_fill_viridis_c(name = "Area") + 
  ggtitle("North Carolina in Mollweide")

triangle <- nc |> 
  filter(NAME %in% c("Durham","Wake","Orange", "Chatham")) |> 
  mutate(AREA = AREA*1000) |> 
  select(NAME,AREA, everything())
head(triangle)

triangle |> st_union() |> ggplot() + geom_sf(fill=NA,col="black") + labs(title = "The Triangle") + theme_minimal()

france = st_as_sf(map('france', plot=FALSE, fill=TRUE))
france = france[-c(95),] # observation causing some issues, not important
head(france)
data("seine", package = "spData")
head(seine)

seine_crs = st_transform(seine, crs = st_crs(france))
france_intersected = st_intersection(france, seine_crs) 

france_intersected |> ggplot() + geom_sf(alpha = 0.8, aes(fill = ID, col = ID)) + labs(title = "Seine, Marne and Yonne rivers", caption = "Colours depict French administrative regions") + theme(legend.title = element_blank())

france_river <- st_join(france,seine_crs) |> # what kind of join is this?
  filter(!is.na(name)) |> 
  distinct(ID, .keep_all = T) # some rows merge twice because 2 branches of river
head(france_river)

ggplot(france_river) + 
  geom_sf(alpha = 0.8, fill = "black", col = "gray50") + 
  geom_sf(data = seine, col = "#05E9FF", lwd = 1) + 
  labs(title = "Intersected regions only")

# OPTIONAL - TO GET EXTRA CENSUS DATA
# get api key for census data, save in Renviron file: 
# request an api key here: https://api.census.gov/data/key_signup.html
#census_api_key("PLACE_YOUR_API_KEY_HERE", install = TRUE)
# optional: store zctas in cache:
options(tigris_use_cache=TRUE)

# tree data
nyc_trees <- fromJSON("https://data.cityofnewyork.us/resource/uvpi-gqnh.json") |> 
  as_tibble() |>
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id, zipcode) |>
  mutate_at(vars(longitude:stump_diam), as.numeric)

# nyc zip code data
ny_zips <- zctas(state = "NY", class = "sf", year = 2010) # zips only available in 2010
head(ny_zips)
urb <- urban_areas(year=2020) |> filter(grepl("New York",NAME10))
ny_urb_zips <- st_join(ny_zips,urb) |> filter(!is.na(NAME10))

# task: create 2 maps
# map 1: zip codes of NYC which contain trees downloaded from NYC Open Data
# map 2: all zip codes in NY urban area, with points representing trees in NYC
# (map 2 is not very interesting)
















## 2 routes: merge on zip, or convert nyc_trees to sf object and take union


# option 1: merge on zip
nyc_data <- nyc_trees |> 
  left_join(ny_urb_zips, by = join_by(zipcode==ZCTA5CE10))



# route 2: union
# convert nyc_trees to sf object
nyc_trees_sf <- st_as_sf(x=nyc_trees,
                     coords=c("longitude","latitude"))
# fix projections 
nyc_trees_sf <- st_set_crs(nyc_trees_sf, st_crs(ny_urb_zips))
ny_data <- st_join(ny_urb_zips,nyc_trees_sf)

#-------------------------------------------------------------------------------
# create graph
ggplot(nyc_data) + 
  geom_sf(aes(geometry=geometry), colour = "white") +
  geom_point(aes(x=longitude, y=latitude, size=stump_diam), alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  ) +
  theme_minimal()

# full ny state
ggplot(ny_data) + 
  geom_sf(aes(geometry=geometry), colour = "white") +
  geom_sf(data=nyc_trees_sf, alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees in NY Urban Area",
    caption = "Source: NYC Open Data"
  ) +
  theme_minimal()





