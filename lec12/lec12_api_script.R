## Load and install the packages that we'll be using today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, jsonlite, fredr, 
               listviewer, usethis)
## My preferred ggplot2 plotting theme (optional)
theme_set(hrbrthemes::theme_ipsum())

nyc_trees = fromJSON("https://data.cityofnewyork.us/resource/uvpi-gqnh.json") |> 
  as_tibble()
head(nyc_trees)

nyc_trees |>
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>% 
  mutate_at(vars(longitude:stump_diam), as.numeric) %>% 
  ggplot(aes(x=longitude, y=latitude, size=stump_diam)) + 
  geom_point(alpha=0.5) +
  scale_size_continuous(name = "Stump diameter") +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  )









endpoint = "series/observations"
params = list(
  api_key=Sys.getenv("FRED_API_KEY"), # change to your own key
  file_type="json",
  series_id="LOADFACTORDD11"
)

fred = GET(
  url = "https://api.stlouisfed.org/",
  path = paste0("fred/",endpoint),
  query = params
)

params2 = list(
  api_key=Sys.getenv("FRED_API_KEY"), # change to your own key
  file_type="json",
  series_id="LOADFACTERDD11"
)
fred2 = GET(
  url = "https://api.stlouisfed.org/",
  path = paste0("fred/",endpoint),
  query = params2
)

fred_content <- fred |> 
  content("text") |>  # text turns everything into a character vector
  fromJSON()
# how can we extract the data frame we want?





fred_obs <- fred_content[[length(fred_content)]] |>  # could also use pluck() from dplyr
  mutate(across(realtime_start:date,ymd),
         value=as.numeric(value))
head(fred_obs)

fred_obs |>
  ggplot(aes(date, value)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="Load Factor (%)",
    title="Airline Load Factors Over Time", caption="Source: FRED"
  )










fiber = GET(
  url = "https://www.googleapis.com/download/storage/v1/b/fiber/o/property-manager%2FRDU1.json?alt=media&apiKey=AIzaSyAeJhAYmfDCntb8-rVn6x9TyRgL6-IkPQw"
)

fiber_content <- fiber |> 
  content("text") |>  # text turns everything into a character vector
  fromJSON()
head(fiber_content)

fiber_content |>
  select(Longitude, Latitude, Ready) |> 
  ggplot(aes(x=Longitude, y=Latitude, col=as.factor(Ready))) + 
  geom_point(alpha=0.5) +
  scale_color_discrete(name = "Fiber Rollout Status",
                       labels = c("Unavailable","Available")) +
  labs(
    x = "Longitude", y = "Latitude",
    title = "Google Fiber Rollout",
    caption = "Source: Google"
  )
