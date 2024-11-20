# ps4 solutions
library(sf)
library(janitor)
library(jsonlite)
library(httr)
library(rvest)
library(tigris)
library(tidyverse)
library(data.table)

# 1 - webscraping, css
start_time <- Sys.time()
root <- "https://books.toscrape.com/"

# get number of pages in total - overengineered, but neat to do it all in 
# code
pages <- read_html(root) |> 
  html_elements("li.current") |> 
  html_text2()
start <- parse_number(strsplit(pages,"of")[[1]][1])
end <- parse_number(strsplit(pages,"of")[[1]][2])

# page list
page_loop <- paste0("/catalogue/page-",start:end,".html")
#/catalogue/page-1.html is same as /catalogue/index.html

# general plan:
# navigate to page of results
# get links to each book on page
# get html for each book on page, collect relevant table+description
# move to next page of results, repeat process until complete
# hence, need 2 for loops: outer loop loops across pages, inner loop loops across
# books on each page

# preallocation strategy: 
full_container = vector(mode = "list",length(page_loop))

for(i in 1:length(page_loop)){
  print(paste("Now scraping page",i))
  # for each page, get books
  book_links <- read_html(paste0(root,page_loop[i])) |> 
    html_elements("article h3 a") |> 
    html_attr("href")
  
  page_container = vector(mode = "list", length = length(book_links))
  
  for(j in 1:length(book_links)){
    # inner loop
    book_page <- read_html(paste0(root,"catalogue/",book_links[j]))
    
    # need to grab 3 things: book name, description, and info table
    book_name <- book_page |> 
      html_element("h1") |> 
      html_text2()
    
    # fancy way of doing this - fine to just get all <p> elements 
    # and take the fourth one
    book_descr <- book_page |> 
      html_elements("p:not([class])") |> 
      html_text2()
    # students code should not break for books without a description 
    # 2 examples: Alice in Wonderland (book 996, page 50)
    # and The Bridge to Consciousness: I'm Writing the Bridge 
    # Between Science and Our Old and New Beliefs, book 161, page 9
    
    book_table <- book_page |> 
      html_elements("table") |> 
      html_table()
    
    # create df with info
    book_df <- book_table[[1]] |> 
      pivot_wider(names_from="X1", values_from = "X2") |> 
      clean_names() |> 
      mutate(name = book_name, 
             descr = ifelse(length(book_descr)==0,"",book_descr))
    
    # add to vector
    page_container[[j]]=book_df
  }
  
  # bind rows for page_container into a dataframe, then add to full_container
  # relocate to put name first for convenience - not necessary on HW
  full_container[[i]]=bind_rows(page_container) |> relocate(name)
}

scraped_books=bind_rows(full_container)
end_time <- Sys.time()
start_time - end_time

### asteroids
# q1
asteroid_root <- "https://ssd-api.jpl.nasa.gov/cad.api"
params <- list(
  body="ALL",
  "date-min"="1900-01-01",
  "date-max"="2100-01-01",
  "dist-max"=100,
  neo=FALSE,
  "total-only"=TRUE
)

asteroid_tot = GET(
  url = asteroid_root,
  query = params
) |> 
  content("text") |> 
  fromJSON()
asteroid_tot$total
# note: if you change the dates, you can get more obs. 
# anyone who got more than 2.7M gets full points. 
# END Q1

# Q2
params_df <- list(
  "date-min"="1900-01-01",
  "date-max"="2100-01-01",
  "dist-max"=100,
  sort="dist"
)

asteroid <- GET(
  url = asteroid_root,
  query = params_df
) |> 
  content("text") |> 
  fromJSON()

asteroid_df <- as.data.frame(asteroid[[length(asteroid)]])
names(asteroid_df) <- asteroid$fields
# END Q2

# Q3 - prep
# clean df
asteroid_df <- asteroid_df |> 
  mutate(cd = ymd_hm(cd),
         dist=as.numeric(dist),
         dist_min=as.numeric(dist_min),
         dist_max=as.numeric(dist_max),
         v_rel=as.numeric(v_rel),
         v_inf=as.numeric(v_inf),
         h=as.numeric(h)) |> 
  arrange(desc(dist))

# GRAPH
asteroid_graph <- asteroid_df |> 
  filter(year(cd)==2024) |> 
  ggplot(aes(x=h, y=dist, col=v_rel)) +
  geom_point(alpha=0.5) + 
  labs(
    title="Asteroid Size and Distance from Earth in 2024",
    x="Size of Asteroid",
    y="Distance from Earth (au)",
    col="Relative Velocity"
  )
asteroid_graph
# END Q3

# Q4
# students can be creative here. just put together something that is 
# moderately insightful
asteroid_graph_close <- asteroid_df |> 
  filter(year(cd)==2024, dist < 0.05) |> 
  ggplot(aes(x=h, y=dist, col=v_rel)) +
  geom_point(alpha=0.5) + 
  labs(
    title="Asteroid Size and Distance from Earth in 2024",
    x="Size of Asteroid",
    y="Distance from Earth (au)",
    col="Relative Velocity"
  )
asteroid_graph_close

# I think velocity decreases with size, but a compelling 
# counterexample would be fine/welcome
summary(lm(v_rel ~ h, asteroid_df)) # looks like velocity drops as size increases

asteroid_graph_colors <- asteroid_df |> 
  filter(year(cd)==2024, dist < 0.05) |> 
  mutate(cflag = v_rel>15) |> 
  ggplot(aes(x=h, y=dist, col=as.factor(cflag))) +
  geom_point(alpha=0.5) + 
  labs(
    title="Asteroid Size and Distance from Earth in 2024",
    x="Size of Asteroid",
    y="Distance from Earth (au)",
    col="Relative Velocity > 15"
  )
asteroid_graph_colors
# END Q4

### HIDDEN API QUESTION
# bullet 1
# get triangle data
fiber_tri = GET(
  url = "https://www.googleapis.com/download/storage/v1/b/fiber/o/property-manager%2FRDU1.json?alt=media&apiKey=AIzaSyAeJhAYmfDCntb8-rVn6x9TyRgL6-IkPQw"
)

tri_content <- fiber_tri |> 
  content("text") |>  # text turns everything into a character vector
  fromJSON()
head(tri_content)

# get charlotte data
fiber_cha = GET(
  url = "https://www.googleapis.com/download/storage/v1/b/fiber/o/property-manager%2FCharlotte.json?alt=media&apiKey=AIzaSyAeJhAYmfDCntb8-rVn6x9TyRgL6-IkPQw"
)

cha_content <- fiber_cha |> 
  content("text") |>  # text turns everything into a character vector
  fromJSON()
head(cha_content)
# end bullet 1

# bullet 2
# read in Zillow smoothed rent data
zillow <- read_csv("./lec12/Zip_zori_uc_sfrcondomfr_sm_month.csv") |> 
  clean_names() |> 
  filter(state=="NC") |> 
  select(region_name, county_name, x2024_09_30) |> 
  mutate(zip=as.numeric(region_name))

# combine, extract zip codes
nc_fiber <- rbind(tri_content, cha_content) |> 
  clean_names() |> 
  mutate(zip = as.numeric(str_sub(building_address,-5,-1)))

# read in NC shapefile
nc <- zctas(state = "NC", class = "sf", year = 2010)

# merge zips and zillow data, so we have average rents for all NC
nc_geo <- nc |> 
  select(ZCTA5CE10,geometry) |> 
  mutate(zip=as.numeric(ZCTA5CE10)) |> 
  left_join(zillow, by=join_by(zip))
# END BULLET 2

# BULLET 3
# graph, will county colors 
ggplot() +
  geom_sf(data = nc_geo, aes(fill = x2024_09_30), color = "white") +
  geom_point(data = nc_fiber, aes(x = longitude, y = latitude, col=ready)) +
  labs(title = "Google Fiber Rollout in North Carolina",
       x = "Longitude", y = "Latitude",
       color="Fiber Installed?",
       fill="Average Monthly Rent on Zillow") +
  theme_minimal()
# END BULLET 3

# BULLET 4
# fiber rollout-specific maps
# Mecklenburg graph
cha_df <- cha_content |> 
  clean_names() |> 
  mutate(zip = as.numeric(str_sub(building_address,-5,-1))) |> 
  left_join(nc_geo, by=join_by(zip))

ggplot() +
  geom_sf(data = cha_df, aes(geometry = geometry, fill = x2024_09_30), 
          color = "white") +
  geom_point(data = cha_df, aes(x = longitude, y = latitude, col=ready)) +
  labs(title = "Google Fiber Rollout in Mecklenburg County, NC",
       x = "Longitude", y = "Latitude",
       color="Fiber Installed?",
       fill="Average Monthly Rent on Zillow") +
  theme_minimal()

# triangle graph
tri_df <- tri_content |> 
  clean_names() |> 
  mutate(zip = as.numeric(str_sub(building_address,-5,-1))) |> 
  left_join(nc_geo, by=join_by(zip))

ggplot() +
  geom_sf(data = tri_df, aes(geometry = geometry, fill = x2024_09_30), 
          color = "white") +
  geom_point(data = tri_df, aes(x = longitude, y = latitude, col=ready)) +
  labs(title = "Google Fiber Rollout in the Research Triangle",
       x = "Longitude", y = "Latitude",
       color="Fiber Installed?",
       fill="Average Monthly Rent on Zillow") +
  theme_minimal()
# END BULLET 4
