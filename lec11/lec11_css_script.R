## Load and install the packages that we'll be using today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, lubridate, janitor, data.table, hrbrthemes)
## ggplot2 plotting theme (optional)
theme_set(hrbrthemes::theme_ipsum())

raw_wiki <- read_html("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression")
raw_wiki
class(raw_wiki)






raw_wiki |> html_elements("table")





raw_wiki |> html_elements("table.wikitable")

table_dfs <- raw_wiki |> html_elements("table.wikitable") |> 
  html_table()
table_dfs[[1]]


# better process
table_dfs_int <- raw_wiki |> html_elements("table.wikitable") |> 
  html_table()

table_dfs <- lapply(table_dfs_int[c(1,3,4)], # drop unwanted tables
                    function(x) x |>
                      clean_names() |> ## fix colnames, from the janitor package #<<
                      mutate(date = mdy(date))) ## from lubridate
table_dfs[[1]]

wr100 <- rbind(
  table_dfs[[1]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Pre-IAAF"),
  table_dfs[[2]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Pre-automatic"),
  table_dfs[[3]] |> select(time, athlete, nationality, date) |> 
    mutate(era="Modern")
)
head(wr100)

wr100 |> ggplot(aes(x=date,y=time,col=fct_reorder2(era,date,time))) +
  geom_point(alpha=0.7) +
  labs(title = "Men's 100m World Record Progression", x="Date", y="Time") + 
  theme(legend.title = element_blank()) # switch off legend title

# read in html, get listing info
web <- "https://raleigh.craigslist.org/search/jwa?query=watch#search=1~gallery~0~0"
craigslist_listings <- read_html(web) |> html_elements("[INSERT HTML ELEMENTS HERE]") 
craigslist_listings[[1]]





# read in html, get listing info
web <- "https://raleigh.craigslist.org/search/jwa?query=watch#search=1~gallery~0~0"
craigslist_listings <- read_html(web) |> html_elements("li a") 
craigslist_listings[[1]]

# follow branching tree further:
# title of listing
title <- craigslist_listings |> html_elements("div.title") |> html_text2()

# seems like 2 pieces of info stored in div.details
details <- craigslist_listings |> html_elements("div.details")
price <- details |> html_element("div.price") |> html_text2()
location <- details |> html_element("div.location") |> html_text2()

# we can use html_attr to grab the link to the listing
link <- craigslist_listings |> html_attr("href")

title[1]
price[1]
location[1]
link[1]

watch_data <- data.frame(title,price,location,link) |> 
  mutate(price = parse_number(price), # get rid of $s
         clean_location = case_when(
           grepl("durham",location, ignore.case=TRUE) ~ "Durham",
           grepl("cary|apex",location, ignore.case=TRUE) ~ "Cary",
           grepl("raleigh",location, ignore.case=TRUE) ~ "Raleigh",
           grepl("chapel hill",location, ignore.case=TRUE) ~ "Chapel Hill",
           grepl("wake forest",location, ignore.case=TRUE) ~ "Wake Forest",
           .default = "Other"
         )) |> 
  filter(price>0)

watch_data |> ggplot(aes(x=clean_location, y=price)) + 
  geom_boxplot(aes(fill=clean_location), show.legend = FALSE,
               shape = 21, size = .5) +
  scale_colour_viridis_d(option = "magma") +
  labs(title="Watches for sale near Raleigh, NC",
       x="Seller Location",
       y="Price (USD)") +
  theme_ipsum()

watch_data |> filter(price < 1000) |> ggplot(aes(x=clean_location, y=price)) + 
  geom_boxplot(aes(fill=clean_location), show.legend = FALSE,
               shape = 21, size = .5) +
  scale_colour_viridis_d(option = "magma") +
  labs(title="Watches for sale near Raleigh, NC",
       x="Seller Location",
       y="Price (USD)") +
  theme_ipsum()
