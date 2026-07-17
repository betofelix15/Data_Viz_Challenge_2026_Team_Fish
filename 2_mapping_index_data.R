# Mapping Index Data
# June 24, 2026
# Author: Jesus Felix, Mumtahinah Zia, Logan Haviland

# notes: QCEW is the quarterly Census Employmnet and Wages. We use this to find
# employment and wages for fishing and acuaculture data

# this line cleans the environment, free up memory.
rm(list = ls())
# base libraries ----------------------------------------------------------------
# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "gganimate",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata", "leaflet","fontawesome","dygraphs","shiny","xts","plotly",
          "ggpattern","UpSetR","colorBlindness","data.table","mapview","usmap","DT",
          "tidycensus","tigris","spData","spDataLarge","ggspatial","r5r","stars",
          "raster","geobr","gtfs2gps","osmdata","h3jsr","viridisLite","ggnewscale",
          "magrittr","lwgeom","cowplot","ggrepel","rnaturalearth","canadianmaps")

# This line says" check and see which of the packages you want is already
# installed.  Those note loadded are ones you "need".
need <- want[!(want %in% installed.packages()[,"Package"])]

# This line says, if a package is "needed", install it.
if (length(need)) install.packages(need)

# This line "requires" all the packages you want.  Meaning that in order 
# to use a function that is loaded, you need to "activate" it, by "requiring" it.
sapply(want, function(i) require(i, character.only = TRUE))

# This line removes the vectors "want" and "need".
rm(want, need)

# working directory --------------------------------------------------------------
# This line creates an empty list, which the next few lines populate with 
# a set of file paths.
dir <- list()

# This line uses getwd() to find where your .Rproj file is.
getwd()

# This line takes this location as the "root" directory for the project.
dir$root <- str_remove(getwd(),"/R codes")  # because the codes are in another file, you have to add it

# Observe the output in the console:
dir$root 

# shapefiles
dir$shp <- paste0(dir$root, "/shapefiles/")

# figures directory 
dir$fig <- paste0(dir$root, "/figures/")

# unprocessed data folder 
dir$rawdata <- paste0(dir$root, "/data/raw data/")


# clean data folder (or rather processed data)
dir$processed <- paste0(dir$root, "/data/processed data/")

# how to make not in
`%not_in%` <- purrr::negate(`%in%`)

# remove scientific notation
options(scipen = 999)

# Load data, add lat longs, save ------------------------------------------------------------------

# load the df_full
df_full <- read.csv(paste0(dir$processed,"shellfish_114112_2015_2025_wages_employment_data.csv"))


# Counties ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load the counties wage data
df_counties <- read.csv(paste0(dir$processed,"shellfish_income_wages_2015_2025_counties.csv"))

# add the padded zeros (to match the original fips)
df_counties$fips <- sprintf("%05d",df_counties$fips)

# Download county shapefile for all states
counties_sf <- counties(cb = TRUE, year = 2023)  # cb=TRUE gives simplified geometry

# join with df_counties
county_polygons <- left_join(df_counties, 
                             counties_sf %>% dplyr::select(GEOID,geometry),
                             by = c("fips" = "GEOID")) 

# verify no missing geometries
nrow(county_polygons[is.na(county_polygons$geometry),])

# turn into an sf object and save

county_polygons <- st_as_sf(
  county_polygons, crs = crs(counties_sf)
)

# change polygon names 
county_p <- county_polygons %>%
  dplyr::select(area_title,county_code,county,state,state_code,fips,geometry) %>%
  distinct()

county_p <- st_as_sf(county_p, crs = crs(county_sf))

# save as a csv and always convert to sf

st_write(
  county_p,
  paste0(dir$shp,"us_counties.shp"),
  append = TRUE
)

# read back to confirm
check <- st_read( paste0(dir$shp,"us_counties.shp"))

# API key for census data: 3c1876e60ac757c4134ff22edf8419c61a7c9585

census_api_key("3c1876e60ac757c4134ff22edf8419c61a7c9585", install = TRUE)
readRenviron("~/.Renviron")  # Reload environment so key is available



# summaries for maps -----------------------------------------------------

# read in employment data
county_emp <- read.csv(paste0(dir$processed,"shellfish_employment_data_monthly_county_2015_2025.csv"))

# add the padded zeros (to match the original fips)
county_emp$fips <- sprintf("%05d",county_emp$fips)


# 2021 to 2025 employment 
sum_county_emp <-  county_emp %>%
  filter(year > 2020) %>%
  group_by(state,county,fips) %>%
  summarize(
    avg_emp_lvl = mean(employment_level, na.rm = T),
    med_emp_lvl = median(employment_level, na.rm = T),
    max_emp_lvl = max(employment_level, na.rm = T),
    
    avg_lq_emp_lvl = mean(lq_employment_level, na.rm = T),
    med_lq_emp_lvl = median(lq_employment_level, na.rm = T),
    max_lq_emp_lvl = max(lq_employment_level, na.rm = T),
    
    avg_oty_emp_lvl_chg = mean(oty_employment_level_chg, na.rm = T),
    med_oty_emp_lvl_chg = median(oty_employment_level_chg, na.rm = T),
    max_oty_emp_lvl_chg = max(oty_employment_level_chg, na.rm = T),
    
    avg_oty_emp_lvl_pct_chg = mean(oty_employment_level_pct_chg, na.rm = T),
    avg_oty_emp_lvl_pct_chg = median(oty_employment_level_pct_chg, na.rm = T),
    avg_oty_emp_lvl_pct_chg = max(oty_employment_level_pct_chg, na.rm = T)
    
  )


# summarize the wage and establishment data
sum_county_wage <- df_counties %>%
  filter(year > 2020) %>%
  group_by(state,county,fips) %>%
  summarize(
    
    avg_weekly_wages = mean(avg_wkly_wage, na.rm = T),
    min_weekly_wage = min(avg_wkly_wage, na.rm = T),
    max_weekly_wage = max(avg_wkly_wage, na.rm = T),
    
    total_wages = sum(total_qtrly_wages, na.rm = T),
    avg_qtrly_wages = mean(total_qtrly_wages, na.rm = T),
    min_qtrly_wages = min(total_qtrly_wages, na.rm = T),
    max_qtrly_wages = max(total_qtrly_wages, na.rm = T),
    
    total_taxable_wages = sum(taxable_qtrly_wages, na.rm = T),
    avg_taxable_qtrly_wages = mean(taxable_qtrly_wages, na.rm = T),
    min_taxable_qtrly_wages = min(taxable_qtrly_wages, na.rm = T),
    max_taxable_qtrly_wages = max(taxable_qtrly_wages, na.rm = T),
    
    total_establishments = sum(qtrly_estabs, na.rm = T),
    avg_qtrly_estabs = mean(qtrly_estabs, na.rm = T),
    min_qtrly_estabs = min(qtrly_estabs, na.rm = T),
    max_qtrly_estabs = max(qtrly_estabs, na.rm = T),
    
    total_qtrly_wages_per_estab = sum(total_qtrly_wages_per_establishment, na.rm = T),
    avg_qtrly_wages_per_estab= mean(total_qtrly_wages_per_establishment, na.rm = T),
    min_qtrly_wages_per_estab = min(total_qtrly_wages_per_establishment, na.rm = T),
    max_qtrly_wages_per_estab = max(total_qtrly_wages_per_establishment, na.rm = T)
    
    
  )

# join these
total_summary <- left_join(sum_county_emp,sum_county_wage,
                           by = c("state","county","fips"))

# save the summary
write.csv(total_summary,
          paste0(dir$processed,"summary_shellfish_2021_2025_wages_employment_establishments.csv"),
          row.names = F)

# read in the summary data
total_summary <- read.csv(paste0(dir$processed,"summary_shellfish_2021_2025_wages_employment_establishments.csv"))


# add other normalization metrics
total_summary$total_taxable_wages_per_estb <- total_summary$total_taxable_wages/total_summary$total_establishments

summary(total_summary$total_taxable_wages_per_estb)


total_summary$total_taxable_wages_per_avg_emplyt_lvl <- total_summary$total_taxable_wages/total_summary$avg_emp_lvl

summary(total_summary$total_taxable_wages_per_avg_emplyt_lvl)


# add the padded zeros (to match the original fips)
total_summary$fips <- sprintf("%05d",total_summary$fips)


# add the geometry
map_county_emp_sf <- full_join(total_summary, 
                             counties_sf %>% dplyr::select(GEOID,geometry,STUSPS,NAMELSAD),
                             by = c("fips" = "GEOID","state" = "STUSPS","county" = "NAMELSAD")) 

map_county_emp_sf <- st_as_sf(
  map_county_emp_sf, crs = crs(counties_sf)
)

u_crs <- st_crs(counties_sf)


# we are focusing on c("MA","RI","NH","ME","WA")
# we want WA to move closer to Northeast

wa_emp_sf <- map_county_emp_sf %>%
  filter(state == "WA")

# shift the coordinates
st_geometry(wa_emp_sf) <- st_geometry(wa_emp_sf) + c(42,-3)
st_crs(wa_emp_sf) <- crs(map_county_emp_sf)


# filter for northeast US
ne_emp_sf <- map_county_emp_sf %>%
  filter(state %in% c("MA","RI","NH","ME")) 



# combine to make a good map
map_emp_sf <- rbind(wa_emp_sf,ne_emp_sf)

# Some common ACS variables:
#   
#   Variable ID	Description
# B01003_001	Total population
# B19013_001	Median household income (in dollars)
# B02001_002	White alone population
# B15003_022	Bachelor's degree

# get the data for the counties in the map_emp_sf
# pop_data_2021 <- get_acs(
#   geography = "county",
#   variables = "B01003_001",  # Total population
#   year = 2021,
#   survey = "acs1"
# )


# check to see where it lands
# flat 
plot(map_emp_sf$geometry)


# plot an index
ggplot() +
  geom_sf(data = map_emp_sf,
          aes(fill = avg_emp_lvl)) +
  labs(fill = "Avg. Employment Levels",
       title = "Employment in WA & Northeast US: 2021-2025")+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title =  element_text(hjust = 0.5)
  )



# practice plots --------------------------------------

# northeast map
plot_usmap(data = map_county_emp,
       values = "avg_emp_lvl",
       include = c("MA","RI","NH","ME")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2015 - 2025 Avg. Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )

# washington state map
plot_usmap(data = map_county_emp,
           values = "avg_emp_lvl",
           include = c("WA")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2015 - 2025 Avg. Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )



# washington state and northeast together
plot_usmap(data = map_county_emp,
           values = "avg_emp_lvl",
           include = c("MA","RI","NH","ME","WA")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2015 - 2025 Avg. Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )


# can we plot cities?


locality_emp <- read.csv(paste0(dir$processed,"shellfish_employment_data_monthly_locality_2015_2025.csv"))


locality_map_21_25_emp_lvl <-  locality_emp %>%
  filter(year >2020) %>%
  group_by(state_abb,locality,fips) %>%
  summarize(
    avg_emp_lvl = mean(employment_level, na.rm = T),
    med_emp_lvl = median(employment_level, na.rm = T),
    max_emp_lvl = max(employment_level, na.rm = T),
    
    avg_lq_emp_lvl = mean(lq_employment_level, na.rm = T),
    med_lq_emp_lvl = median(lq_employment_level, na.rm = T),
    max_lq_emp_lvl = max(lq_employment_level, na.rm = T),
    
    avg_oty_emp_lvl_chg = mean(oty_employment_level_chg, na.rm = T),
    med_oty_emp_lvl_chg = median(oty_employment_level_chg, na.rm = T),
    max_oty_emp_lvl_chg = max(oty_employment_level_chg, na.rm = T),
    
    avg_oty_emp_lvl_pct_chg = mean(oty_employment_level_pct_chg, na.rm = T),
    avg_oty_emp_lvl_pct_chg = median(oty_employment_level_pct_chg, na.rm = T),
    avg_oty_emp_lvl_pct_chg = max(oty_employment_level_pct_chg, na.rm = T)
    
  )


# localities state and northeast together
plot_usmap(data = map_emp_sf,
           values = "avg_emp_lvl",
           include = c("MA","RI","NH","ME")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2015 - 2025 Avg. Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )

                         
# layered plot ----------------------------------------------------------

# original function for tilting created by Stefan Juenger
# https://stefanjuenger.github.io/gesis-workshop-geospatial-techniques-R/slides/2_4_Advanced_Maps_II/2_4_Advanced_Maps_II.html#8

#~~~~~~~~~~~~~~~~~~~~
# ROTATE SF
#~~~~~~~~~~~~~~~~~~~

# use the function to rotate the data
rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

# function to rotate sf object
rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}


# try to tilt one layer
simple_square <-
  sf::st_point(1:2) %>% 
  sf::st_sfc() %>%
  sf::st_sf() %>% 
  sf::st_buffer(
    10, 
    endCapStyle = "SQUARE"
  )

ggplot() +
  geom_sf(data = simple_square)

simple_square_rotated <-
  simple_square %>% 
  rotate_data()

ggplot() +
  geom_sf(data = simple_square_rotated)

# plot one county
one_c <- map_emp_sf %>% filter(county == "Grays Harbor County")

ggplot() +
  geom_sf(data = one_c)

one_c_rotated <- one_c %>%
  rotate_data()

ggplot() +
  geom_sf(data = one_c_rotated)


# what happens when you add multiple polygons
two_c <- map_emp_sf %>% filter(county %in% c("Grays Harbor County","Island County"))

two_c_rotated <- two_c %>%
  rotate_data() 

# returns an error

# so this function can only do it for one polygon at a time. This means that we
# have to repeat it for each county (could use a loop or a sapply)

#rotated_county <- sapply(1:nrow(map_county_emp), rotate_data)

rotated_county <- NULL
for(i in as.list(map_emp_sf$fips)){
  
  # select the fip
  temp <- map_emp_sf %>% filter(fips == i)
  
  # rotate the polygon
  temp <- temp %>% rotate_data()
  
  # save the rotated
  rotated_county <- rbind(rotated_county,temp)
  
}

rotated_county <- st_as_sf(rotated_county, crs = st_crs(counties_sf))


# we neeed to do this for each layer but adding .1 in the add_y
rotated_county1 <- NULL
for(i in as.list(map_emp_sf$fips)){
  
  # select the fip
  temp <- map_emp_sf %>% filter(fips == i)
  
  # rotate the polygon
  temp <- temp %>% rotate_data( y_add = -5)
  
  # save the rotated
  rotated_county1 <- rbind(rotated_county1,temp)
  
}

rotated_county1 <- st_as_sf(rotated_county1, crs = st_crs(counties_sf))

# add .2
rotated_county2 <- NULL
for(i in as.list(map_emp_sf$fips)){
  
  # select the fip
  temp <- map_emp_sf %>% filter(fips == i)
  
  # rotate the polygon
  temp <- temp %>% rotate_data( y_add = -10)
  
  # save the rotated
  rotated_county2 <- rbind(rotated_county2,temp)
  
}

rotated_county2 <- st_as_sf(rotated_county2, crs = st_crs(counties_sf))


# try plotting


plot(rotated_county$geometry)

ggplot()+
  geom_sf(data = rotated_county) +
  geom_sf(data = rotated_county1) +
  geom_sf(data = rotated_county2)


#~~~~~~~~~~~~~~~~~~~~
# PLOT ROTATED/LAYERED SF
#~~~~~~~~~~~~~~~~~~~

top_county_ne <- rotated_county %>%
  filter(state != "WA") %>%
  slice_max(avg_emp_lvl, n = 3, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",round(avg_emp_lvl,1)),
    centroid = st_centroid(geometry)
  )

top_county_ne$lat <- unlist(map(top_county_ne$centroid,1))
top_county_ne$long <- unlist(map(top_county_ne$centroid,2))

top_county_wa <- rotated_county %>%
  filter(state == "WA") %>%
  slice_max(avg_emp_lvl, n = 1, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",avg_emp_lvl),
    centroid = st_centroid(geometry)
  )

top_county_wa$lat <- unlist(map(top_county_wa$centroid,1))
top_county_wa$long <- unlist(map(top_county_wa$centroid,2))



# top total taxable wages per employment

top_county_ne1 <- rotated_county1 %>%
  filter(state != "WA") %>%
  slice_max(total_taxable_wages_per_avg_emplyt_lvl, n = 3, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",round(total_taxable_wages_per_avg_emplyt_lvl,1)),
    centroid = st_centroid(geometry)
  )

top_county_ne1$lat <- unlist(map(top_county_ne1$centroid,1))
top_county_ne1$long <- unlist(map(top_county_ne1$centroid,2))

top_county_wa1 <- rotated_county1 %>%
  filter(state == "WA") %>%
  slice_max(total_taxable_wages_per_avg_emplyt_lvl, n = 1, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",round(total_taxable_wages_per_avg_emplyt_lvl,1)),
    centroid = st_centroid(geometry)
  )

top_county_wa1$lat <- unlist(map(top_county_wa1$centroid,1))
top_county_wa1$long <- unlist(map(top_county_wa1$centroid,2))



# top total taxable wages per establishment

top_county_ne2 <- rotated_county2 %>%
  filter(state != "WA") %>%
  slice_max(total_taxable_wages_per_estb, n = 3, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",round(total_taxable_wages_per_estb,1)),
    centroid = st_centroid(geometry)
  )

top_county_ne2$lat <- unlist(map(top_county_ne2$centroid,1))
top_county_ne2$long <- unlist(map(top_county_ne2$centroid,2))

top_county_wa2 <- rotated_county2 %>%
  filter(state == "WA") %>%
  slice_max(total_taxable_wages_per_estb, n = 1, with_ties = FALSE) %>%
  mutate(
    label_emp_level = paste0(county,", ",round(total_taxable_wages_per_estb,1)),
    centroid = st_centroid(geometry)
  )

top_county_wa2$lat <- unlist(map(top_county_wa2$centroid,1))
top_county_wa2$long <- unlist(map(top_county_wa2$centroid,2))





emp_level <- ggplot() +
  # EMPLOYMENT LEVELS
  geom_sf(data = rotated_county, 
          aes(fill=avg_emp_lvl),
              color = "darkgray") +
  scale_fill_continuous(
  low = "azure", 
  high = "darkgreen",
  na.value = "lightgray",
  name = str_wrap("Avg. Employment Level", width = 5), 
  label = scales::comma
  ) +
  #labs(title = paste0("WA and New England Shellfish Fishing Industry (2021 - 2025)")
  #) +
  xlab(" ")+
  ylab(" ")+
  # Add points for county with high value
  geom_label_repel(
    data = top_county_ne,
    aes(x = lat, y = long, label = str_wrap(label_emp_level, width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 3,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_label_repel(
    data = top_county_wa,
    aes(x = lat, y = long, label = str_wrap(label_emp_level,width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(1, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  )  
emp_level

oty <- ggplot() +
  # taxable wages
  geom_sf(data = rotated_county, 
          aes(fill=avg_oty_emp_lvl_chg),
          color = "darkgray") +
  scale_fill_continuous(
              low = "darkred", 
              high = "lavenderblush",
              na.value = "lightgray",
              name = str_wrap("Avg. Over The Year Emp. Level Change", width = 10),
              label = scales::comma
            ) +
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(.75, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  ) 

estab <- ggplot()+
  # wages per establishment
  geom_sf(data = rotated_county, 
          aes(fill= avg_qtrly_wages_per_estab),
          color = "darkgray") +
  scale_fill_continuous(
    low = "orchid1", 
    high = "orchid4",
    na.value = "lightgray",
    name = str_wrap("Avg. Qtrly Wages per Establishment",  width = 12),
    label = scales::comma
  )+
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(.75, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  )   


# put them in the same plot
plot_grid(
  emp_level,
  oty,
  estab, 
  align = "v",
  ncol = 1,
  rel_heights = c(1,1,1),
  rel_widths = c(1,1,1)
)



# or together in ggplot
ggplot() +
  # EMPLOYMENT LEVELS
  geom_sf(data = rotated_county, 
          aes(fill=avg_emp_lvl),
          color = "darkgray") +
  scale_fill_continuous(
    low = "azure", 
    high = "darkgreen",
    na.value = "lightgray",
    name = str_wrap("Avg. Employment Level", width = 5), 
    label = scales::comma
  ) +
  #labs(title = paste0("WA and New England Shellfish Fishing Industry (2021 - 2025)")
  #) +
  xlab(" ")+
  ylab(" ")+
  # Add points for county with high value
  geom_label_repel(
    data = top_county_ne,
    aes(x = lat, y = long, label = str_wrap(label_emp_level, width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 3,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_label_repel(
    data = top_county_wa,
    aes(x = lat, y = long, label = str_wrap(paste0(county,
                                                   label_emp_level,
                                                   width = 3))),
    point.padding = 1,
    segment.color = "darkred",
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  new_scale_fill()+
  geom_sf(data = rotated_county1, 
          aes(fill= total_taxable_wages_per_avg_emplyt_lvl),
          color = "darkgray") +
  scale_fill_continuous(
    low = "orchid1", 
    high = "orchid4",
    na.value = "lightgray",
    name = str_wrap("Total Taxable Wages per Employment",  width = 12),
    label = scales::comma
  )+
  # Add points for county with high value
  geom_label_repel(
    data = top_county_ne1,
    aes(x = lat, y = long, label = str_wrap(label_emp_level, width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 3,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_label_repel(
    data = top_county_wa1,
    aes(x = lat, y = long, label = str_wrap(label_emp_level,width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  
  new_scale_fill()+
  geom_sf(data = rotated_county2, 
          aes(fill=total_taxable_wages_per_estb),
          color = "darkgray") +
  scale_fill_continuous(
    high = "darkblue", 
    low = "skyblue",
    na.value = "lightgray",
    name = str_wrap("Taxable wages per establishment", width = 10),
    label = scales::comma
  ) +
  
  # Add points for county with high value
  geom_label_repel(
    data = top_county_ne2,
    aes(x = lat, y = long, label = str_wrap(label_emp_level, width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 3,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_label_repel(
    data = top_county_wa2,
    aes(x = lat, y = long, label = str_wrap(label_emp_level,width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(.75, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  ) 


## different years summary --------------------------------------------



  







# Get USA map data
usa_map <- map_data("state")



# Example city coordinates (lat/long)
cities <- data.frame(
  name = c("New York", "Los Angeles", "Chicago", "Houston", "Miami"),
  lon = c(-74.006, -118.2437, -87.6298, -95.3698, -80.1918),
  lat = c(40.7128, 34.0522, 41.8781, 29.7604, 25.7617)
)

# Plot map with labels using ggrepel
ggplot() +
  # Draw the map
  geom_polygon(
    data = usa_map,
    aes(x = long, y = lat, group = group),
    fill = "white", color = "black"
  ) +
  # Add points for cities
  geom_point(
    data = cities,
    aes(x = lon, y = lat),
    color = "red", size = 3
  ) +
  # Add non-overlapping labels
  geom_text_repel(
    data = cities,
    aes(x = lon, y = lat, label = name),
    size = 4,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey50"
  ) +
  coord_fixed(1.3) +  # Keep aspect ratio
  theme_minimal() +
  labs(title = "Major US Cities with Non-Overlapping Labels")

















# ocean boundary map --------------------

# filter for northeast and wa
ne <- county_p %>%
  filter(state %in% c("MA","RI","NH","ME")) 
wa <- county_p %>%
  filter(state == "WA")

library(rnaturalearth)

#  Load global coastline from Natural Earth
coastline <- ne_download(scale = 50, type = "coastline", category = "physical", returnclass = "sf") %>%
  st_transform(4326)

# 3. Identify counties that touch the coastline
# st_intersects returns a list; we check if any intersection exists
counties <- counties %>%
  mutate(on_coast = lengths(st_intersects(geometry, coastline)) > 0)

# 4. View results
table(counties$on_coast)

### raster for sst ----------------------

# load the sst raster
sst <- rast(paste0(dir$shp,"MA_Essex County_2021_100km4k.tif"))


# make the new sf
ma_sf <- map_county_emp_sf %>%
  filter(state %in% c("MA","RI","CT","NH")) 

# for WA you have to include canada
canada_sf <- ne_states(
  country = "Canada",
  returnclass = "sf"
)


wa_emp_sf <- map_county_emp_sf %>%
  filter(state %in% c("WA","OR"))

canada_sf <- st_transform(canada_sf, st_crs(wa_emp_sf))

combined <- dplyr::bind_rows(canada_sf, wa_emp_sf)
wa_vector <- st_union(combined)


# paste them together
plot(sst)
plot(st_geometry(ne_emp_sf), add = TRUE, border = "black", lwd = 0.5)
plot(st_geometry(wa_emp_sf), add = TRUE, border = "black", lwd = 0.5)


# create land mask from counties
county_mask <- vect(ne_emp_sf)

sst_ocean <- mask(sst, county_mask, inverse = TRUE)

plot(sst_ocean)
plot(st_geometry(ne_emp_sf), add = TRUE, border="black")
plot(st_geometry(wa_emp_sf), add = TRUE, border="black")





# now only look at two counties, bristol and pacific county

# bristol county buffer covers the entire coast of MA
b_county_sst1 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/MA_Bristol County_2023_100km4k.tif"))
b_county_sst2 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/MA_Bristol County_2022_100km4k.tif"))
b_county_sst3 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/MA_Bristol County_2021_100km4k.tif"))



stacked <- c(b_county_sst1, b_county_sst2, b_county_sst3)

b_county_sst <- app(stacked, mean, na.rm = TRUE)


# all of WA
wa1 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/WA_2023_summer_sst.tif"))
wa2 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/WA_2022_summer_sst.tif"))
wa3 <- rast(paste0(dir$shp,"rasters_100km_buffer_WA_MA_4km_scale/WA_2021_summer_sst.tif"))


stacked <- c(wa1, wa2, wa3)

p_county_sst <- app(stacked, mean, na.rm = TRUE)
plot(p_county_sst)

# mask the land data
ma_county_mask <- vect(ma_sf)
wa_county_mask <- vect(wa_vector)

# identify only the ocean
crs(b_county_sst) <- crs(ma_county_mask) # match the crs
crs(p_county_sst) <- crs(wa_county_mask)


b_ocean_sst <- mask(b_county_sst, ma_county_mask, inverse = TRUE)
p_ocean_sst <- mask(p_county_sst, wa_county_mask, inverse = TRUE)


# Convert raster to data frame for ggplot
b_sst_df <- as.data.frame(b_ocean_sst, xy = TRUE, na.rm = TRUE) %>%
  rename(value = layer)  # Rename column for clarity

p_sst_df <- as.data.frame(p_ocean_sst, xy = TRUE, na.rm = TRUE) %>%
  rename(value = layer)  # Rename column for clarity

# remove new hampshire
ma_sf <- map_county_emp_sf %>%
  filter(state %in% c("MA")) 

# remove oregon
wa_emp_sf <- map_county_emp_sf %>%
  filter(state %in% c("WA"))

# bristol county sf
#ma_sf <- ne_emp_sf %>% filter(state %in% c("MA","RI"))
# use the points for bristol county
b_point <- ma_sf %>% filter(state == "MA" & county == "Bristol County") %>%
  mutate(
    label_emp_level = paste0(county,", ",round(total_taxable_wages_per_estb,1)),
    centroid = st_centroid(geometry)
  )
b_point$lat <- unlist(map(b_point$centroid,1))
b_point$long <- unlist(map(b_point$centroid,2))



# plot massachussetts
ma_plot <- ggplot() +
  geom_raster(
    data = b_sst_df,
    aes(x = x, y = y, fill = value)
  ) +
  scale_fill_viridis_c(name = "Avg.\nSST (°C)") +
  
  xlab(" ")+ # remove x and y axis titles
  ylab(" ")+
  new_scale_fill() +
  
  geom_sf(
    data = ma_sf,
    aes(fill = avg_emp_lvl),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_gradient(
    low = "white",
    high = "orchid4",
    na.value = "lightgray",
    name = str_wrap("Avg. Emp. Level",
                    width = 3),
    labels = scales::comma
  ) +
  # Add points for county with high value
  geom_label_repel(
    data = b_point,
    aes(x = lat, 
        y = long, 
        label = str_wrap(
          paste0(county,", ",
                 comma(round(avg_emp_lvl,0))),
                  width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 1,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(.75, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  ) 
ma_plot
# save the plot
# png(filename = paste0(dir$fig,"ma_sst_taxable_wages.png"),
#     width =  650, height = 500, res = 300)
# ma_plot
# dev.off()


# WA point
# use the points for bristol county
p_point <- wa_emp_sf %>% filter(state == "WA" & county == "Pacific County") %>%
  mutate(
    centroid = st_centroid(geometry)
  )
p_point$lat <- unlist(map(p_point$centroid,1))
p_point$long <- unlist(map(p_point$centroid,2))




# WA plot

wa_plot <- ggplot() +
  geom_raster(
    data = p_sst_df,
    aes(x = x, y = y, fill = value)
  ) +
  scale_fill_viridis_c(name = "Avg.\nSST (°C)") +
  
  xlab(" ")+ # remove x and y axis titles
  ylab(" ")+
  new_scale_fill() +
  
  geom_sf(
    data = wa_emp_sf,
    aes(fill = avg_emp_lvl),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_gradient(
    low = "white",
    high = "orchid4",
    na.value = "lightgray",
    name = str_wrap("Avg. Emp. Level",
                    width = 3),
    labels = scales::comma
  ) +
  # Add points for county with high value
  geom_label_repel(
    data = p_point,
    aes(x = lat, 
        y = long, 
        label = str_wrap(
          paste0(county,", ",
                 comma(round(avg_emp_lvl,0))),
          width = 3)),
    point.padding = 1,
    segment.color = "darkred",
    nudge_y = 1,
    box.padding = 1,
    size = 2,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    
    title = element_text(size = 12), # adjust the position of title
    legend.title = element_text(size = 7), # size of legend titlte
    legend.text = element_text(size = 5), # adjust legend text
    legend.key.size = unit(.75, "lines"), # Size of legend keys (symbols/boxes)
    legend.position = "right",          # moves the legend to the right
    panel.background = element_blank(), # removes background 
    panel.border = element_blank(),     # removes box around plot
    panel.grid.major = element_blank(), # removes major grid lines
    panel.grid.minor = element_blank(), # removes minor grid lines
    axis.text = element_blank(),        # removes axis text
    axis.ticks = element_blank()        # removes axis ticks
    
  ) 
wa_plot

# save the plot
# png(filename = paste0(dir$fig,"wa_sst_taxable_wages.png"),
#     width =  650, height = 500, res = 300)
# ma_plot
# dev.off()


# make a parplot
library(gridExtra)
grid.arrange(wa_plot, ma_plot, ncol = 2)


