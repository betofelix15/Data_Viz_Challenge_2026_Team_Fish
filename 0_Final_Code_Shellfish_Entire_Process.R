# June 25, 2026
# Author: Jesus Felix, Mumtahinah Zia, Logan Haviland


# notes: QCEW is the quarterly Census Employmnet and Wages. We use this to find
# employment and wages for fishing and acuaculture data

# This code is runnable from anywhere, you don't need to download the data,
# It's all here baby!
# If you want to save files, just add where you want to save them to
# use alt+o to collapse the sections
# alt+l to collapse the section you are currently in

# Pre-code preparations --------------------------------------------------------------------
# this line cleans the environment, free up memory.
rm(list = ls())

# load libraries
# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "gganimate",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata", "leaflet","fontawesome","dygraphs","shiny","xts","plotly",
          "ggpattern","UpSetR","colorBlindness","data.table","mapview","usmap","DT",
          "tidycensus","tigris","spData","spDataLarge","ggspatial","r5r","stars",
          "raster","geobr","gtfs2gps","osmdata","h3jsr","viridisLite","ggnewscale",
          "magrittr","lwgeom","cowplot","ggrepel","xts")

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

# other stuff ~~~
# how to make not in
`%not_in%` <- purrr::negate(`%in%`)

# remove scientific notation
options(scipen = 999)


# working directory --------------------------------------------------------------
# If you want to add a aworking directory, this would be a good way to do it, 
# a set of file paths.
dir <- list()

# This line uses getwd() to find where your .Rproj file is.
getwd()

# This line takes this location as the "root" directory for the project. The below is in case you have a different folder for codes
dir$root <- str_remove(getwd(),"/R codes")  # because the codes are in another file, you have to add it

# Observe the output in the console:
dir$root 

# shapefiles
dir.create(paste0(dir$root, "/shapefiles/"))
dir$shp <- paste0(dir$root, "/shapefiles/")

# figures directory (these line create a new folder for you to export figures)
dir.create(paste0(dir$root, "/figures/"))
dir$fig <- paste0(dir$root, "/figures/")


# unprocessed data folder (this line creates a new folder for "raw" data)
dir.create(paste0(dir$root, "/data/raw data/"))
dir$rawdata <- paste0(dir$root, "/data/raw data/")

# processed data folder (this line creates a new folder for "clean" data)
dir.create(paste0(dir$root, "/data/processed data/"))
dir$processed <- paste0(dir$root, "/data/processed data/")

# multiplot function -----

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Extract data from API at the county level---------------------------------------------------

# check the qcew_rscript_example script for other functions
# qcewGetIndustryData : This function takes a year, quarter, and industry code
# and returns an array containing the associated industry data. Use 'a' for 
# annual averages. Some industry codes contain hyphens. The CSV files use
# underscores instead of hyphens. So 31-33 becomes 31_33. 
# For all industry codes and titles see:
# http://data.bls.gov/cew/doc/titles/industry/industry_titles.htm

qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# identify the industry data: we want shellfish fishing
# load the data, do it in a way that it can be looped potentially
num_id <- "114112" # Shellfish fishing is 114112
q <- 1
year <- 2025

df <- qcewGetIndustryData(year,q,num_id )

# we also need some identifiers for the area_fip codes, use tidycensus package
data(fips_codes)
fips_codes

# use the same format in df
fips_codes$area_fips <- paste0(fips_codes$state_code,fips_codes$county_code)

# join with df
df_counties <- left_join(df,fips_codes, by = "area_fips") %>%
  filter(!is.na(county))

# now do a loop for 2020 to 2025, I found that 1990 to 2010 does not work
df_full <- NULL

for(year in 2015:2025){
  
  df_y <- NULL # create a null df_y
  
  for(q in 1:4){ # loop for each quarter
    
    # print the q
    print(paste0(year,": Quarter ",q))
    
    df_q <- qcewGetIndustryData(year,q,num_id ) # load the data for that quarter
    
    # save
    df_y <- rbind(df_y,df_q)
  } 
  
  # save the four quarters for that year
  df_full <- rbind(df_full,df_y)
}

# change area_fips to fips (easily recognizable by software)
df_full <- rename(df_full, 
                  "fips" = 1)


# while we are here, we can also load the shapefiles
# Download county shapefile for all states
counties_sf <- counties(cb = TRUE, year = 2023)  # cb=TRUE gives simplified geometry

# we also need census data, so get an API
# here you can request: https://api.census.gov/data/key_signup.html
# census_api_key("YOUR_API_KEY_HERE", install = TRUE)
# readRenviron("~/.Renviron")  # Reload environment so key is available




# transform full data and save by levels of aggregation  -------------------------------------------------

# percentage of taxable wages
df_full$pcnt_qtrly_taxable_wages <- df_full$taxable_qtrly_wages/df_full$total_qtrly_wages

# total wages per count of establishments
df_full$total_qtrly_wages_per_establishment <- df_full$total_qtrly_wages/df_full$qtrly_estabs


# create a date fpr quarters
df_full$year_qtr <- as.yearqtr(paste0(df_full$year," Q",df_full$qtr))


# the annual data has the area titles which is useful to identify boroughs and parishes
num_id <- "114112"
d_title <- "Shellfish fishing"

df <- read.csv(paste0(dir$rawdata,"2025.q1-q4 ",num_id," NAICS ",num_id," ",d_title,".csv"))

# identify the area title and fip
area_df <- df %>%
  select(area_fips, area_title) %>%
  distinct()

# add the area_df to df_full to get any names
df_full <- left_join(df_full, area_df, by = c("fips" = "area_fips"))


# save it 
write.csv(df_full,
          paste0(dir$processed,"shellfish_114112_2015_2025_wages_employment_data_clean.csv"),
          row.names = F)


# we also need some identifiers for the area_fip codes, use tidycensus package
data(fips_codes)
fips_codes

# use the same format in df
fips_codes$area_fips <- paste0(fips_codes$state_code,fips_codes$county_code)


# counties ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# join with df
df_counties <- left_join(df_full,fips_codes, by = c("fips" = "area_fips") )%>%
  filter(!is.na(county))

# save the df_counties
write.csv(df_counties,
          paste0(dir$processed,"shellfish_income_wages_2015_2025_counties.csv"),
          row.names = F)


# states ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if it contains -- Statewide -> it is a state
df_state <- df_full %>%
  filter(str_detect(area_title, "Statewide")) %>%
  mutate(
    
    state = sub(" --.*", "",area_title)  # extract the state name
  )

# add the state abbreviations
df_state$state_abb <- state.abb[match(df_state$state, state.name)]

# save the df_state
write.csv(df_state,
          paste0(dir$processed,"shellfish_income_wages_2015_2025_states.csv"),
          row.names = F)


# cities and parishes ~~~~~~~~~~~~~~~~~~~~~~

# find the fips that are not in df_state and df_counties
df_na <- df_full  %>%
  filter(fips %not_in% c(df_counties$fips,df_state$fips))

# identify the localities (cities and parishes)
df_locality <- df_na %>%
  filter(!is.na(area_title))  %>% # these have an area title
  filter(!str_detect(area_title,"Unknown"))   # we dont want unknowns

# split the area name to when the comma, then make a locality and state column
df_locality <- df_locality %>%
  separate(area_title, into = c("locality","state_abb"), sep = ",") %>%
  mutate(
    state_abb = str_remove(state_abb," MSA") # Remove what I believe means Metropolitan area?
  )


# remove the us total
df_nation <- df_locality %>% filter(locality == "U.S. TOTAL")
df_locality <- df_locality %>% filter(locality != "U.S. TOTAL")

# verify that there are no NA's
nrow(df_locality[is.na(df_locality$locality),])


## transform employment data ------------------------------

# make a monthly dataset from employment level
monthly_employment <- df_full %>%
  select(year,qtr,fips,month1_emplvl,month2_emplvl,month3_emplvl) %>%
  group_by(fips,year,qtr) %>%
  pivot_longer(
    cols = c("month1_emplvl","month2_emplvl","month3_emplvl"),
    names_to = "month_emplvl",
    values_to = "employment_level"
  ) %>%
  ungroup()

monthly_employment <- monthly_employment %>%
  mutate(month = case_when(
    
    qtr == 1 & month_emplvl == "month1_emplvl" ~ 1,
    qtr == 1 & month_emplvl == "month2_emplvl" ~ 2,
    qtr == 1 & month_emplvl == "month3_emplvl" ~ 3,
    qtr == 2 & month_emplvl == "month1_emplvl" ~ 4,
    qtr == 2 & month_emplvl == "month2_emplvl" ~ 5,
    qtr == 2 & month_emplvl == "month3_emplvl" ~ 6,
    qtr == 3 & month_emplvl == "month1_emplvl" ~ 7,
    qtr == 3 & month_emplvl == "month2_emplvl" ~ 8,
    qtr == 3 & month_emplvl == "month3_emplvl" ~ 9,
    qtr == 4 & month_emplvl == "month1_emplvl" ~ 10,
    qtr == 4 & month_emplvl == "month2_emplvl" ~ 11,
    qtr == 4 & month_emplvl == "month3_emplvl" ~ 12
  )
  )



# make a monthly dataset from employment level location quotient (coefficient for concentration of employment at a national level)
lq_emplvl_monthly <- df_full %>%
  select(year,qtr,fips,lq_month1_emplvl,lq_month2_emplvl,lq_month3_emplvl) %>%
  group_by(fips,year,qtr) %>%
  pivot_longer(
    cols = c("lq_month1_emplvl","lq_month2_emplvl","lq_month3_emplvl"),
    names_to = "lq_month_emplvl",
    values_to = "lq_employment_level"
  ) %>%
  ungroup()

lq_emplvl_monthly <- lq_emplvl_monthly %>%
  mutate(month = case_when(
    
    qtr == 1 & lq_month_emplvl == "lq_month1_emplvl" ~ 1,
    qtr == 1 & lq_month_emplvl == "lq_month2_emplvl" ~ 2,
    qtr == 1 & lq_month_emplvl == "lq_month3_emplvl" ~ 3,
    qtr == 2 & lq_month_emplvl == "lq_month1_emplvl" ~ 4,
    qtr == 2 & lq_month_emplvl == "lq_month2_emplvl" ~ 5,
    qtr == 2 & lq_month_emplvl == "lq_month3_emplvl" ~ 6,
    qtr == 3 & lq_month_emplvl == "lq_month1_emplvl" ~ 7,
    qtr == 3 & lq_month_emplvl == "lq_month2_emplvl" ~ 8,
    qtr == 3 & lq_month_emplvl == "lq_month3_emplvl" ~ 9,
    qtr == 4 & lq_month_emplvl == "lq_month1_emplvl" ~ 10,
    qtr == 4 & lq_month_emplvl == "lq_month2_emplvl" ~ 11,
    qtr == 4 & lq_month_emplvl == "lq_month3_emplvl" ~ 12
  )
  )

# make a monthly dataset from over the year (oty) change in employment (emp_lvl_chg)
oty_emplvl_monthly_chg <- df_full %>%
  select(year,qtr,fips,oty_month1_emplvl_chg,oty_month2_emplvl_chg,oty_month3_emplvl_chg) %>%
  group_by(fips,year,qtr) %>%
  pivot_longer(
    cols = c("oty_month1_emplvl_chg","oty_month2_emplvl_chg","oty_month3_emplvl_chg"),
    names_to = "oty_month_emplvl_chg",
    values_to = "oty_employment_level_chg"
  ) %>%
  ungroup()

oty_emplvl_monthly_chg <- oty_emplvl_monthly_chg %>%
  mutate(month = case_when(
    
    qtr == 1 & oty_month_emplvl_chg == "oty_month1_emplvl_chg" ~ 1,
    qtr == 1 & oty_month_emplvl_chg == "oty_month2_emplvl_chg" ~ 2,
    qtr == 1 & oty_month_emplvl_chg == "oty_month3_emplvl_chg" ~ 3,
    qtr == 2 & oty_month_emplvl_chg == "oty_month1_emplvl_chg" ~ 4,
    qtr == 2 & oty_month_emplvl_chg == "oty_month2_emplvl_chg" ~ 5,
    qtr == 2 & oty_month_emplvl_chg == "oty_month3_emplvl_chg" ~ 6,
    qtr == 3 & oty_month_emplvl_chg == "oty_month1_emplvl_chg" ~ 7,
    qtr == 3 & oty_month_emplvl_chg == "oty_month2_emplvl_chg" ~ 8,
    qtr == 3 & oty_month_emplvl_chg == "oty_month3_emplvl_chg" ~ 9,
    qtr == 4 & oty_month_emplvl_chg == "oty_month1_emplvl_chg" ~ 10,
    qtr == 4 & oty_month_emplvl_chg == "oty_month2_emplvl_chg" ~ 11,
    qtr == 4 & oty_month_emplvl_chg == "oty_month3_emplvl_chg" ~ 12
  )
  )






# make a monthly dataset from over the year percent change in employment (oty)
oty_emplvl_monthly_pct_chg <- df_full %>%
  select(year,qtr,fips,oty_month1_emplvl_pct_chg,oty_month2_emplvl_pct_chg,oty_month3_emplvl_pct_chg) %>%
  group_by(fips,year,qtr) %>%
  pivot_longer(
    cols = c("oty_month1_emplvl_pct_chg","oty_month2_emplvl_pct_chg","oty_month3_emplvl_pct_chg"),
    names_to = "oty_month_emplvl_pct_chg",
    values_to = "oty_employment_level_pct_chg"
  ) %>%
  ungroup()

oty_emplvl_monthly_pct_chg <- oty_emplvl_monthly_pct_chg %>%
  mutate(month = case_when(
    
    qtr == 1 & oty_month_emplvl_pct_chg == "oty_month1_emplvl_pct_chg" ~ 1,
    qtr == 1 & oty_month_emplvl_pct_chg == "oty_month2_emplvl_pct_chg" ~ 2,
    qtr == 1 & oty_month_emplvl_pct_chg == "oty_month3_emplvl_pct_chg" ~ 3,
    qtr == 2 & oty_month_emplvl_pct_chg == "oty_month1_emplvl_pct_chg" ~ 4,
    qtr == 2 & oty_month_emplvl_pct_chg == "oty_month2_emplvl_pct_chg" ~ 5,
    qtr == 2 & oty_month_emplvl_pct_chg == "oty_month3_emplvl_pct_chg" ~ 6,
    qtr == 3 & oty_month_emplvl_pct_chg == "oty_month1_emplvl_pct_chg" ~ 7,
    qtr == 3 & oty_month_emplvl_pct_chg == "oty_month2_emplvl_pct_chg" ~ 8,
    qtr == 3 & oty_month_emplvl_pct_chg == "oty_month3_emplvl_pct_chg" ~ 9,
    qtr == 4 & oty_month_emplvl_pct_chg == "oty_month1_emplvl_pct_chg" ~ 10,
    qtr == 4 & oty_month_emplvl_pct_chg == "oty_month2_emplvl_pct_chg" ~ 11,
    qtr == 4 & oty_month_emplvl_pct_chg == "oty_month3_emplvl_pct_chg" ~ 12
  )
  )


# join the employment level annual data

# Put all data frames into a list
dfs <- list(monthly_employment, lq_emplvl_monthly, oty_emplvl_monthly_chg, oty_emplvl_monthly_pct_chg)

# Perform multiple left joins by 'id'
df_empl_lvl <- reduce(dfs, function(x, y) {
  left_join(x, y, by = c("year","qtr","month","fips"))
})

# remove unnecessary columns
df_empl_lvl <- df_empl_lvl %>%
  select(-month_emplvl,-oty_month_emplvl_chg,-oty_month_emplvl_pct_chg,-lq_month_emplvl)


# add the geographic aggregation
county_fips <- df_counties %>% 
  select(fips, state,state_code,state_name,county_code,county) %>%
  distinct()

state_fips <- df_state %>%
  select(fips, state,state_abb,area_title) %>%
  distinct()

locality_fips <- df_locality %>%
  select(fips,locality,state_abb) %>%
  distinct()


# join with employment 
emp_county <- df_empl_lvl %>%   
  left_join(county_fips, by = "fips") %>%
  filter(!is.na(county))


emp_state <- df_empl_lvl %>%
  left_join(state_fips, by = "fips") %>%
  filter(!is.na(state))

emp_locality <- df_empl_lvl %>%
  left_join(locality_fips, by = "fips") %>%
  filter(!is.na(locality))

# save the data sets
write.csv(emp_county,
          paste0(dir$processed,"shellfish_employment_data_monthly_county_2015_2025.csv"),
          row.names = F)


write.csv(emp_state,
          paste0(dir$processed,"shellfish_employment_data_monthly_state_2015_2025.csv"),
          row.names = F)


write.csv(emp_locality,
          paste0(dir$processed,"shellfish_employment_data_monthly_locality_2015_2025.csv"),
          row.names = F)

# County Mapping  ------------------------------------------------------------------

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


st_write(county_polygons,
         paste0(dir$shp,"shellfish_income_wages_2015_2025_counties.gpkg"),
         delete_dsn = TRUE)

# read back to confirm
check <- st_read( paste0(dir$shp,"shellfish_income_wages_2015_2025_counties.shp"))



# Summaries for Maps -----------------------------------------------------

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

# check to see where it lands
# flat 
plot(map_emp_sf$geometry)


# plot an index (to verify it worked)
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




# Layered Plot ----------------------------------------------------------

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

# verify that you can layer
ggplot()+
  geom_sf(data = rotated_county) +
  geom_sf(data = rotated_county1) +
  geom_sf(data = rotated_county2)


#~~~~~~~~~~~~~~~~~~~~
# PLOT ROTATED/LAYERED SF
#~~~~~~~~~~~~~~~~~~~


# find the top counties for the first layer
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



# top total taxable wages per employment (second layer)

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



# top total taxable wages per establishment (third layer)

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


# Plot all three using ggplot() + geom_sf()
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
    aes(x = lat, y = long, label = str_wrap(label_emp_level,width = 3)),
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

### MA and WA comparison only -------------------------------------

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



### supporting graph ------------------------------

# load the sst data
sst_df <- read.csv(paste0(dir$processed,"county_ma_wa_monthly_sst_2021_2025_100km_buffer.csv"))

# load the employment data
emp_df <- read.csv(paste0(dir$processed,"shellfish_employment_data_monthly_county_2015_2025.csv"))


# filter the sst data to include state averages and bristol/pacific counties
sst_state_avg <- sst_df %>%
  filter(state == c("MA","WA")) %>%
  group_by(state,year,month) %>%
  summarise(
    avg_sst = mean(avg_sst, na.rm = T) # save the average sst
  ) %>%
  mutate(
    avg_sst = avg_sst*0.01, # convert to celsius with scale,
    
    state = case_when(    # rename the state variables
      state == "MA" ~ "MA Avg.",
      state == "WA" ~ "WA Avg."
    )
    
  ) %>%
  rename(
    "area" = state
  )
  

sst_county_avg <- sst_df %>%
  filter(county == c("Bristol County","Pacific County") & 
           state == c("MA","WA")) %>%  # there is a bristol county in Rhode Island, so make sure it takes the MA
  group_by(county,year,month) %>%
  summarise(
    avg_sst = mean(avg_sst, na.rm = T) # save the average sst
  ) %>%
  mutate(
    avg_sst = avg_sst*0.01 # convert to celsius with scale
  ) %>%
  rename(
    "area" = county
  )


# combine
sst_avg <- rbind(sst_county_avg,sst_state_avg)

# change date formate
sst_avg$date <- ymd(paste(sst_avg$year, sst_avg$month, "01"))
sst_avg$year_month <- format(sst_avg$date, "%B %Y")


# summarize Employment

# filter the sst data to include state averages and bristol/pacific counties
emp_state_avg <- emp_df %>%
  filter(state == c("MA","WA")) %>% 
  filter(year %in% c(2021:2025)) %>%
  group_by(state,year,month) %>%
  summarise(
    emp_level = mean(employment_level, na.rm = T) # save the average sst
  ) %>%
  mutate(
    state = case_when(    # rename the state variables
      state == "MA" ~ "MA Avg.",
      state == "WA" ~ "WA Avg."
    )
    
  ) %>%
  rename(
    "area" = state
  )


emp_county_avg <- emp_df %>%
  filter(year %in% c(2021:2025)) %>%
  filter(county == c("Bristol County","Pacific County") & 
           state == c("MA","WA")) %>%  # there is a bristol county in Rhode Island, so make sure it takes the MA
  group_by(county,year,month) %>%
  summarise(
    emp_level = employment_level
  ) %>%
  rename(
    "area" = county
  )


# combine
emp_avg <- rbind(emp_state_avg,emp_county_avg)

# change date formate
emp_avg$date <- ymd(paste(emp_avg$year, emp_avg$month, "01"))
emp_avg$year_month <- format(emp_avg$date, "%B %Y")


# rate of decrease or increase
e1 <- emp_avg[emp_avg$area == "Pacific County" & emp_avg$year_month == "February 2021",]$emp_level 
e2 <- emp_avg[emp_avg$area == "Pacific County" & emp_avg$year_month == "November 2025",]$emp_level 

p_rate <- (e2-e1)/e1
p_label <- paste0(round(p_rate*100,0),"% \nEmployment")

e1 <- emp_avg[emp_avg$area == "Bristol County" & emp_avg$year_month == "March 2021",]$emp_level 
e2 <- emp_avg[emp_avg$area == "Bristol County" & emp_avg$year_month == "December 2025",]$emp_level 

b_rate <- (e2-e1)/e1
b_label <- paste0(round(b_rate*100,0),"% \nEmployment")


# plot
p1 <- ggplot(data = sst_avg,
       aes(
         x = date,
         y = avg_sst,
         linetype = area,
         colour = area
       )) +
  geom_line(
    size = 1
  ) +
  scale_linetype_manual(values = c("solid", "dotted","dotdash","longdash")) +
  xlab("Date") +
  ylab("Avg. SST") +
  theme_bw()

p1




p2 <- ggplot(data = emp_avg %>% filter(area == c("MA Avg.",
                                                "Bristol County")),
             aes(
               x = date,
               y = emp_level,
               linetype = area,
               colour = area
             )) +
  geom_line(
    size = 1
  ) +
  scale_linetype_manual(values = c("solid", "dotted","dotdash","longdash")) +
  xlab("Date") +
  ylab("Employment Level") +
  annotate(
    "label",
    x = as.Date("2025-01-01"),
    y = 675,
    label = b_label,
    fill = "orangered",   # background color
    color = "white",       # text color
    label.r = unit(0.15, "lines"),  # rounded corners
    size = 4
  ) +
  theme_bw()
p2

p3 <- ggplot(data = emp_avg %>% filter(area == c("WA Avg.",
                                                 "Pacific County")),
             aes(
               x = date,
               y = emp_level,
               colour = area,
               linetype = area,
             )) +
  geom_line(
    size = 1
  ) +
  scale_linetype_manual(values = c("solid", "dotted","dotdash","longdash")) +
  xlab("Date") +
  ylab("Employment Level") +
  annotate(
    "label",
    x = as.Date("2025-10-01"),
    y = 150,
    label = p_label,
    fill = "orangered",   # background color
    color = "white",        # text color
    label.r = unit(0.15, "lines"),  # rounded corners
    size = 4
  ) +
  theme_bw()
p3

multiplot(p1, p2,p3, cols=1)
#> `geom_smooth()` using method = 'loess'

