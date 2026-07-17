# Sand box code

# this line cleans the environment, free up memory.
rm(list = ls())
# base libraries ----------------------------------------------------------------
# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "gganimate",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata", "leaflet","fontawesome","dygraphs","shiny","xts","plotly",
          "ggpattern","UpSetR","colorBlindness","data.table","mapview","usmap","DT")

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


# 2025.q1-q4 1125 NAICS 1125 Aquaculture -----------------------------------------

# I want to see how this data looks like, can I map it? 
# I want to play with it (Jesus)

# load the data, do it in a way that it can be looped potentially
num_id <- "114112"
d_title <- "Shellfish fishing"

df <- read.csv(paste0(dir$rawdata,"2025.q1-q4 ",num_id," NAICS ",num_id," ",d_title,".csv"))

# (optional) load the data description file
field_desc <- readxl::read_excel(paste0(dir$rawdata,"field_layouts_variable_descriptions_2025_q1-q4_NAICS_QCEW.xlsx"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PROCESSING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I noticed that the states and counties are joined in a single column. I HATE THAT!

df_clean <- df %>%
  separate(area_title, into = c("county_or_city","state"), sep = ",") # there are counties and cities, separated by the state by a comma, but also statewide aggregations

# states are sometimes abbreviated, needs standardizing
unique(df_clean$state)

# we can separate cities and counties to have cleaner sets
df_counties <- df_clean %>%
  filter(str_detect(county_or_city,"County")) %>% # this selects the strings with county
  rename(county = county_or_city) %>%
  filter(!str_detect(county,"-")) # remove the ones with a hyphen, its actually a city

unique(df_counties$state) # there's a space before the state name
df_counties <- df_counties %>%
  mutate(state = str_remove(state," "))

unique(df_counties$state)
length(unique(df_counties$state)) # only 46 states

# make a dataset for cities, actually it also contains parishes and other non-county aggregations
df_cities <- df_clean %>% 
  filter(!is.na(state) & area_fips %not_in% df_counties$area_fips)


# we see the quarters in columns, but also the months within that quarter....
# can we turn it into monthly data? do like a pivot
# there are monthly values for employment, wages, and percent changes

  
df_temp <- df_clean %>% 
    pivot_longer(
      cols = c("month1_emplvl","month2_emplvl","month3_emplvl"),
      names_to = "month",
      values_to = "employment_level"
    )

# do some more as time needs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# summarize by locality/county, state
df_sum <- df_clean %>%
  group_by(county_or_city,state,qtr,area_fips) %>%
  summarise(
    
    
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mapping
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import libraries
packages <- c("ggplot2", "sf","DT","knitr","data.table","mapview","usmap")

lapply(packages, library, character.only = TRUE)

# join the fips from df_clean to usmap to plot (using ggplot)
# summarize to whatever value you are interested in
df_map <- df_counties %>%
  filter(!is.na(state)) %>% # removes the statewide aggregations
  group_by(area_fips,industry_title,year) %>%
  summarise(
    annual_avg_wkly_wage = mean(avg_wkly_wage,na.rm = T),
    annual_total_wages = sum(total_qtrly_wages, na.rm = T),
    annual_estabs_count = sum(qtrly_estabs_count, na.rm = T),
    annual_emplvl = sum(month1_emplvl,month2_emplvl,month3_emplvl, na.rm = T)
  )  %>%
  rename(fips = area_fips)


# plot a map of the annual_emplvl

emp_northeast<-plot_usmap(data = df_map, values= "annual_emplvl",
           include = c("MA","RI","NH","ME")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )

wages_northeast<-plot_usmap(data = df_map, values= "annual_avg_wkly_wage",
                          include = c("MA","RI","NH","ME")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Avg. Weekly Wage", 
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Avg. Weekly Wages")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )

# 2025.q1-q4 42446 NAICS 42446 Fish and seafood wholesalers -----------------------------------------

# I want to see how this data looks like, can I map it? 
# I want to play with it (Jesus)

# load the data, do it in a way that it can be looped potentially
num_id <- "42446"
d_title <- "Fish and seafood merchant wholesalers"

df <- read.csv(paste0(dir$rawdata,"2025.q1-q4 ",num_id," NAICS ",num_id," ",d_title,".csv"))

# (optional) load the data description file
field_desc <- readxl::read_excel(paste0(dir$rawdata,"field_layouts_variable_descriptions_2025_q1-q4_NAICS_QCEW.xlsx"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PROCESSING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I noticed that the states and counties are joined in a single column. I HATE THAT!

df_clean <- df %>%
  separate(area_title, into = c("county_or_city","state"), sep = ",") # there are counties and cities, separated by the state by a comma, but also statewide aggregations

# states are sometimes abbreviated, needs standardizing
unique(df_clean$state)

# we can separate cities and counties to have cleaner sets
df_counties <- df_clean %>%
  filter(str_detect(county_or_city,"County")) %>% # this selects the strings with county
  rename(county = county_or_city) %>%
  filter(!str_detect(county,"-")) # remove the ones with a hyphen, its actually a city

unique(df_counties$state) # there's a space before the state name
df_counties <- df_counties %>%
  mutate(state = str_remove(state," "))

unique(df_counties$state)
length(unique(df_counties$state)) # only 46 states

# make a dataset for cities, actually it also contains parishes and other non-county aggregations
df_cities <- df_clean %>% 
  filter(!is.na(state) & area_fips %not_in% df_counties$area_fips)


# we see the quarters in columns, but also the months within that quarter....
# can we turn it into monthly data? do like a pivot
# there are monthly values for employment, wages, and percent changes


df_temp <- df_clean %>% 
  pivot_longer(
    cols = c("month1_emplvl","month2_emplvl","month3_emplvl"),
    names_to = "month",
    values_to = "employment_level"
  )

# do some more as time needs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# summarize by locality/county, state
df_sum <- df_clean %>%
  group_by(county_or_city,state,qtr,area_fips) %>%
  summarise(
    
    
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mapping
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import libraries
packages <- c("usmap", "data.table","DT")

lapply(packages, library, character.only = TRUE)

# join the fips from df_clean to usmap to plot (using ggplot)
# summarize to whatever value you are interested in
df_map <- df_counties %>%
  filter(!is.na(state)) %>% # removes the statewide aggregations
  group_by(area_fips,state,industry_title,year) %>%
  summarise(
    annual_avg_wkly_wage = mean(avg_wkly_wage,na.rm = T),
    annual_total_wages = sum(total_qtrly_wages, na.rm = T),
    annual_estabs_count = sum(qtrly_estabs_count, na.rm = T),
    annual_emplvl = sum(month1_emplvl,month2_emplvl,month3_emplvl, na.rm = T)
  )  %>%
  rename(fips = area_fips)


# plot a map of the annual_emplvl

plot_usmap(data = df_map, values= "annual_emplvl") +
  scale_fill_continuous(
    low = "aquamarine", high = "darkgreen",  name = "Employment Level", label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.background = NULL
  )

# focus on western US
plot_usmap(data = df_map, 
           values= "annual_emplvl",
           include = c("CA", "OR", "WA"),
           color = "darkgreen") +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen", 
    name = "Employment Level", 
    na.value = "white",
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )


# cities and parishes

df_map <- df_cities %>%
  filter(!is.na(state)) %>% # removes the statewide aggregations
  group_by(area_fips,state,industry_title,year) %>%
  summarise(
    annual_avg_wkly_wage = mean(avg_wkly_wage,na.rm = T),
    annual_total_wages = sum(total_qtrly_wages, na.rm = T),
    annual_estabs_count = sum(qtrly_estabs_count, na.rm = T),
    annual_emplvl = sum(month1_emplvl,month2_emplvl,month3_emplvl, na.rm = T)
  )  %>%
  rename(fips = area_fips)


plot_usmap(data = df_map, 
           values= "annual_emplvl",
           #include = c("CA", "OR", "WA"),
           color = "darkgreen") +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen", 
    name = "Employment Level", 
    na.value = "white",
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )


# 114112 NAICS 114112 Shellfish fishing -----------------------------------------

# I want to see how this data looks like, can I map it? 
# I want to play with it (Jesus)

# load the data, do it in a way that it can be looped potentially
num_id <- "114112"
d_title <- "Shellfish fishing"

df <- read.csv(paste0(dir$rawdata,"2025.q1-q4 ",num_id," NAICS ",num_id," ",d_title,".csv"))

# (optional) load the data description file
field_desc <- readxl::read_excel(paste0(dir$rawdata,"field_layouts_variable_descriptions_2025_q1-q4_NAICS_QCEW.xlsx"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PROCESSING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I noticed that the states and counties are joined in a single column. I HATE THAT!

df_clean <- df %>%
  separate(area_title, into = c("county_or_city","state"), sep = ",") # there are counties and cities, separated by the state by a comma, but also statewide aggregations

# states are sometimes abbreviated, needs standardizing
unique(df_clean$state)

# we can separate cities and counties to have cleaner sets
df_counties <- df_clean %>%
  filter(str_detect(county_or_city,"County")) %>% # this selects the strings with county
  rename(county = county_or_city) %>%
  filter(!str_detect(county,"-")) # remove the ones with a hyphen, its actually a city

unique(df_counties$state) # there's a space before the state name
df_counties <- df_counties %>%
  mutate(state = str_remove(state," "))

unique(df_counties$state)
length(unique(df_counties$state)) # only 46 states

# make a dataset for cities, actually it also contains parishes and other non-county aggregations
df_cities <- df_clean %>% 
  filter(!is.na(state) & area_fips %not_in% df_counties$area_fips)


# we see the quarters in columns, but also the months within that quarter....

# employment level has multiple months per quarter, so we could do a monthly perspective
# can we turn it into monthly data? do like a pivot
# there are monthly values for employment, lq_employment, and percent changes


monthly_employment <- df_counties %>%
  select(year,qtr,area_fips,state,county,month1_emplvl,month2_emplvl,month3_emplvl) %>%
  group_by(area_fips,year,qtr) %>%
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


# do some more as time needs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# summarize by locality/county, state
df_sum <- df_clean %>%
  group_by(county_or_city,state,qtr,area_fips) %>%
  summarise(
    
    
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mapping
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import libraries
packages <- c("usmap", "data.table","DT")

lapply(packages, library, character.only = TRUE)

# join the fips from df_clean to usmap to plot (using ggplot)
# summarize to whatever value you are interested in
df_map <- df_counties %>%
  filter(!is.na(state)) %>% # removes the statewide aggregations
  group_by(area_fips,state,industry_title,year) %>%
  summarise(
    annual_avg_wkly_wage = mean(avg_wkly_wage,na.rm = T),
    annual_total_wages = sum(total_qtrly_wages, na.rm = T),
    annual_estabs_count = sum(qtrly_estabs_count, na.rm = T),
    annual_emplvl = sum(month1_emplvl,month2_emplvl,month3_emplvl, na.rm = T)
  )  %>%
  rename(fips = area_fips)


# plot a map of the annual_emplvl

plot_usmap(data = df_map, values= "annual_emplvl") +
  scale_fill_continuous(
    low = "aquamarine", high = "darkgreen",  name = "Employment Level", label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.background = NULL
  )

# Northeast
plot_usmap(data = df_map, 
           values= "annual_emplvl",
           include = c("CA", "OR", "WA"),
           color = "darkgreen") +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen", 
    name = "Employment Level", 
    na.value = "white",
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )


# cities and parishes

df_map <- df_cities %>%
  filter(!is.na(state)) %>% # removes the statewide aggregations
  group_by(area_fips,state,industry_title,year) %>%
  summarise(
    annual_avg_wkly_wage = mean(avg_wkly_wage,na.rm = T),
    annual_total_wages = sum(total_qtrly_wages, na.rm = T),
    annual_estabs_count = sum(qtrly_estabs_count, na.rm = T),
    annual_emplvl = sum(month1_emplvl,month2_emplvl,month3_emplvl, na.rm = T)
  )  %>%
  rename(fips = area_fips)

# focus on the northeast employment
emp_northeast<-plot_usmap(data = df_map, values= "annual_emplvl",
                          include = c("MA","RI","NH","ME")) +
  scale_fill_continuous(
    low = "aquamarine", 
    high = "darkgreen",
    na.value = "white",
    name = "Employment Level", 
    label = scales::comma
  ) +
  labs(title = paste0("2025 ",d_title, " Employment")
  ) +
  theme(
    title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right",
    
  )

emp_northeast


# Try the seasonality of employment in monthly shellfish catch

# add regions
monthly_employment <- monthly_employment %>%
  mutate(region = case_when (
    
    state %in% c("Connecticut", 'Maine', 'Massachusetts', "New Hampshire", "Rhode Island", "Vermont") ~ "New England",
    state %in% c('New Jersey', 'New York', 'Pennsylvania') ~ "Mid-Atlantic",
    state %in% c( 'Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin') ~ 'East North Central',
    state %in% c('Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'South Dakota') ~ "West North Central",
                  
    state %in% c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 'West Virginia', 'Washington D.C.') ~ "South Atlantic",
    state %in% c('Alabama', 'Kentucky', 'Mississippi', 'Tennessee') ~ 'East South Central',
    state %in% c('Arkansas', 'Louisiana', 'Oklahoma', 'Texas') ~ "West South Central",
    state %in% c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Utah', 'Wyoming') ~ "Mountain West",
    state %in% c( 'Alaska', 'California', 'Hawaii', 'Oregon', 'Washington') ~ 'Pacific West'
    
  ))

# line plot for employment by region
df_plot <- monthly_employment %>%
  group_by(year,month,region) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T)
  )

# plot it
ggplot(data = df_plot,
       aes(x = month,
           y = total_emplvl,
           colour = region)) +
  geom_line()



# filter for area_fip codes that do not see any employment all year
emp_per_fip <- monthly_employment %>%
  group_by(area_fips) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  )

no_emp_fip <- emp_per_fip %>% filter(total_emplvl == 0)


# filter for states that do not see any employment all year
emp_per_state <- monthly_employment %>%
  group_by(state) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  )

no_emp_per_state <- emp_per_state %>% filter(total_emplvl == 0)

# plot the states with employment
df_plot <- monthly_employment %>%
  filter(state %not_in% no_emp_per_state$state) %>%
  group_by(year,month, state) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  )

# plot state monthly employment
ggplot(data = df_plot,
       aes(x = month,
           y = total_emplvl,
           colour = state)) +
  geom_line()


# remove these for a different dataframe






# Food sales ---------------------------------------

# load the onfarmmarket
fm <- read_excel(paste0(dir$rawdata,"onfarmmarket.xlsx"))

# what are the production methods?
unique(fm$specialproductionmethods)

fish_list <- unique(fm$specialproductionmethods)[
  str_detect(unique(fm$specialproductionmethods), "fish")
]

aquaculture_list <- unique(fm$specialproductionmethods)[
  str_detect(unique(fm$specialproductionmethods), "aquaculture")
]


# Restaurants and Food Services ---------------------------------------

# load the onfarmmarket
fm <- read_excel(paste0(dir$rawdata,"RestaurantsAndFoodServices.xlsx"),
                 sheet = 2)

# what are the production methods?
unique(fm$Name)

fish_list <- unique(fm$Name)[
  str_detect(unique(fm$Name), "fish")
]


seafood_list <- unique(fm$Name)[
  str_detect(unique(fm$Name), "seafood|Seafood")
]

aquaculture_list <- unique(fm$Name)[
  str_detect(unique(fm$Name), "aquaculture")
]

mariscos_list <- unique(fm$Name)[
  str_detect(unique(fm$Name), "Mariscos|marisco|mariscos")
]


