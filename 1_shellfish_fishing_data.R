# Shellfish Fishing Data
# June 8, 2026
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
          "tidycensus","tigris")

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

# save the df_full for county level info on shellfish
write.csv(df_full,
          paste0(dir$processed,"shellfish_",num_id,"_2015_2025_wages_employment_data.csv"),
          row.names = F)


# transform full data and save by levels of aggregation  -------------------------------------------------

# read in df_full (so you dont have to do the loop again)
df_full <- read.csv(paste0(dir$processed,"shellfish_114112_2015_2025_wages_employment_data.csv"))


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


# save 
write.csv(df_locality,
          paste0(dir$processed,"shellfish_income_wages_2015_2025_localities.csv"),
          row.names = F)

write.csv(df_nation,
          paste0(dir$processed,"shellfish_income_wages_2015_2025_nationwide.csv"),
          row.names = F)

# Employment summaries ---------------------------------------------- 


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



#check <- read.csv( paste0(dir$processed,"shellfish_employment_data_monthly_county_2015_2025.csv"))


# wages summaries-------------------------------------------------




# make the counties data
df_counties <- read.csv(paste0(dir$processed,"shellfish_income_wages_2015_2025_counties.csv"))

top_5_counties_wages <- df_counties %>%
group_by(county) %>%
  summarise(
    avg_total_wages_p_est = mean(total_qtrly_wages_per_establishment, na.rm = T) 
  ) %>%
  arrange(desc(avg_total_wages_p_est)) %>%
  slice_head(n = 5)


# Queries ---------------------------------


# how do wages change over time?

wage_change <- df_counties %>% group_by(state, year) %>%
  summarise(
    
    avg_wage = mean(avg_wkly_wage, na.rm = T)
    
  )



# filter for states that do not see any employment all year
emp_per_state <- county_emp %>%
  group_by(state) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  )

top_5_state <- county_emp %>%
  group_by(state) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  ) %>%
  arrange(desc(total_emplvl)) %>%
  slice_head(n = 5)

no_emp_per_state <- emp_per_state %>% filter(total_emplvl == 0)



# what about counties? Top 5 counties

top_5_counties <- monthly_employment %>%
  group_by(area_fips) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T)
  ) %>%
  arrange(desc(total_emplvl)) %>%
  slice_head(n = 5)



# summarize the employment
df_plot <- monthly_employment %>%
  filter(state %in% top_5_state$state) %>%
  group_by(year,month, state) %>%
  summarise(
    total_emplvl = sum(employment_level, na.rm = T) 
  ) %>%
  mutate(
    date = as.Date(paste(year,month,"01", sep = "-"))
  )







# PLOTs -----------------------------------------
# plot state monthly employment
ggplot(data = df_plot ,#%>% filter(county != "Bristol County"),
       aes(x = date,
           y = total_emplvl,
           colour = state)) +
  geom_line(size = 1) +
  xlab("Date") +
  ylab("Total Employment") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# wages per establishment

# summarize
df_plot <- df_counties %>%
  filter(county %in% top_5_counties_wages$county)


# wages per establishment
ggplot(data = df_plot ,#%>% filter(county != "Bristol County"),
       aes(x = year_qtr,
           y = total_qtrly_wages_per_establishment,
           colour = county)) +
  geom_line(size = 1) +
  xlab("Date") +
  ylab("Total Employment") +
  #scale_x_date(date_labels = "%b %d, %Y", date_breaks = "3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





