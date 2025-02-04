################################################################################
#                                READ ME                                       #
################################################################################

# This is the code to set up the dataset to run the forward stepwise regression.
# I used tidycensus package to download the dataset directly from Census.
# I also obtained CT heat pump data - Energize CT - Heat Pump.
################################################################################

rm(list=ls())
graphics.off()

# Change it to your directory 
setwd("/Users/user/Documents/HeatPump")

##########################################################

library(dplyr)
library(tidyverse)
library(tidycensus)

# Add your Census API key (if necessary)
#census_api_key("", install = TRUE)
##################

# Obtaining the variables I want to obtain - codes belong to census
variables <- c(
  median_income = "B19013_001",
  median_home_value = "B25077_001",
  civilian_labor_force = "B23025_003",
  median_home_age = "B25035_001",
  owner_occupied = "DP04_0046P",
  total_housing_units = "B25003_001",
  total_population = "B02001_001",
  poverty_population = "S1701_C03_001",
  snap_recipients = "S2201_C04_001E",
  unemployment_rate = "DP03_0020PE",
  utility_gas = "B25040_002",
  electricity = "B25040_003",
  fuel_oil_kerosene = "B25040_004",
  bottled_tank_LP_gas = "B25040_005",
  white_population = "B02001_002",
  black_population = "B02001_003",
  asian_population = "B02001_005",
  hispanic_population = "B03002_012"
)

# Get ACS data - the 2015 AGS 5 year average - 2011 to 2015
ct_town_data <- get_acs(
  geography = "county subdivision", 
  state = "CT", 
  variables = variables, 
  year = 2015, 
  survey = "acs5"
)

# Convert to wide format
ct_town_data_wide <- ct_town_data %>%
  dplyr::select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Calculate values
# Have to manually calculate them since the codes I pulled from, don't match with the actual census values
ct_town_data_wide <- ct_town_data_wide %>%
  dplyr::mutate(
    utility_gas_percentage = (utility_gas / total_housing_units) * 100,
    electricity_percentage = (electricity / total_housing_units) * 100,
    fuel_oil_kerosene_percentage = (fuel_oil_kerosene / total_housing_units) * 100,
    bottled_tank_LP_gas_percentage = (bottled_tank_LP_gas / total_housing_units) * 100,
    white_percentage = (white_population / total_population) * 100,
    black_percentage = (black_population / total_population) * 100,
    asian_percentage = (asian_population / total_population) * 100,
    hispanic_percentage = (hispanic_population / total_population) * 100,
    median_home_age = 2015 - median_home_age
  )

# Deleting the extra words attached to the town names
#Removing the long name of rows
ct_town_data_wide$NAME <- gsub(" town, Naugatuck Valley Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Capitol Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Greater Bridgeport Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Lower Connecticut River Valley Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Northeastern Connecticut Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Northwest Hills Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, South Central Connecticut Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Southeastern Connecticut Planning Region, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Western Connecticut Planning Region, Connecticut", "", ct_town_data_wide$NAME)

#Removing the long name of rows
ct_town_data_wide$NAME <- gsub(" town, Fairfield County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Hartford County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Litchfield County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Middlesex County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, New Haven County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, New London County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Tolland County, Connecticut", "", ct_town_data_wide$NAME)
ct_town_data_wide$NAME <- gsub(" town, Windham County, Connecticut", "", ct_town_data_wide$NAME)
#ct_town_data_wide$NAME <- gsub(" town, Western Connecticut Planning Region, Connecticut", "", ct_town_data_wide$NAME)

row_remove = c("County subdivisions not defined, Fairfield County, Connecticut",
               "County subdivisions not defined, Middlesex County, Connecticut",
               "County subdivisions not defined, New Haven County, Connecticut",
               "County subdivisions not defined, New London County, Connecticut") #,


CT_u = ct_town_data_wide %>% filter(!NAME %in% row_remove)
# Changing the column name
colnames(CT_u)[2] = "TOWN"

# Uploading the heat pump data 
HPU = read.csv("WS_Town_Cross.csv")
# Subsetting and changing the column names
HPU = dplyr::select(HPU,"X","X2017.3" ,"X2018.3", "X2019.3", "X2020.3","X2021.4" ,"X2022.4","X2023.4")
HPU = HPU[-c(1,2),]
# Changing the column names
colnames(HPU)[1] = "TOWN"
colnames(HPU)[2] = "Heat_Pumps.2017."
colnames(HPU)[3] = "Heat_Pumps.2018."
colnames(HPU)[4] = "Heat_Pumps.2019."
colnames(HPU)[5] = "Heat_Pumps.2020."
colnames(HPU)[6] = "Heat_Pumps.2021."
colnames(HPU)[7] = "Heat_Pumps.2022."
colnames(HPU)[8] = "Heat_Pumps.2023."

# Joining the census data with heat pump data 
CTj = CT_u %>% left_join( HPU, "TOWN")
# Ensuring the heat pump values are numbers and creating an aggregated variable
CTHPAll = as.numeric(CTj$Heat_Pumps.2017.) + as.numeric(CTj$Heat_Pumps.2018.) + as.numeric(CTj$Heat_Pumps.2019.) + as.numeric(CTj$Heat_Pumps.2020.) + as.numeric(CTj$Heat_Pumps.2021.) + as.numeric(CTj$Heat_Pumps.2022.) + as.numeric(CTj$Heat_Pumps.2023.)
colnames(CTj)[5] = "Housing.Units"

# Calculating heat pump adoption 
CTHP17 = CTj$Heat_Pumps.2017.
CTHP18 = CTj$Heat_Pumps.2018.
CTHP19 = CTj$Heat_Pumps.2019.
CTHP20 = CTj$Heat_Pumps.2020. 
CTHP21 = CTj$Heat_Pumps.2021.
CTHP22 = CTj$Heat_Pumps.2022.
CTHP23 = CTj$Heat_Pumps.2023.
CTHU = CTj$Housing.Units
CTHU = as.numeric(CTHU)
CTHP17 = as.numeric(CTHP17)
CTHP18 = as.numeric(CTHP18)
CTHP19 = as.numeric(CTHP19)
CTHP20 = as.numeric(CTHP20)
CTHP21 = as.numeric(CTHP21)
CTHP22 = as.numeric(CTHP22)
CTHP23 = as.numeric(CTHP23)

CTAdopt17 = CTHP17/CTHU
CTAdopt18 = CTHP18/CTHU
CTAdopt19 = CTHP19/CTHU
CTAdopt20 = CTHP20/CTHU
CTAdopt21 = CTHP21/CTHU
CTAdopt22 = CTHP22/CTHU
CTAdopt23 = CTHP23/CTHU

# Adding the adoption values to the main table
CTj = cbind(CTj, CTAdopt17)
CTj = cbind(CTj, CTAdopt18)
CTj = cbind(CTj, CTAdopt19)
CTj = cbind(CTj, CTAdopt20)
CTj = cbind(CTj, CTAdopt21)
CTj = cbind(CTj, CTAdopt22)
CTj = cbind(CTj, CTAdopt23)


# Selecting the variables I want
CTj = CTj %>% dplyr::select(GEOID, TOWN, DP03_0020P, poverty_population, S2201_C04_001, owner_occupied,
                            white_percentage, black_percentage, asian_percentage, hispanic_percentage, 
                            median_income, utility_gas_percentage, electricity_percentage, fuel_oil_kerosene_percentage,
                            bottled_tank_LP_gas_percentage, median_home_age , total_housing_units, median_home_value, Heat_Pumps.2017., Heat_Pumps.2018.,
                            Heat_Pumps.2019., Heat_Pumps.2020., Heat_Pumps.2021., Heat_Pumps.2022., Heat_Pumps.2023., 
                            CTAdopt17, CTAdopt18, CTAdopt19, CTAdopt20, CTAdopt21, CTAdopt22, CTAdopt23, median_home_age)

# Renaming the variables so they can match with the models 
# Don't change the names of these values because models all have these exact names 
# and it will be hard to change them all of them
# CTj - for reference, the j doesn't mean anything
CTj <- CTj %>%
  rename(
    "White" = "white_percentage",
    "Hispanic_or_Latino" = "hispanic_percentage",
    "Tenure_Owner_Occupied..." = "owner_occupied",
    "Unemployment.Rate" = "DP03_0020P",
    "Black_or_African_American" = "black_percentage",
    "SNAP_Recipients" = "S2201_C04_001",
    "Poverty..." = "poverty_population",
    "Median_Household_Income" = "median_income",
    "Asian" = "asian_percentage",
    "Median.Home.Value" = "median_home_value",
   "Median.Home.Age" = "median_home_age",
    "Electricity" = "electricity_percentage",
    "Fuel_oil_kerosene_etc." = "fuel_oil_kerosene_percentage",
  "Utility_gas" = "utility_gas_percentage",
  "Bottled_tank_or_LP_gas" = "bottled_tank_LP_gas_percentage",
 "Housing.Units" = "total_housing_units"
  )

# Saving the dataset as a .csv, you will use this csv for the next script
file_path <- "CTj_Jan25.csv"
# Save the dataset as a CSV file
write.csv(CTj, file = file_path, row.names = FALSE) 


