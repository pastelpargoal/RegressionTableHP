rm(list=ls())
graphics.off()

setwd("C:\\Users\\pargo\\OneDrive\\Documents\\ResearchFall23")
##########################################################

library(dplyr)
library(tigris)
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)
library(RColorBrewer)
library(mapdata)
library(ggmap)
library(usmap)
library(rio)
library(viridis)
library(mapproj)
library(msm) # SP - for deltamethod()
library(stargazer)
library(MASS)
##########################################################


CTj= read.csv("CTj.csv")
CTj = CTj %>% filter(TOWN != "Connecticut")
CTHPAll = CTj$Heat_Pumps.2017. + CTj$Heat_Pumps.2018. + CTj$Heat_Pumps.2019. + CTj$Heat_Pumps.2020. + CTj$Heat_Pumps.2021. + CTj$Heat_Pumps.2022. + CTj$Heat_Pumps.2023.

###### SMALLEST AIC ######

ll = as.numeric(CTj$Housing.Units)
model1 = glm(CTHPAll ~  as.numeric(`Electricity`)+ offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model1, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
residuals(model1)

model2 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model2, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model3 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model3, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model4 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model4, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model5 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`)  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model5, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)


model6 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model6, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model7 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model7, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model8 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model8, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model9 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model9, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients)*10

model10 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model10, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

#left here - 5/13
model11 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`)  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model11, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model12 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model12, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)


model13 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model14, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)

model14 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + `Median.Home.Value` + offset(log(ll)) , family = "poisson", data = CTj)
#final_model = step(model14, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)

#############################
##    POISSON REGRESSION   ##
#############################

# Unscaled version

models = list(
  glm(CTHPAll ~ as.numeric(`Electricity`) + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`)  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`)  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + `Median.Home.Value` + offset(log(ll)) , family = "poisson", data = CTj)
)

######## Scaling coefficients first, then exponentiating

###### Scale coefficients then exponentiate, with same p values and new standard errors



# Function to transform coefficients
transform_coef_for_tbl <- function(model) {
  coefs <- coef(model)
  std_errs <- summary(model)$coefficients[, "Std. Error"]
  p_vals <- summary(model)$coefficients[, "Pr(>|z|)"] # assign p-values before transformation
  
  # Selectively apply transformations (multiply by 10)
  coefs["White"] <- coefs["White"]*10
  coefs["Tenure_Owner_Occupied..."] <- coefs["Tenure_Owner_Occupied..."]*10
  coefs[as.numeric("Fuel_oil_kerosene_etc.")] <- coefs[as.numeric("Fuel_oil_kerosene_etc.")]*10
  coefs[as.numeric("Electricity")] <- coefs[as.numeric("Electricity")]*10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  coefs["Median.Home.Value"] <- coefs["Median.Home.Value"]*100000
  
  std_errs["White"] <- std_errs["White"] * 10
  std_errs["Tenure_Owner_Occupied..."] <- std_errs["Tenure_Owner_Occupied..."]*10
  std_errs[as.numeric("Electricity")] <- std_errs[as.numeric("Electricity")] * 10
  std_errs[as.numeric("Fuel_oil_kerosene_etc.")] <- std_errs[as.numeric("Fuel_oil_kerosene_etc.")] * 10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  std_errs["Median.Home.Value"] <-  std_errs["Median.Home.Value"]*10
  
  coefs <- exp(coefs)
  std_errs <- exp(coefs)*std_errs # delta method for computing standard error of IRR
  return(list(coef = coefs, std_err = std_errs, p = p_vals))
}

# Apply the transformation function to each model
transformed_models_for_tbl <- lapply(models, transform_coef_for_tbl)

# Extract transformed coefficients and standard errors
transformed_coefs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$coef)
transformed_std_errs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$std_err)
p_vals_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$p)

stargazer(models,
          coef = transformed_coefs_for_tbl, 
          se = transformed_std_errs_for_tbl,
          p = p_vals_for_tbl,
          type = "text",
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_P_Regression_Rate_Ratios.txt",
          align = TRUE,
          dep.var.labels = "Heat Pump Adoption",
          covariate.labels = c("Electricity",
                               "White",
                               "Hispanic or Latino",
                               "Owner",
                               "Fuel Oil Kerosene",
                               "Unemployment Rate",
                               "Black or African American",
                               "Utility Gas",
                               "SNAP",
                               "Poverty",
                               "Bottled Tank or LP Gas",
                               "Median Household Income", 
                               # "Some Other Race", 
                               "Asian", 
                               "Median Home Value"),
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)

###########################
###       Binomial       ##
###########################


models = list(
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + offset(log(ll)) , data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`)  + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`  + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + offset(log(ll)) , data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients` + offset(log(ll)) , data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...` + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`)  + offset(log(ll)) , data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + offset(log(ll)) ,  data = CTj),
  glm.nb(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + `Median.Home.Value` + offset(log(ll)) , data = CTj)
)

# Function to transform coefficients
transform_coef_for_tbl <- function(model) {
  coefs <- coef(model)
  std_errs <- summary(model)$coefficients[, "Std. Error"]
  p_vals <- summary(model)$coefficients[, "Pr(>|z|)"] # assign p-values before transformation
  
  # Selectively apply transformations (multiply by 10)
  coefs["White"] <- coefs["White"]*10
  coefs["Tenure_Owner_Occupied..."] <- coefs["Tenure_Owner_Occupied..."]*10
  coefs[as.numeric("Fuel_oil_kerosene_etc.")] <- coefs[as.numeric("Fuel_oil_kerosene_etc.")]*10
  coefs[as.numeric("Electricity")] <- coefs[as.numeric("Electricity")]*10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  coefs["Median.Home.Value"] <- coefs["Median.Home.Value"]*100000
  
  std_errs["White"] <- std_errs["White"] * 10
  std_errs["Tenure_Owner_Occupied..."] <- std_errs["Tenure_Owner_Occupied..."]*10
  std_errs[as.numeric("Electricity")] <- std_errs[as.numeric("Electricity")] * 10
  std_errs[as.numeric("Fuel_oil_kerosene_etc.")] <- std_errs[as.numeric("Fuel_oil_kerosene_etc.")] * 10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  std_errs["Median.Home.Value"] <-  std_errs["Median.Home.Value"]*10
  
  coefs <- exp(coefs)
  std_errs <- exp(coefs)*std_errs # delta method for computing standard error of IRR
  return(list(coef = coefs, std_err = std_errs, p = p_vals))
}

# Apply the transformation function to each model
transformed_models_for_tbl <- lapply(models, transform_coef_for_tbl)

# Extract transformed coefficients and standard errors
transformed_coefs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$coef)
transformed_std_errs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$std_err)
p_vals_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$p)

stargazer(models,
          coef = transformed_coefs_for_tbl, 
          se = transformed_std_errs_for_tbl,
          p = p_vals_for_tbl,
          type = "text",
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_NB_Regression.txt",
          align = TRUE,
          dep.var.labels = "Heat Pump Adoption",
          covariate.labels = c("Electricity",
                               "White",
                               "Hispanic or Latino",
                               "Owner",
                               "Fuel Oil Kerosene",
                               "Unemployment Rate",
                               "Black or African American",
                               "Utility Gas",
                               "SNAP",
                               "Poverty",
                               "Bottled Tank or LP Gas",
                               "Median Household Income", 
                               # "Some Other Race", 
                               "Asian", 
                               "Median Home Value"),
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)


##################################
###         MORAN's I       #####
#################################
# Moran I test
# residuals(model)
#install.packages("spdep")
#install.packages("lmtest")
#install.packages("sf")
#install.packages("sp")
library(spdep)
library(sp)
library(lmtest)
library(sf)

shapefile = st_read( "C:/Users/pargo/OneDrive/Documents/ResearchFall23/Town_Polygon.shp")

# erasing the water area 
CT_towns <- shapefile %>%
  st_transform(point, crs = 3857) %>%
  erase_water(area_threshold = 0.20 )

CTj_ = CTj %>% dplyr::select(-TOWN_NO, - COUNTY)
CT_townsU = left_join(CT_towns , CTj_, by = "TOWN")

# I think the error occurred in this code, I'm trying to condense it
CT_townsU_area <- CT_townsU %>%
  # Group by TOWN_NO
  group_by(TOWN_NO) %>%
  # Summarize attributes while keeping geometry
  summarise(
    total_area = sum(SHAPE_Area, na.rm = TRUE),
    total_length = sum(SHAPE_Leng, na.rm = TRUE),
    total_acreage = sum(ACREAGE, na.rm = TRUE),
    total_area_sqmi = sum(AREA_SQMI, na.rm = TRUE),
    geometry = st_union(geometry)  # Combine geometries using union
  )

CT_townsU_area <- CT_townsU_area %>% filter(TOWN_NO != "0")

CT = left_join(CTj, CT_townsU_area, by = "TOWN_NO")

# Residuals from the full model
model14 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied...` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment.Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty...`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Asian` + `Median.Home.Value` + offset(log(ll)) , family = "poisson", data = CT)
residuals_model = residuals(model14)

# Creating coordinates 
centroids <- st_centroid(CT_townsU_area) # or this centroids <- st_centroid(CT$geometry)
coords <- st_coordinates(centroids) # Replace with your actual coordinate columns
coords <- as.data.frame(coords,  na.rm= TRUE )


knn <- knearneigh(coords, k = 4)

# Convert to neighbors list
nb <- knn2nb(knn)

# Create the weights list
listw <- nb2listw(nb)

# This code doesn't work because the lengths don't match. I think I made an error when making changes to 
#the spatial dataset, but I'm not sure how to fix it
moran_result <- moran.test(residuals_model, listw)

