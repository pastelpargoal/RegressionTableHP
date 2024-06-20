rm(list=ls())
graphics.off()
#setwd("C:\\Users\\pargo\\Downloads\\ResearchFall23")
setwd("C:\\Users\\pargo\\OneDrive\\Documents\\ResearchFall23")
# C:\Users\pargo\OneDrive\Documents\ResearchFall23

##########################################################

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("sf")
install.packages("RColorBrewer")
install.packages("mapdata")
install.packages("ggmap")
install.packages("usmap")
install.packages("rio")
install.packages("viridis")
install.packages
install.packages("mapproj")
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
##########################################################

install.packages("stargazer")
library(stargazer)

CTj= read.csv("CTj.csv")
CTHPAll = CTj$Heat_Pumps.2017. + CTj$Heat_Pumps.2018. + CTj$Heat_Pumps.2019. + CTj$Heat_Pumps.2020. + CTj$Heat_Pumps.2021. + CTj$Heat_Pumps.2022. + CTj$Heat_Pumps.2023.

###### SMALLEST AIC ######

ll = as.numeric(CTj$Housing.Units)
model1 = glm(CTHPAll ~  as.numeric(`Electricity`)+ offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model1, direction = "forward")
summary(final_model)
exp(final_model$coefficients)

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

model4 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model4, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

#model5 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + offset(log(ll)) , family = "poisson", data = CTj)
#final_model = step(model4, direction = "forward")
#summary(final_model)
#exp(final_model$coefficients)
#exp(final_model$coefficients*10)

model5 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`)  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model5, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)


model6 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model6, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model7 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model7, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model8 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model8, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model9 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model9, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients)*10

model10 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model10, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

#left here - 5/13
model11 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`)  + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model11, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)

model12 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model12, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)


model13 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model13, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)

model14 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + `Asian` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model14, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)

model15 = glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + `Asian` + `Median Home Value` + offset(log(ll)) , family = "poisson", data = CTj)
final_model = step(model15, direction = "forward")
summary(final_model)
exp(final_model$coefficients)
exp(final_model$coefficients*10)
exp(final_model$coefficients*10000)

########################################
## UNSCALED VERSION ####

covariate_labels <- c("Electricity",
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
                      "Median Household Income", # Replace with actual variable names
                      "Some Other Race", # Replace with actual variable names
                      "Asian ", # Replace with actual variable names
                      "Median Home Value") # Replace with actual variable names)
models = list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14, model15)
# Never works 
stargazer(models,
          type = "text",
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_Regression_unscaled.txt",
          align = TRUE,
          dep.var.labels = "Heat Pump Adoption",
          covariate.labels = covariate_labels,
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)

models = list(
  glm(CTHPAll ~ as.numeric(`Electricity`) + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`)  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`)  + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + `Asian` + offset(log(ll)) , family = "poisson", data = CTj),
  glm(CTHPAll ~ as.numeric(`Electricity`) + `White` + `Hispanic_or_Latino` + `Tenure_Owner_Occupied(%)` + as.numeric(`Fuel_oil_kerosene_etc.`) + `Unemployment Rate`+ `Black_or_African_American`  + as.numeric(`Utility_gas`) + `SNAP_Recipients`+ `Poverty(%)`+ as.numeric(`Bottled_tank_or_LP_gas`) + `Median_Household_Income` + `Some_Other_Race` + `Asian` + `Median Home Value` + offset(log(ll)) , family = "poisson", data = CTj)
)


# Scaled version where we expotentiate and scale at the same time

transform_coef <- function(model) {
  coefs <- coef(model)
  std_errs <- summary(model)$coefficients[, "Std. Error"]
  
  # Selectively apply transformations
  coefs[as.numeric("Electricity")] <- exp(coefs[as.numeric("Electricity")])
  coefs["White"] <- exp(coefs["White"]*10)
  coefs["Hispanic_or_Latino"] <- exp(coefs["Hispanic_or_Latino"])
  coefs["Tenure_Owner_Occupied(%)"] <- exp(coefs["Tenure_Owner_Occupied(%)"])
  coefs[as.numeric("Fuel_oil_kerosene_etc.")] <- exp(coefs[as.numeric("Fuel_oil_kerosene_etc.")]*10)
  coefs["Unemployment Rate"] <- exp(coefs["Unemployment Rate"])
  coefs["Black_or_African_American"] <- exp(coefs["Black_or_African_American"]*10)
  coefs[as.numeric("Utility_gas")] <- exp(coefs[as.numeric("Utility_gas")])
  coefs["SNAP_Recipients"] <- exp(coefs["SNAP_Recipients"]*10)
  coefs["Poverty(%)"] <- exp(coefs["Poverty(%)"]*10)
  coefs[as.numeric("Bottled_tank_or_LP_gas")] <- exp(coefs[as.numeric("Bottled_tank_or_LP_gas")]*10)
  coefs["Median_Household_Income"] <- exp(coefs["Median_Household_Income"] * 10000)
  coefs["Some_Other_Race"] <- exp(coefs["Some_Other_Race"])
  coefs["Asian"] <- exp(coefs["Asian"])
  coefs["Median Home Value"] <- exp(coefs["Median Home Value"]*10)
  
  std_errs[as.numeric("Electricity")] <- exp(std_errs[as.numeric("Electricity")])
  std_errs["White"] <- exp(std_errs["White"]*10)
  std_errs["Hispanic_or_Latino"] <- exp(std_errs["Hispanic_or_Latino"])
  std_errs["Tenure_Owner_Occupied(%)"] <- exp(std_errs["Tenure_Owner_Occupied(%)"])
  std_errs[as.numeric("Fuel_oil_kerosene_etc.")] <- exp(std_errs[as.numeric("Fuel_oil_kerosene_etc.")]*10)
  std_errs["Unemployment Rate"] <- exp(std_errs["Unemployment Rate"])
  std_errs["Black_or_African_American"] <- exp(std_errs["Black_or_African_American"]*10)
  std_errs[as.numeric("Utility_gas")] <- exp(std_errs[as.numeric("Utility_gas")])
  std_errs["SNAP_Recipients"] <- exp(std_errs["SNAP_Recipients"]*10)
  std_errs["Poverty(%)"] <- exp(std_errs["Poverty(%)"]*10)
  std_errs[as.numeric("Bottled_tank_or_LP_gas")] <- exp(std_errs[as.numeric("Bottled_tank_or_LP_gas")]*10)
  std_errs["Median_Household_Income"] <- exp(std_errs["Median_Household_Income"] * 10000)
  std_errs["Some_Other_Race"] <- exp(std_errs["Some_Other_Race"])
  std_errs["Asian"] <- exp(std_errs["Asian"])
  std_errs["Median Home Value"] <- exp(std_errs["Median Home Value"]*10)
  
  return(list(coef = coefs, std_err = std_errs))
}

# Apply the transformation function to each model
transformed_models <- lapply(models, transform_coef)

# Extract transformed coefficients and standard errors
transformed_coefs <- lapply(transformed_models, function(x) x$coef)
transformed_std_errs <- lapply(transformed_models, function(x) x$std_err)

stargazer(models,
          coef = transformed_coefs, 
          se = transformed_std_errs,
          type = "text", # Use "text" for console output, "html" for HTML, "latex" for LaTeX
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_Regression_exp_scale_same_time_Organized.txt",
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
                               "Median Household Income", # Replace with actual variable names
                               "Some Other Race", # Replace with actual variable names
                               "Asian ", # Replace with actual variable names
                               "Median Home Value"), # Replace with actual variable names
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)
## New version - 5/26 - Exponentiate first then scale


# Function to transform coefficients
transform_coef <- function(model) {
  coefs <- coef(model)
  std_errs <- summary(model)$coefficients[, "Std. Error"]
  
  # Exponentiate coefficients
  coefs <- exp(coefs)
  
  # Selectively apply transformations (multiply by 10)
  coefs["White"] <- coefs["White"]*10
  coefs[as.numeric("Fuel_oil_kerosene_etc.")] <- coefs[as.numeric("Fuel_oil_kerosene_etc.")]*10
  coefs["Black_or_African_American"] <- coefs["Black_or_African_American"]*10
  coefs["SNAP_Recipients"] <- coefs["SNAP_Recipients"]*10
  coefs[as.numeric("Bottled_tank_or_LP_gas")] <- coefs[as.numeric("Bottled_tank_or_LP_gas")]*10
  coefs["Poverty(%)"] <- coefs["Poverty(%)"]*10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  coefs["Median Home Value"] <- coefs["Median Home Value"]*10
  
  std_errs <- exp(std_errs)
  std_errs["White"] <- std_errs["White"] * 10
  std_errs[as.numeric("Fuel_oil_kerosene_etc.")] <- std_errs[as.numeric("Fuel_oil_kerosene_etc.")] * 10
  std_errs["Black_or_African_American"] <- std_errs["Black_or_African_American"] * 10
  std_errs["SNAP_Recipients"] <- std_errs["SNAP_Recipients"] * 10
  std_errs[as.numeric("Bottled_tank_or_LP_gas")] <-  std_errs[as.numeric("Bottled_tank_or_LP_gas")]*10
  std_errs["Poverty(%)"] <-  std_errs["Poverty(%)"]*10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  std_errs["Median Home Value"] <-  std_errs["Median Home Value"]*10
  
  return(list(coef = coefs, std_err = std_errs))
}

# Apply the transformation function to each model
transformed_models <- lapply(models, transform_coef)

# Extract transformed coefficients and standard errors
transformed_coefs <- lapply(transformed_models, function(x) x$coef)
transformed_std_errs <- lapply(transformed_models, function(x) x$std_err)

stargazer(models,
          coef = transformed_coefs, 
          se = transformed_std_errs,
          type = "text", # Use "text" for console output, "html" for HTML, "latex" for LaTeX
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_Regression_ExpFirst.txt",
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
                               "Median Household Income", # Replace with actual variable names
                               "Some Other Race", # Replace with actual variable names
                               "Asian ", # Replace with actual variable names
                               "Median Home Value"), # Replace with actual variable names
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)

##########

#SCALE first then exp

# Function to transform coefficients
transform_coef <- function(model) {
  coefs <- coef(model)
  std_errs <- summary(model)$coefficients[, "Std. Error"]
  
  # Exponentiate coefficients
  
  
  # Selectively apply transformations (multiply by 10)
  coefs["White"] <- coefs["White"]*10
  coefs[as.numeric("Fuel_oil_kerosene_etc.")] <- coefs[as.numeric("Fuel_oil_kerosene_etc.")]*10
  coefs["Black_or_African_American"] <- coefs["Black_or_African_American"]*10
  coefs["SNAP_Recipients"] <- coefs["SNAP_Recipients"]*10
  coefs[as.numeric("Bottled_tank_or_LP_gas")] <- coefs[as.numeric("Bottled_tank_or_LP_gas")]*10
  coefs["Poverty(%)"] <- coefs["Poverty(%)"]*10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  coefs["Median Home Value"] <- coefs["Median Home Value"]*10
  
  std_errs["White"] <- std_errs["White"] * 10
  std_errs[as.numeric("Fuel_oil_kerosene_etc.")] <- std_errs[as.numeric("Fuel_oil_kerosene_etc.")] * 10
  std_errs["Black_or_African_American"] <- std_errs["Black_or_African_American"] * 10
  std_errs["SNAP_Recipients"] <- std_errs["SNAP_Recipients"] * 10
  std_errs[as.numeric("Bottled_tank_or_LP_gas")] <-  std_errs[as.numeric("Bottled_tank_or_LP_gas")]*10
  std_errs["Poverty(%)"] <-  std_errs["Poverty(%)"]*10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  std_errs["Median Home Value"] <-  std_errs["Median Home Value"]*10
  
  coefs <- exp(coefs)
  std_errs <- exp(std_errs)
  return(list(coef = coefs, std_err = std_errs))
}

# Apply the transformation function to each model
transformed_models <- lapply(models, transform_coef)

# Extract transformed coefficients and standard errors
transformed_coefs <- lapply(transformed_models, function(x) x$coef)
transformed_std_errs <- lapply(transformed_models, function(x) x$std_err)

stargazer(models,
          coef = transformed_coefs, 
          se = transformed_std_errs,
          type = "text", # Use "text" for console output, "html" for HTML, "latex" for LaTeX
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_Regression_ScaleFirst.txt",
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
                               "Median Household Income", # Replace with actual variable names
                               "Some Other Race", # Replace with actual variable names
                               "Asian ", # Replace with actual variable names
                               "Median Home Value"), # Replace with actual variable names
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)

######

