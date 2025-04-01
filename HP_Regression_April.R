#################################
# 4/1/25
# This is the most updated script with the regression. There is no spatial autocorrelation, so it won't be 
# the main analysis, but it will still be apart of the code.
# The code will almost be the same, but I am adding Moran's I analysis and moving the spatial term 
# code at the end.

######################################################################################
##                                    Read Me                                       ##
######################################################################################
# In this code, I do more data cleaning then I plot a few maps of heat pumps.
# I create a spatial term then apply it to the models (stepwise forward regression)

######################################################################################

rm(list=ls())
graphics.off()

# Change it to your own directory 
setwd("/Users/user/Documents/HeatPump")


# Loading packages
library(devtools) ; library(MRFtools); library(Matrix); library(sf); library(spatialreg)
library(here); library(tidyverse); library(sf)  # For spatial data manipulation
library(viridis); library(gam) ; library(tidycensus)
library(mgcv)    # For Generalized Additive Models
library(patchwork); library(dplyr); library(renv); library(spdep)   # For spatial dependence and weights
library(MASS)    # For negative binomial regression
library(sp); library(spatial); library(ggplot2); 
#install.packages("equatiomatic")
library(equatiomatic)
# Packages for Moran's I
library(gam); library(splines); library(foreach); library(spdep)
#install.packages('DCluster')
library(DCluster); library(boot)

##############################################
#                DATA CLEANING               #
##############################################

# This is an older dataset - I am just adding this dataset since the new dataset 
# doesn't have the column - TOWN_NO
CTjj= read.csv( "CTj.csv")
CTj = read.csv( "CTj_Jan25.csv") #the dataset that was created in the other script

# Joining these two datasets
CTjj = CTjj %>% dplyr::select("TOWN_NO", "TOWN")
CTj = left_join(CTj, CTjj, by = "TOWN")

# Replacing the NAs with zeros and removing three towns (the three towns mainly have NAs)
# I confirmed this with Daniel
CTj <- CTj %>%
  mutate(Heat_Pumps.2017. = ifelse(is.na(Heat_Pumps.2017.), 0, Heat_Pumps.2017.),
         Heat_Pumps.2018. = ifelse(is.na(Heat_Pumps.2018.), 0, Heat_Pumps.2018.),
         Heat_Pumps.2019. = ifelse(is.na(Heat_Pumps.2019.), 0, Heat_Pumps.2019.),
         Heat_Pumps.2020. = ifelse(is.na(Heat_Pumps.2020.), 0, Heat_Pumps.2020.),
         Heat_Pumps.2021. = ifelse(is.na(Heat_Pumps.2021.), 0, Heat_Pumps.2021.),
         Heat_Pumps.2022. = ifelse(is.na(Heat_Pumps.2022.), 0, Heat_Pumps.2022.),
         Heat_Pumps.2023. = ifelse(is.na(Heat_Pumps.2023.), 0, Heat_Pumps.2023.),)
# Remove towns with all NAs - Confirmed this with Daniel
CTj = CTj %>% filter (!TOWN %in% c("Norwich", "Bozrah", "Wallingford"))

# Creating an aggregated variable of heat pumps
CTHPAll = as.numeric(CTj$Heat_Pumps.2017.) + as.numeric(CTj$Heat_Pumps.2018.) + as.numeric(CTj$Heat_Pumps.2019.) + as.numeric(CTj$Heat_Pumps.2020.) + as.numeric(CTj$Heat_Pumps.2021.) + as.numeric(CTj$Heat_Pumps.2022.) + as.numeric(CTj$Heat_Pumps.2023.)

# Bad naming convention but will need this variable for the regression
ll = as.numeric(CTj$Housing.Units)


# Loading the shapefile of CT towns - may need to change this depending on if you have a Mac or PC
# I am using the mac version
shapefile = st_read( "/Users/user/Documents/HeatPump/CT_Vicinity_Town_Polygon.shp")

# Filtering this to only CT
CT_towns <- shapefile %>%
  dplyr::filter(CT_LEGEND == "Connecticut")

CT <- CT_towns %>%
  mutate(area = st_area(geometry)) %>%
  group_by(TOWN_NAME) %>%
  top_n(1, area) %>%
  rename("TOWN" = TOWN_NAME) %>%
  dplyr::select(TOWN, geometry) %>%
  left_join(., CTj, by = "TOWN") %>%
  # removing these polygons since we removed them from the other dataset
  filter (!TOWN %in% c("Norwich", "Bozrah", "Wallingford")) %>%
  # Creating an aggregated heat pump variable
  dplyr::mutate(CTHPAll = as.numeric(Heat_Pumps.2017.) + 
                  as.numeric(Heat_Pumps.2018.) + 
                  as.numeric(Heat_Pumps.2019.) + 
                  as.numeric(Heat_Pumps.2020.) + 
                  as.numeric(Heat_Pumps.2021.) + 
                  as.numeric(Heat_Pumps.2022.) + 
                  as.numeric(Heat_Pumps.2023.))

CTj = CT

# More data cleaning - I am creating heat pump adoption variables per year
CTj = CTj %>% dplyr::select(-CTAdopt17, -CTAdopt18, -CTAdopt19, -CTAdopt20, -CTAdopt21, -CTAdopt22, -CTAdopt23) 
CTj = CTj %>% dplyr::mutate(CTAdopt17 = Heat_Pumps.2017./Housing.Units,
                            CTAdopt18 = Heat_Pumps.2018./Housing.Units,
                            CTAdopt19 = Heat_Pumps.2019./Housing.Units,
                            CTAdopt20 = Heat_Pumps.2020./Housing.Units,
                            CTAdopt21 = Heat_Pumps.2021./Housing.Units,
                            CTAdopt22 = Heat_Pumps.2022./Housing.Units,
                            CTAdopt23 = Heat_Pumps.2023./Housing.Units)


############################
### Moran's I Analysis  ####
############################

model = gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + 
              as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +
              `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...` + `Median_Household_Income` + as.numeric(`Bottled_tank_or_LP_gas`) +  `Black_or_African_American` + `White`    +  # MRF smooth added here
              offset(log(ll)),  data = CTj, method = 'REML',  family = nb())
residuals_model = residuals(model)
CTj = CTj %>% 
  ungroup() %>%
  mutate(residuals = residuals(model))
CTj$Observed <- CTj$CTHPAll  #  observed values
CTj$Expected <- fitted(model)  #  expected values from the model


# Creating coordinates 
centroids <- st_centroid(CTj) # or this centroids <- st_centroid(CT$geometry)
centroids <- st_centroid(CT$geometry)
coords <- st_coordinates(centroids) 
coords <- as.data.frame(coords,  na.rm= TRUE )

knn <- knearneigh(coords, k = 4)

# Convert to neighbors list
nb <- knn2nb(knn)

# Create the weights list
listw <- nb2listw(nb)

 moranI.stat(data=CTj, applyto="residuals", listw=listw, n=length(nb2listw(nb)), 
                S0=Szero(listw) )
 
 # 0.0004819395 - There is no spatial autocorrelation
 ##############################
 
 # Creating an aggregated variables of all heat pump adoptions per town
 CTU = CTj %>% dplyr::mutate (CTHP_Adopt_All = as.numeric(CTj$CTAdopt17) + as.numeric(CTj$CTAdopt18) + as.numeric(CTj$CTAdopt19) + as.numeric(CTj$CTAdopt20) + as.numeric(CTj$CTAdopt21) + as.numeric(CTj$CTAdopt22) + as.numeric(CTj$CTAdopt23))
 # Percent version
 CTU = CTU %>% dplyr::mutate (CTHP_Adopt_All_percent = CTHP_Adopt_All*100)
 
 
##################################
#        PLOTTING MAPS           #
##################################

# Map of total heat pumps (all years) per town
ggplot() +
  geom_sf(data = CTj, aes(fill = CTHPAll)) +
  scale_fill_gradient(low = "lightblue", high = "orange", name = "Total Heat Pumps") +
  theme_void() +
  labs(title = "Heat Pump per Town in Connecticut")

# Map of Heat pump adoption rates
ggplot() +
  geom_sf(data = CTU, aes(fill = CTHP_Adopt_All)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue",  name = "Heat Pump Rates") +
  theme_void() +
  labs(title = "Heat Pump Adoptation Rates per Town in Connecticut")
# Map of heat pump adoption percentages 
ggplot() +
  geom_sf(data = CTU, aes(fill = CTHP_Adopt_All_percent)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue",  name = "Heat Pump Adoptions(%)") +
  theme_void() +
  labs(title = "Heat Pump Adoption per Town in Connecticut")
##############################

#################################
# This is the forward stepwise regression model with all the variables
# I keep adding on variable 
# The order of the variables are based on the smallest AICs to the largest AICs 
# Smallest AIC - the most statistically significant variable

###################################################
####         STEPWISE FORWARD REGRESSION       ####
###################################################
# The order is based on the smallest AICs
# Negative binomial regression with spatial term
# GAM was used instead of GLM to account for spatial term

models = list(
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + offset(log(ll)), 
      data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + 
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + offset(log(ll)), 
      data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + 
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + Tenure_Owner_Occupied... +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + Tenure_Owner_Occupied... +  
        Median_Household_Income + offset(log(ll)), 
      data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + Tenure_Owner_Occupied... +  
        Median_Household_Income + as.numeric(Bottled_tank_or_LP_gas) +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + Tenure_Owner_Occupied... +  
        Median_Household_Income + as.numeric(Bottled_tank_or_LP_gas) + Black_or_African_American +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb()),
  
  gam(CTHPAll ~ as.numeric(Fuel_oil_kerosene_etc.) + Asian + as.numeric(Electricity) + 
        Unemployment.Rate + as.numeric(Utility_gas) + SNAP_Recipients + Median.Home.Age + 
        Hispanic_or_Latino + Poverty... + Median.Home.Value + Tenure_Owner_Occupied... +  
        Median_Household_Income + as.numeric(Bottled_tank_or_LP_gas) + Black_or_African_American + White +  
        offset(log(ll)), data = CTj, method = 'REML', family = nb())
)


transform_coef_for_tbl <- function(model) {
  coefs <- coef(model)
  summ <- summary(model)
  
  # Extract standard errors and p-values
  if("coefficients" %in% names(summ)) {
    std_errs <- summ$coefficients[, "Std. Error"]
    p_vals <- summ$coefficients[, "Pr(>|z|)"]
  } else {
    std_errs <- summ$p.table[, "Std. Error"]
    p_vals <- summ$p.table[, "Pr(>|z|)"]
  }
  
  # Ensure the lengths of coefs and std_errs are the same
  if (length(coefs) != length(std_errs)) {
    std_errs <- std_errs[seq_len(length(coefs))]
  }
  
  # Scaling specific variables 
  #Apply transformations (multiply by 10 or 10000 as specified) before exponentiation
  coefs["Hispanic_or_Latino"] <- coefs["Hispanic_or_Latino"] * 10
  coefs["Asian"] <- coefs["Asian"] * 10
  coefs["Black_or_African_American"] <- coefs["Black_or_African_American"] * 10
  coefs["White"] <- coefs["White"] * 10
  coefs["Tenure_Owner_Occupied..."] <- coefs["Tenure_Owner_Occupied..."] * 10
  coefs["Median.Home.Age"] <- coefs["Median.Home.Age"] * 10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  
  std_errs["Hispanic_or_Latino"] <- std_errs["Hispanic_or_Latino"] * 10
  std_errs["Asian"] <- std_errs["Asian"] * 10
  std_errs["Black_or_African_American"] <- std_errs["Black_or_African_American"] * 10
  std_errs["White"] <- std_errs["White"] * 10
  std_errs["Tenure_Owner_Occupied..."] <- std_errs["Tenure_Owner_Occupied..."]*10
  std_errs["Median.Home.Age"] <- std_errs["Median.Home.Age"] * 10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  
  # Apply exponentiation to coefficients
  coefs <- exp(coefs)
  
  # Adjust standard errors using the delta method for IRR, then exponentiate
  std_errs <- exp(coefs) * std_errs
  return(list(coef = coefs, std_err = std_errs, p = p_vals))
}

# Apply the transformation function to each model
transformed_models_for_tbl <- lapply(models, transform_coef_for_tbl)

# Extract transformed coefficients, standard errors, and p-values
transformed_coefs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$coef)
transformed_std_errs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$std_err)
p_vals_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$p)

# Creating a nice table of the forward regression

library(stargazer)
# Use stargazer to create the table with exponentiated coefficients and standard errors
stargazer(models,
          coef = transformed_coefs_for_tbl, 
          se = transformed_std_errs_for_tbl,
          p = p_vals_for_tbl,
          type = "html",
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_NB_HL_NoSpatialTermApril.html",
          align = TRUE,
          dep.var.labels = "Heat Pump Adoption",
          covariate.labels = c("Fuel oil kerosene etc.",
                               "Asian",
                               "Electricity",
                               "Unemployment.Rate",
                               "Utility gas",
                               "SNAP Recipients",
                               "Median Home Age",
                               "Hispanic or Latino",
                               "Poverty",
                               "Median Home Value",
                               "Tenure Owner Occupied",
                               "Median Household Income",
                               "Bottled tank or LP gas",
                               
                               "Black or African American",
                               "White" ),
          omit = "s\\(TOWN_NAME\\)",  # This line omits s(TOWN_NAME) terms
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)


################################################
###        Creating Spatial Term             ###
################################################
# Creating the spatial term 
# Daniel helped me create this code, so I may not understand all parts of the (spatial) code
CT_penalty <- CT |>
  mrf_penalty(node_labels = CT$TOWN)

levs <- with(CT, unique(TOWN))
CT_for_mrf <- CT |>
  dplyr::mutate(TOWN_NAME = factor(TOWN, levels = levs))

mrf_smooth_gam <- gam(CTHPAll ~ s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)),
                      data = CT_for_mrf,
                      method = 'REML',  # Fast version of REML smoothness selection
                      drop.unused.levels = F,
                      family = nb())

CT_for_mrf <- CT_for_mrf |>
  dplyr::select(TOWN, TOWN_NAME, CTHPAll) %>%
  ungroup() %>%
  mutate(.fitted = fitted(mrf_smooth_gam))

cb <- guide_colorbar(title = "Smoothed population")
fill_sc <- scale_fill_viridis_c(option = "magma",
                                guide = cb,
                                limits = c(0, 1400))
range(CT_for_mrf$CTHPAll)

fitted <- CT_for_mrf |>
  ggplot(aes(fill = .fitted)) +
  geom_sf(color = NA) +
  fill_sc +
  labs(title = "Smoothed_Heat_Pumps")

actual_pop <- CT_for_mrf |>
  ggplot(aes(fill = CTHPAll)) +
  geom_sf(color = NA) +
  fill_sc +
  labs(title = "Heat Pumps")

actual_pop + fitted + plot_layout(guides = "collect")

mrf_smooth_gam 
CT_for_mrf 
# Adding the spatial term to the main table
CTj = left_join(CT_for_mrf, CTj, by = "TOWN") 

###################################################
####         STEPWISE FORWARD REGRESSION       ####
###################################################
# The order is based on the smallest AICs
# Negative binomial regression with spatial term
# GAM was used instead of GLM to account for spatial term

models = list(
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) +  s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` +  s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) +  s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +    s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients`  + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age`  + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino`  + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...`   + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...` + `Median_Household_Income`   + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...` + `Median_Household_Income` + as.numeric(`Bottled_tank_or_LP_gas`)   + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...` + `Median_Household_Income` + as.numeric(`Bottled_tank_or_LP_gas`) +  `Black_or_African_American`   + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb()),
  gam(CTHPAll ~ as.numeric(`Fuel_oil_kerosene_etc.`) + `Asian` + as.numeric(`Electricity`) + `Unemployment.Rate` + as.numeric(`Utility_gas`) +   `SNAP_Recipients` +    `Median.Home.Age` +    `Hispanic_or_Latino` +`Poverty...` +   `Median.Home.Value` +   `Tenure_Owner_Occupied...` + `Median_Household_Income` + as.numeric(`Bottled_tank_or_LP_gas`) +  `Black_or_African_American` + `White`   + s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) +  # MRF smooth added here
        offset(log(ll)),  data = CTj, method = 'REML',  family = nb())
)

transform_coef_for_tbl <- function(model) {
  coefs <- coef(model)
  summ <- summary(model)
  
  # Extract standard errors and p-values
  if("coefficients" %in% names(summ)) {
    std_errs <- summ$coefficients[, "Std. Error"]
    p_vals <- summ$coefficients[, "Pr(>|z|)"]
  } else {
    std_errs <- summ$p.table[, "Std. Error"]
    p_vals <- summ$p.table[, "Pr(>|z|)"]
  }
  
  # Ensure the lengths of coefs and std_errs are the same
  if (length(coefs) != length(std_errs)) {
    std_errs <- std_errs[seq_len(length(coefs))]
  }
  
  # Scaling specific variables 
  #Apply transformations (multiply by 10 or 10000 as specified) before exponentiation
  coefs["Hispanic_or_Latino"] <- coefs["Hispanic_or_Latino"] * 10
  coefs["Asian"] <- coefs["Asian"] * 10
  coefs["Black_or_African_American"] <- coefs["Black_or_African_American"] * 10
  coefs["White"] <- coefs["White"] * 10
  coefs["Tenure_Owner_Occupied..."] <- coefs["Tenure_Owner_Occupied..."] * 10
  coefs["Median.Home.Age"] <- coefs["Median.Home.Age"] * 10
  coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
  
  std_errs["Hispanic_or_Latino"] <- std_errs["Hispanic_or_Latino"] * 10
  std_errs["Asian"] <- std_errs["Asian"] * 10
  std_errs["Black_or_African_American"] <- std_errs["Black_or_African_American"] * 10
  std_errs["White"] <- std_errs["White"] * 10
  std_errs["Tenure_Owner_Occupied..."] <- std_errs["Tenure_Owner_Occupied..."]*10
  std_errs["Median.Home.Age"] <- std_errs["Median.Home.Age"] * 10
  std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
  
  # Apply exponentiation to coefficients
  coefs <- exp(coefs)
  
  # Adjust standard errors using the delta method for IRR, then exponentiate
  std_errs <- exp(coefs) * std_errs
  return(list(coef = coefs, std_err = std_errs, p = p_vals))
}

# Apply the transformation function to each model
transformed_models_for_tbl <- lapply(models, transform_coef_for_tbl)

# Extract transformed coefficients, standard errors, and p-values
transformed_coefs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$coef)
transformed_std_errs_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$std_err)
p_vals_for_tbl <- lapply(transformed_models_for_tbl, function(x) x$p)

# Creating a nice table of the forward regression

library(stargazer)
# Use stargazer to create the table with exponentiated coefficients and standard errors
stargazer(models,
          coef = transformed_coefs_for_tbl, 
          se = transformed_std_errs_for_tbl,
          p = p_vals_for_tbl,
          type = "html",
          title = "Heat Pump Adoption, Regression Estimates",
          out = "HP_NB_HL_withSpatialTermApril.html",
          align = TRUE,
          dep.var.labels = "Heat Pump Adoption",
          covariate.labels = c("Fuel oil kerosene etc.",
                               "Asian",
                               "Electricity",
                               "Unemployment.Rate",
                               "Utility gas",
                               "SNAP Recipients",
                               "Median Home Age",
                               "Hispanic or Latino",
                               "Poverty",
                               "Median Home Value",
                               "Tenure Owner Occupied",
                               "Median Household Income",
                               "Bottled tank or LP gas",
                               
                               "Black or African American",
                               "White" ),
          omit = "s\\(TOWN_NAME\\)",  # This line omits s(TOWN_NAME) terms
          omit.stat = c("f", "ser", "adj.rsq"),
          no.space = TRUE,
          digits = 2)
