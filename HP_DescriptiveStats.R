################################################################
##                      READ ME                               ##
################################################################
# This code is for the descriptive statistics. I calculated the 
# quartiles then set up a table.
# I used the same code to set up the spatial term 

################################################################


rm(list=ls())
graphics.off()

setwd("/Users/user/Documents/HeatPump")

library(here); library(tidyverse); library(sf); library(viridis); library(gam); library(tidycensus)
library(here); library(mgcv); library(viridis); library(patchwork); library(MRFtools)
library(spdep); library(mgcv); library(MASS); library(sp); library(spatialreg); library(spatial)
library(dplyr); library(flextable); library(gt)
# When I try to create a .csv with the spatial term, the dimensions of the dataset changes
# Same code for the first part 
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

# More data cleaning - I am creating heat pump adoption variables per year
CTj = CTj %>% dplyr::select(-CTAdopt17, -CTAdopt18, -CTAdopt19, -CTAdopt20, -CTAdopt21, -CTAdopt22, -CTAdopt23) 
CTj = CTj %>% dplyr::mutate(CTAdopt17 = Heat_Pumps.2017./Housing.Units,
                            CTAdopt18 = Heat_Pumps.2018./Housing.Units,
                            CTAdopt19 = Heat_Pumps.2019./Housing.Units,
                            CTAdopt20 = Heat_Pumps.2020./Housing.Units,
                            CTAdopt21 = Heat_Pumps.2021./Housing.Units,
                            CTAdopt22 = Heat_Pumps.2022./Housing.Units,
                            CTAdopt23 = Heat_Pumps.2023./Housing.Units)

######################################
##.           NEW CODE              ##
######################################

# Creating the quartiles
actual_breaks <- quantile(CTj$CTHPAll, 
                          probs = seq(0, 1, 0.25),  # This creates breaks at 0%, 25%, 50%, 75%, 100%
                          type = 7)                  # Type 7 is R's default and matches most statistical software

print("Actual distribution quartile breaks:")
print(actual_breaks)
# 3. Categorize data into quartile groups based on these breaks
CTj$quartile_group <- cut(CTj$CTHPAll,
                          breaks = actual_breaks,
                          labels = c("Q1", "Q2", "Q3", "Q4"),
                          include.lowest = TRUE)

# 4. Verify the distribution
quartile_counts <- table(CTj$quartile_group)
print("\nNumber of observations in each quartile:")
print(quartile_counts)


# Calculate the percentages for each quartile
quartile_percentages <- round(table(cut(CTj$CTHPAll, actual_breaks)) / length(CTj$CTHPAll) * 100, 2)


# Add percentages to the quartile labels
CTj <- CTj %>%
  mutate(
    quartile_group = cut(CTHPAll, breaks = actual_breaks, labels = c(
      paste0("Q1 (2.15 - 11.44%)"),
      paste0("Q2 (11.44 - 19.03%)"),
      paste0("Q3 (19.03 - 31.31%)"),
      paste0("Q4 (31.31 - 100%)")
    ))
  )

# Define quartile labels with percentages
quartile_labels <- c(
  "Q1 (2.15 - 11.44%)",
  "Q2 (11.44 - 19.03%)",
  "Q3 (19.03 - 31.31%)",
  "Q4 (31.31 - 100%)"
)

# Categorize towns into quartiles
CTj <- CTj %>%
  mutate(
    quartile_group = cut(CTHPAll, 
                         breaks = actual_breaks, 
                         labels = quartile_labels, 
                         include.lowest = TRUE)
  )

# Check for unexpected rows or missing values
table(CTj$quartile_group, useNA = "ifany")
quartile_summary <- CTj %>%
  group_by(quartile_group) %>%
  summarise(
    # Number of census tracts
    n_tracts = n(),
    
    # Heat Pump Statistics
     sd_mean = paste0(round(mean(CTHPAll), 2), 
                     " (", round(sd(CTHPAll), 2), ")"),
    
    # Race/ethnicity means(sd)
    nh_black = paste0(round(mean(Black_or_African_American), 2), 
                      " (", round(sd(Black_or_African_American), 2), ")"),
    nh_white = paste0(round(mean(White), 2), 
                      " (", round(sd(White), 2), ")"),
    hispanic = paste0(round(mean(Hispanic_or_Latino), 2), 
                      " (", round(sd(Hispanic_or_Latino), 2), ")"),
    asian = paste0(round(mean(Asian), 2), 
                   " (", round(sd(Asian), 2), ")"),
    
    # Socioeconomic characteristics
    snap = paste0(round(mean(SNAP_Recipients), 2), 
                  " (", round(sd(SNAP_Recipients), 2), ")"),
    unemployment_rate = paste0(round(mean(Unemployment.Rate), 2), 
                               " (", round(sd(Unemployment.Rate), 2), ")"),
    poverty = paste0(round(mean(Poverty...), 2), 
                     " (", round(sd(Poverty...), 2), ")"),
    hi = paste0(format(round(mean(Median_Household_Income), 0), big.mark=","),
                " (", format(round(sd(Median_Household_Income), 0), big.mark=","), ")"),
    hage = paste0(round(mean(Median.Home.Age),0),
                  " (", round(sd(Median.Home.Age), 0), ")"),
    
    # Energy Types
    electricity = paste0(round(mean(Electricity), 2), 
                         " (", round(sd(Electricity), 2), ")"),
    utility_gas = paste0(round(mean(Utility_gas), 2), 
                         " (", round(sd(Utility_gas), 2), ")"),
    fuel_oil = paste0(round(mean(Fuel_oil_kerosene_etc.), 2), 
                      " (", round(sd(Fuel_oil_kerosene_etc.), 2), ")"),
    lp_gas = paste0(round(mean(Bottled_tank_or_LP_gas), 2), 
                    " (", round(sd(Bottled_tank_or_LP_gas), 2), ")")
  )

# Create the flextable
ft <- flextable(quartile_summary)
write.csv(ft, "HP_DesStats.csv")

# Add the main header labels
ft <- set_header_labels(ft, 
                        `quartile_group` = "Quartiles",
                        `n_tracts` = "Number of Towns",
                        `sd_mean` = "Heat Pumps",
                        `nh_black` = "Black or African American(%)",
                        `nh_white` = "White(%)",
                        `hispanic` = "Hispanic/Latino(%)",
                        `asian` = "Asian(%)",
                        `hi` = "Median Household Income (USD)",
                        `hage` = "Median Home Age(years)",
                        `snap` = "SNAP Recipients",
                        `unemployment_rate` = "Unemployment",
                        `poverty` = "Poverty",
                        `electricity` = "Electricity(%)",
                        `utility_gas` = "Utility Gas(%)",
                        `fuel_oil` = "Fuel Oil Kerosene, Etc.(%)",
                        `lp_gas` = "Bottled Tank or LP Gas(%)"
)

ft <- bg(ft, i = 1, bg = "#E6F3F7", part = "header")

ft <- border(ft, 
             border.left = officer::fp_border(color = "black", width = 0.5), 
             part = "all")
ft <- delete_part(ft, part = "footer")
ft <- vline(ft, 
            border = officer::fp_border(color="black", width=0.5), 
            part="all")
ft <- border(ft, 
             border.bottom = officer::fp_border(color="black", width=0.5), 
             part="body")

ft <- add_footer_lines(ft, values = c("Notes: mean(sd)",
                                      "SNAP - Supplemental Nutrition Assistance Program"))

# Autofit for better formatting
ft <- autofit(ft)
ft

# Save the table as HTML or another format
save_as_html(ft, path = "Overall_Summary.html")
