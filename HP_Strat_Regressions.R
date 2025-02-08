######################################################################################
##                                    Read Me                                       ##
######################################################################################
# In this code, I used the same code to create spatial term. I had issues creating a
# .csv file with the spatial term so I decided to rerun it in this code.
# Then I do the forward regression which is stratified by each race/ethnicity group
# With the results of the regression, I create tables
# I do a sensitivity analysis (kinda) where I test the model with and without the spatial
# term 

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
library(sp); library(spatial); library(ggplot2)


# When I try to create a .csv with the spatial term, the dimensions of the dataset changes
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

#####################################################
#.                        NEW CODE                  #
#####################################################

########################################################################
#  Creating Models and Calculating Coefficients, P-values, and CIS.    #
########################################################################
# Now I will do the same model (forward regression) from the previous code
# but it will be stratified by race

###############
library(broom)

#########################
##. With Spatial Term  ##
#########################
racial_groups <- c("Asian", "Hispanic_or_Latino", "White", "Black_or_African_American")

# Function to run GAM analysis for multiple racial/ethnic groups
# It goes through each R/E group
run_gam_analyses <- function(data, racial_groups, CT_penalty) {
  # List to store results
  results <- list()
  
  for(group in racial_groups) {
    
    # The gam model
    # Create formula dynamically
    formula_str <- paste0("CTHPAll ~ `", group, "` + 
                         as.numeric(`Fuel_oil_kerosene_etc.`) + 
                         as.numeric(`Electricity`) + 
                         `Median.Home.Age` + 
                         `Tenure_Owner_Occupied...` + 
                         `Median_Household_Income` + 
                         s(TOWN_NAME, bs = 'mrf', xt = list(penalty = CT_penalty)) + 
                         offset(log(ll))")
    
    # Fit the model
    model <- gam(as.formula(formula_str), data = data, method = 'REML', family = nb())
    
    # Transform and extract coefficients
    summ <- summary(model)
    coefs <- coef(model)
    
    # Extract standard errors and p-values
    if ("coefficients" %in% names(summ)) {
      std_errs <- summ$coefficients[, "Std. Error"]
      p_vals <- summ$coefficients[, "Pr(>|z|)"]
    } else {
      std_errs <- summ$p.table[, "Std. Error"]
      p_vals <- summ$p.table[, "Pr(>|z|)"]
    }
    
    # Apply transformations
    vars_to_transform <- c(group, "Tenure_Owner_Occupied...", "Median.Home.Age")
    for(var in vars_to_transform) {
      if(var %in% names(coefs)) {
        coefs[var] <- coefs[var] * 10
        std_errs[var] <- std_errs[var] * 10
      }
    }
    
    # Transform Median Household Income
    if("Median_Household_Income" %in% names(coefs)) {
      coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
      std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
    }
    
    # Compute confidence intervals
    crit_val <- qnorm(0.975)
    ci_lower <- coefs - (crit_val * std_errs)
    ci_upper <- coefs + (crit_val * std_errs)
    
    # Exponentiate all values
    exp_coefs <- exp(coefs)
    exp_std_errs <- exp(coefs) * std_errs
    exp_ci_lower <- exp(ci_lower)
    exp_ci_upper <- exp(ci_upper)
    
    # Filter out smooth terms
    relevant_terms <- grep("^s\\(", names(coefs), invert = TRUE, value = TRUE)
    
    # Create results table
    results[[group]] <- data.frame(
      Variable = relevant_terms,
      Coefficient = exp_coefs[relevant_terms],
      Std_Error = exp_std_errs[relevant_terms],
      CI_Lower = exp_ci_lower[relevant_terms],
      CI_Upper = exp_ci_upper[relevant_terms],
      P_value = p_vals[relevant_terms]
    )
  }
  
  return(results)
}

racial_groups <- c("Asian", "Hispanic_or_Latino", "White", "Black_or_African_American")
results <- run_gam_analyses(CTj, racial_groups, CT_penalty)

# Access results for each group
a_table <- results[["Asian"]]
hl_table <- results[["Hispanic_or_Latino"]]
w_table <- results[["White"]]
aa_table <- results[["Black_or_African_American"]]

##############################
############################
# Generate the table for the final model
# Add p-values
aa_table$`Black or African American` <- paste0(
  round(aa_table$Coefficient, 2), " (", round(aa_table$Std_Error, 2), "), p = ", round(aa_table$P_value, 3)
)
w_table$`White` <- paste0(
  round(w_table$Coefficient, 2), " (", round(w_table$Std_Error, 2), "), p = ", round(w_table$P_value, 3)
)
a_table$`Asian` <- paste0(
  round(a_table$Coefficient, 2), " (", round(a_table$Std_Error, 2), "), p = ", round(a_table$P_value, 3)
)
hl_table$`Hispanic or Latino` <- paste0(
  round(hl_table$Coefficient, 2), " (", round(hl_table$Std_Error, 2), "), p = ", round(hl_table$P_value, 3)
)
#############################################
# Add CIs
aa_table$`Black or African American` <- paste0(
  round(aa_table$Coefficient, 2), " (95% CI:[", round(aa_table$CI_Lower, 2), ",", round(aa_table$CI_Upper, 2), "])")
w_table$`White` <- paste0(
  round(w_table$Coefficient, 2), " (95% CI:[", round(w_table$CI_Lower, 2), ",", round(w_table$CI_Upper, 2), "])")
a_table$`Asian` <- paste0(
  round(a_table$Coefficient, 2), " (95% CI:[", round(a_table$CI_Lower, 2), ",", round(a_table$CI_Upper, 2), "])")
hl_table$`Hispanic or Latino` <- paste0(
  round(hl_table$Coefficient, 2), " (95% CI:[", round(hl_table$CI_Lower, 2), ",", round(hl_table$CI_Upper, 2), "])")


# Remove the first row (race/ethnicity) from each table
aa_table <- aa_table[-1, ]
w_table <- w_table[-1, ]
a_table <- a_table[-1, ]
hl_table <- hl_table[-1, ]

# Subset and rename the first column of each table to match
aa_table <- aa_table[, c("Variable", "Black or African American")]
w_table <- w_table[, c("Variable", "White")]
a_table <- a_table[, c("Variable", "Asian")]
hl_table <- hl_table[, c("Variable", "Hispanic or Latino")]

# Merge all the tables by the "Term" column
combined_table <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), 
                         list(aa_table, w_table, a_table, hl_table))

# Get column names from combined_table
col_names <- names(combined_table)

# Create race_ethnicity_row using those exact column names
race_ethnicity_row <- data.frame(matrix(ncol = length(col_names), nrow = 1))
names(race_ethnicity_row) <- col_names

# Fill in the values
race_ethnicity_row$Variable <- "Race/Ethnicity"
race_ethnicity_row$`Black or African American` <- aa_table$`Black or African American`[1]
race_ethnicity_row$`White` <- w_table$`White`[1]
race_ethnicity_row$`Asian` <- a_table$`Asian`[1]
race_ethnicity_row$`Hispanic or Latino` <- hl_table$`Hispanic or Latino`[1]

# Now rbind and remove individual race rows
combined_table <- rbind(race_ethnicity_row, combined_table)
#combined_table <- combined_table[!combined_table$Predictor %in% c("Asian", "Black or African American", "Hispanic or Latino", "White"), ]

# Rename the "Term" column to "Predictor"
colnames(combined_table)[1] <- "Predictor"

# Clean up predictor names
combined_table$Predictor <- gsub("as.numeric\\(", "", combined_table$Predictor)  # Remove "as.numeric("
combined_table$Predictor <- gsub("\\)", "", combined_table$Predictor)            # Remove ")"
combined_table$Predictor <- gsub("_", " ", combined_table$Predictor)             # Replace underscores with spaces
combined_table$Predictor <- tools::toTitleCase(combined_table$Predictor)         # Capitalize first letters

# Filtering out single row R&E variables
combined_table <- combined_table %>% 
  filter(!(Predictor %in% c("Asian", "Black or African American", "Hispanic or Latino", "White")))
# Rename rows

combined_table <- combined_table %>%
  mutate(Predictor = case_when(
    Predictor == "Median.Home.Age" ~ "Median Home Age",   # Rename "A" to "Alpha"
    Predictor == "Tenure Owner Occupied..." ~ "Owner Occupied",    # Rename "B" to "Beta"
    TRUE ~ Predictor              # Keep other rows unchanged
  ))

write.csv(combined_table, "ForwardRegressionSpatial.csv")

# Create a flextable with the combined table
library(flextable)
ft <- flextable(combined_table)

# Customize the flextable header labels
ft <- set_header_labels(ft, 
                        Predictor = "Predictor",
                        `Black or African American` = "Black or African American",
                        `White` = "White",
                        `Asian` = "Asian",
                        `Hispanic or Latino` = "Hispanic or Latino"
)
# For a light blue shade
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
ft <- add_footer_lines(ft, values = c("Note: With spatial term"))
ft <- add_header_lines(ft, values = "Table 2. Stepwise Forward Regression of Heat Pump Adoption Stratified by Race and Ethnicity")

# Optional: Style the title (bold and centered)
ft <- bold(ft, i = 1, part = "header") 

# Finalize the table appearance
ft <- autofit(ft)

# View the flextable
ft

###########################
## Without spatial term  ##
###########################
# The same process but without the spatial term 
# This is like a sensitivity analysis where I am testing the model with and without the spatial term
racial_groups <- c("Asian", "Hispanic_or_Latino", "White", "Black_or_African_American")

# Function to run GAM analysis for multiple racial/ethnic groups
run_gam_analyses <- function(data, racial_groups, CT_penalty) {
  # List to store results
  results <- list()
  
  for(group in racial_groups) {
    # Create formula dynamically
    formula_str <- paste0("CTHPAll ~ `", group, "` + 
                         as.numeric(`Fuel_oil_kerosene_etc.`) + 
                         as.numeric(`Electricity`) + 
                         `Median.Home.Age` + 
                         `Tenure_Owner_Occupied...` + 
                         `Median_Household_Income` + 
                         offset(log(ll))")
    
    # Fit the model
    model <- gam(as.formula(formula_str), data = data, method = 'REML', family = nb())
    
    # Transform and extract coefficients
    summ <- summary(model)
    coefs <- coef(model)
    
    # Extract standard errors and p-values
    if ("coefficients" %in% names(summ)) {
      std_errs <- summ$coefficients[, "Std. Error"]
      p_vals <- summ$coefficients[, "Pr(>|z|)"]
    } else {
      std_errs <- summ$p.table[, "Std. Error"]
      p_vals <- summ$p.table[, "Pr(>|z|)"]
    }
    
    # Apply transformations
    vars_to_transform <- c(group, "Tenure_Owner_Occupied...", "Median.Home.Age")
    for(var in vars_to_transform) {
      if(var %in% names(coefs)) {
        coefs[var] <- coefs[var] * 10
        std_errs[var] <- std_errs[var] * 10
      }
    }
    
    # Transform Median Household Income
    if("Median_Household_Income" %in% names(coefs)) {
      coefs["Median_Household_Income"] <- coefs["Median_Household_Income"] * 10000
      std_errs["Median_Household_Income"] <- std_errs["Median_Household_Income"] * 10000
    }
    
    # Compute confidence intervals
    crit_val <- qnorm(0.975)
    ci_lower <- coefs - (crit_val * std_errs)
    ci_upper <- coefs + (crit_val * std_errs)
    
    # Exponentiate all values
    exp_coefs <- exp(coefs)
    exp_std_errs <- exp(coefs) * std_errs
    exp_ci_lower <- exp(ci_lower)
    exp_ci_upper <- exp(ci_upper)
    
    # Filter out smooth terms
    relevant_terms <- grep("^s\\(", names(coefs), invert = TRUE, value = TRUE)
    
    # Create results table
    results[[group]] <- data.frame(
      Variable = relevant_terms,
      Coefficient = exp_coefs[relevant_terms],
      Std_Error = exp_std_errs[relevant_terms],
      CI_Lower = exp_ci_lower[relevant_terms],
      CI_Upper = exp_ci_upper[relevant_terms],
      P_value = p_vals[relevant_terms]
    )
  }
  
  return(results)
}

racial_groups <- c("Asian", "Hispanic_or_Latino", "White", "Black_or_African_American")
results <- run_gam_analyses(CTj, racial_groups, CT_penalty)

# Access results for each group
a_table <- results[["Asian"]]
hl_table <- results[["Hispanic_or_Latino"]]
w_table <- results[["White"]]
aa_table <- results[["Black_or_African_American"]]

aa_table$`Black or African American` <- paste0(
  round(aa_table$Coefficient, 2), " (", round(aa_table$Std_Error, 2), "), p = ", round(aa_table$P_value, 3)
)
w_table$`White` <- paste0(
  round(w_table$Coefficient, 2), " (", round(w_table$Std_Error, 2), "), p = ", round(w_table$P_value, 3)
)
a_table$`Asian` <- paste0(
  round(a_table$Coefficient, 2), " (", round(a_table$Std_Error, 2), "), p = ", round(a_table$P_value, 3)
)
hl_table$`Hispanic or Latino` <- paste0(
  round(hl_table$Coefficient, 2), " (", round(hl_table$Std_Error, 2), "), p = ", round(hl_table$P_value, 3)
)
# Add CIs
aa_table$`Black or African American` <- paste0(
  round(aa_table$Coefficient, 2), " (95% CI:[", round(aa_table$CI_Lower, 2), ",", round(aa_table$CI_Upper, 2), "])")
w_table$`White` <- paste0(
  round(w_table$Coefficient, 2), " (95% CI:[", round(w_table$CI_Lower, 2), ",", round(w_table$CI_Upper, 2), "])")
a_table$`Asian` <- paste0(
  round(a_table$Coefficient, 2), " (95% CI:[", round(a_table$CI_Lower, 2), ",", round(a_table$CI_Upper, 2), "])")
hl_table$`Hispanic or Latino` <- paste0(
  round(hl_table$Coefficient, 2), " (95% CI:[", round(hl_table$CI_Lower, 2), ",", round(hl_table$CI_Upper, 2), "])")


# Remove the first row (race/ethnicity) from each table
aa_table <- aa_table[-1, ]
w_table <- w_table[-1, ]
a_table <- a_table[-1, ]
hl_table <- hl_table[-1, ]

# Subset and rename the first column of each table to match
aa_table <- aa_table[, c("Variable", "Black or African American")]
w_table <- w_table[, c("Variable", "White")]
a_table <- a_table[, c("Variable", "Asian")]
hl_table <- hl_table[, c("Variable", "Hispanic or Latino")]

# Merge all the tables by the "Term" column
combined_table <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), 
                         list(aa_table, w_table, a_table, hl_table))

# Get column names from combined_table
col_names <- names(combined_table)

# Create race_ethnicity_row using those exact column names
race_ethnicity_row <- data.frame(matrix(ncol = length(col_names), nrow = 1))
names(race_ethnicity_row) <- col_names

# Fill in the values
race_ethnicity_row$Variable <- "Race/Ethnicity"
race_ethnicity_row$`Black or African American` <- aa_table$`Black or African American`[1]
race_ethnicity_row$`White` <- w_table$`White`[1]
race_ethnicity_row$`Asian` <- a_table$`Asian`[1]
race_ethnicity_row$`Hispanic or Latino` <- hl_table$`Hispanic or Latino`[1]

# Now rbind and remove individual race rows
combined_table <- rbind(race_ethnicity_row, combined_table)
#combined_table <- combined_table[!combined_table$Predictor %in% c("Asian", "Black or African American", "Hispanic or Latino", "White"), ]

# Rename the "Term" column to "Predictor"
colnames(combined_table)[1] <- "Predictor"

# Clean up predictor names
combined_table$Predictor <- gsub("as.numeric\\(", "", combined_table$Predictor)  # Remove "as.numeric("
combined_table$Predictor <- gsub("\\)", "", combined_table$Predictor)            # Remove ")"
combined_table$Predictor <- gsub("_", " ", combined_table$Predictor)             # Replace underscores with spaces
combined_table$Predictor <- tools::toTitleCase(combined_table$Predictor)         # Capitalize first letters


# Filtering out single row R&E variables
combined_table <- combined_table %>% 
  filter(!(Predictor %in% c("Asian", "Black or African American", "Hispanic or Latino", "White")))
# Rename rows

combined_table <- combined_table %>%
  mutate(Predictor = case_when(
    Predictor == "Median.Home.Age" ~ "Median Home Age",   # Rename "A" to "Alpha"
    Predictor == "Tenure Owner Occupied..." ~ "Owner Occupied",    # Rename "B" to "Beta"
    TRUE ~ Predictor              # Keep other rows unchanged
  ))

write.csv(combined_table, "ForwardRegressionWO_Spatial.csv")

# Create a flextable with the combined table
ft <- flextable(combined_table)

# Customize the flextable header labels
ft <- set_header_labels(ft, 
                        Predictor = "Predictor",
                        `Black or African American` = "Black or African American",
                        `White` = "White",
                        `Asian` = "Asian",
                        `Hispanic or Latino` = "Hispanic or Latino"
)
# For a light blue shade
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
ft <- add_footer_lines(ft, values = c("Note: Without spatial term"))
ft <- add_header_lines(ft, values = "Table 3. Stepwise Forward Regression of Heat Pump Adoption Stratified by Race and Ethnicity")

# Optional: Style the title (bold and centered)
ft <- bold(ft, i = 1, part = "header") 
#ft <- align(ft, i = 1, align = "center", part = "header")


# Finalize the table appearance
ft <- autofit(ft)

# View the flextable
ft
