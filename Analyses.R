setwd("/Users/evrencansari/Desktop/Thesis/DATA") 

rm(list = ls())
dir()

getwd()

##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(plm)
library(pglm)
library(lme4)
library(psych)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(car)
library(MASS)
library(censReg)
library(texreg)
library(lmtest)
library(stargazer)
library(lmtest)
library(sandwich)
library(survival)
library(feisr)
library(survey)


options(max.print = 1000000)


####################################################


# Read the data
dependent_hh <- read.csv("dependent_hh_evren.csv")
dependent_ent <- read.csv("dependent_ent_a.csv")


# DELETE DUPLICATES

dependent_ent <- dependent_ent %>%
  filter(NA_qui_c != 1 & is_duplicate_qui_c != 1)



# Create separate datasets for each wave
wave_1_data <- dependent_ent %>% filter(wave == 1)
wave_2_data <- dependent_ent %>% filter(wave == 2)
wave_3_data <- dependent_ent %>% filter(wave == 3)
wave_4_data <- dependent_ent %>% filter(wave == 4)

# Define the wave pairs and the corresponding age differences
wave_info <- list(
  list("current_wave" = 2, "reference_wave" = 1, "age_diff" = 0.5),
  list("current_wave" = 3, "reference_wave" = 2, "age_diff" = 1),
  list("current_wave" = 4, "reference_wave" = 3, "age_diff" = 0.5),
  list("current_wave" = 6, "reference_wave" = 4, "age_diff" = 1.5)
)

# Loop through each wave info and apply transformations
for (info in wave_info) {
  current_wave <- info$current_wave
  reference_wave <- info$reference_wave
  age_diff <- info$age_diff
  
  # Select the reference wave data
  reference_data <- switch(as.character(reference_wave),
                           "1" = wave_1_data,
                           "2" = wave_2_data,
                           "3" = wave_3_data,
                           "4" = wave_4_data)
  
  # Join the reference data with the main dataset for the current wave
  dependent_ent <- dependent_ent %>%
    left_join(reference_data %>% dplyr::select(ent_id_ind_qui_c, age_own, gender_own, read_own, ent_size_min, home, industrial, traditional, comercial, roadside, mobile, other, credit_use),
              by = "ent_id_ind_qui_c",
              suffix = c("", "_ref")) %>%
    mutate(
      age_own = if_else(wave == current_wave & Currently_Operating == 0 & is.na(age_own) & !is.na(age_own_ref), age_own_ref + age_diff, age_own),
      gender_own = if_else(wave == current_wave & Currently_Operating == 0 & is.na(gender_own) & !is.na(gender_own_ref), gender_own_ref, gender_own),
      read_own = if_else(wave == current_wave & Currently_Operating == 0 & is.na(read_own) & !is.na(read_own_ref), read_own_ref, read_own),
      ent_size_min = if_else(wave == current_wave & Currently_Operating == 0 & is.na(ent_size_min) & !is.na(ent_size_min_ref), ent_size_min_ref, ent_size_min),
      home = if_else(wave == current_wave & Currently_Operating == 0 & is.na(home) & !is.na(home_ref), home_ref, home),
      industrial = if_else(wave == current_wave & Currently_Operating == 0 & is.na(industrial) & !is.na(industrial_ref), industrial_ref, industrial),
      traditional = if_else(wave == current_wave & Currently_Operating == 0 & is.na(traditional) & !is.na(traditional_ref), traditional_ref, traditional),
      comercial = if_else(wave == current_wave & Currently_Operating == 0 & is.na(comercial) & !is.na(comercial_ref), comercial_ref, comercial),
      roadside = if_else(wave == current_wave & Currently_Operating == 0 & is.na(roadside) & !is.na(roadside_ref), roadside_ref, roadside),
      mobile = if_else(wave == current_wave & Currently_Operating == 0 & is.na(mobile) & !is.na(mobile_ref), mobile_ref, mobile),
      other = if_else(wave == current_wave & Currently_Operating == 0 & is.na(other) & !is.na(other_ref), other_ref, other),
      credit_use = if_else(wave == current_wave & Currently_Operating == 0 & is.na(credit_use) & !is.na(credit_use_ref), credit_use_ref, credit_use)
    ) %>%
    dplyr::select(-ends_with("_ref"))
}

# Ensure the dataset is arranged correctly
dependent_ent <- dependent_ent %>%
  arrange(ent_id_ind_qui_c, wave)




# only rural 
dependent_hh <- subset(dependent_hh, sector == 2)
dependent_ent <- subset(dependent_ent, sector == 2)


pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating



# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_events
formula_2 <- prevalence_currently_operating ~ fat_conflict_events + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")



# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_events
formula_2 <- num_operating ~ fat_conflict_events + lga:wave
formula_3 <- num_operating ~ fat_conflict_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_events
formula_2 <- Currently_Operating ~ fat_conflict_events + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

formula_x <- Currently_Operating ~ fat_conflict_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + home + post_harvest + lga:wave
pooled_model_x <- plm(formula_x, data = pdata_ent, model = "pooling")
vif(pooled_model_x)


pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_events
  formula_2 <- log_value_capital ~ fat_conflict_events + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}



###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_events
formula_2 <- log_value_sale ~ fat_conflict_events + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))




if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###################### POISSON
  
  poisson_model_1 <- pglm(formula_1, data = pdata_ent, model = "within", family = poisson)
  poisson_model_2 <- pglm(formula_2, data = pdata_ent, model = "within", family = poisson)
  
  fixed_model_1 <- plm(formula_1, data = pdata_ent, model = "within")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  htmlreg(list(fixed_model_1, fixed_model_2, poisson_model_1, poisson_model_2),
          file = "alternative_regression_results_ENT_value_sale.html")
}



# DESCRIPTIVE


# Overall

selected_data <- dependent_hh %>%
  dplyr::select(num_operating, prevalence_currently_operating, fat_conflict_events, fat_conflict_eventsb, fat_conflict_eventsc, fat_conflict_fatalities, fat_conflict_exposure, fat_conflict_exposure_10, fat_conflict_exposure_15, gender_head, age_head, read_head, hh_size, land_cultivate, housing_own, credit_use, dist_admctr, anntot_avg, post_harvest, hh_value_capital, hh_value_sale, hh_total_cost_min)

# Obtaining Descriptive Statistics
descriptive_stats <- describe(selected_data)

# View the results
print(descriptive_stats)

# Waves

selected_data <- dependent_hh %>%
  dplyr::select(wave, num_operating, prevalence_currently_operating, fat_conflict_events, fat_conflict_eventsb, fat_conflict_eventsc, fat_conflict_fatalities, fat_conflict_exposure, fat_conflict_exposure_10, fat_conflict_exposure_15, gender_head, age_head, read_head, hh_size, land_cultivate, housing_own, credit_use, dist_admctr, anntot_avg, post_harvest) %>%
  group_by(wave)

# Obtaining Descriptive Statistics for each wave
descriptive_stats_by_wave <- describeBy(selected_data[, -1], group = selected_data$wave, mat = TRUE)

# View the results
print(descriptive_stats_by_wave)




selected_data <- dependent_ent %>%
  dplyr::select(Currently_Operating, fat_conflict_events, fat_conflict_eventsb, fat_conflict_eventsc, fat_conflict_fatalities, fat_conflict_exposure, fat_conflict_exposure_10, fat_conflict_exposure_15, gender_own, age_own, read_own, ent_size_min, land_cultivate, housing_own, credit_use, dist_admctr, anntot_avg, post_harvest, home, industrial, traditional, comercial, roadside, mobile, value_capital, value_sale, cost_inventory,	cost_transport,	cost_fuel,	cost_maintenance,	cost_insurance,	cost_rent,	cost_interest,	cost_raw)

# Obtaining Descriptive Statistics
descriptive_stats <- describe(selected_data)

# View the results
print(descriptive_stats)

# Waves

selected_data <- dependent_ent %>%
  dplyr::select(wave, Currently_Operating, fat_conflict_events, fat_conflict_eventsb, fat_conflict_eventsc, fat_conflict_fatalities, fat_conflict_exposure, fat_conflict_exposure_10, fat_conflict_exposure_15, gender_own, age_own, read_own, ent_size_min, land_cultivate, housing_own, credit_use, dist_admctr, anntot_avg, post_harvest, home, industrial, traditional, comercial, roadside, mobile, value_capital, value_sale, cost_inventory,	cost_transport,	cost_fuel,	cost_maintenance,	cost_insurance,	cost_rent,	cost_interest,	cost_raw) %>%
  group_by(wave)

# Obtaining Descriptive Statistics for each wave
descriptive_stats_by_wave <- describeBy(selected_data[, -1], group = selected_data$wave, mat = TRUE)

# View the results
print(descriptive_stats_by_wave)











### Robustness lag 6


pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))



#if (FALSE) {
# This code will not run
#  result <- computeSomething(data)



####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_eventsb
formula_2 <- prevalence_currently_operating ~ fat_conflict_eventsb + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag6_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_eventsb
formula_2 <- num_operating ~ fat_conflict_eventsb + lga:wave
formula_3 <- num_operating ~ fat_conflict_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag6_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_eventsb
formula_2 <- Currently_Operating ~ fat_conflict_eventsb + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag6_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_eventsb
  formula_2 <- log_value_capital ~ fat_conflict_eventsb + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag6_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_eventsb
formula_2 <- log_value_sale ~ fat_conflict_eventsb + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag6_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))













### Robustness 3 years


pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))



#if (FALSE) {
# This code will not run
#  result <- computeSomething(data)



####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_eventsc
formula_2 <- prevalence_currently_operating ~ fat_conflict_eventsc + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag12_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_eventsc
formula_2 <- num_operating ~ fat_conflict_eventsc + lga:wave
formula_3 <- num_operating ~ fat_conflict_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag12_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_eventsc
formula_2 <- Currently_Operating ~ fat_conflict_eventsc + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag12_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_eventsc
  formula_2 <- log_value_capital ~ fat_conflict_eventsc + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag12_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_eventsc
formula_2 <- log_value_sale ~ fat_conflict_eventsc + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_lag12_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))









#### robustness fatalities



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_fatalities
formula_2 <- prevalence_currently_operating ~ fat_conflict_fatalities + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_fatalities + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_fatalities
formula_2 <- num_operating ~ fat_conflict_fatalities + lga:wave
formula_3 <- num_operating ~ fat_conflict_fatalities + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_fatalities
formula_2 <- Currently_Operating ~ fat_conflict_fatalities + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_fatalities + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_fatalities
  formula_2 <- log_value_capital ~ fat_conflict_fatalities + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_fatalities + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_fatalities
formula_2 <- log_value_sale ~ fat_conflict_fatalities + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_fatalities + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





#### robustness fatalities - 2 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_fatalitiesb
formula_2 <- prevalence_currently_operating ~ fat_conflict_fatalitiesb + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_fatalitiesb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_2y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_fatalitiesb
formula_2 <- num_operating ~ fat_conflict_fatalitiesb + lga:wave
formula_3 <- num_operating ~ fat_conflict_fatalitiesb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_2y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_fatalitiesb
formula_2 <- Currently_Operating ~ fat_conflict_fatalitiesb + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_fatalitiesb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_2y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_fatalitiesb
  formula_2 <- log_value_capital ~ fat_conflict_fatalitiesb + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_fatalitiesb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_2y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_fatalitiesb
formula_2 <- log_value_sale ~ fat_conflict_fatalitiesb + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_fatalitiesb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_2y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))















#### robustness fatalities - 3 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_fatalitiesc
formula_2 <- prevalence_currently_operating ~ fat_conflict_fatalitiesc + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_fatalitiesc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_3y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_fatalitiesc
formula_2 <- num_operating ~ fat_conflict_fatalitiesc + lga:wave
formula_3 <- num_operating ~ fat_conflict_fatalitiesc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_3y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_fatalitiesc
formula_2 <- Currently_Operating ~ fat_conflict_fatalitiesc + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_fatalitiesc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_3y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_fatalitiesc
  formula_2 <- log_value_capital ~ fat_conflict_fatalitiesc + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_fatalitiesc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_3y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_fatalitiesc
formula_2 <- log_value_sale ~ fat_conflict_fatalitiesc + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_fatalitiesc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_fatalities_3y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))




#### robustness exposure 5



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating



# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_exposure_5
formula_2 <- prevalence_currently_operating ~ fat_conflict_exposure_5 + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_exposure_5 + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_5_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_exposure_5
formula_2 <- num_operating ~ fat_conflict_exposure_5 + lga:wave
formula_3 <- num_operating ~ fat_conflict_exposure_5 + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_5_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_exposure_5b
formula_2 <- Currently_Operating ~ fat_conflict_exposure_5b + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_exposure_5b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_5_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_exposure_5
  formula_2 <- log_value_capital ~ fat_conflict_exposure_5 + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_exposure_5 + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_5_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}


###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_exposure_5b
formula_2 <- log_value_sale ~ fat_conflict_exposure_5b + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_exposure_5b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_5_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))






#### robustness exposure 15



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating



# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_exposure_15b
formula_2 <- prevalence_currently_operating ~ fat_conflict_exposure_15b + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_exposure_15b + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_15_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_exposure_15
formula_2 <- num_operating ~ fat_conflict_exposure_15 + lga:wave
formula_3 <- num_operating ~ fat_conflict_exposure_15 + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_15_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_exposure_15b
formula_2 <- Currently_Operating ~ fat_conflict_exposure_15b + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_exposure_15b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_15_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_exposure_15
  formula_2 <- log_value_capital ~ fat_conflict_exposure_15 + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_exposure_15 + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_15_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}


###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_exposure_15b
formula_2 <- log_value_sale ~ fat_conflict_exposure_15b + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_exposure_15b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_15_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))








#### robustness exposure 10



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating



# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_conflict_exposure_10
formula_2 <- prevalence_currently_operating ~ fat_conflict_exposure_10 + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_conflict_exposure_10 + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_10_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_conflict_exposure_10
formula_2 <- num_operating ~ fat_conflict_exposure_10 + lga:wave
formula_3 <- num_operating ~ fat_conflict_exposure_10 + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_10_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_conflict_exposure_10b
formula_2 <- Currently_Operating ~ fat_conflict_exposure_10b + lga:wave
formula_3 <- Currently_Operating ~ fat_conflict_exposure_10b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_10_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_conflict_exposure_10
  formula_2 <- log_value_capital ~ fat_conflict_exposure_10 + lga:wave
  formula_3 <- log_value_capital ~ fat_conflict_exposure_10 + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_10_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}



###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_conflict_exposure_10b
formula_2 <- log_value_sale ~ fat_conflict_exposure_10b + lga:wave
formula_3 <- log_value_sale ~ fat_conflict_exposure_10b + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_exposure_10_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))




#### robustness battles



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_battles_events
formula_2 <- prevalence_currently_operating ~ fat_battles_events + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_battles_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_battles_events
formula_2 <- num_operating ~ fat_battles_events + lga:wave
formula_3 <- num_operating ~ fat_battles_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))




###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_battles_events
formula_2 <- Currently_Operating ~ fat_battles_events + lga:wave
formula_3 <- Currently_Operating ~ fat_battles_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_battles_events
  formula_2 <- log_value_capital ~ fat_battles_events + lga:wave
  formula_3 <- log_value_capital ~ fat_battles_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}


###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_battles_events
formula_2 <- log_value_sale ~ fat_battles_events + lga:wave
formula_3 <- log_value_sale ~ fat_battles_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))









#### robustness battles_events - 2 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_battles_eventsb
formula_2 <- prevalence_currently_operating ~ fat_battles_eventsb + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_battles_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_2y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_battles_eventsb
formula_2 <- num_operating ~ fat_battles_eventsb + lga:wave
formula_3 <- num_operating ~ fat_battles_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_2y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_battles_eventsb
formula_2 <- Currently_Operating ~ fat_battles_eventsb + lga:wave
formula_3 <- Currently_Operating ~ fat_battles_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_2y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_battles_eventsb
  formula_2 <- log_value_capital ~ fat_battles_eventsb + lga:wave
  formula_3 <- log_value_capital ~ fat_battles_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_2y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_battles_eventsb
formula_2 <- log_value_sale ~ fat_battles_eventsb + lga:wave
formula_3 <- log_value_sale ~ fat_battles_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_2y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))














#### robustness battles_events - 3 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_battles_eventsc
formula_2 <- prevalence_currently_operating ~ fat_battles_eventsc + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_battles_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_3y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_battles_eventsc
formula_2 <- num_operating ~ fat_battles_eventsc + lga:wave
formula_3 <- num_operating ~ fat_battles_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_3y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_battles_eventsc
formula_2 <- Currently_Operating ~ fat_battles_eventsc + lga:wave
formula_3 <- Currently_Operating ~ fat_battles_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_3y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_battles_eventsc
  formula_2 <- log_value_capital ~ fat_battles_eventsc + lga:wave
  formula_3 <- log_value_capital ~ fat_battles_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_3y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_battles_eventsc
formula_2 <- log_value_sale ~ fat_battles_eventsc + lga:wave
formula_3 <- log_value_sale ~ fat_battles_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_battles_events_3y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))






#### robustness violence



pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))




####### HH - currently_operating



# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_violence_events
formula_2 <- prevalence_currently_operating ~ fat_violence_events + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_violence_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))





###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_violence_events
formula_2 <- num_operating ~ fat_violence_events + lga:wave
formula_3 <- num_operating ~ fat_violence_events + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))









###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_violence_events
formula_2 <- Currently_Operating ~ fat_violence_events + lga:wave
formula_3 <- Currently_Operating ~ fat_violence_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_violence_events
  formula_2 <- log_value_capital ~ fat_violence_events + lga:wave
  formula_3 <- log_value_capital ~ fat_violence_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}


###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_violence_events
formula_2 <- log_value_sale ~ fat_violence_events + lga:wave
formula_3 <- log_value_sale ~ fat_violence_events + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))







#### robustness violence_events - 2 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_violence_eventsb
formula_2 <- prevalence_currently_operating ~ fat_violence_eventsb + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_violence_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_2y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_violence_eventsb
formula_2 <- num_operating ~ fat_violence_eventsb + lga:wave
formula_3 <- num_operating ~ fat_violence_eventsb + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_2y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))



###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_violence_eventsb
formula_2 <- Currently_Operating ~ fat_violence_eventsb + lga:wave
formula_3 <- Currently_Operating ~ fat_violence_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_2y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_violence_eventsb
  formula_2 <- log_value_capital ~ fat_violence_eventsb + lga:wave
  formula_3 <- log_value_capital ~ fat_violence_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_2y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_violence_eventsb
formula_2 <- log_value_sale ~ fat_violence_eventsb + lga:wave
formula_3 <- log_value_sale ~ fat_violence_eventsb + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_2y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))











#### robustness violence_events - 3 years

pdata_hh <- pdata.frame(dependent_hh, index = c("hhid", "wave"))
pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))


####### HH - currently_operating

# Define the formulas
formula_1 <- prevalence_currently_operating ~ fat_violence_eventsc
formula_2 <- prevalence_currently_operating ~ fat_violence_eventsc + lga:wave
formula_3 <- prevalence_currently_operating ~ fat_violence_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Perform coefficient tests with clustered standard errors
pooled_model_1_cls <- coeftest(pooled_model_1, vcov = clustered_se_1)
fixed_model_2_cls <- coeftest(fixed_model_2, vcov = clustered_se_2)
fixed_model_3_cls <- coeftest(fixed_model_3, vcov = clustered_se_3)

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_3y_HH_currently_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

###### HH - num_operating

# Define the formulas
formula_1 <- num_operating ~ fat_violence_eventsc
formula_2 <- num_operating ~ fat_violence_eventsc + lga:wave
formula_3 <- num_operating ~ fat_violence_eventsc + gender_head + age_head + read_head + hh_size + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + lga:wave

# Estimate the pooled and fixed effects models
pooled_model_1 <- plm(formula_1, data = pdata_hh, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_hh, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_hh, model = "within")

# Calculate clustered standard errors
clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "hhid")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "hhid")

# Generate the stargazer report with R-squared and F-statistics
stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_3y_HH_num_operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))


###### ENT - Currently_Operating

formula_1 <- Currently_Operating ~ fat_violence_eventsc
formula_2 <- Currently_Operating ~ fat_violence_eventsc + lga:wave
formula_3 <- Currently_Operating ~ fat_violence_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_3y_ENT_Currently_Operating.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  ###### ENT - log_value_capital
  
  pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
  pdata_ent <- subset(pdata_ent, !is.na(value_capital))
  pdata_ent$log_value_capital <- log(pdata_ent$value_capital + 1)  # Adding 1 or another small constant
  
  
  formula_1 <- log_value_capital ~ fat_violence_eventsc
  formula_2 <- log_value_capital ~ fat_violence_eventsc + lga:wave
  formula_3 <- log_value_capital ~ fat_violence_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave
  
  pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
  fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
  fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")
  
  clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
  
  stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_3y_ENT_log_value_capital.html",
            se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))
  
}

###### ENT - log_value_sale

pdata_ent <- pdata.frame(dependent_ent, index = c("ent_id_ind_qui_c", "wave"))
pdata_ent <- subset(pdata_ent, !is.na(value_sale))
pdata_ent$log_value_sale <- log(pdata_ent$value_sale + 1)  # Adding 1 or another small constant


formula_1 <- log_value_sale ~ fat_violence_eventsc
formula_2 <- log_value_sale ~ fat_violence_eventsc + lga:wave
formula_3 <- log_value_sale ~ fat_violence_eventsc + gender_own + age_own + read_own + ent_size_min + land_cultivate + housing_own + credit_use + dist_admctr + anntot_avg + post_harvest + home + lga:wave

pooled_model_1 <- plm(formula_1, data = pdata_ent, model = "pooling")
fixed_model_2 <- plm(formula_2, data = pdata_ent, model = "within")
fixed_model_3 <- plm(formula_3, data = pdata_ent, model = "within")

clustered_se_1 <- vcovHC(pooled_model_1, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_2 <- vcovHC(fixed_model_2, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")
clustered_se_3 <- vcovHC(fixed_model_3, method = "arellano", type = "HC1", cluster = "group", group = "ent_id_ind_qui_c")

stargazer(pooled_model_1, fixed_model_2, fixed_model_3, type = "html", out = "regression_results_violence_events_3y_ENT_log_value_sale.html",
          se = list(sqrt(diag(clustered_se_1)), sqrt(diag(clustered_se_2)), sqrt(diag(clustered_se_3))))

