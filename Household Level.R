setwd("/Users/evrencansari/Desktop/Thesis/DATA") 
rm(list = ls())
dir()

getwd()


##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(purrr)
library(geosphere)
library(data.table)
library(plm)

options(max.print = 1000000)


####################################################




# Dependent HH



#################################################


dependent_hh <-read.csv("dependent_ent_a.csv")



############## now is time to make it an hh level data #########################


######## PREVALENCE OF A NEW ENTERPRISE

dependent_hh_new_na <- dependent_hh %>%
  filter(!is.na(new))


dependent_hh_new <- dependent_hh_new_na %>%
  group_by(hhid, wave) %>%
  summarize(new = ifelse(any(new == 1), 
                         1, 
                         ifelse(all(is.na(new) | new == 0), 
                                0, 
                                NA)))
dependent_hh_new <- subset(dependent_hh_new, select = c('hhid', 'wave', 'new'))






######## PREVALENCE OF A {STATUS} ENTERPRISE


# prevalence of in operation enterprise (currently operating / seasonally closed / temporarily closed enterprise)


dependent_hh_in_operation <- dependent_hh %>%
  group_by(hhid, wave) %>%
  summarize(in_operation = ifelse(any(in_operation == 1), 
                                  1, 
                                  ifelse(all(is.na(in_operation) | in_operation == 0), 
                                         0, 
                                         NA)))
dependent_hh_in_operation <- subset(dependent_hh_in_operation, select = c('hhid', 'wave', 'in_operation'))



dependent_hh_in_operation_na <- dependent_hh %>%
  filter(!is.na(in_operation))

dependent_hh_in_operation <- dependent_hh_in_operation_na %>%
  group_by(hhid, wave) %>%
  summarize(prevalence_in_operation = ifelse(any(in_operation == 1), 
                                             1, 
                                             ifelse(any(is.na(in_operation) | in_operation == 0, na.rm = TRUE), 
                                                    0, 
                                                    NA)))

dependent_hh_in_operation <- subset(dependent_hh_in_operation, select = c('hhid', 'wave', 'prevalence_in_operation'))




# prevalence of currently operating / premanently closed / temporarily closed enterprise


dependent_hh$currently_operating <- ifelse(dependent_hh$status == "Currently Operating", 1, 0)

dependent_hh_operating_na <- dependent_hh %>%
  filter(!is.na(currently_operating))

dependent_hh_operating <- dependent_hh_operating_na %>%
  group_by(hhid, wave) %>%
  summarize(prevalence_currently_operating = ifelse(any(currently_operating == 1), 
                                                    1, 
                                                    ifelse(any(is.na(currently_operating) | currently_operating == 0, na.rm = TRUE), 
                                                           0, 
                                                           NA)))






# Temporarily Closed
dependent_hh$closed_temporarily <- ifelse(dependent_hh$status == "Closed, Temporarily", 1, 0)

dependent_hh_temporarily_na <- dependent_hh %>%
  filter(!is.na(closed_temporarily))

dependent_hh_temporarily <- dependent_hh_temporarily_na %>%
  group_by(hhid, wave) %>%
  summarize(prevalence_closed_temporarily = ifelse(any(closed_temporarily == 1), 
                                                   1, 
                                                   ifelse(all(is.na(closed_temporarily) | closed_temporarily == 0, na.rm = TRUE), 
                                                          0, 
                                                          NA)))


# Permanently Closed
dependent_hh$closed_permanently <- ifelse(dependent_hh$status == "Closed, Permanently", 1, 0)

dependent_hh_permanently_na <- dependent_hh %>%
  filter(!is.na(closed_permanently))

dependent_hh_permanently <- dependent_hh_permanently_na %>%
  group_by(hhid, wave) %>%
  summarize(prevalence_closed_permanently = ifelse(any(closed_permanently == 1), 
                                                   1, 
                                                   ifelse(all(is.na(closed_permanently) | closed_permanently == 0, na.rm = TRUE), 
                                                          0, 
                                                          NA)))


# Seasonally Closed
dependent_hh$closed_seasonally <- ifelse(dependent_hh$status == "Closed, Seasonally", 1, 0)


dependent_hh_seasonally_na <- dependent_hh %>%
  filter(!is.na(closed_seasonally))

dependent_hh_seasonally <- dependent_hh_seasonally_na %>%
  group_by(hhid, wave) %>%
  summarize(prevalence_closed_seasonally = ifelse(any(closed_seasonally == 1), 
                                                  1, 
                                                  ifelse(all(is.na(closed_seasonally) | closed_seasonally == 0, na.rm = TRUE), 
                                                         0, 
                                                         NA)))


##### CATEGORIES

# List of industry categories
categories <- c("G", "Q", "H", "C", "F", "S", "T", "I", "M", "E", "A", "R", "N", "NA", "J", "B", "P", "L", "O", "U", "K", "D")

# Create a column for each category with specified conditions
for(cat in categories) {
  dependent_hh[paste0("prev_", cat)] <- NA  # Initialize with NA
  
  # Apply conditions
  dependent_hh <- dependent_hh %>%
    mutate(!!paste0("prev_", cat) := case_when(
      status == "Currently Operating" & ind_cat_let == cat ~ 1,
      status == "Currently Operating" & ind_cat_let != cat & ind_cat_let != "NA" ~ 0,
      status == "Currently Operating" & ind_cat_let == "NA" ~ NA_real_,
      status != "Currently Operating" & ind_cat_let != "NA" ~ 0,
      status == "NA" ~ NA_real_
    ))
}



prevalance_cat <- dependent_hh %>%
  group_by(hhid, wave) %>%
  summarize(across(starts_with("prev_"), 
                   ~ if(any(. == 1, na.rm = TRUE)) 1 else if(all(is.na(.))) NA_real_ else 0),
            .groups = "drop")









# List of industry categories
categories <- c("1", "2", "3", "4a", "4b", "5_6_7_8", "9_10")

# Create a column for each category with specified conditions
for(high in categories) {
  dependent_hh[paste0("prev_high_", high)] <- NA  # Initialize with NA
  
  # Apply conditions
  dependent_hh <- dependent_hh %>%
    mutate(!!paste0("prev_high_", high) := case_when(
      status == "Currently Operating" & ind_high == high ~ 1,
      status == "Currently Operating" & ind_high != high & ind_high != "NA" ~ 0,
      status == "Currently Operating" & ind_high == "NA" ~ NA_real_,
      status != "Currently Operating" & ind_high != "NA" ~ 0,
      status == "NA" ~ NA_real_
    ))
}


prevalance_high <- dependent_hh %>%
  group_by(hhid, wave) %>%
  summarize(across(starts_with("prev_high_"), 
                   ~ if(any(. == 1, na.rm = TRUE)) 1 else if(all(is.na(.))) NA_real_ else 0),
            .groups = "drop")



# View the first few rows of the summary dataframe


######## Credit Use



# prevalence of credit use

dependent_hh_credit_use_na <- dependent_hh %>%
  filter(!is.na(credit_use))

dependent_hh_credit_use <- dependent_hh_credit_use_na %>%
  group_by(hhid, wave) %>%
  summarize(credit_use = ifelse(any(credit_use == 1), 
                                1, 
                                ifelse(all(is.na(credit_use) | credit_use == 0, na.rm = TRUE), 
                                       0, 
                                       NA)))
dependent_hh_credit_use <- subset(dependent_hh_credit_use, select = c('hhid', 'wave', 'credit_use'))





############ TOTAL VALUES OF CAPITAL, SALES, AND COST


grouped_df <- group_by(dependent_hh, hhid, wave)

# Calculate the sum of value_capital, value_sale, and total_cost_min for each group
summarized_data <- summarize(grouped_df, 
                             hh_value_capital = sum(value_capital, na.rm = TRUE),
                             hh_value_sale = sum(value_sale, na.rm = TRUE),
                             hh_total_cost_min = sum(total_cost_min, na.rm = TRUE))

# Merge the summarized data back into the original data frame
dependent_hh <- left_join(dependent_hh, summarized_data, by = c("hhid", "wave"))


dependent_hh_value_capital <- dependent_hh[!duplicated(dependent_hh[, c('hhid', 'wave','hh_value_capital')]), ]
dependent_hh_value_capital <- subset(dependent_hh_value_capital, select = c('hhid', 'wave','hh_value_capital'))

dependent_hh_value_sale <- dependent_hh[!duplicated(dependent_hh[, c('hhid', 'wave','hh_value_sale')]), ]
dependent_hh_value_sale <- subset(dependent_hh_value_sale, select = c('hhid', 'wave','hh_value_sale'))

dependent_hh_total_cost_min <- dependent_hh[!duplicated(dependent_hh[, c('hhid', 'wave','hh_total_cost_min')]), ]
dependent_hh_total_cost_min <- subset(dependent_hh_total_cost_min, select = c('hhid', 'wave','hh_total_cost_min'))






######## NUMBER OF CURRENTLY OPERATING ENTERPRISES


# Group the data frame once
grouped_df <- group_by(dependent_hh, hhid, wave)



# Using dplyr::summarize instead of mutate to directly get the summarized num_operating
dependent_hh_operating_num <- summarize(grouped_df, num_operating = ifelse(any(currently_operating == 1, na.rm = TRUE), 
                                                                           sum(currently_operating == 1, na.rm = TRUE), 
                                                                           ifelse(all(is.na(currently_operating)), NA, 0)))

# Selecting relevant columns
dependent_hh_operating_num <- dplyr::select(dependent_hh_operating_num, hhid, wave, num_operating)

# Handling NA values
dependent_hh_operating_num <- mutate(dependent_hh_operating_num, num_operating_min = ifelse(is.na(num_operating), 0, num_operating))





# Operations on the grouped data frame
dependent_hh_temporarily_num <- summarize(grouped_df, num_temporarily = ifelse(any(closed_temporarily == 1, na.rm = TRUE), 
                                                                               sum(closed_temporarily == 1, na.rm = TRUE), 
                                                                               ifelse(all(is.na(closed_temporarily)), NA, 0)))
dependent_hh_temporarily_num <- dplyr::select(dependent_hh_temporarily_num, hhid, wave, num_temporarily)
dependent_hh_temporarily_num <- mutate(dependent_hh_temporarily_num, num_temporarily_min = ifelse(is.na(num_temporarily), 0, num_temporarily))





dependent_hh_permanently_num <- summarize(grouped_df, num_permanently = ifelse(any(closed_permanently == 1, na.rm = TRUE), 
                                                                               sum(closed_permanently == 1, na.rm = TRUE), 
                                                                               ifelse(all(is.na(closed_permanently)), NA, 0)))
dependent_hh_permanently_num <- dplyr::select(dependent_hh_permanently_num, hhid, wave, num_permanently)
dependent_hh_permanently_num <- mutate(dependent_hh_permanently_num, num_permanently_min = ifelse(is.na(num_permanently), 0, num_permanently))






dependent_hh_seasonally_num <- summarize(grouped_df, num_seasonally = ifelse(any(closed_seasonally == 1, na.rm = TRUE), 
                                                                             sum(closed_seasonally == 1, na.rm = TRUE), 
                                                                             ifelse(all(is.na(closed_seasonally)), NA, 0)))
dependent_hh_seasonally_num <- dplyr::select(dependent_hh_seasonally_num, hhid, wave, num_seasonally)
dependent_hh_seasonally_num <- mutate(dependent_hh_seasonally_num, num_seasonally_min = ifelse(is.na(num_seasonally), 0, num_seasonally))





##########

dependent_hh <- dependent_hh %>%
  distinct(hhid, wave, .keep_all = FALSE)

dependent_hh <- list(dependent_hh, dependent_hh_new, dependent_hh_in_operation, dependent_hh_operating,dependent_hh_temporarily,dependent_hh_permanently,dependent_hh_seasonally,
                     dependent_hh_operating_num,dependent_hh_temporarily_num,dependent_hh_permanently_num,dependent_hh_seasonally_num, dependent_hh_credit_use, dependent_hh_value_capital, dependent_hh_value_sale, dependent_hh_total_cost_min, prevalance_cat, prevalance_high) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave')) 

dependent_hh$nonfarm_survey <- 1


rm(grouped_df,dependent_hh_in_operation,dependent_hh_new,dependent_hh_operating,dependent_hh_temporarily,dependent_hh_permanently,dependent_hh_seasonally,
   dependent_hh_operating_num,dependent_hh_temporarily_num,dependent_hh_permanently_num,dependent_hh_seasonally_num, dependent_hh_credit_use, dependent_hh_value_capital, dependent_hh_value_sale, dependent_hh_total_cost_min)


##########








## CONTROL - GEOVARIABLES ##


geo_var_gps <-read.csv("geo_var_gps.csv")

dependent_hh <- list(geo_var_gps, dependent_hh) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave')) 

dependent_hh$nonfarm_survey <- ifelse(is.na(dependent_hh$nonfarm_survey), 0, dependent_hh$nonfarm_survey)

dependent_hh <- dependent_hh %>%
  mutate(across(c(new, prevalence_currently_operating, prevalence_closed_temporarily, prevalence_closed_permanently, prevalence_closed_seasonally,
                  num_operating, num_operating_min, num_temporarily, num_temporarily_min,
                  num_permanently, num_permanently_min, num_seasonally, num_seasonally_min, credit_use, prev_G, 
                  prev_Q, prev_H, prev_C, prev_F, prev_S, prev_T, prev_I, prev_M, prev_E, prev_A, prev_R, prev_N, 
                  prev_NA, prev_J, prev_B, prev_P, prev_L, prev_O, prev_U, prev_K, prev_D),
                ~ if_else(nonfarm_survey == 0 & is.na(.), 0, .)))

dependent_hh <- mutate(dependent_hh, prevalence_currently_operating_min = ifelse(is.na(prevalence_currently_operating), 0, prevalence_currently_operating))

###########################






# CONFLICT







###########################




conflict <-read.csv("conflict.csv")



dependent_hh  <- list(dependent_hh ,conflict) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave'))

rm(conflict)




###### AGRICULTURAL CONTROLS

agr_control <-read.csv("agr_control.csv")
agr_control$agriculture_survey <- 1

dependent_hh <- list(dependent_hh, agr_control) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave')) 

dependent_hh$agriculture_survey <- ifelse(is.na(dependent_hh$agriculture_survey), 0, dependent_hh$agriculture_survey)

dependent_hh <- dependent_hh %>%
  mutate(across(c(new, land_cultivate, land_own, land_min_size),
                ~ if_else(agriculture_survey == 0 & is.na(.), 0, .)))


#### Seasonal control

dependent_hh$post_harvest <- ifelse(dependent_hh$wave %in% c(2, 4, 6), 1, 0)





##### only necessary columns

dependent_hh <- dependent_hh[, c("hhid", "lga", "wave", "wt", "anntot_avg", "dist_road", "dist_road2", 
                                 "dist_admctr", "dist_popcenter", "dist_popcenter2", "marital_head", 
                                 "age_head", "gender_head", "hh_size", "read_head", "housing_own", 
                                 "housing_outer", "housing_roof", "housing_floor", "housing_electricity", 
                                 "new", "prevalence_in_operation", "prevalence_currently_operating", "prevalence_closed_temporarily", "prevalence_closed_permanently", "prevalence_closed_seasonally", 
                                 "num_operating", "num_operating_min", "credit_use", "hh_value_capital", 
                                 "hh_value_sale", "hh_total_cost_min", "nonfarm_survey", 
                                 "conflict_events", "conflict_fatalities", "conflict_exposure", 
                                 "conflict_eventsb", "conflict_fatalitiesb", "conflict_exposureb",
                                 "conflict_eventsc", "conflict_fatalitiesc", "conflict_exposurec",
                                 "conflict_exposurea", "conflict_eventsa", "conflict_fatalitiesa",
                                 "fat_conflict_events", "fat_conflict_fatalities", "fat_conflict_exposure",
                                 "fat_conflict_eventsb", "fat_conflict_fatalitiesb", "fat_conflict_exposureb",
                                 "fat_conflict_eventsc", "fat_conflict_fatalitiesc", "fat_conflict_exposurec",
                                 "fat_conflict_exposurea", "fat_conflict_eventsa", "fat_conflict_fatalitiesa",
                                 "fat_conflict_exposure_5", "fat_conflict_exposure_10", "fat_conflict_exposure_15", 
                                 "fat_conflict_exposure_5b", "fat_conflict_exposure_10b", "fat_conflict_exposure_15b", 
                                 "fat_conflict_exposure_5c", "fat_conflict_exposure_10c", "fat_conflict_exposure_15c",
                                 "fat_battles_events", "fat_violence_events", "fat_remote_events",
                                 "fat_battles_eventsb", "fat_violence_eventsb", "fat_remote_eventsb",
                                 "fat_battles_eventsc", "fat_violence_eventsc", "fat_remote_eventsc",
                                 "land_cultivate", "land_own", 
                                 "land_min_size", "agriculture_survey", "post_harvest", "sector",
                                 "prev_G", "prev_Q", "prev_H", "prev_C", "prev_F", "prev_S", "prev_T", 
                                 "prev_I", "prev_M", "prev_E", "prev_A", "prev_R", "prev_N", "prev_NA", 
                                 "prev_J", "prev_B", "prev_P", "prev_L", "prev_O", "prev_U", "prev_K", 
                                 "prev_D")]





write.csv(dependent_hh, "dependent_hh_evren.csv", row.names = FALSE)




