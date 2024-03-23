setwd("/Users/evrencansari/Desktop/Thesis/DATA") 
rm(list=ls()) 
dir()


##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(purrr)
library(geosphere)
library(data.table)
library(plm)

options(max.print = 1000000)


####################################################


if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
  conflict <-read.csv("geo_var_gps_exposure_evren.csv")
  
  
  
  conflict <- conflict[, c("hhid", "wave", "battles_exposure_5", "battles_exposure_10", "explosion_exposure_5", "explosion_exposure_10", "violence_exposure_5", "violence_exposure_10", "boko_haram_exposure_5", "boko_haram_exposure_10", "fulani_exposure_5", "fulani_exposure_10")]
  
  conflict[] <- lapply(conflict, function(x) replace(x, x == "TRUE", 1))
  conflict[] <- lapply(conflict, function(x) replace(x, x == "FALSE", 0))
  
  conflict$conflict_exposure_5 <- as.integer(conflict$battles_exposure_5 | conflict$explosion_exposure_5 | conflict$violence_exposure_5)
  conflict$conflict_exposure_10 <- as.integer(conflict$battles_exposure_10 | conflict$explosion_exposure_10 | conflict$violence_exposure_10)
  
  
  
  
  #########
  
  conflict2 <-read.csv("geo_var_gps_fatalities_evren.csv")
  conflict2$conflict_fat_sum_5 <- conflict2$battles_fat_sum_5 + conflict2$violence_fat_sum_5 + conflict2$explosion_fat_sum_5
  conflict2$conflict_fat_sum_10 <- conflict2$battles_fat_sum_10 + conflict2$violence_fat_sum_10 + conflict2$explosion_fat_sum_10
  conflict2 <- conflict2[, c("hhid", "wave",  "battles_fat_sum_5",	"battles_fat_sum_10",	"violence_fat_sum_5",	"violence_fat_sum_10","conflict_fat_sum_5",	"conflict_fat_sum_10", "fulani_fat_sum_5", "fulani_fat_sum_10", "boko_haram_fat_sum_5",	"boko_haram_fat_sum_10")]
  
  
  conflict  <- list(conflict ,conflict2) %>% 
    reduce(left_join, by = c('hhid'='hhid','wave'='wave'))
  
  
  
  
  
  #########
  
  conflict3 <-read.csv("geo_var_gps_exposure_count_evren.csv")
  conflict3$conflict_exposure_count_5 <- conflict3$battles_exposure_count_5 + conflict3$violence_exposure_count_5 + conflict3$explosion_exposure_count_5
  conflict3$conflict_exposure_count_10 <- conflict3$battles_exposure_count_10 + conflict3$violence_exposure_count_10 + conflict3$explosion_exposure_count_10
  conflict3 <- conflict3[, c("hhid", "wave",  "battles_exposure_count_5",	"battles_exposure_count_10",	"violence_exposure_count_5",	"violence_exposure_count_10", "conflict_exposure_count_5",	"conflict_exposure_count_10", "fulani_exposure_count_5", "fulani_exposure_count_10", "boko_haram_exposure_count_5",	"boko_haram_exposure_count_10")]
  
  
  conflict  <- list(conflict ,conflict3) %>% 
    reduce(left_join, by = c('hhid'='hhid','wave'='wave'))
  
  
  
  
  
  ######
  
  #conflict4 <-read.csv("conflict_lga.csv")
  #conflict4 <- conflict4[, !(names(conflict4) %in% c("date"))]
  
  
  #conflict  <- list(conflict ,conflict4) %>% 
  #  reduce(left_join, by = c('hhid'='hhid','wave'='wave'))
  
  
  
  
  ######
  
  conflict5 <-read.csv("conflict_lga_lagged6.csv")
  conflict5 <- conflict5[, !(names(conflict5) %in% c("date"))]
  
  
  
  conflict  <- list(conflict ,conflict5) %>% 
    reduce(left_join, by = c('hhid'='hhid','wave'='wave'))
  
}


##
conflict6 <-read.csv("conflict_new.csv")
conflict6 <- conflict6[, !(names(conflict6) %in% c("date"))]






#############



# Function to create new columns for previous wave conflict exposure
create_previous_wave_columns <- function(data, conflict_vars) {
  for (var in conflict_vars) {
    new_var_name <- paste0(var, "a")
    data[[new_var_name]] <- NA  # Initialize new column with NAs
    
    for (i in 1:nrow(data)) {
      current_hhid <- data$hhid[i]
      current_wave <- data$wave[i]
      
      # Define the previous wave based on the current wave
      previous_wave <- switch(as.character(current_wave),
                              '2' = '1',
                              '3' = '2',
                              '4' = '3',
                              '6' = '4')
      
      # Find the row index for the previous wave of the same hhid
      previous_row <- which(data$hhid == current_hhid & data$wave == previous_wave)
      
      if (length(previous_row) == 1) {
        data[[new_var_name]][i] <- data[[var]][previous_row]
      }
    }
  }
  return(data)
}

# List of conflict variables
conflict_vars <- c('conflict_exposure','conflict_events', 'conflict_fatalities','fat_conflict_exposure','fat_conflict_events', 'fat_conflict_fatalities')

# Apply the function to the data
conflict <- create_previous_wave_columns(conflict6, conflict_vars)




write.csv(conflict, "conflict.csv", row.names = FALSE)
