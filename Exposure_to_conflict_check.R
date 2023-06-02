setwd("/Users/evrencansari/Desktop/Thesis/DATA") 
rm(list=ls()) 
dir()



##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(purrr)
library(geosphere)
library(data.table)



options(max.print = 1000000)


####################################################


ACLED <-read.csv("2007-01-01-2023-02-01-Nigeria.csv")

ACLED$date <- as.Date(ACLED$event_date, format = "%d.%b.%y")

# Specify the start and end dates for filtering
# I just added/substracted 6 months

start_date <- as.Date("2009-02-15")
end_date <- as.Date("2016-09-27")


#start	end
#pp w1	1	28.07.2010	31.08.2010
#ph w1	2	1.02.2011	31.03.2011
#pp w2	3	10.09.2012	16.10.2012
#ph w2	4	23.02.2013	5.04.2013
#ph w3	6	22.02.2016	30.04.2016


ACLED <- subset(ACLED, date >= start_date & date <= end_date)

battles <- subset(ACLED, event_type %in% c("Battles"))
violence <- subset(ACLED, event_type %in% c("Violence against civilians"))
boko_haram <- subset(ACLED, actor1 %in% c("Boko Haram - Jamaatu Ahli is-Sunnah lid-Dawati wal-Jihad") | actor2 %in% c("Boko Haram - Jamaatu Ahli is-Sunnah lid-Dawati wal-Jihad") )
fulani <- subset(ACLED, actor1 %in% c("Fulani Ethnic Militia (Nigeria)","Fulani Ethnic Militia (Niger)","Fulani Ethnic Militia (Chad)","Hausa-Fulani Ethnic Militia (Nigeria)") | actor2 %in% c("Fulani Ethnic Militia (Nigeria)","Fulani Ethnic Militia (Niger)","Fulani Ethnic Militia (Chad)","Hausa-Fulani Ethnic Militia (Nigeria)") )

##graph

battles_daily_incidence <- aggregate(battles$data_id, by = list(category = battles$date), FUN = length)
names(battles_daily_incidence)[1] <- "date"
names(battles_daily_incidence)[2] <- "incidence"

battles_daily_incidence_monthly <- aggregate(incidence ~ format(date, "%Y-%m"), data = battles_daily_incidence, sum)

barplot(battles_daily_incidence_monthly$incidence, names.arg = battles_daily_incidence_monthly[,1], xlab = "Months", ylab = "Incidence")


battles_daily_fatality <- aggregate(battles$fatalities, by = list(category = battles$date), FUN = sum)
names(battles_daily_fatality)[1] <- "date"
names(battles_daily_fatality)[2] <- "fatality"

battles_daily_fatality_monthly <- aggregate(fatality ~ format(date, "%Y-%m"), data = battles_daily_fatality, sum)

barplot(battles_daily_fatality_monthly$fatality, names.arg = battles_daily_fatality_monthly[,1], xlab = "Months", ylab = "Fatality")




##

violence_daily_incidence <- aggregate(violence$data_id, by = list(category = violence$date), FUN = length)
names(violence_daily_incidence)[1] <- "date"
names(violence_daily_incidence)[2] <- "incidence"

violence_daily_incidence_monthly <- aggregate(incidence ~ format(date, "%Y-%m"), data = violence_daily_incidence, sum)

barplot(violence_daily_incidence_monthly$incidence, names.arg = violence_daily_incidence_monthly[,1], xlab = "Months", ylab = "Incidence")


violence_daily_fatality <- aggregate(violence$fatalities, by = list(category = violence$date), FUN = sum)
names(violence_daily_fatality)[1] <- "date"
names(violence_daily_fatality)[2] <- "fatality"

violence_daily_fatality_monthly <- aggregate(fatality ~ format(date, "%Y-%m"), data = violence_daily_fatality, sum)

barplot(violence_daily_fatality_monthly$fatality, names.arg = violence_daily_fatality_monthly[,1], xlab = "Months", ylab = "Fatality")

#

fulani_daily_incidence <- aggregate(fulani$data_id, by = list(category = fulani$date), FUN = length)
names(fulani_daily_incidence)[1] <- "date"
names(fulani_daily_incidence)[2] <- "incidence"

fulani_daily_incidence_monthly <- aggregate(incidence ~ format(date, "%Y-%m"), data = fulani_daily_incidence, sum)

barplot(fulani_daily_incidence_monthly$incidence, names.arg = fulani_daily_incidence_monthly[,1], xlab = "Months", ylab = "Incidence")

fulani_daily_fatality <- aggregate(fulani$fatalities, by = list(category = fulani$date), FUN = sum)
names(fulani_daily_fatality)[1] <- "date"
names(fulani_daily_fatality)[2] <- "fatality"

fulani_daily_fatality_monthly <- aggregate(fatality ~ format(date, "%Y-%m"), data = fulani_daily_fatality, sum)

barplot(fulani_daily_fatality_monthly$fatality, names.arg = fulani_daily_fatality_monthly[,1], xlab = "Months", ylab = "Fatality")

#

boko_haram_daily_incidence <- aggregate(boko_haram$data_id, by = list(category = boko_haram$date), FUN = length)
names(boko_haram_daily_incidence)[1] <- "date"
names(boko_haram_daily_incidence)[2] <- "incidence"

boko_haram_daily_incidence_monthly <- aggregate(incidence ~ format(date, "%Y-%m"), data = boko_haram_daily_incidence, sum)

barplot(boko_haram_daily_incidence_monthly$incidence, names.arg = boko_haram_daily_incidence_monthly[,1], xlab = "Months", ylab = "Incidence")

boko_haram_daily_fatality <- aggregate(boko_haram$fatalities, by = list(category = boko_haram$date), FUN = sum)
names(boko_haram_daily_fatality)[1] <- "date"
names(boko_haram_daily_fatality)[2] <- "fatality"

boko_haram_daily_fatality_monthly <- aggregate(fatality ~ format(date, "%Y-%m"), data = boko_haram_daily_fatality, sum)

barplot(boko_haram_daily_fatality_monthly$fatality, names.arg = boko_haram_daily_fatality_monthly[,1], xlab = "Months", ylab = "Fatality")







####################################################















###########################################

geovariables_pp_w1 <-read.csv("nga_householdgeovariables_y1.csv")
names(geovariables_pp_w1)[names(geovariables_pp_w1) == "lat_dd_mod"] <- "LAT_DD_MOD"
names(geovariables_pp_w1)[names(geovariables_pp_w1) == "lon_dd_mod"] <- "LON_DD_MOD"
geovariables_pp_w1$wave <- 1

geovariables_ph_w1 <-read.csv("nga_householdgeovariables_y1.csv")
names(geovariables_ph_w1)[names(geovariables_ph_w1) == "lat_dd_mod"] <- "LAT_DD_MOD"
names(geovariables_ph_w1)[names(geovariables_ph_w1) == "lon_dd_mod"] <- "LON_DD_MOD"
geovariables_ph_w1$wave <- 2

geovariables_pp_w2 <-read.csv("nga_householdgeovars_y2.csv")
geovariables_pp_w2$wave <- 3

geovariables_ph_w2 <-read.csv("nga_householdgeovars_y2.csv")
geovariables_ph_w2$wave <- 4

geovariables_ph_w3 <-read.csv("nga_householdgeovars_y3.csv")
geovariables_ph_w3$wave <- 6

#######################

# ISSUE - one household, in w4, didn't have location information. but it has the same loc information in the previous and following waves. so, i'm fixing that here.

# Find the row with household 130004 in both datasets
row_130004_loc <- which(geovariables_ph_w3$hhid == 130004)
row_130004_non_loc_ph <- which(geovariables_ph_w2$hhid == 130004)
row_130004_non_loc_pp <- which(geovariables_pp_w2$hhid == 130004)

# Get the value of col_X for household 130004 in dataset_1
latitude_130004 <- geovariables_ph_w3[row_130004_loc, "LAT_DD_MOD"]

# Get the value of col_X for household 130004 in dataset_1
longitude_130004 <- geovariables_ph_w3[row_130004_loc, "LON_DD_MOD"]

# Copy that value to dataset_2
geovariables_ph_w2[row_130004_non_loc_ph, "LAT_DD_MOD"] <- latitude_130004
geovariables_ph_w2[row_130004_non_loc_ph, "LON_DD_MOD"] <- longitude_130004

geovariables_pp_w2[row_130004_non_loc_pp, "LAT_DD_MOD"] <- latitude_130004
geovariables_pp_w2[row_130004_non_loc_pp, "LON_DD_MOD"] <- longitude_130004


# ISSUE - one household doesn't exist in the geovariable data (w2) but exists in ph_w2.


new_row <- subset(geovariables_ph_w3, hhid == "300165")
new_row$wave[new_row$wave == 6] <- 4
geovariables_ph_w2 <- bind_rows(geovariables_ph_w2, new_row)


##########################################

geo_var <- bind_rows(geovariables_pp_w1, geovariables_ph_w1, geovariables_pp_w2, geovariables_ph_w2, geovariables_ph_w3)
names(geo_var)[names(geo_var) == "LON_DD_MOD"] <- "longitude"
names(geo_var)[names(geo_var) == "LAT_DD_MOD"] <- "latitude"

geo_var_gps <- geo_var[, c("latitude", "longitude", "hhid", "wave")]





##########################################################

geo_var_gps$wave[geo_var_gps$wave == 1] <- "14.Aug.10"
geo_var_gps$wave[geo_var_gps$wave == 2] <- "02.Mar.11"
geo_var_gps$wave[geo_var_gps$wave == 3] <- "28.Sep.12"
geo_var_gps$wave[geo_var_gps$wave == 4] <- "15.Mar.13" 
geo_var_gps$wave[geo_var_gps$wave == 6] <- "27.Mar.16"

geo_var_gps$wave <- as.Date(geo_var_gps$wave, format = "%d.%b.%y")

names(geo_var_gps)[names(geo_var_gps) == "wave"] <- "date"

write.csv(geo_var_gps, "geo_var_gps22.csv", row.names = FALSE)

######################################



battles <- battles[, c("event_id_no_cnty", "data_id", "fatalities", "date", "latitude","longitude")]
violence <- violence[, c("event_id_no_cnty", "data_id", "fatalities", "date", "latitude","longitude")]
boko_haram <- boko_haram[, c("event_id_no_cnty", "data_id", "fatalities", "date", "latitude","longitude")]
fulani <- fulani[, c("event_id_no_cnty", "data_id", "fatalities", "date", "latitude","longitude")]

battles_fat <- battles[battles$fatalities > 0, ]
violence_fat <- violence[violence$fatalities > 0, ]
boko_haram_fat <- boko_haram[boko_haram$fatalities > 0, ]
fulani_fat <- fulani[fulani$fatalities > 0, ]

geo_var_gps <- geo_var_gps[, c("hhid","date", "latitude","longitude")]






# Create a function to check if a household is within the radius of a conflict location that happened within the time window
is_within_time_window <- function(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months) {
  dist <- distGeo(c(long1, lat1), c(long2, lat2))
  time_diff <- as.numeric(difftime(date1, date2, units = "days"))
  return(dist <= radius_m && time_diff >= 0 && time_diff <= time_window_months * 30.44)
}

time_window_months <- 12

if (FALSE){

##########


# Set radius in meters and time window in months
radius_m <- 10000
time_window_months <- 12


# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_10 <- FALSE
  for (j in 1:nrow(battles)) {
    lat2 <- battles[j, "latitude"]
    long2 <- battles[j, "longitude"]
    date2 <- as.Date(battles[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "battles_10"] <- within_time_window_10
}




##################


# Set radius in meters and time window in months
radius_m <- 5000


# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_5 <- FALSE
  for (j in 1:nrow(battles)) {
    lat2 <- battles[j, "latitude"]
    long2 <- battles[j, "longitude"]
    date2 <- as.Date(battles[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "battles_5"] <- within_time_window_5
}



################

radius_m <- 5000

# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_5 <- FALSE
  for (j in 1:nrow(violence)) {
    lat2 <- violence[j, "latitude"]
    long2 <- violence[j, "longitude"]
    date2 <- as.Date(violence[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "violence_5"] <- within_time_window_5
}

radius_m <- 10000

# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_10 <- FALSE
  for (j in 1:nrow(violence)) {
    lat2 <- violence[j, "latitude"]
    long2 <- violence[j, "longitude"]
    date2 <- as.Date(violence[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "violence_10"] <- within_time_window_10
}



radius_m <- 5000

# Repeat the above process for "boko_haram"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(boko_haram)) {
    lat2 <- boko_haram[j, "latitude"]
    long2 <- boko_haram[j, "longitude"]
    date2 <- as.Date(boko_haram[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "boko_haram_5"] <- within_time_window_5
}

radius_m <- 10000

# Repeat the above process for "boko_haram"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(boko_haram)) {
    lat2 <- boko_haram[j, "latitude"]
    long2 <- boko_haram[j, "longitude"]
    date2 <- as.Date(boko_haram[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "boko_haram_10"] <- within_time_window_10
}




radius_m <- 5000

# Repeat the above process for "fulani"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(fulani)) {
    lat2 <- fulani[j, "latitude"]
    long2 <- fulani[j, "longitude"]
    date2 <- as.Date(fulani[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "fulani_5"] <- within_time_window_5
}

radius_m <- 10000

# Repeat the above process for "fulani"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(fulani)) {
    lat2 <- fulani[j, "latitude"]
    long2 <- fulani[j, "longitude"]
    date2 <- as.Date(fulani[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "fulani_10"] <- within_time_window_10
}


write.csv(geo_var_gps, "geo_var_gps_111.csv", row.names = FALSE)
##



}


radius_m <- 5000

# Repeat the above process for "battles_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(battles_fat)) {
    lat2 <- battles_fat[j, "latitude"]
    long2 <- battles_fat[j, "longitude"]
    date2 <- as.Date(battles_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "battles_fat_5"] <- within_time_window_5
}


radius_m <- 10000

# Repeat the above process for "battles_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(battles_fat)) {
    lat2 <- battles_fat[j, "latitude"]
    long2 <- battles_fat[j, "longitude"]
    date2 <- as.Date(battles_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "battles_fat_10"] <- within_time_window_10
}


radius_m <- 5000

# Repeat the above process for "violence_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(violence_fat)) {
    lat2 <- violence_fat[j, "latitude"]
    long2 <- violence_fat[j, "longitude"]
    date2 <- as.Date(violence_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "violence_fat_5"] <- within_time_window_5
}

radius_m <- 10000

# Repeat the above process for "violence_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(violence_fat)) {
    lat2 <- violence_fat[j, "latitude"]
    long2 <- violence_fat[j, "longitude"]
    date2 <- as.Date(violence_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "violence_fat_10"] <- within_time_window_10
}

radius_m <- 5000

# Repeat the above process for "boko_haram_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(boko_haram_fat)) {
    lat2 <- boko_haram_fat[j, "latitude"]
    long2 <- boko_haram_fat[j, "longitude"]
    date2 <- as.Date(boko_haram_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "boko_haram_fat_5"] <- within_time_window_5
}

radius_m <- 10000

# Repeat the above process for "boko_haram_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(boko_haram_fat)) {
    lat2 <- boko_haram_fat[j, "latitude"]
    long2 <- boko_haram_fat[j, "longitude"]
    date2 <- as.Date(boko_haram_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "boko_haram_fat_10"] <- within_time_window_10
}



radius_m <- 5000

# Repeat the above process for "fulani_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_5 <- FALSE
  for (j in 1:nrow(fulani_fat)) {
    lat2 <- fulani_fat[j, "latitude"]
    long2 <- fulani_fat[j, "longitude"]
    date2 <- as.Date(fulani_fat[j, "date"], format = "%
  
Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "fulani_fat_5"] <- within_time_window_5
}


radius_m <- 10000

# Repeat the above process for "fulani_fat"
for (i in 1:nrow(geo_var_gps)) {
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  within_time_window_10 <- FALSE
  for (j in 1:nrow(fulani_fat)) {
    lat2 <- fulani_fat[j, "latitude"]
    long2 <- fulani_fat[j, "longitude"]
    date2 <- as.Date(fulani_fat[j, "date"], format = "%
  
Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10 <- TRUE
      break
    }
  }
  
  geo_var_gps[i, "fulani_fat_10"] <- within_time_window_10
}




write.csv(geo_var_gps, "geo_var_gps_222x.csv", row.names = FALSE)




















################### |-|-Post Harvest - Household #####################

# Time to work on your dependent variable! So far you have decided to work on non-farm enterprises. And the data you need was collected in 9th section of the post-harvest surveys.
# ISSUE - Similar questions were asked in the post-planting surveys of firs two waves. And some questions refer to the "previous survey". Yes, I agree, making this kind of drastic change at the last wave is stupid. But they've done it anyway.. So I hope you've found a smart way to deal with it. So far you are just working with PH surveys of each wave but will that be enough?

# Post Harvest

ph9_w1 <-read.csv("sect9_harvestw1.csv")
ph9_w1$wave <- 2
names(ph9_w1)[names(ph9_w1) == "s9q1b"] <- "orid"
names(ph9_w1)[names(ph9_w1) == "s9q1a"] <- "s9q1b"
names(ph9_w1)[names(ph9_w1) == "s9q28h"] <- "s9q28j"
names(ph9_w1)[names(ph9_w1) == "s9q28g"] <- "s9q28i"
names(ph9_w1)[names(ph9_w1) == "s9q28f"] <- "s9q28h"
names(ph9_w1)[names(ph9_w1) == "s9q28e"] <- "s9q28g"
names(ph9_w1)[names(ph9_w1) == "s9q28d"] <- "s9q28f"

# ISSUE - some households don't have entid? wtf? :/


ph9_w2 <-read.csv("sect9_harvestw2.csv")
ph9_w2$wave <- 4
names(ph9_w2)[names(ph9_w2) == "s9q1c"] <- "orid"

ph9_w3 <-read.csv("sect9_harvestw3.csv")
ph9b_w3 <-read.csv("sect9b_harvestw3.csv")
#to be able to merge with the other w3
# ISSUE - there is no "ent_id" in ph9b_w3
ph9b_w3 <- ph9b_w3[, -which(names(ph9b_w3) %in% c("zone","state","lga","sector","ea"))]
ph9_w3 <- list(ph9_w3,ph9b_w3) %>% 
  reduce(left_join, by = c('hhid'='hhid')) 
ph9_w3$wave <- 6
names(ph9_w3)[names(ph9_w3) == "ent_id"] <- "entid"

ph_dependent <- bind_rows(ph9_w1, ph9_w2, ph9_w3)



names(ph_dependent)[names(ph_dependent) == "s9q1b"] <- "ind_code"
names(ph_dependent)[names(ph_dependent) == "s9q2"] <- "new"
names(ph_dependent)[names(ph_dependent) == "s9q3"] <- "status"
names(ph_dependent)[names(ph_dependent) == "s9q4"] <- "exit"
names(ph_dependent)[names(ph_dependent) == "s9q11"] <- "location"
names(ph_dependent)[names(ph_dependent) == "s9q11b"] <- "location_oth"
names(ph_dependent)[names(ph_dependent) == "s9q12"] <- "official"
names(ph_dependent)[names(ph_dependent) == "s9q14a"] <- "male"
names(ph_dependent)[names(ph_dependent) == "s9q14b"] <- "female"
names(ph_dependent)[names(ph_dependent) == "s9q15a"] <- "capital"
names(ph_dependent)[names(ph_dependent) == "s9q16"] <- "credit_formal_attempt"
names(ph_dependent)[names(ph_dependent) == "s9q17"] <- "credit_formal_get"
names(ph_dependent)[names(ph_dependent) == "s9q18"] <- "credit_use"
names(ph_dependent)[names(ph_dependent) == "s9q19a"] <- "credit_source_1"
names(ph_dependent)[names(ph_dependent) == "s9q19b"] <- "credit_source_2"
names(ph_dependent)[names(ph_dependent) == "s9q20"] <- "credit_amount"
names(ph_dependent)[names(ph_dependent) == "s9q24"] <- "value_capital"
names(ph_dependent)[names(ph_dependent) == "s9q27"] <- "value_sale"
names(ph_dependent)[names(ph_dependent) == "s9q28a"] <- "cost_wage"
names(ph_dependent)[names(ph_dependent) == "s9q28b"] <- "cost_inventory"
names(ph_dependent)[names(ph_dependent) == "s9q28c"] <- "cost_transport"
names(ph_dependent)[names(ph_dependent) == "s9q28d"] <- "cost_fuel"
names(ph_dependent)[names(ph_dependent) == "s9q28e"] <- "cost_maintenance"
names(ph_dependent)[names(ph_dependent) == "s9q28f"] <- "cost_insurance"
names(ph_dependent)[names(ph_dependent) == "s9q28g"] <- "cost_rent"
names(ph_dependent)[names(ph_dependent) == "s9q28h"] <- "cost_interest"
names(ph_dependent)[names(ph_dependent) == "s9q28i"] <- "cost_raw"
names(ph_dependent)[names(ph_dependent) == "s9q28j"] <- "cost_other"
names(ph_dependent)[names(ph_dependent) == "s9q29a"] <- "constraint_growth_1"
names(ph_dependent)[names(ph_dependent) == "s9q29b"] <- "constraint_growth_2"
names(ph_dependent)[names(ph_dependent) == "s9q29c"] <- "constraint_growth_3"
names(ph_dependent)[names(ph_dependent) == "s9q30a"] <- "constraint_operation_1"
names(ph_dependent)[names(ph_dependent) == "s9q30b"] <- "constraint_operation_2"
names(ph_dependent)[names(ph_dependent) == "s9q30c"] <- "constraint_operation_3"

# delete rows
# ph_dependent <- ph_dependent[, -c(12:22)]









# Post Planting

pp6_w1 <-read.csv("sect6_plantingw1.csv")
pp6_w1$wave <- 1
pp6_w1 <- pp6_w1[, -which(names(pp6_w1) %in% c("s6q1"))]
names(pp6_w1)[names(pp6_w1) == "s6q2"] <- "s6q1a"

pp6_w2 <-read.csv("sect6_plantingw2.csv")
pp6_w2 <- pp6_w2[, -which(names(pp6_w2) %in% c("s6q4b"))]
pp6_w2$wave <- 3



##

names(pp6_w1)[names(pp6_w1) == "s6q2"] <- "ind_code"



names(pp6_w1)[names(pp6_w1) == "s6q7"] <- "location"
names(pp6_w1)[names(pp6_w1) == "s6q9"] <- "official"
names(pp6_w1)[names(pp6_w1) == "s6q11a"] <- "male"
names(pp6_w1)[names(pp6_w1) == "s6q11b"] <- "female"
names(pp6_w1)[names(pp6_w1) == "s6q12a"] <- "capital"
names(pp6_w1)[names(pp6_w1) == "s6q13"] <- "credit_formal_attempt"
names(pp6_w1)[names(pp6_w1) == "s6q14"] <- "credit_formal_get"
names(pp6_w1)[names(pp6_w1) == "s6q15"] <- "credit_use"
names(pp6_w1)[names(pp6_w1) == "s6q16a"] <- "credit_source_1"
names(pp6_w1)[names(pp6_w1) == "s6q16b"] <- "credit_source_2"
names(pp6_w1)[names(pp6_w1) == "s6q17"] <- "credit_amount"
names(pp6_w1)[names(pp6_w1) == "s6q21"] <- "value_capital"
names(pp6_w1)[names(pp6_w1) == "s6q24"] <- "value_sale"
names(pp6_w1)[names(pp6_w1) == "s6q25a"] <- "cost_wage"
names(pp6_w1)[names(pp6_w1) == "s6q25b"] <- "cost_inventory"
names(pp6_w1)[names(pp6_w1) == "s6q25c"] <- "cost_transport"


names(pp6_w1)[names(pp6_w1) == "s6q25d"] <- "cost_insurance"
names(pp6_w1)[names(pp6_w1) == "s6q25e"] <- "cost_rent"
names(pp6_w1)[names(pp6_w1) == "s6q25f"] <- "cost_interest"
names(pp6_w1)[names(pp6_w1) == "s6q25g"] <- "cost_raw"
names(pp6_w1)[names(pp6_w1) == "s6q25h"] <- "cost_other"


##

names(pp6_w2)[names(pp6_w2) == "s6q1b"] <- "orid"
names(pp6_w2)[names(pp6_w2) == "s6q1a"] <- "ind_code"
names(pp6_w2)[names(pp6_w2) == "s6q1b"] <- "new"
names(pp6_w2)[names(pp6_w2) == "s6q3"] <- "status"
names(pp6_w2)[names(pp6_w2) == "s6q4"] <- "exit"
names(pp6_w2)[names(pp6_w2) == "s6q12"] <- "location"
names(pp6_w2)[names(pp6_w2) == "s6q12b"] <- "location_oth"
names(pp6_w2)[names(pp6_w2) == "s6q13"] <- "official"
names(pp6_w2)[names(pp6_w2) == "s6q15a"] <- "male"
names(pp6_w2)[names(pp6_w2) == "s6q15b"] <- "female"
names(pp6_w2)[names(pp6_w2) == "s6q16a"] <- "capital"
names(pp6_w2)[names(pp6_w2) == "s6q17"] <- "credit_formal_attempt"
names(pp6_w2)[names(pp6_w2) == "s6q18"] <- "credit_formal_get"
names(pp6_w2)[names(pp6_w2) == "s6q19"] <- "credit_use"
names(pp6_w2)[names(pp6_w2) == "s6q20a"] <- "credit_source_1"
names(pp6_w2)[names(pp6_w2) == "s6q20b"] <- "credit_source_2"
names(pp6_w2)[names(pp6_w2) == "s6q21"] <- "credit_amount"
names(pp6_w2)[names(pp6_w2) == "s6q27"] <- "value_capital"
names(pp6_w2)[names(pp6_w2) == "s6q30"] <- "value_sale"
names(pp6_w2)[names(pp6_w2) == "s6q31a"] <- "cost_wage"
names(pp6_w2)[names(pp6_w2) == "s6q31b"] <- "cost_inventory"
names(pp6_w2)[names(pp6_w2) == "s6q31c"] <- "cost_transport"
names(pp6_w2)[names(pp6_w2) == "s6q31d"] <- "cost_fuel"
names(pp6_w2)[names(pp6_w2) == "s6q31e"] <- "cost_maintenance"
names(pp6_w2)[names(pp6_w2) == "s6q31f"] <- "cost_insurance"
names(pp6_w2)[names(pp6_w2) == "s6q31g"] <- "cost_rent"
names(pp6_w2)[names(pp6_w2) == "s6q31h"] <- "cost_interest"
names(pp6_w2)[names(pp6_w2) == "s6q31i"] <- "cost_raw"
names(pp6_w2)[names(pp6_w2) == "s6q31j"] <- "cost_other"
names(pp6_w2)[names(pp6_w2) == "s6q32a"] <- "constraint_growth_1"
names(pp6_w2)[names(pp6_w2) == "s6q32b"] <- "constraint_growth_2"
names(pp6_w2)[names(pp6_w2) == "s6q32c"] <- "constraint_growth_3"
names(pp6_w2)[names(pp6_w2) == "s6q33a"] <- "constraint_operation_1"
names(pp6_w2)[names(pp6_w2) == "s6q33b"] <- "constraint_operation_2"
names(pp6_w2)[names(pp6_w2) == "s6q33c"] <- "constraint_operation_3"


pp_dependent <- bind_rows(pp6_w1, pp6_w2)
dependent_ent <- bind_rows(ph_dependent, pp_dependent)

dependent_ent <- dependent_ent[, -c(13:22, 26:31, 35:37, 43, 45:49, 51:52, 64:72, 82:163)]

dependent_ent$size <- dependent_ent$male + dependent_ent$female





############ Our DEPENDENT_Ent is ready. now let's organize our variables

# status

dependent_ent$status[dependent_ent$status == 1] <- "Currently Operating"
dependent_ent$status[dependent_ent$status == 2] <- "Closed, Permanently"
dependent_ent$status[dependent_ent$status == 3] <- "Closed, Temporarily"
dependent_ent$status[dependent_ent$status == 4] <- "Closed, Seasonally"

# exit
# Answers to the "exit" quesyion differs between the waves. So you did some changes. Most importantly you put "conflict" from the last wave into "security".

dependent_ent$exit[dependent_ent$exit == 1] <- "Legal problems"
dependent_ent$exit[dependent_ent$exit == 2] <- "Could not obtain inputs"
dependent_ent$exit[dependent_ent$exit == 3] <- "Lack of demand"
dependent_ent$exit[dependent_ent$exit == 4] <- "Low profit"
dependent_ent$exit[dependent_ent$exit == 5] <- "Could not obtain credit"
dependent_ent$exit[dependent_ent$exit == 6] <- "Too much debt"
dependent_ent$exit[dependent_ent$exit == 7] <- "Security issues"
dependent_ent$exit[dependent_ent$exit == 8] <- "Other"
dependent_ent$exit[dependent_ent$exit == 9] <- "Other"
dependent_ent$exit[dependent_ent$exit == 10] <- "Security issues"

# location

dependent_ent$location[dependent_ent$location == 1] <- "Home (inside residence)"
dependent_ent$location[dependent_ent$location == 2] <- "Home (outside residence)"
dependent_ent$location[dependent_ent$location == 3] <- "Industrial side"
dependent_ent$location[dependent_ent$location == 4] <- "Traditional market"
dependent_ent$location[dependent_ent$location == 5] <- "Comercial area shop"
dependent_ent$location[dependent_ent$location == 6] <- "Roadside"
dependent_ent$location[dependent_ent$location == 7] <- "Other fixed place"
dependent_ent$location[dependent_ent$location == 8] <- "Mobile"
dependent_ent$location[dependent_ent$location == 9] <- "Other"

# official
# You made this binary. If officially registered "1", if not "0"

dependent_ent$official[dependent_ent$official == 2] <- 0

# capital
# Answers are slightly different in the wave 1. It doesn't separate cooperative assoc and remit from abroad. You combined two options in others too but you will check if you need to separate them later.

dependent_ent$capital[dependent_ent$capital == 1 & dependent_ent$wave != 1] <- "Household savings"
dependent_ent$capital[dependent_ent$capital == 2 & dependent_ent$wave != 1] <- "NGO support"
dependent_ent$capital[dependent_ent$capital == 3 & dependent_ent$wave != 1] <- "Loan from bank (commercial, micro finance, credit union)"
dependent_ent$capital[dependent_ent$capital == 4 & dependent_ent$wave != 1] <- "Money lender"
dependent_ent$capital[dependent_ent$capital == 5 & dependent_ent$wave != 1] <- "Esusu/Adashi"
dependent_ent$capital[dependent_ent$capital == 6 & dependent_ent$wave != 1] <- "Other loans"
dependent_ent$capital[dependent_ent$capital == 7 & dependent_ent$wave != 1] <- "District/Town association support"
dependent_ent$capital[dependent_ent$capital == 8 & dependent_ent$wave != 1] <- "Cooperative/trade association & Remittances from abroad"
dependent_ent$capital[dependent_ent$capital == 9 & dependent_ent$wave != 1] <- "Cooperative/trade association & Remittances from abroad"
dependent_ent$capital[dependent_ent$capital == 10 & dependent_ent$wave != 1] <- "Proceeds from family farm"
dependent_ent$capital[dependent_ent$capital == 11 & dependent_ent$wave != 1] <- "Church/Mosque assistance"
dependent_ent$capital[dependent_ent$capital == 12 & dependent_ent$wave != 1] <- "Proceeds from family non-enterprise"
dependent_ent$capital[dependent_ent$capital == 13 & dependent_ent$wave != 1] <- "Relatives/Friends"
dependent_ent$capital[dependent_ent$capital == 14 & dependent_ent$wave != 1] <- "Other"

dependent_ent$capital[dependent_ent$capital == 1 & dependent_ent$wave == 1] <- "Household savings"
dependent_ent$capital[dependent_ent$capital == 2 & dependent_ent$wave == 1] <- "NGO support"
dependent_ent$capital[dependent_ent$capital == 3 & dependent_ent$wave == 1] <- "Loan from bank (commercial, micro finance, credit union)"
dependent_ent$capital[dependent_ent$capital == 4 & dependent_ent$wave == 1] <- "Money lender"
dependent_ent$capital[dependent_ent$capital == 5 & dependent_ent$wave == 1] <- "Esusu/Adashi"
dependent_ent$capital[dependent_ent$capital == 6 & dependent_ent$wave == 1] <- "Other loans"
dependent_ent$capital[dependent_ent$capital == 7 & dependent_ent$wave == 1] <- "District/Town association support"
dependent_ent$capital[dependent_ent$capital == 8 & dependent_ent$wave == 1] <- "Cooperative/trade association & Remittances from abroad"
dependent_ent$capital[dependent_ent$capital == 9 & dependent_ent$wave == 1] <- "Proceeds from family farm"
dependent_ent$capital[dependent_ent$capital == 10 & dependent_ent$wave == 1] <- "Church/Mosque assistance"
dependent_ent$capital[dependent_ent$capital == 11 & dependent_ent$wave == 1] <- "Proceeds from family non-enterprise"
dependent_ent$capital[dependent_ent$capital == 12 & dependent_ent$wave == 1] <- "Relatives/Friends"
dependent_ent$capital[dependent_ent$capital == 13 & dependent_ent$wave == 1] <- "Other"

# credit_formal_attempt
# You made this binary. If there was an attempt "1", if not "0"

dependent_ent$credit_formal_attempt[dependent_ent$credit_formal_attempt == 2] <- 0

# credit_formal_get
# You made this binary. If got a formal credit "1", if not "0"

dependent_ent$credit_formal_get[dependent_ent$credit_formal_get == 2] <- 0

# credit_use
# You made this binary. If formal credit used "1", if not "0"

dependent_ent$credit_use[dependent_ent$credit_use == 2] <- 0

# credit_use
# You made this binary. If formal credit used "1", if not "0"

dependent_ent$credit_use[dependent_ent$credit_use == 2] <- 0

# credit_source_1

dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 1] <- "Bank (commercial, micro finance, credit union"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 2] <- "Money lender"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 3] <- "Esusu/Adashi"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 4] <- "Other loans"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 5] <- "Cooperative/Trade associations"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 6] <- "Relatives/Friends"
dependent_ent$credit_source_1[dependent_ent$credit_source_1 == 7] <- "Other"

dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 1] <- "Bank (commercial, micro finance, credit union"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 2] <- "Money lender"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 3] <- "Esusu/Adashi"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 4] <- "Other loans"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 5] <- "Cooperative/Trade associations"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 6] <- "Relatives/Friends"
dependent_ent$credit_source_2[dependent_ent$credit_source_2 == 7] <- "Other"

# constraint_growth_1

dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 11] <- "Electricity"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 12] <- "Electricity"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 13] <- "Electricity"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 21] <- "Telecommunications"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 22] <- "Telecommunications"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 23] <- "Telecommunications"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 31] <- "Water"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 32] <- "Water"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 33] <- "Water"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 41] <- "Postal services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 42] <- "Postal services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 43] <- "Postal services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 61] <- "Transportation"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 62] <- "Transportation"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 63] <- "Transportation"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 64] <- "Transportation"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 71] <- "Financial services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 72] <- "Financial services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 73] <- "Financial services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 74] <- "Financial services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 75] <- "Financial services"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 81] <- "Markets"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 82] <- "Markets"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 83] <- "Markets"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 91] <- "Government"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 92] <- "Government"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 93] <- "Government"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 101] <- "Safety"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 102] <- "Safety"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 111] <- "Technology"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 112] <- "Technology"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 113] <- "Technology"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 114] <- "Technology"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 121] <- "Registration & permits"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 122] <- "Registration & permits"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 123] <- "Registration & permits"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 131] <- "Taxation"
dependent_ent$constraint_growth_1[dependent_ent$constraint_growth_1 == 132] <- "Taxation"

dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 11] <- "Electricity"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 12] <- "Electricity"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 13] <- "Electricity"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 21] <- "Telecommunications"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 22] <- "Telecommunications"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 23] <- "Telecommunications"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 31] <- "Water"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 32] <- "Water"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 33] <- "Water"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 41] <- "Postal services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 42] <- "Postal services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 43] <- "Postal services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 61] <- "Transportation"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 62] <- "Transportation"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 63] <- "Transportation"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 64] <- "Transportation"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 71] <- "Financial services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 72] <- "Financial services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 73] <- "Financial services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 74] <- "Financial services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 75] <- "Financial services"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 81] <- "Markets"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 82] <- "Markets"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 83] <- "Markets"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 91] <- "Government"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 92] <- "Government"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 93] <- "Government"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 101] <- "Safety"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 102] <- "Safety"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 111] <- "Technology"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 112] <- "Technology"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 113] <- "Technology"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 114] <- "Technology"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 121] <- "Registration & permits"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 122] <- "Registration & permits"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 123] <- "Registration & permits"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 131] <- "Taxation"
dependent_ent$constraint_growth_2[dependent_ent$constraint_growth_2 == 132] <- "Taxation"

dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 11] <- "Electricity"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 12] <- "Electricity"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 13] <- "Electricity"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 21] <- "Telecommunications"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 22] <- "Telecommunications"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 23] <- "Telecommunications"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 31] <- "Water"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 32] <- "Water"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 33] <- "Water"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 41] <- "Postal services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 42] <- "Postal services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 43] <- "Postal services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 61] <- "Transportation"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 62] <- "Transportation"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 63] <- "Transportation"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 64] <- "Transportation"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 71] <- "Financial services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 72] <- "Financial services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 73] <- "Financial services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 74] <- "Financial services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 75] <- "Financial services"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 81] <- "Markets"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 82] <- "Markets"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 83] <- "Markets"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 91] <- "Government"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 92] <- "Government"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 93] <- "Government"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 101] <- "Safety"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 102] <- "Safety"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 111] <- "Technology"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 112] <- "Technology"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 113] <- "Technology"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 114] <- "Technology"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 121] <- "Registration & permits"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 122] <- "Registration & permits"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 123] <- "Registration & permits"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 131] <- "Taxation"
dependent_ent$constraint_growth_3[dependent_ent$constraint_growth_3 == 132] <- "Taxation"

# constraint_operation_1

dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 11] <- "Electricity"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 12] <- "Electricity"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 13] <- "Electricity"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 21] <- "Telecommunications"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 22] <- "Telecommunications"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 23] <- "Telecommunications"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 31] <- "Water"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 32] <- "Water"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 33] <- "Water"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 41] <- "Postal services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 42] <- "Postal services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 43] <- "Postal services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 61] <- "Transportation"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 62] <- "Transportation"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 63] <- "Transportation"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 64] <- "Transportation"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 71] <- "Financial services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 72] <- "Financial services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 73] <- "Financial services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 74] <- "Financial services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 75] <- "Financial services"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 81] <- "Markets"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 82] <- "Markets"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 83] <- "Markets"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 91] <- "Government"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 92] <- "Government"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 93] <- "Government"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 101] <- "Safety"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 102] <- "Safety"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 111] <- "Technology"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 112] <- "Technology"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 113] <- "Technology"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 114] <- "Technology"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 121] <- "Registration & permits"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 122] <- "Registration & permits"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 123] <- "Registration & permits"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 131] <- "Taxation"
dependent_ent$constraint_operation_1[dependent_ent$constraint_operation_1 == 132] <- "Taxation"

dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 11] <- "Electricity"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 12] <- "Electricity"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 13] <- "Electricity"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 21] <- "Telecommunications"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 22] <- "Telecommunications"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 23] <- "Telecommunications"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 31] <- "Water"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 32] <- "Water"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 33] <- "Water"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 41] <- "Postal services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 42] <- "Postal services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 43] <- "Postal services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 61] <- "Transportation"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 62] <- "Transportation"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 63] <- "Transportation"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 64] <- "Transportation"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 71] <- "Financial services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 72] <- "Financial services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 73] <- "Financial services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 74] <- "Financial services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 75] <- "Financial services"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 81] <- "Markets"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 82] <- "Markets"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 83] <- "Markets"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 91] <- "Government"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 92] <- "Government"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 93] <- "Government"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 101] <- "Safety"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 102] <- "Safety"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 111] <- "Technology"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 112] <- "Technology"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 113] <- "Technology"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 114] <- "Technology"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 121] <- "Registration & permits"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 122] <- "Registration & permits"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 123] <- "Registration & permits"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 131] <- "Taxation"
dependent_ent$constraint_operation_2[dependent_ent$constraint_operation_2 == 132] <- "Taxation"

dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 11] <- "Electricity"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 12] <- "Electricity"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 13] <- "Electricity"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 21] <- "Telecommunications"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 22] <- "Telecommunications"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 23] <- "Telecommunications"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 31] <- "Water"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 32] <- "Water"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 33] <- "Water"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 41] <- "Postal services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 42] <- "Postal services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 43] <- "Postal services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 61] <- "Transportation"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 62] <- "Transportation"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 63] <- "Transportation"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 64] <- "Transportation"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 71] <- "Financial services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 72] <- "Financial services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 73] <- "Financial services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 74] <- "Financial services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 75] <- "Financial services"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 81] <- "Markets"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 82] <- "Markets"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 83] <- "Markets"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 91] <- "Government"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 92] <- "Government"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 93] <- "Government"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 101] <- "Safety"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 102] <- "Safety"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 111] <- "Technology"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 112] <- "Technology"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 113] <- "Technology"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 114] <- "Technology"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 121] <- "Registration & permits"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 122] <- "Registration & permits"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 123] <- "Registration & permits"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 131] <- "Taxation"
dependent_ent$constraint_operation_3[dependent_ent$constraint_operation_3 == 132] <- "Taxation"

dependent_ent$zone[dependent_ent$zone == 1] <- "North Central"
dependent_ent$zone[dependent_ent$zone == 2] <- "North East"
dependent_ent$zone[dependent_ent$zone == 3] <- "North West"
dependent_ent$zone[dependent_ent$zone == 4] <- "South East"
dependent_ent$zone[dependent_ent$zone == 5] <- "South South"
dependent_ent$zone[dependent_ent$zone == 6] <- "South West"

dependent_ent$new[dependent_ent$new == 1] <- 0
dependent_ent$new[dependent_ent$new == 2] <- 1



##########################################################

dependent_ent$wave[dependent_ent$wave == 1] <- "14.Aug.10"
dependent_ent$wave[dependent_ent$wave == 2] <- "02.Mar.11"
dependent_ent$wave[dependent_ent$wave == 3] <- "28.Sep.12"
dependent_ent$wave[dependent_ent$wave == 4] <- "15.Mar.13"
dependent_ent$wave[dependent_ent$wave == 6] <- "27.Mar.16"

dependent_ent$wave <- as.Date(dependent_ent$wave, format = "%d.%b.%y")

names(dependent_ent)[names(dependent_ent) == "wave"] <- "date"



###################################### MERGE

dependent_ent <- list(dependent_ent,geo_var_gps) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date')) 

write.csv(dependent_ent, "dependent_ent.csv", row.names = FALSE)
write.csv(geo_var_gps, "geo_var_gps.csv", row.names = FALSE)














############################################








# Set radius in meters and time window in months
radius_m <- 20000
time_window_months <- 12


# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_20_fat <- FALSE
  for (j in 1:nrow(battles_fat)) {
    lat2 <- battles_fat[j, "latitude"]
    long2 <- battles_fat[j, "longitude"]
    date2 <- as.Date(battles_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_20_fat <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "within_time_window_20_fat"] <- within_time_window_20_fat
}

summary(geo_var_gps)




##########


# Set radius in meters and time window in months
radius_m <- 10000
time_window_months <- 12


# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_10_fat <- FALSE
  for (j in 1:nrow(battles_fat)) {
    lat2 <- battles_fat[j, "latitude"]
    long2 <- battles_fat[j, "longitude"]
    date2 <- as.Date(battles_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_10_fat <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "within_time_window_10_fat"] <- within_time_window_10_fat
}

summary(geo_var_gps)

##################


# Set radius in meters and time window in months
radius_m <- 5000
time_window_months <- 12



# Loop over all households in dataset_x
for (i in 1:nrow(geo_var_gps)) {
  # Get the household's latitude, longitude, and date of survey
  lat1 <- geo_var_gps[i, "latitude"]
  long1 <- geo_var_gps[i, "longitude"]
  date1 <- as.Date(geo_var_gps[i, "date"], format = "%Y-%m-%d")
  
  # Check if the household is within the radius of any conflict location that happened within the time window
  within_time_window_5_fat <- FALSE
  for (j in 1:nrow(battles_fat)) {
    lat2 <- battles_fat[j, "latitude"]
    long2 <- battles_fat[j, "longitude"]
    date2 <- as.Date(battles_fat[j, "date"], format = "%Y-%m-%d")
    if (is_within_time_window(lat1, long1, date1, lat2, long2, date2, radius_m, time_window_months)) {
      within_time_window_5_fat <- TRUE
      break
    }
  }
  
  # Add a column to dataset_x indicating if the household is within the radius of a conflict location that happened within the time window
  geo_var_gps[i, "within_time_window_5_fat"] <- within_time_window_5_fat
}

summary(geo_var_gps)

dependent_ent <- list(dependent_ent,geo_var_gps) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date')) 

write.csv(dependent_ent, "dependent_ent.csv", row.names = FALSE)
write.csv(geo_var_gps, "geo_var_gps.csv", row.names = FALSE)



###############





