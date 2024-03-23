setwd("/Users/evrencansari/Desktop/Thesis/DATA") 
rm(list=ls()) 
dir()



##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(purrr)
library(geosphere)
library(data.table)
library(sf)
library(tidyr)
library(lubridate)



options(max.print = 1000000)


####################################################


ACLED <-read.csv("2007-01-01-2023-02-01-Nigeria.csv")

ACLED$date <- as.Date(ACLED$event_date, format = "%d.%b.%y")

# Specify the start and end dates for filtering
# I used latest ans earliest survey dates

start_date <- as.Date("2007-01-01")
end_date <- as.Date("2016-06-01")




ACLED <- subset(ACLED, date >= start_date & date <= end_date)

conflict <- subset(ACLED, event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence", "Riots", "Protests"))

rm(ACLED)




###################################### LGA BASED

lga_data <- st_read("ngaadmbndaadm2osgof20170222.geojson")

geo_var_gps <-read.csv("geo_var_gps.csv")
geo_var_gps <- geo_var_gps[, c("hhid", "lga", "date", "wave", "latitude","longitude")]

# Assuming 'latitude' and 'longitude' are the columns with the location data
geo_var_gps_sf <- st_as_sf(geo_var_gps, coords = c("longitude", "latitude"), 
                           crs = st_crs(lga_data), agr = "constant")

# Perform the spatial join
geo_var_gps_sf <- st_join(geo_var_gps_sf, lga_data, join = st_within)
geo_var_gps_sf$admin2Name[geo_var_gps_sf$lga == 2810] <- "Ondo"



######################################



conflict <- conflict[, c("event_id_no_cnty", "admin1", "admin2", "location", "data_id", "fatalities", "date", "latitude","longitude", "event_type")]

#conflict_fat <- conflict[conflict$fatalities > 0, ]





###### LGA BASED


conflict_sf <- st_as_sf(conflict, coords = c("longitude", "latitude"), 
                        crs = st_crs(lga_data), agr = "constant")

# Assuming 'conflict_sf' is your spatial data frame (sf) with conflict events
conflict_sf <- st_join(conflict_sf, lga_data, join = st_within)


conflict_sf$admin2Name[conflict_sf$admin2 == "Burutu"] <- "Burutu"
conflict_sf$admin2Name[conflict_sf$admin2 == "Kala Balge"] <- "Kala/Balge"
conflict_sf$admin2Name[conflict_sf$admin2 == "Kala/Balge"] <- "Kala/Balge"
conflict_sf$admin2Name[conflict_sf$admin2 == "Michika"] <- "Michika"
conflict_sf$admin2Name[conflict_sf$admin2 == "Warri North"] <- "Warri North"
conflict_sf$admin2Name[conflict_sf$admin2 == "Mbo"] <- "Mbo"
conflict_sf$admin2Name[conflict_sf$admin2 == "Gwoza"] <- "Gwoza"
conflict_sf$admin2Name[conflict_sf$admin2 == "Boki"] <- "Boki"

#############








conflict <- as.data.frame(conflict_sf)
geo_var_gps <- as.data.frame(geo_var_gps_sf)









conflict <- conflict[, c("event_id_no_cnty", "fatalities", "admin2", "date.x", "event_type", "admin2Name")]
geo_var_gps <- geo_var_gps[, c("hhid", "date.x", "wave", "admin2Name")]


names(conflict)[names(conflict) == "date.x"] <- "date"
names(geo_var_gps)[names(geo_var_gps) == "date.x"] <- "date"



##################



conflict$date <- as.Date(conflict$date, format = "%Y-%m-%d")
geo_var_gps$date <- as.Date(geo_var_gps$date, format = "%Y-%m-%d")


write.csv(conflict, "conflictxxx.csv", row.names = FALSE)
write.csv(geo_var_gps, "geo_var_gpsxxx.csv", row.names = FALSE)


################# analysis




one_year <- as.difftime(365, units = "days")



################ conflict

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict exposure
    conflict_exposure = as.integer(any(conflict$admin2Name == admin2Name & 
                                         conflict$date >= (date - one_year) & 
                                         conflict$date < (date))),
    conflict_events = sum(conflict$admin2Name == admin2Name & 
                            conflict$date >= (date - one_year) & 
                            conflict$date < (date)),
    conflict_fatalities = sum(conflict$fatalities[conflict$admin2Name == admin2Name & 
                                                    conflict$date >= (date - one_year) & 
                                                    conflict$date < (date)])
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "conflict_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")

################ conflict lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict exposure
    conflict_exposureb = as.integer(any(conflict$admin2Name == admin2Name & 
                                          conflict$date >= (date - one_year - one_year) & 
                                          conflict$date < (date))),
    conflict_eventsb = sum(conflict$admin2Name == admin2Name & 
                             conflict$date >= (date - one_year - one_year) & 
                             conflict$date < (date)),
    conflict_fatalitiesb = sum(conflict$fatalities[conflict$admin2Name == admin2Name & 
                                                     conflict$date >= (date - one_year - one_year) & 
                                                     conflict$date < (date)])
  ) %>%
  ungroup()



################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ conflict lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict exposure
    conflict_exposurec = as.integer(any(conflict$admin2Name == admin2Name & 
                                          conflict$date >= (date - one_year - one_year - one_year) & 
                                          conflict$date < (date))),
    conflict_eventsc = sum(conflict$admin2Name == admin2Name & 
                             conflict$date >= (date - one_year - one_year - one_year) & 
                             conflict$date < (date)),
    conflict_fatalitiesc = sum(conflict$fatalities[conflict$admin2Name == admin2Name & 
                                                     conflict$date >= (date - one_year - one_year - one_year) & 
                                                     conflict$date < (date)])
  ) %>%
  ungroup()




################ fat conflict
conflict_fat <- conflict[conflict$fatalities > 0, ]


# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict_fat exposure
    fat_conflict_exposure = as.integer(any(conflict_fat$admin2Name == admin2Name & 
                                             conflict_fat$date >= (date - one_year) & 
                                             conflict_fat$date < (date))),
    fat_conflict_events = sum(conflict_fat$admin2Name == admin2Name & 
                                conflict_fat$date >= (date - one_year) & 
                                conflict_fat$date < (date)),
    fat_conflict_fatalities = sum(conflict_fat$fatalities[conflict_fat$admin2Name == admin2Name & 
                                                            conflict_fat$date >= (date - one_year) & 
                                                            conflict_fat$date < (date)])
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "conflict_fat_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")

################ conflict_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict_fat exposure
    fat_conflict_exposureb = as.integer(any(conflict_fat$admin2Name == admin2Name & 
                                              conflict_fat$date >= (date - one_year - one_year) & 
                                              conflict_fat$date < (date))),
    fat_conflict_eventsb = sum(conflict_fat$admin2Name == admin2Name & 
                                 conflict_fat$date >= (date - one_year - one_year) & 
                                 conflict_fat$date < (date)),
    fat_conflict_fatalitiesb = sum(conflict_fat$fatalities[conflict_fat$admin2Name == admin2Name & 
                                                             conflict_fat$date >= (date - one_year - one_year) & 
                                                             conflict_fat$date < (date)])
  ) %>%
  ungroup()


################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ conflict_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for conflict_fat exposure
    fat_conflict_exposurec = as.integer(any(conflict_fat$admin2Name == admin2Name & 
                                              conflict_fat$date >= (date - one_year - one_year - one_year) & 
                                              conflict_fat$date < (date))),
    fat_conflict_eventsc = sum(conflict_fat$admin2Name == admin2Name & 
                                 conflict_fat$date >= (date - one_year - one_year - one_year) & 
                                 conflict_fat$date < (date)),
    fat_conflict_fatalitiesc = sum(conflict_fat$fatalities[conflict_fat$admin2Name == admin2Name & 
                                                             conflict_fat$date >= (date - one_year - one_year - one_year) & 
                                                             conflict_fat$date < (date)])
  ) %>%
  ungroup()



# exposure different measures and time periods

geo_var_gps$fat_conflict_exposure_5 <- ifelse(geo_var_gps$fat_conflict_events >= 5, 1, 0)
geo_var_gps$fat_conflict_exposure_10 <- ifelse(geo_var_gps$fat_conflict_events >= 10, 1, 0)
geo_var_gps$fat_conflict_exposure_15 <- ifelse(geo_var_gps$fat_conflict_events >= 15, 1, 0)


geo_var_gps$fat_conflict_exposure_5b <- ifelse(geo_var_gps$fat_conflict_eventsb >= 5, 1, 0)
geo_var_gps$fat_conflict_exposure_10b <- ifelse(geo_var_gps$fat_conflict_eventsb >= 10, 1, 0)
geo_var_gps$fat_conflict_exposure_15b <- ifelse(geo_var_gps$fat_conflict_eventsb >= 15, 1, 0)

geo_var_gps$fat_conflict_exposure_5c <- ifelse(geo_var_gps$fat_conflict_eventsc >= 5, 1, 0)
geo_var_gps$fat_conflict_exposure_10c <- ifelse(geo_var_gps$fat_conflict_eventsc >= 10, 1, 0)
geo_var_gps$fat_conflict_exposure_15c <- ifelse(geo_var_gps$fat_conflict_eventsc >= 15, 1, 0)















################# Battles

battles <- subset(conflict, event_type %in% c("Battles"))


################ battles

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles exposure
    battles_events = sum(battles$admin2Name == admin2Name & 
                           battles$date >= (date - one_year) & 
                           battles$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "battles_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")

################ battles lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles exposure
    battles_eventsb = sum(battles$admin2Name == admin2Name & 
                            battles$date >= (date - one_year - one_year) & 
                            battles$date < (date))
  ) %>%
  ungroup()



################# 3 years


# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ battles lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles exposure
    battles_eventsbc = sum(battles$admin2Name == admin2Name & 
                             battles$date >= (date - one_year - one_year - one_year) & 
                             battles$date < (date))
  ) %>%
  ungroup()


#write.csv(geo_var_gps, "battles_lga_lagged6.csv", row.names = FALSE)


################ fat battles
battles_fat <- battles[battles$fatalities > 0, ]


# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles_fat exposure
    fat_battles_events = sum(battles_fat$admin2Name == admin2Name & 
                               battles_fat$date >= (date - one_year) & 
                               battles_fat$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "battles_fat_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")

################ battles_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles_fat exposure
    fat_battles_eventsb = sum(battles_fat$admin2Name == admin2Name & 
                                battles_fat$date >= (date - one_year - one_year) & 
                                battles_fat$date < (date))
  ) %>%
  ungroup()


################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ battles_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for battles_fat exposure
    fat_battles_eventsc = sum(battles_fat$admin2Name == admin2Name & 
                                battles_fat$date >= (date - one_year - one_year - one_year) & 
                                battles_fat$date < (date))
  ) %>%
  ungroup()






















################# violence

violence <- subset(conflict, event_type %in% c("Violence against civilians"))


################ violence

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence exposure
    violence_events = sum(violence$admin2Name == admin2Name & 
                            violence$date >= (date - one_year) & 
                            violence$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "violence_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ violence lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence exposure
    violence_eventsb = sum(violence$admin2Name == admin2Name & 
                             violence$date >= (date - one_year - one_year) & 
                             violence$date < (date))
  ) %>%
  ungroup()



################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ violence lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence exposure
    violence_eventsbc = sum(violence$admin2Name == admin2Name & 
                              violence$date >= (date - one_year - one_year - one_year) & 
                              violence$date < (date))
  ) %>%
  ungroup()


#write.csv(geo_var_gps, "violence_lga_lagged6.csv", row.names = FALSE)


################ fat violence
violence_fat <- violence[violence$fatalities > 0, ]


# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence_fat exposure
    fat_violence_events = sum(violence_fat$admin2Name == admin2Name & 
                                violence_fat$date >= (date - one_year) & 
                                violence_fat$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "violence_fat_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ violence_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence_fat exposure
    fat_violence_eventsb = sum(violence_fat$admin2Name == admin2Name & 
                                 violence_fat$date >= (date - one_year - one_year) & 
                                 violence_fat$date < (date))
  ) %>%
  ungroup()


################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ violence_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for violence_fat exposure
    fat_violence_eventsc = sum(violence_fat$admin2Name == admin2Name & 
                                 violence_fat$date >= (date - one_year - one_year - one_year) & 
                                 violence_fat$date < (date))
  ) %>%
  ungroup()









################# remote

remote <- subset(conflict, event_type %in% c("Explosions/Remote remote"))


################ remote

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote exposure
    remote_events = sum(remote$admin2Name == admin2Name & 
                          remote$date >= (date - one_year) & 
                          remote$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "remote_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ remote lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote exposure
    remote_eventsb = sum(remote$admin2Name == admin2Name & 
                           remote$date >= (date - one_year - one_year) & 
                           remote$date < (date))
  ) %>%
  ungroup()



################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ remote lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote exposure
    remote_eventsbc = sum(remote$admin2Name == admin2Name & 
                            remote$date >= (date - one_year - one_year - one_year) & 
                            remote$date < (date))
  ) %>%
  ungroup()


#write.csv(geo_var_gps, "remote_lga_lagged6.csv", row.names = FALSE)


################ fat remote
remote_fat <- remote[remote$fatalities > 0, ]


# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote_fat exposure
    fat_remote_events = sum(remote_fat$admin2Name == admin2Name & 
                              remote_fat$date >= (date - one_year) & 
                              remote_fat$date < (date))
  ) %>%
  ungroup()

#write.csv(geo_var_gps, "remote_fat_lga.csv", row.names = FALSE)



################# 2 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ remote_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote_fat exposure
    fat_remote_eventsb = sum(remote_fat$admin2Name == admin2Name & 
                               remote_fat$date >= (date - one_year - one_year) & 
                               remote_fat$date < (date))
  ) %>%
  ungroup()


################# 3 years



# Define the period of 12 months and 6 months as difftime objects
one_year <- as.difftime(365, units = "days")


################ remote_fat lag

# Add the exposure data to the household dataframe
geo_var_gps <- geo_var_gps %>%
  rowwise() %>%
  mutate(
    # Update the time window for remote_fat exposure
    fat_remote_eventsc = sum(remote_fat$admin2Name == admin2Name & 
                               remote_fat$date >= (date - one_year - one_year - one_year) & 
                               remote_fat$date < (date))
  ) %>%
  ungroup()







write.csv(geo_var_gps, "conflict_new.csv", row.names = FALSE)
