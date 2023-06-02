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

##########################################

geo_var <- geo_var[, c("hhid","zone","state","sector","ea","lga","wave", "anntot_avg","dist_road","dist_road2","dist_admctr","dist_popcenter","dist_popcenter2","wetQ_avg","anntot_avg")]

write.csv(geo_var, "geo_var.csv", row.names = FALSE)





