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


# Dependent ENT


#################################################



# Upload the Data

pp6_w1 <-read.csv("sect6_plantingw1.csv")
pp6_w2 <-read.csv("sect6_plantingw2.csv")
ph9_w1 <-read.csv("sect9_harvestw1.csv")
ph9_w2 <-read.csv("sect9_harvestw2.csv")
ph9_w3 <-read.csv("sect9_harvestw3.csv")
ph9b_w3 <-read.csv("sect9b_harvestw3.csv")

# To be able to merge ph9_w3 and ph9b_w3, delete common columns
# ISSUE - there is no "ent_id" in ph9b_w3


# this is to merge two datasets from the same wave. However those questions, which are given in another excel in the last wave are not consıstent ın the previous surveys. basically there are multıble answers from same households. but the question was on the household itself. so each household should've got a unıque answer.
ph9b_w3 <- ph9b_w3[, -which(names(ph9b_w3) %in% c("zone","state","lga","sector","ea"))]
ph9_w3 <- list(ph9_w3,ph9b_w3) %>% 
  reduce(left_join, by = c('hhid'='hhid')) 




# Add relative waves

pp6_w1$wave <- 1
ph9_w1$wave <- 2
pp6_w2$wave <- 3
ph9_w2$wave <- 4
ph9_w3$wave <- 6

# Change column names according to the questionnaire

# ph9_w1

names(ph9_w1)[names(ph9_w1) == "s9q1b"] <- "orid"
names(ph9_w1)[names(ph9_w1) == "s9q1a"] <- "s9q1b"
names(ph9_w1)[names(ph9_w1) == "s9q5a"] <- "ent_own_a"
names(ph9_w1)[names(ph9_w1) == "s9q5b"] <- "ent_own_b"
names(ph9_w1)[names(ph9_w1) == "s9q6a"] <- "ent_man_a"
names(ph9_w1)[names(ph9_w1) == "s9q6b"] <- "ent_man_b"
names(ph9_w1)[names(ph9_w1) == "s9q7"] <- "ent_own_man_chg"
names(ph9_w1)[names(ph9_w1) == "s9q10"] <- "months_opr"
names(ph9_w1)[names(ph9_w1) == "s9q23a"] <- "ent_target"
names(ph9_w1)[names(ph9_w1) == "s9q28h"] <- "s9q28j"
names(ph9_w1)[names(ph9_w1) == "s9q28g"] <- "s9q28i"
names(ph9_w1)[names(ph9_w1) == "s9q28f"] <- "s9q28h"
names(ph9_w1)[names(ph9_w1) == "s9q28e"] <- "s9q28g"
names(ph9_w1)[names(ph9_w1) == "s9q28d"] <- "s9q28f"

# ph9_w2

names(ph9_w2)[names(ph9_w2) == "s9q1c"] <- "orid"
names(ph9_w2)[names(ph9_w2) == "s9q5a1"] <- "ent_own_a"
names(ph9_w2)[names(ph9_w2) == "s9q5a2"] <- "ent_own_b"
names(ph9_w2)[names(ph9_w2) == "s9q6a"] <- "ent_man_a"
names(ph9_w2)[names(ph9_w2) == "s9q6b"] <- "ent_man_b"
names(ph9_w2)[names(ph9_w2) == "s9q7"] <- "ent_own_man_chg"
names(ph9_w2)[names(ph9_w2) == "s9q10"] <- "months_opr"
names(ph9_w2)[names(ph9_w2) == "s9q23a1"] <- "ent_target"

# ph9_w3

names(ph9_w3)[names(ph9_w3) == "ent_id"] <- "entid"
names(ph9_w3)[names(ph9_w3) == "s9q5a1"] <- "ent_own_a"
names(ph9_w3)[names(ph9_w3) == "s9q5a2"] <- "ent_own_b"
names(ph9_w3)[names(ph9_w3) == "s9q6a"] <- "ent_man_a"
names(ph9_w3)[names(ph9_w3) == "s9q6b"] <- "ent_man_b"
names(ph9_w3)[names(ph9_w3) == "s9q7"] <- "ent_own_man_chg"

# Specify the columns you want to count 'X' values in
operational_months <- c('s9q10a', 's9q10b', 's9q10c', 's9q10d', 's9q10e', 's9q10f', 's9q10g', 's9q10h', 's9q10i', 's9q10j', 's9q10k', 's9q10l', 's9q10m', 's9q10n', 's9q10o')

# Use rowSums to count 'X' values in the specified columns for each row
ph9_w3$months_opr <- rowSums(ph9_w3[operational_months] == 'X', na.rm = TRUE)

names(ph9_w3)[names(ph9_w3) == "s9q23a1"] <- "ent_target"

# Merge Post Harvest
# To change the names at the same time you need to merge post harvest datasets

ph_dependent <- bind_rows(ph9_w1, ph9_w2, ph9_w3)

names(ph_dependent)[names(ph_dependent) == "s9q1b"] <- "ind_code"
names(ph_dependent)[names(ph_dependent) == "s9q2"] <- "new"
names(ph_dependent)[names(ph_dependent) == "s9q3"] <- "status"
names(ph_dependent)[names(ph_dependent) == "s9q4"] <- "exit"
names(ph_dependent)[names(ph_dependent) == "s9q11"] <- "location"
names(ph_dependent)[names(ph_dependent) == "s9q11b"] <- "location_oth"
names(ph_dependent)[names(ph_dependent) == "s9q12"] <- "official"

names(ph_dependent)[names(ph_dependent) == "s9q13a"] <- "hh_mem_paid_a"
names(ph_dependent)[names(ph_dependent) == "s9q13b"] <- "hh_mem_paid_b"
names(ph_dependent)[names(ph_dependent) == "s9q13c"] <- "hh_mem_paid_c"
names(ph_dependent)[names(ph_dependent) == "s9q13d"] <- "hh_mem_nonpaid_a"
names(ph_dependent)[names(ph_dependent) == "s9q13e"] <- "hh_mem_nonpaid_b"
names(ph_dependent)[names(ph_dependent) == "s9q13f"] <- "hh_mem_nonpaid_c"

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



# pp6_w1

# ISSUE
# Some columns have same data but different names.

#pp6_w1 <- pp6_w1[, -which(names(pp6_w1) %in% c("s6q1"))]



# Different names for same data


names(pp6_w1)[names(pp6_w1) == "s6q2"] <- "ind_code"
names(pp6_w1)[names(pp6_w1) == "s6q3a"] <- "ent_own_a"
names(pp6_w1)[names(pp6_w1) == "s6q3b"] <- "ent_own_b"
names(pp6_w1)[names(pp6_w1) == "s6q4a"] <- "ent_man_a"
names(pp6_w1)[names(pp6_w1) == "s6q4b"] <- "ent_man_b"
pp6_w1$ent_own_man_chg <- 1
names(pp6_w1)[names(pp6_w1) == "s6q6"] <- "months_opr"
names(pp6_w1)[names(pp6_w1) == "s6q7"] <- "location"
names(pp6_w1)[names(pp6_w1) == "s6q9"] <- "official"
names(pp6_w1)[names(pp6_w1) == "s6q10a"] <- "hh_mem_paid_a"
names(pp6_w1)[names(pp6_w1) == "s6q10b"] <- "hh_mem_paid_b"
names(pp6_w1)[names(pp6_w1) == "s6q10c"] <- "hh_mem_nonpaid_a"
names(pp6_w1)[names(pp6_w1) == "s6q10d"] <- "hh_mem_nonpaid_b"
names(pp6_w1)[names(pp6_w1) == "s6q11a"] <- "male"
names(pp6_w1)[names(pp6_w1) == "s6q11b"] <- "female"
names(pp6_w1)[names(pp6_w1) == "s6q12a"] <- "capital"
names(pp6_w1)[names(pp6_w1) == "s6q13"] <- "credit_formal_attempt"
names(pp6_w1)[names(pp6_w1) == "s6q14"] <- "credit_formal_get"
names(pp6_w1)[names(pp6_w1) == "s6q15"] <- "credit_use"
names(pp6_w1)[names(pp6_w1) == "s6q16a"] <- "credit_source_1"
names(pp6_w1)[names(pp6_w1) == "s6q16b"] <- "credit_source_2"
names(pp6_w1)[names(pp6_w1) == "s6q17"] <- "credit_amount"
names(pp6_w1)[names(pp6_w1) == "s6q20a"] <- "ent_target"
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


# pp6_w2

# This column has text-data, which is different from what we have in the previous data frame
pp6_w2 <- pp6_w2[, -which(names(pp6_w2) %in% c("s6q4b"))]

names(pp6_w2)[names(pp6_w2) == "s6q1b"] <- "orid"
names(pp6_w2)[names(pp6_w2) == "s6q1a"] <- "ind_code"
names(pp6_w2)[names(pp6_w2) == "s6q2"] <- "new"
names(pp6_w2)[names(pp6_w2) == "s6q3"] <- "status"
names(pp6_w2)[names(pp6_w2) == "s6q4"] <- "exit"
names(pp6_w2)[names(pp6_w2) == "s6q5a"] <- "ent_own_a"
names(pp6_w2)[names(pp6_w2) == "s6q5b"] <- "ent_own_b"
names(pp6_w2)[names(pp6_w2) == "s6q7a"] <- "ent_man_a"
names(pp6_w2)[names(pp6_w2) == "s6q7b"] <- "ent_man_b"
names(pp6_w2)[names(pp6_w2) == "s6q8"] <- "ent_own_man_chg"
names(pp6_w2)[names(pp6_w2) == "s6q11"] <- "months_opr"
names(pp6_w2)[names(pp6_w2) == "s6q12"] <- "location"
names(pp6_w2)[names(pp6_w2) == "s6q12b"] <- "location_oth"
names(pp6_w2)[names(pp6_w2) == "s6q13"] <- "official"
names(pp6_w2)[names(pp6_w2) == "s6q14a"] <- "hh_mem_paid_a"
names(pp6_w2)[names(pp6_w2) == "s6q14b"] <- "hh_mem_paid_b"
names(pp6_w2)[names(pp6_w2) == "s6q14c"] <- "hh_mem_paid_c"
names(pp6_w2)[names(pp6_w2) == "s6q14d"] <- "hh_mem_nonpaid_a"
names(pp6_w2)[names(pp6_w2) == "s6q14e"] <- "hh_mem_nonpaid_b"
names(pp6_w2)[names(pp6_w2) == "s6q14f"] <- "hh_mem_nonpaid_c"
names(pp6_w2)[names(pp6_w2) == "s6q15a"] <- "male"
names(pp6_w2)[names(pp6_w2) == "s6q15b"] <- "female"
names(pp6_w2)[names(pp6_w2) == "s6q16a"] <- "capital"
names(pp6_w2)[names(pp6_w2) == "s6q17"] <- "credit_formal_attempt"
names(pp6_w2)[names(pp6_w2) == "s6q18"] <- "credit_formal_get"
names(pp6_w2)[names(pp6_w2) == "s6q19"] <- "credit_use"
names(pp6_w2)[names(pp6_w2) == "s6q20a"] <- "credit_source_1"
names(pp6_w2)[names(pp6_w2) == "s6q20b"] <- "credit_source_2"
names(pp6_w2)[names(pp6_w2) == "s6q21"] <- "credit_amount"
names(pp6_w2)[names(pp6_w2) == "s6q24a"] <- "ent_target"
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

# Merge Datasets

pp_dependent <- bind_rows(pp6_w1, pp6_w2)
dependent_ent <- bind_rows(ph_dependent, pp_dependent)



# Delete unnecessary columns (check the result of this code after use)
#dependent_ent <- dependent_ent[, -c(13:22, 26:31, 35:37, 43, 45:49, 51:52, 64:72, 82:163)]
#dependent_ent <- dependent_ent %>% select(-contains("s9q"), -contains("s6q"))
dependent_ent <- dependent_ent %>% dplyr::select(-contains("s9q"), -contains("s6q"))





# Calculate total amount of household non-member workers (this gives the min size because there are NAs)
dependent_ent$ent_size <- dependent_ent$male + dependent_ent$female


# let's make non-NA version and call it the min size
dependent_ent <- dependent_ent %>%
  mutate(male_min = ifelse(is.na(male), 0, male),
         female_min = ifelse(is.na(female), 0, female))

dependent_ent$ent_size_min <- dependent_ent$male_min + dependent_ent$female_min



## HH member size



# Define the columns representing household members
hh_member_cols <- c('hh_mem_paid_a', 'hh_mem_paid_b', 'hh_mem_paid_c', 
                    'hh_mem_nonpaid_a', 'hh_mem_nonpaid_b', 'hh_mem_nonpaid_c')

# Replace 'NA' and 0 with NA for easier counting
dependent_ent[hh_member_cols] <- lapply(dependent_ent[hh_member_cols], function(x) ifelse(x == 0, NA, x))
dependent_ent[hh_member_cols] <- lapply(dependent_ent[hh_member_cols], function(x) ifelse(x == 'NA', NA, x))

# Count non-NA values in each row for the specified columns and add as a new column
dependent_ent$ent_size_mem_min <- rowSums(!is.na(dependent_ent[hh_member_cols]), na.rm = TRUE)

dependent_ent$ent_size_tot_min <- dependent_ent$ent_size_min + dependent_ent$ent_size_mem_min


## Total Min Cost



# Define the columns representing household members
cost_cols <- c('cost_wage', 'cost_inventory', 'cost_transport', 
               'cost_insurance', 'cost_rent', 'cost_interest', 'cost_raw', 'cost_other', 'cost_fuel',	'cost_maintenance')

# Replace 'NA' and 0 with NA for easier counting
dependent_ent[cost_cols] <- lapply(dependent_ent[cost_cols], function(x) ifelse(x == 0, NA, x))
dependent_ent[cost_cols] <- lapply(dependent_ent[cost_cols], function(x) ifelse(x == 'NA', NA, x))

# Sum the values in each row for the specified columns and add as a new column
dependent_ent$total_cost_min <- rowSums(dependent_ent[cost_cols], na.rm = TRUE)


# NA management for value of capital and sales

dependent_ent$value_capital[dependent_ent$value_capital == 99999998] <- NA
dependent_ent$value_capital[dependent_ent$value_capital == 99998] <- NA
dependent_ent$value_capital[dependent_ent$value_capital == 9998] <- NA

dependent_ent$value_sale[dependent_ent$value_sale == 99999998] <- NA
dependent_ent$value_sale[dependent_ent$value_sale == 99998] <- NA
dependent_ent$value_sale[dependent_ent$value_sale == 9998] <- NA

dependent_ent$cost_rent[dependent_ent$cost_rent == 999998] <- NA
dependent_ent$cost_rent[dependent_ent$cost_rent == 9998] <- NA

dependent_ent$cost_transport[dependent_ent$cost_transport == 999998] <- NA
dependent_ent$cost_transport[dependent_ent$cost_transport == 9998] <- NA

dependent_ent$cost_inventory[dependent_ent$cost_inventory == 99999998] <- NA
dependent_ent$cost_inventory[dependent_ent$cost_inventory == 999998] <- NA
dependent_ent$cost_inventory[dependent_ent$cost_inventory == 9998] <- NA

dependent_ent$cost_inventory[dependent_ent$cost_inventory == 99999998] <- NA
dependent_ent$cost_inventory[dependent_ent$cost_inventory == 999998] <- NA
dependent_ent$cost_inventory[dependent_ent$cost_inventory == 9998] <- NA

dependent_ent$cost_raw[dependent_ent$cost_raw == 99999998] <- NA
dependent_ent$cost_raw[dependent_ent$cost_raw == 999998] <- NA

dependent_ent$cost_insurance[dependent_ent$cost_insurance == 999998] <- NA

dependent_ent$cost_interest[dependent_ent$cost_interest == 9999998] <- NA
dependent_ent$cost_interest[dependent_ent$cost_interest == 999998] <- NA

############ Insert the text information in the columns




# Load the necessary library
library(dplyr)

# Assuming your dataframe is named df

# Add the 'type_of_business' column

dependent_ent <- dependent_ent %>%
  mutate(ind_name = case_when(
    ind_code == "01" ~ "Crop and animal production, hunting and related service activities",
    ind_code == "02" ~ "Forestry and logging",
    ind_code == "03" ~ "Fishing and aquaculture",
    ind_code == "05" ~ "Mining of coal and lignite",
    ind_code == "06" ~ "Extraction of crude petroleum and natural gas",
    ind_code == "07" ~ "Mining of metal ores",
    ind_code == "08" ~ "Other mining and quarrying",
    ind_code == "09" ~ "Mining support service activities",
    ind_code == "10" ~ "Manufacture of food products",
    ind_code == "11" ~ "Manufacture of beverages",
    ind_code == "12" ~ "Manufacture of tobacco products",
    ind_code == "13" ~ "Manufacture of textiles",
    ind_code == "14" ~ "Manufacture of wearing apparel",
    ind_code == "15" ~ "Manufacture of leather and related products",
    ind_code == "16" ~ "Manufacture of wood and products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",
    ind_code == "17" ~ "Manufacture of paper and paper products",
    ind_code == "18" ~ "Printing and reproduction of recorded media",
    ind_code == "19" ~ "Manufacture of coke and refined petroleum products",
    ind_code == "20" ~ "Manufacture of chemicals and chemical products",
    ind_code == "21" ~ "Manufacture of basic pharmaceutical products and pharmaceutical preparations",
    ind_code == "22" ~ "Manufacture of rubber and plastic products",
    ind_code == "23" ~ "Manufacture of other non-metallic mineral products",
    ind_code == "24" ~ "Manufacture of basic metals",
    ind_code == "25" ~ "Manufacture of fabricated metal products, except machinery and equipment",
    ind_code == "26" ~ "Manufacture of computer, electronic and optical products",
    ind_code == "27" ~ "Manufacture of electrical equipment",
    ind_code == "28" ~ "Manufacture of machinery and equipment",
    ind_code == "29" ~ "Manufacture of motor vehicles, trailers and semi-trailers",
    ind_code == "30" ~ "Manufacture of other transport equipment",
    ind_code == "31" ~ "Manufacture of furniture",
    ind_code == "32" ~ "Other manufacturing",
    ind_code == "33" ~ "Repair and installation of machinery and equipment",
    ind_code == "35" ~ "Electricity, gas, steam and air conditioning supply",
    ind_code == "36" ~ "Water collection, treatment and supply",
    ind_code == "37" ~ "Sewerage",
    ind_code == "38" ~ "Waste collection, treatment and disposal activities; materials recovery",
    ind_code == "39" ~ "Remediation activities and other waste management services",
    ind_code == "41" ~ "Construction of buildings",
    ind_code == "42" ~ "Civil engineering",
    ind_code == "43" ~ "Specialized construction activities",
    ind_code == "45" ~ "Wholesale and retail trade and repair of motor vehicles and motorcycles",
    ind_code == "46" ~ "Wholesale trade, except of motor vehicles and motorcycles",
    ind_code == "47" ~ "Retail trade, except of motor vehicles and motor cycles",
    ind_code == "49" ~ "Land transport and transport via pipe lines",
    ind_code == "50" ~ "Water transport",
    ind_code == "51" ~ "Air transport",
    ind_code == "52" ~ "Warehousing and support activities for transportation",
    ind_code == "53" ~ "Postal and courier activities",
    ind_code == "55" ~ "Accommodation",
    ind_code == "56" ~ "Food and beverage service activities",
    ind_code == "58" ~ "Publishing activities",
    ind_code == "59" ~ "Motion picture, video and television programme production, sound recording and music publishing activities",
    ind_code == "60" ~ "Programming and broadcasting activities",
    ind_code == "61" ~ "Telecommunications",
    ind_code == "62" ~ "Computer programming, consultancy and related activities",
    ind_code == "63" ~ "Information service activities",
    ind_code == "64" ~ "Financial service activities, except insurance and pension funding",
    ind_code == "65" ~ "Insurance, reinsurance and pension funding, except compulsory social security",
    ind_code == "66" ~ "Activities auxiliary to financial service and insurance activities",
    ind_code == "68" ~ "Real estate activities",
    ind_code == "69" ~ "Legal and accounting activities",
    ind_code == "70" ~ "Activities of head offices; management consultancy activities",
    ind_code == "71" ~ "Architectural and engineering activities; technical testing and analysis",
    ind_code == "72" ~ "Scientific research and development",
    ind_code == "73" ~ "Advertising and market research",
    ind_code == "74" ~ "Other professional, scientific and technical activities",
    ind_code == "75" ~ "Veterinary activities",
    ind_code == "77" ~ "Rental and leasing activities",
    ind_code == "78" ~ "Employment activities",
    ind_code == "79" ~ "Travel agency, tour operator, reservation service and related activities",
    ind_code == "80" ~ "Security and investigation activities",
    ind_code == "81" ~ "Services to buildings and landscape activities",
    ind_code == "82" ~ "Office administrative, office support and other business support activities",
    ind_code == "84" ~ "Public administration and defense; compulsory social security",
    ind_code == "85" ~ "Education",
    ind_code == "86" ~ "Human health activities",
    ind_code == "87" ~ "Residential care activities",
    ind_code == "88" ~ "Social work activities without accommodation",
    ind_code == "90" ~ "Creative, arts and entertainment activities",
    ind_code == "91" ~ "Libraries, archives, museums and other cultural activities",
    ind_code == "92" ~ "Gambling and betting activities",
    ind_code == "93" ~ "Sports activities and amusement and recreation activities",
    ind_code == "94" ~ "Activities of membership organizations",
    ind_code == "95" ~ "Repair of computers and personal and household goods",
    ind_code == "96" ~ "Other personal service activities",
    ind_code == "97" ~ "Activities of households as employers of domestic personnel",
    ind_code == "98" ~ "Undifferentiated goods- and services-producing activities of private households for own use",
    ind_code == "99" ~ "Activities of extraterritorial organizations and bodies",
    TRUE ~ NA_character_  # This will set NA for any unmatched code
  ))


# Add the 'category_of_business' column
dependent_ent <- dependent_ent %>%
  mutate(ind_cat = case_when(
    ind_code %in% c("1", "2", "3") ~ "A - Agriculture, forestry and fishing",
    ind_code %in% c("5", "6", "7", "8", "9") ~ "B - Mining and quarrying",
    ind_code %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33") ~ "C - Manufacturing",
    ind_code == "35" ~ "D - Electricity, gas, steam and air conditioning supply",
    ind_code %in% c("36", "37", "38", "39") ~ "E - Water supply; sewerage, waste management and remediation activities",
    ind_code %in% c("41", "42", "43") ~ "F - Construction",
    ind_code %in% c("45", "46", "47") ~ "G - Wholesale and retail trade; repair of motor vehicles and motorcycles",
    ind_code %in% c("49", "50", "51", "52", "53") ~ "H - Transportation and storage",
    ind_code %in% c("55", "56") ~ "I - Accommodation and food service activities",
    ind_code %in% c("58", "59", "60", "61", "62", "63") ~ "J - Information and communication",
    ind_code %in% c("64", "65", "66") ~ "K - Financial and insurance activities",
    ind_code == "68" ~ "L - Real estate activities",
    ind_code %in% c("69", "70", "71", "72", "73", "74", "75") ~ "M - Professional, scientific and technical activities",
    ind_code %in% c("77", "78", "79", "80", "81", "82") ~ "N - Administrative and support service activities",
    ind_code == "84" ~ "O - Public administration and defense; compulsory social security",
    ind_code == "85" ~ "P - Education",
    ind_code %in% c("86", "87", "88") ~ "Q - Human health and social work activities",
    ind_code %in% c("90", "91", "92", "93") ~ "R - Arts, entertainment and recreation",
    ind_code %in% c("94", "95", "96") ~ "S - Other service activities",
    ind_code %in% c("97", "98") ~ "T - Activities of households as employers; undifferentiated goods and services-producing activities of households for own use",
    ind_code == "99" ~ "U - Activities of extraterritorial organizations and bodies",
    TRUE ~ NA_character_  # This will set NA for any unmatched category
  ))



####


library(dplyr)

# Assuming 'dependent_ent' is your dataframe and 'ind_code' contains industry codes as characters
dependent_ent <- dependent_ent %>%
  mutate(ind_high = case_when(
    ind_cat %in% c("A - Agriculture, forestry and fishing") ~ "1",
    ind_cat %in% c("B - Mining and quarrying", "C - Manufacturing", "D - Electricity, gas, steam and air conditioning supply", "E - Water supply; sewerage, waste management and remediation activities") ~ "2",
    ind_cat %in% c("F - Construction") ~ "3",
    ind_cat %in% c("G - Wholesale and retail trade; repair of motor vehicles and motorcycles") ~ "4a",
    ind_cat %in% c("H - Transportation and storage", "I - Accommodation and food service activities") ~ "4b",
    ind_cat %in% c("J - Information and communication", "K - Financial and insurance activities", "L - Real estate activities", "M - Professional, scientific and technical activities", "N - Administrative and support service activities") ~ "5_6_7_8",
    ind_cat %in% c("O - Public administration and defense; compulsory social security", "P - Education", "Q - Human health and social work activities", "R - Arts, entertainment and recreation", "S - Other service activities", "T - Activities of households as employers; undifferentiated goods and services-producing activities of households for own use", "U - Activities of extraterritorial organizations and bodies") ~ "9_10",
    TRUE ~ NA_character_  # This will set NA for any unmatched category
  ))


#####

# Add the 'category_of_business' column
dependent_ent <- dependent_ent %>%
  mutate(ind_cat_let = case_when(
    ind_code %in% c("1", "2", "3") ~ "A",
    ind_code %in% c("5", "6", "7", "8", "9") ~ "B",
    ind_code %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33") ~ "C",
    ind_code == "35" ~ "D",
    ind_code %in% c("36", "37", "38", "39") ~ "E",
    ind_code %in% c("41", "42", "43") ~ "F",
    ind_code %in% c("45", "46", "47") ~ "G",
    ind_code %in% c("49", "50", "51", "52", "53") ~ "H",
    ind_code %in% c("55", "56") ~ "I",
    ind_code %in% c("58", "59", "60", "61", "62", "63") ~ "J",
    ind_code %in% c("64", "65", "66") ~ "K",
    ind_code == "68" ~ "L",
    ind_code %in% c("69", "70", "71", "72", "73", "74", "75") ~ "M",
    ind_code %in% c("77", "78", "79", "80", "81", "82") ~ "N",
    ind_code == "84" ~ "O",
    ind_code == "85" ~ "P",
    ind_code %in% c("86", "87", "88") ~ "Q",
    ind_code %in% c("90", "91", "92", "93") ~ "R",
    ind_code %in% c("94", "95", "96") ~ "S",
    ind_code %in% c("97", "98") ~ "T",
    ind_code == "99" ~ "U",
    TRUE ~ NA_character_  # This will set NA for any unmatched category
  ))





# target (whom is the product for)

dependent_ent$ent_target[dependent_ent$ent_target == 1] <- "Final Consumers"
dependent_ent$ent_target[dependent_ent$ent_target == 2] <- "Traders"
dependent_ent$ent_target[dependent_ent$ent_target == 3] <- "Other Small Businesses"
dependent_ent$ent_target[dependent_ent$ent_target == 4] <- "Large Established Businesses"
dependent_ent$ent_target[dependent_ent$ent_target == 5] <- "Institutions (Schools, Hospitals, Govt Ministries)"
dependent_ent$ent_target[dependent_ent$ent_target == 6] <- "Export"
dependent_ent$ent_target[dependent_ent$ent_target == 7] <- "Manufacturers"
dependent_ent$ent_target[dependent_ent$ent_target == 8] <- "Other"






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


dependent_ent$`home` <- ifelse(dependent_ent$location %in% c("Home (inside residence)", "Home (outside residence)") , 1, 
                               ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`industrial` <- ifelse(dependent_ent$location == "Industrial side", 1, 
                                     ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`traditional` <- ifelse(dependent_ent$location == "Traditional market", 1, 
                                      ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`comercial` <- ifelse(dependent_ent$location == "Comercial area shop", 1, 
                                    ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`roadside` <- ifelse(dependent_ent$location == "Roadside", 1, 
                                   ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`mobile` <- ifelse(dependent_ent$location == "Mobile", 1, 
                                 ifelse(is.na(dependent_ent$location), NA, 0))

dependent_ent$`other` <- ifelse(dependent_ent$location  %in% c("Other", "Other fixed place"), 1, 
                                ifelse(is.na(dependent_ent$location), NA, 0))




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

# Insert the dates to relative waves

dependent_ent$date[dependent_ent$wave == 1] <- "14.Aug.10"
dependent_ent$date[dependent_ent$wave == 2] <- "02.Mar.11"
dependent_ent$date[dependent_ent$wave == 3] <- "28.Sep.12"
dependent_ent$date[dependent_ent$wave == 4] <- "15.Mar.13"
dependent_ent$date[dependent_ent$wave == 6] <- "27.Mar.16"

# Change the date format

dependent_ent$date <- as.Date(dependent_ent$date, format = "%d.%b.%y")

rm(ph9_w1, ph9_w2, ph9_w3, ph9b_w3, pp6_w1, pp6_w2, ph_dependent, pp_dependent)

# Export data

#write.csv(dependent_ent, "dependent_ent_oct.csv", row.names = FALSE)





############CONFLICT TREATMENT


conflict <-read.csv("conflict.csv")


###########



if (FALSE) {
  # This code will not run
  result <- computeSomething(data)
  
  
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
  conflict_vars <- c('battles_fat_5', 'battles_fat_10', 'battles_fat_20', 
                     'explosion_fat_5', 'explosion_fat_10', 'explosion_fat_20', 
                     'violence_fat_5', 'violence_fat_10', 'violence_fat_20', 
                     'boko_haram_fat_5', 'boko_haram_fat_10', 'boko_haram_fat_20', 
                     'fulani_fat_5', 'fulani_fat_10', 'fulani_fat_20', 
                     'conflict_fat_5', 'conflict_fat_10', 'conflict_fat_20')
  
  # Apply the function to the data
  conflict <- create_previous_wave_columns(conflict, conflict_vars)
  
  write.csv(conflict, "conflict.csv", row.names = FALSE)
}





dependent_ent_a  <- list(dependent_ent ,conflict) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave'))


rm(conflict)





######### NA MANAGEMENT - FOR CURRENTLY OPERATING

# this code written according to the questionnaire and interview manual

dependent_ent_a <- dependent_ent_a %>%
  mutate(status = ifelse(is.na(status) & new == 1 & is.na(exit) & value_sale != 0 | total_cost_min != 0, "Currently Operating", status))

dependent_ent_a <- dependent_ent_a %>%
  mutate(status = ifelse(is.na(status) & !is.na(exit), "Closed", status))

# ASSUMPTION - if there is positive sale last month, the enterprise is operating

dependent_ent_a <- dependent_ent_a %>%
  mutate(status = ifelse(is.na(status)  & value_sale != 0 | total_cost_min != 0, "Currently Operating", status))







## let's create binaries for status


# Assuming dependent_ent_a is already loaded as a data frame

# Manually create and fill the binary columns for each unique status
# Column for "Currently Operating"
dependent_ent_a$`Currently_Operating` <- ifelse(dependent_ent_a$status == "Currently Operating", 1, 
                                                ifelse(is.na(dependent_ent_a$status), NA, 0))

# Column for "Closed, Temporarily"
dependent_ent_a$`Closed_Temporarily` <- ifelse(dependent_ent_a$status == "Closed, Temporarily", 1, 
                                               ifelse(is.na(dependent_ent_a$status), NA, 0))

# Column for "Closed, Permanently"
dependent_ent_a$`Closed_Permanently` <- ifelse(dependent_ent_a$status == "Closed, Permanently", 1, 
                                               ifelse(is.na(dependent_ent_a$status), NA, 0))

# Column for "Closed, Seasonally"
dependent_ent_a$`Closed_Seasonally` <- ifelse(dependent_ent_a$status == "Closed, Seasonally", 1, 
                                              ifelse(is.na(dependent_ent_a$status), NA, 0))

# Column for "Closed, Permanently" or "Closed, Temporarily"
dependent_ent_a$Closed_Perm_Temp <- ifelse(dependent_ent_a$status == "Closed, Temporarily" | 
                                             dependent_ent_a$status == "Closed, Permanently", 1, 0)




# Column for "In Operation" (Currently Operating, Closed Temporarily, Closed Seasonally)
dependent_ent_a$in_operation <- ifelse(
  dependent_ent_a$status == "Currently Operating" | 
    dependent_ent_a$status == "Closed, Temporarily" | 
    dependent_ent_a$status == "Closed, Seasonally", 
  1, 
  0
)

dependent_ent_a$in_operation <- ifelse(dependent_ent_a$wave == 1, 1, dependent_ent_a$in_operation)




## FIXING EXIT

# Assuming your dataframe is named 'your_dataframe' and the columns are named 'status' and 'exit'
dependent_ent <- dependent_ent %>%
  mutate(exit = ifelse(status == "Currently Operating", NA, exit))





## let's create binaries for EXIT

#Other
#NA
#Low profit
#Could not obtain credit
#Too much debt
#Lack of demand
#Legal problems
#Could not obtain inputs
#Security issues


# Assuming dependent_ent_a is already loaded as a data frame

# Manually create and fill the binary columns for each unique exit

# Column for "Other"
dependent_ent_a$`exit_other` <- ifelse(dependent_ent_a$exit == "Other", 1, 
                                       ifelse(is.na(dependent_ent_a$exit), NA, 0))

# Column for "Could not obtain credit"
dependent_ent_a$`exit_credit` <- ifelse(dependent_ent_a$exit == "Could not obtain credit", 1, 
                                        ifelse(is.na(dependent_ent_a$exit), NA, 0))

# Column for "Too much debt"
dependent_ent_a$`exit_debt` <- ifelse(dependent_ent_a$exit == "Too much debt", 1, 
                                      ifelse(is.na(dependent_ent_a$exit), NA, 0))


# Column for "Lack of demand"
dependent_ent_a$`exit_demand` <- ifelse(dependent_ent_a$exit == "Lack of demand", 1, 
                                        ifelse(is.na(dependent_ent_a$exit), NA, 0))

# Column for "Legal problems"
dependent_ent_a$`exit_legal` <- ifelse(dependent_ent_a$exit == "Legal problems", 1, 
                                       ifelse(is.na(dependent_ent_a$exit), NA, 0))

# Column for "Could not obtain inputs"
dependent_ent_a$`exit_input` <- ifelse(dependent_ent_a$exit == "Could not obtain inputs", 1, 
                                       ifelse(is.na(dependent_ent_a$exit), NA, 0))

# Column for "Security issues"
dependent_ent_a$`exit_security` <- ifelse(dependent_ent_a$exit == "Security issues", 1, 
                                          ifelse(is.na(dependent_ent_a$exit), NA, 0))














######### create ent level panel data. let's gooooooo ###############


dependent_ent_a$ent_id_ind <- paste(dependent_ent_a$hhid, dependent_ent_a$ind_code, sep="-")
dependent_ent_a$ent_id_ind_own <- paste(dependent_ent_a$hhid, dependent_ent_a$ind_code, dependent_ent_a$ent_own_a, sep="-")

# ADD CONTROL VARIABLES FOR THE OWNER OF THE ENTERPRISE
# GENDER, MARITAL STATUS, AGE



# Upload the Data
# These datasets include the list of households and individuals who were surveyed during the relative wave

pp1_w1 <-read.csv("sect1_plantingw1.csv")
ph1_w1 <-read.csv("sect1_harvestw1.csv")
pp1_w2 <-read.csv("sect1_plantingw2.csv")
ph1_w2 <-read.csv("sect1_harvestw2.csv")
ph1_w3 <-read.csv("sect1_harvestw3.csv")

# Add relative waves

pp1_w1$wave <- 1
ph1_w1$wave <- 2
pp1_w2$wave <- 3
ph1_w2$wave <- 4
ph1_w3$wave <- 6


names(pp1_w1)[names(pp1_w1) == "s1q8"] <- "marital_own"
names(ph1_w1)[names(ph1_w1) == "s1q7"] <- "marital_own"
names(pp1_w2)[names(pp1_w2) == "s1q8"] <- "marital_own"
names(ph1_w2)[names(ph1_w2) == "s1q7"] <- "marital_own"
names(ph1_w3)[names(ph1_w3) == "s1q7"] <- "marital_own"

names(pp1_w1)[names(pp1_w1) == "s1q4"] <- "age_own"
names(ph1_w1)[names(ph1_w1) == "s1q4"] <- "age_own"
names(pp1_w2)[names(pp1_w2) == "s1q6"] <- "age_own"
names(ph1_w2)[names(ph1_w2) == "s1q4"] <- "age_own"
names(ph1_w3)[names(ph1_w3) == "s1q4"] <- "age_own"

names(pp1_w1)[names(pp1_w1) == "s1q2"] <- "gender_own"
names(ph1_w1)[names(ph1_w1) == "s1q2"] <- "gender_own"
names(pp1_w2)[names(pp1_w2) == "s1q2"] <- "gender_own"
names(ph1_w2)[names(ph1_w2) == "s1q2"] <- "gender_own"
names(ph1_w3)[names(ph1_w3) == "s1q2"] <- "gender_own"

pp1_w1 <- pp1_w1[, c("hhid", "indiv", "marital_own","age_own","gender_own")]
ph1_w1 <- ph1_w1[, c("hhid", "indiv", "marital_own","age_own","gender_own")]
pp1_w2 <- pp1_w2[, c("hhid", "indiv", "marital_own","age_own","gender_own")]
ph1_w2 <- ph1_w2[, c("hhid", "indiv", "marital_own","age_own","gender_own")]
ph1_w3 <- ph1_w3[, c("hhid", "indiv", "marital_own","age_own","gender_own")]




pp1_w1$wave <- 1
ph1_w1$wave <- 2
pp1_w2$wave <- 3
ph1_w2$wave <- 4
ph1_w3$wave <- 6



control_indiv_dem <- bind_rows(pp1_w1, ph1_w1, pp1_w2, ph1_w2, ph1_w3)

control_indiv_dem$age_own[control_indiv_dem$age_own < 1 | control_indiv_dem$age_own > 120] <- NA

rm(pp1_w1, ph1_w1, pp1_w2, ph1_w2, ph1_w3)




# 1 is male and 0 is female.
control_indiv_dem$gender_own[control_indiv_dem$gender_own == 2] <- 0


# making this variablble binary. 1= married (monogamous) , 2= married (polugamous), 3= informal union
control_indiv_dem$marital_own <- ifelse(control_indiv_dem$marital_own <= 3, 1, 0)



dependent_ent_a <- list(dependent_ent_a, control_indiv_dem) %>% 
  reduce(left_join, by = c('hhid'='hhid','ent_own_a'='indiv','wave'='wave')) 


rm(control_indiv_dem)




# EDUCATION

# education

# ISSUE questions weren't asked in the post harvest surveys during waves 1 & 2. so i used post planting responses

pp2_w1 <-read.csv("sect2_plantingw1.csv")
ph2a_w1 <-read.csv("sect2a_harvestw1.csv")
pp2_w2 <-read.csv("sect2_plantingw2.csv")
ph2a_w2 <-read.csv("sect2a_harvestw2.csv")
ph2_w3 <-read.csv("sect2_harvestw3.csv")



#

names(pp2_w1)[names(pp2_w1) == "s2q3"] <- "read_own"
names(ph2a_w1)[names(ph2a_w1) == "s2aq5"] <- "read_own"
names(pp2_w2)[names(pp2_w2) == "s2q4"] <- "read_own"
names(ph2a_w2)[names(ph2a_w2) == "s2aq5"] <- "read_own"
names(ph2_w3)[names(ph2_w3) == "s2aq5"] <- "read_own"

#

names(pp2_w1)[names(pp2_w1) == "s2q7"] <- "level_own"
names(ph2a_w1)[names(ph2a_w1) == "s2aq9"] <- "level_own"
names(pp2_w2)[names(pp2_w2) == "s2q8"] <- "level_own"
names(ph2a_w2)[names(ph2a_w2) == "s2aq9"] <- "level_own"
names(ph2_w3)[names(ph2_w3) == "s2aq9"] <- "level_own"

#


names(pp2_w1)[names(pp2_w1) == "s2q8"] <- "qual_own"
names(ph2a_w1)[names(ph2a_w1) == "s2aq10"] <- "qual_own"
names(pp2_w2)[names(pp2_w2) == "s2q9"] <- "qual_own"
names(ph2a_w2)[names(ph2a_w2) == "s2aq10"] <- "qual_own"
names(ph2_w3)[names(ph2_w3) == "s2aq10"] <- "qual_own"




#############


edu_w1 <- pp2_w1[, c("hhid", "indiv", "read_own")]


ph2a_w1 <- ph2a_w1[, c("hhid", "indiv", "read_own")]
ph2a_w1 <- ph2a_w1[complete.cases(ph2a_w1$read_own), ]
edu_w2 <- bind_rows(edu_w1, ph2a_w1)

all_duplicates <- duplicated(edu_w2[c("hhid", "indiv")]) | 
  duplicated(edu_w2[c("hhid", "indiv")], fromLast = TRUE)
edu_w2 <- edu_w2[!all_duplicates, ]




#

edu_w3 <- pp2_w2[, c("hhid", "indiv", "read_own")]


ph2a_w2 <- ph2a_w2[, c("hhid", "indiv", "read_own")]
ph2a_w2 <- ph2a_w2[complete.cases(ph2a_w2$read_own), ]
edu_w4 <- bind_rows(edu_w3, ph2a_w2)

all_duplicates <- duplicated(edu_w4[c("hhid", "indiv")]) | 
  duplicated(edu_w4[c("hhid", "indiv")], fromLast = TRUE)
edu_w4 <- edu_w4[!all_duplicates, ]

#
edu_w6 <- ph2_w3[, c("hhid", "indiv", "read_own")]






rm(pp2_w1, ph2a_w1, pp2_w2, ph2a_w2, ph2_w3, edu_w1a, edu_w2a, edu_w3a, edu_w4a, edu_w6a)


edu_w1$wave <- 1
edu_w2$wave <- 2
edu_w3$wave <- 3
edu_w4$wave <- 4
edu_w6$wave <- 6

control_indiv_edu <- bind_rows(edu_w1, edu_w2, edu_w3, edu_w4, edu_w6)

rm(edu_w1, edu_w2, edu_w3, edu_w4, edu_w6)




# 1 is "can read and write" and 0 is "can't".
control_indiv_edu$read_own[control_indiv_edu$read_own == 2] <- 0



dependent_ent_a <- list(dependent_ent_a, control_indiv_edu) %>% 
  reduce(left_join, by = c('hhid'='hhid','ent_own_a'='indiv','wave'='wave')) 

rm(control_indiv_edu)


#write.csv(dependent_ent_a, "dependent_ent_a.csv", row.names = FALSE)



####### HH & GEO CONTROLS

geo_var_gps <-read.csv("geo_var_gps.csv")

geo_var_gps$geo <- 1

geo_var_gps <- geo_var_gps[, !(names(geo_var_gps) %in% c("latitude", "longitude", "zone", "state", "sector", "ea", "lga", "date", "marital_head", "gender_head", "age_head", "read_head"))]




dependent_ent_a <- list(dependent_ent_a, geo_var_gps) %>% 
  reduce(left_join, by = c('hhid'='hhid', 'wave'='wave'))

rm(geo_var_gps)

###### AGRICULTURAL CONTROLS

agr_control <-read.csv("agr_control.csv")
agr_control$agriculture_survey <- 1

dependent_ent_a <- list(dependent_ent_a, agr_control) %>% 
  reduce(left_join, by = c('hhid'='hhid','wave'='wave')) 

dependent_ent_a$agriculture_survey <- ifelse(is.na(dependent_ent_a$agriculture_survey), 0, dependent_ent_a$agriculture_survey)

dependent_ent_a <- dependent_ent_a %>%
  mutate(across(c(new, land_cultivate, land_own, land_min_size),
                ~ if_else(agriculture_survey == 0 & is.na(.), 0, .)))


### Season control

dependent_ent_a$post_harvest <- ifelse(dependent_ent_a$wave %in% c(2, 4, 6), 1, 0)


## operational months fixing the numbers. (there are numbers exceeding the maximum)

#X - SurveyMonths - Question - Month Difference
#1 -	Sep.10 -	past year -	12
#2 -	Mar.11 -	last interview -	7
#3 -	Oct.12 -	last interview -	20
#4 - 	Mar.13 -	last interview -	6
#6 -	Mar.16 -	past year -	12



dependent_ent_a <- dependent_ent_a %>%
  mutate(months_opr = case_when(
    wave == 1 & months_opr > 12 ~ NA_real_,
    wave == 2 & months_opr > 7  ~ NA_real_,
    wave == 3 & months_opr > 20 ~ NA_real_,
    wave == 4 & months_opr > 6  ~ NA_real_,
    wave == 6 & months_opr > 15 ~ NA_real_,
    TRUE ~ months_opr
  ))



dependent_ent_a <- dependent_ent_a %>%
  mutate(months_opr = case_when(
    wave == 1 & !is.na(months_opr) & months_opr > 12 ~ NA_real_,
    wave == 1 & !is.na(months_opr) & months_opr != 0 ~ months_opr / 12,  # Operation for wave 1
    
    wave == 2 & !is.na(months_opr) & months_opr > 7  ~ NA_real_,
    wave == 2 & !is.na(months_opr) & months_opr != 0 ~ months_opr / 7,   # Operation for wave 2
    
    wave == 3 & !is.na(months_opr) & months_opr > 20 ~ NA_real_,
    wave == 3 & !is.na(months_opr) & months_opr != 0 ~ months_opr / 20,  # Operation for wave 3
    
    wave == 4 & !is.na(months_opr) & months_opr > 6  ~ NA_real_,
    wave == 4 & !is.na(months_opr) & months_opr != 0 ~ months_opr / 6,   # Operation for wave 4
    
    wave == 6 & !is.na(months_opr) & months_opr > 15 ~ NA_real_,
    wave == 6 & !is.na(months_opr) & months_opr != 0 ~ months_opr / 15,  # Operation for wave 6
    
    TRUE ~ months_opr  # Default case, no change
  ))





# put values in capital and sales when it is not operating and NA




dependent_ent_a <- dependent_ent_a %>%
  mutate(value_sale = if_else(Currently_Operating == 0 & is.na(value_sale), 0, value_sale),
         value_capital = if_else(Currently_Operating == 0 & is.na(value_capital), 0, value_capital))









## only necessary columns

dependent_ent <- dependent_ent_a[, c("ent_id_ind", "ent_id_ind_own", "wave", "lga", "sector", "hhid", "entid", "ent_own_a","ind_code", "orid", "new", "status", "exit",
                                     "exit_other", "exit_credit", "exit_security", "exit_input", "exit_debt", "exit_demand", "exit_legal",
                                     "cost_inventory", "cost_transport", "cost_fuel", "cost_maintenance", "cost_insurance", "cost_rent", "cost_interest", "cost_raw", "cost_other",
                                     "ent_own_a", "location", "official", "credit_formal_attempt", "in_operation",
                                     "credit_formal_get", "credit_use", "ent_target", "months_opr", "value_capital", 
                                     "value_sale", "ent_size", "ent_size_min","total_cost_min", "ind_name", "ind_cat", "ind_cat_let", 
                                     "ind_high", "home", "industrial", "traditional", 
                                     "comercial", "roadside", "mobile", "other", 
                                     "conflict_events", "conflict_fatalities", "conflict_exposure", 
                                     "conflict_eventsb", "conflict_fatalitiesb", "conflict_exposureb",
                                     "conflict_eventsc", "conflict_fatalitiesc", "conflict_exposurec",
                                     "conflict_exposurea", "conflict_eventsa", "conflict_fatalitiesa",
                                     "fat_conflict_events", "fat_conflict_fatalities", "fat_conflict_exposure",
                                     "fat_conflict_eventsb", "fat_conflict_fatalitiesb", "fat_conflict_exposureb",
                                     "fat_conflict_eventsc", "fat_conflict_fatalitiesc", "fat_conflict_exposurec",
                                     "fat_conflict_exposurea", "fat_conflict_eventsa", "fat_conflict_fatalitiesa",
                                     "fat_battles_events", "fat_violence_events", "fat_remote_events",
                                     "fat_battles_eventsb", "fat_violence_eventsb", "fat_remote_eventsb",
                                     "fat_battles_eventsc", "fat_violence_eventsc", "fat_remote_eventsc",
                                     "fat_conflict_exposure_5", "fat_conflict_exposure_10", "fat_conflict_exposure_15", 
                                     "fat_conflict_exposure_5b", "fat_conflict_exposure_10b", "fat_conflict_exposure_15b", 
                                     "fat_conflict_exposure_5c", "fat_conflict_exposure_10c", "fat_conflict_exposure_15c",
                                     "Currently_Operating", 
                                     "marital_own", "age_own", "gender_own", "read_own", 
                                     "anntot_avg", "dist_road", "dist_road2", "dist_admctr", "dist_popcenter", 
                                     "dist_popcenter2", "hh_size", "indiv", "housing_own", "housing_outer", 
                                     "housing_roof", "housing_floor", "housing_electricity", "land_cultivate", 
                                     "land_own", "land_min_size", "agriculture_survey", "post_harvest")]




#write.csv(dependent_ent, "dependent_ent_0.csv", row.names = FALSE)












####### CREATING THE PANEL DATA



# QUINTILE

# Assuming 'dependent_ent' is your dataframe
dependent_ent <- dependent_ent %>%
  # Group by 'wave' to perform calculations within each wave
  group_by(wave) %>%
  # Calculate quintiles within each group
  mutate(
    quintile_capital = ntile(value_capital, 5),
    quintile_sale = ntile(value_sale, 5)
  ) %>%
  # Ungroup to avoid accidental grouping effects in further data manipulation
  ungroup()


dependent_ent <- dependent_ent %>%
  mutate(NA_ind = as.numeric(grepl("NA$", ent_id_ind)))


# duplicate check

dependent_ent <- dependent_ent %>%
  # Group by the relevant identifiers for duplication
  group_by(ent_id_ind, wave) %>%
  # Add a new column to indicate duplicates
  mutate(is_duplicate = if_else(n() > 1, 1, 0)) %>%
  # Ungroup to avoid accidental grouping effects later
  ungroup()


# adjust for the owner

dependent_ent <- dependent_ent %>%
  mutate(ent_id_ind_own = if_else(is_duplicate == 0 & NA_ind == 0, 
                                  paste(ent_id_ind, "-0", sep = ""), 
                                  paste(ent_id_ind, "-", ent_own_a, sep = "")))

dependent_ent <- dependent_ent %>%
  mutate(NA_own = as.numeric(grepl("NA$", ent_id_ind_own)))


# duplicate check

dependent_ent <- dependent_ent %>%
  # Group by the relevant identifiers for duplication
  group_by(ent_id_ind_own, wave) %>%
  # Add a new column to indicate duplicates
  mutate(is_duplicate_own = if_else(n() > 1, 1, 0)) %>%
  # Ungroup to avoid accidental grouping effects later
  ungroup()



# adjust for the sales quintile

dependent_ent <- dependent_ent %>%
  mutate(ent_id_ind_qui_s = if_else(is_duplicate_own == 0 & NA_own == 0, 
                                    paste(ent_id_ind_own, "-0", sep = ""), 
                                    paste(ent_id_ind_own, "-", quintile_sale, sep = "")))

dependent_ent <- dependent_ent %>%
  mutate(NA_qui_s = as.numeric(grepl("NA$", ent_id_ind_qui_s)))



# duplicate check

dependent_ent <- dependent_ent %>%
  # Group by the relevant identifiers for duplication
  group_by(ent_id_ind_qui_s, wave) %>%
  # Add a new column to indicate duplicates
  mutate(is_duplicate_qui_s = if_else(n() > 1, 1, 0)) %>%
  # Ungroup to avoid accidental grouping effects later
  ungroup()


# adjust for the capital quintile

dependent_ent <- dependent_ent %>%
  mutate(ent_id_ind_qui_c = if_else(is_duplicate_qui_s == 0 & NA_qui_s == 0, 
                                    paste(ent_id_ind_qui_s, "-0", sep = ""), 
                                    paste(ent_id_ind_qui_s, "-", quintile_capital, sep = "")))


dependent_ent <- dependent_ent %>%
  mutate(NA_qui_c = as.numeric(grepl("NA$", ent_id_ind_qui_c)))


# duplicate check

dependent_ent <- dependent_ent %>%
  # Group by the relevant identifiers for duplication
  group_by(ent_id_ind_qui_c, wave) %>%
  # Add a new column to indicate duplicates
  mutate(is_duplicate_qui_c = if_else(n() > 1, 1, 0)) %>%
  # Ungroup to avoid accidental grouping effects later
  ungroup()



write.csv(dependent_ent, "dependent_ent_a.csv", row.names = FALSE)



