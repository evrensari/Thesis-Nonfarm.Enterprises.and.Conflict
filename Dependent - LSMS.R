setwd("/Users/evrencansari/Desktop/Thesis/DATA") 
rm(list=ls()) 
dir()


##############################LIBRARIES######################################### 
library(dplyr) 
library(psych) 
library(purrr)
library(geosphere)
library(data.table)
library(pushoverr)

options(max.print = 1000000)


####################################################


# Upload the Data

pp6_w1 <-read.csv("sect6_plantingw1.csv")
pp6_w2 <-read.csv("sect6_plantingw2.csv")
ph9_w1 <-read.csv("sect9_harvestw1.csv")
ph9_w2 <-read.csv("sect9_harvestw2.csv")
ph9_w3 <-read.csv("sect9_harvestw3.csv")
ph9b_w3 <-read.csv("sect9b_harvestw3.csv")

# To be able to merge ph9_w3 and ph9b_w3, delete common columns
# ISSUE - there is no "ent_id" in ph9b_w3

ph9b_w3 <- ph9b_w3[, -which(names(ph9b_w3) %in% c("zone","state","lga","sector","ea"))]
ph9_w3 <- list(ph9_w3,ph9b_w3) %>% 
  reduce(left_join, by = c('hhid'='hhid')) 

# Add relative waves

pp6_w1$wave <- 1
ph9_w1$wave <- 2
pp6_w2$wave <- 3
ph9_w2$wave <- 4
ph9_w3$wave <- 6

# Change column names

# ph9_w1

names(ph9_w1)[names(ph9_w1) == "s9q1b"] <- "orid"
names(ph9_w1)[names(ph9_w1) == "s9q1a"] <- "s9q1b"
names(ph9_w1)[names(ph9_w1) == "s9q28h"] <- "s9q28j"
names(ph9_w1)[names(ph9_w1) == "s9q28g"] <- "s9q28i"
names(ph9_w1)[names(ph9_w1) == "s9q28f"] <- "s9q28h"
names(ph9_w1)[names(ph9_w1) == "s9q28e"] <- "s9q28g"
names(ph9_w1)[names(ph9_w1) == "s9q28d"] <- "s9q28f"

# ph9_w2

names(ph9_w2)[names(ph9_w2) == "s9q1c"] <- "orid"

# ph9_w3

names(ph9_w3)[names(ph9_w3) == "ent_id"] <- "entid"

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


# pp6_w2

# This column has text-data, which is different from what we have in the previous data frame
pp6_w2 <- pp6_w2[, -which(names(pp6_w2) %in% c("s6q4b"))]

names(pp6_w2)[names(pp6_w2) == "s6q1b"] <- "orid"
names(pp6_w2)[names(pp6_w2) == "s6q1a"] <- "ind_code"
names(pp6_w2)[names(pp6_w2) == "s6q2"] <- "new"
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

# Merge Datasets

pp_dependent <- bind_rows(pp6_w1, pp6_w2)
dependent_ent <- bind_rows(ph_dependent, pp_dependent)

# Delete unnecessary columns (check the result of this code after use)
dependent_ent <- dependent_ent[, -c(13:22, 26:31, 35:37, 43, 45:49, 51:52, 64:72, 82:163)]

# Calculate total amount of household non-member workers
dependent_ent$size <- dependent_ent$male + dependent_ent$female





############ Insert the text information in the columns

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

# Insert the dates to relative waves

dependent_ent$wave[dependent_ent$wave == 1] <- "14.Aug.10"
dependent_ent$wave[dependent_ent$wave == 2] <- "02.Mar.11"
dependent_ent$wave[dependent_ent$wave == 3] <- "28.Sep.12"
dependent_ent$wave[dependent_ent$wave == 4] <- "15.Mar.13"
dependent_ent$wave[dependent_ent$wave == 6] <- "27.Mar.16"

# Change the date format

dependent_ent$wave <- as.Date(dependent_ent$wave, format = "%d.%b.%y")

names(dependent_ent)[names(dependent_ent) == "wave"] <- "date"

# Export data

write.csv(dependent_ent, "dependent_ent_READY.csv", row.names = FALSE)

# Merge with geovariables dataframe

# dependent_ent <- list(dependent_ent,geo_var_gps) %>% 
# reduce(left_join, by = c('hhid'='hhid','date'='date')) 










################### |-|-Post Harvest - Household #####################

# Time to work on your dependent variable! So far you have decided to work on non-farm enterprises. And the data you need was collected in 9th section of the post-harvest surveys.
# ISSUE - Similar questions were asked in the post-planting surveys of firs two waves. And some questions refer to the "previous survey". Yes, I agree, making this kind of drastic change at the last wave is stupid. But they've done it anyway.. So I hope you've found a smart way to deal with it. So far you are just working with PH surveys of each wave but will that be enough?

# Post Harvest





# ISSUE - some households don't have entid? wtf? :/

















# Post Planting






