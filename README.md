# LSMS

setwd("/Users/evrencansari/Desktop/Thesis/DATA")
rm(list=ls())
dir()


##############################LIBRARIES#########################################
library(dplyr)
library(psych)
library(purrr)

options(max.print = 1000000)

################### |-Uploading DATA ###################################################

################### |-|-Post Harvest - Household #####################

PH9W1 <-read.csv("sect9_harvestw1.csv")

PH9W2 <-read.csv("sect9_harvestw2.csv")

PH9W3 <-read.csv("sect9_harvestw3.csv")

PH9bW3 <-read.csv("sect9b_harvestw3.csv")

#to be able to merge with the other w3
PH9bW3 <- PH9bW3[, -which(names(PH9bW3) %in% c("zone","state","lga","sector","ea"))]

################### |-|-Geo Var - Household  #####################

GH_W1 <-read.csv("nga_householdgeovariables_y1.csv")
GH_W1 <- GH_W1[, -which(names(GH_W1) %in% c("zone","state","lga","sector","ea"))]

GH_W2 <-read.csv("nga_householdgeovars_y2.csv")
GH_W2 <- GH_W2[, -which(names(GH_W2) %in% c("zone","state","lga","sector","ea"))]

GH_W3 <-read.csv("nga_householdgeovars_y3.csv")
GH_W3 <- GH_W3[, -which(names(GH_W3) %in% c("zone","state","lga","sector","ea"))]

################### |-Merging DATA - Leftjoin ###################################################

W1 <- list( PH9W1, GH_W1) %>% 
  reduce(left_join, by = c('hhid'='hhid'))
W1$Wave <- 1

W2 <- list( PH9W2, GH_W2) %>% 
  reduce(left_join, by = c('hhid'='hhid'))  
W2$Wave <- 2

W3 <- list( PH9W3,PH9bW3,GH_W3) %>% 
  reduce(left_join, by = c('hhid'='hhid'))  
W3$Wave <- 3

################### |-Q1 Arrangements ##########

################### |-|-W1-Q1 - different column names from the survey fixed (also unneccessary columnd is deleted) ##########

W1 <- W1[, -which(names(W1) == "s9q1b")]

names(W1)[names(W1) == "s9q1a"] <- "s9q1b"

################### |-|-W2-Q1 - unneccessary columns are deleted ##########

W2 <- W2[, -which(names(W2) == "s9q1a")]
W2 <- W2[, -which(names(W2) == "s9q1c")]

################### |-|-W2-Q1 - unneccessary column is deleted ##########

W3 <- W3[, -which(names(W3) == "s9q1a")]


################### |-|-W123 unneccessary columns (questions) are deleted - refer to your printouts ##########

W1 <- W1[, -which(names(W1) %in% c("s9q5a", "s9q5b","s9q6a","s9q6b","s9q7","s9q8","s9q8b","s9q9","s9q10"))]

W2 <- W2[, -which(names(W2) %in% c("s9q5a1","s9q5a2","s9q5b1","s9q5b2","s9q6a","s9q6b","s9q7","s9q8","s9q8b","s9q9","s9q10"))]

W3 <- W3[, -which(names(W3) %in% c("s9q4a1","s9q4b1","s9q5a1","s9q5a2","s9q5b1","s9q5b2","s9q6a","s9q6b","s9q7","s9q8","s9q8b","s9q9","s9q10a","s9q10b","s9q10c","s9q10d","s9q10e","s9q10f","s9q10g","s9q10h","s9q10i","s9q10j","s9q10k","s9q10l","s9q10m","s9q10n","s9q10o"))]


################### |-|-W123-Q13 is deleted - refer to your printouts ##########

W1 <- W1[, -which(names(W1) %in% c("s9q13a","s9q13b","s9q13c","s9q13d","s9q13e","s9q13f"))]

W2 <- W2[, -which(names(W2) %in% c("s9q13a","s9q13b","s9q13c","s9q13d","s9q13e","s9q13f"))]

W3 <- W3[, -which(names(W3) %in% c("s9q13a","s9q13_2","s9q13b","s9q13_4","s9q13c","s9q13_6","s9q13d","s9q13_8","s9q13e","s9q13_10","s9q13f","s9q13_12","s9q13a1"))]

################### |-|-W123-Q15 - remove "specify" to make it comparable ##########

W1 <- W1[, -which(names(W1) %in% c("s9q15d"))]

W2 <- W2[, -which(names(W2) %in% c("s9q15d"))]

W3 <- W3[, -which(names(W3) %in% c("s9q15a_os","s9q15b_os","s9q15c_os"))]

################### |-|-W123-Q19 - remove "specify" to make it comparable ##########

W1 <- W1[, -which(names(W1) %in% c("s9q19c"))]

W2 <- W2[, -which(names(W2) %in% c("s9q19c"))]

W3 <- W3[, -which(names(W3) %in% c("s9q19a_os"))]

################### |-|-W123-Q23 - remove  ##########

W1 <- W1[, -which(names(W1) %in% c("s9q23a","s9q23b","s9q23c"))]

W2 <- W2[, -which(names(W2) %in% c("s9q23a1","s9q23a2","s9q23a3","s9q23a","s9q23b"))]

W3 <- W3[, -which(names(W3) %in% c("s9q23a1","s9q23a1_os","s9q23a2","s9q23a2_os","s9q23a","s9q23b"))]

################### |-|-W3-Q27a - remove because it's new  ##########

W3 <- W3[, -which(names(W3) %in% c("s9q27a"))]




################### NOTES ##########

# There is an issue of "change in respondent" "change in ownership/management". what should i do with the change? i will just ignore it for now

# Q16-18-19-20 are asked differently









################### Other

#New_W3 <- PH9W3[,c("hhid","s9q12")]


#New_W3 <- na.omit(New_W3[c("hhid","s9q12")])

#summary(New_W3)  



# Create two data frames with different numbers of columns and different names
#df1 <- data.frame(a = 1:3, b = 4:6)
#df2 <- data.frame(c = 4:6, d = 7:9, e = 10:12)

# Merge the two data frames on top of each other
#XXX <- bind_rows(W1, W2)
