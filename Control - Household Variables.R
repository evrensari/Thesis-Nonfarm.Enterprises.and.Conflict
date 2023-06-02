setwd("/Users/evrencansari/Desktop/Thesis/DATA")
rm(list=ls())
dir()


##############################LIBRARIES#########################################
library(dplyr)
library(psych)
library(purrr)

options(max.print = 1000000)


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


################### |-|-|- Household Size ###############################

# Household size is an important demographic variable. Here you choose to consider members older than 15 years old. Why did you do that?
# ISSUE - Yo. you don't know which age limit you should consider. I hope you've found a reasonable limit.

# ISSUE - are the household members?

hh_w1_size <- pp1_w1[pp1_w1$s1q4 > 15, ]
hh_w1_size <- aggregate(hh_w1_size$s1q4, by = list(category = hh_w1_size$hhid), FUN = length)
names(hh_w1_size)[1] <- "hhid"
names(hh_w1_size)[2] <- "hh_size"
hh_w1_size$date <- 1

hh_w2_size <- ph1_w1[ph1_w1$s1q4 > 15, ]
hh_w2_size <- aggregate(hh_w2_size$s1q4, by = list(category = hh_w2_size$hhid), FUN = length)
names(hh_w2_size)[1] <- "hhid"
names(hh_w2_size)[2] <- "hh_size"
hh_w2_size$date <- 2

# here the the column name changes
hh_w3_size <- pp1_w2[pp1_w2$s1q6 > 15, ]
hh_w3_size <- aggregate(hh_w3_size$s1q4, by = list(category = hh_w3_size$hhid), FUN = length)
names(hh_w3_size)[1] <- "hhid"
names(hh_w3_size)[2] <- "hh_size"
hh_w3_size$date <- 3

hh_w4_size <- ph1_w2[ph1_w2$s1q4 > 15, ]
hh_w4_size <- aggregate(hh_w4_size$s1q4, by = list(category = hh_w4_size$hhid), FUN = length)
names(hh_w4_size)[1] <- "hhid"
names(hh_w4_size)[2] <- "hh_size"
hh_w4_size$date <- 4

hh_w6_size <- ph1_w3[ph1_w3$s1q4 > 15, ]
hh_w6_size <- aggregate(hh_w6_size$s1q4, by = list(category = hh_w6_size$hhid), FUN = length)
names(hh_w6_size)[1] <- "hhid"
names(hh_w6_size)[2] <- "hh_size"
hh_w6_size$date <- 6


hh_size <- bind_rows(hh_w1_size, hh_w2_size, hh_w3_size, hh_w4_size, hh_w6_size)



write.csv(hh_size, "hh_size.csv", row.names = FALSE)




#




hh_w1_head <- filter(pp1_w1, s1q3 == 1)
hh_w2_head <- filter(ph1_w1, s1q3 == 1)
hh_w3_head <- filter(pp1_w2, s1q3 == 1)
hh_w4_head <- filter(ph1_w2, s1q3 == 1)
hh_w6_head <- filter(ph1_w3, s1q3 == 1)


names(hh_w1_head)[names(hh_w1_head) == "s1q8"] <- "marital"
names(hh_w2_head)[names(hh_w2_head) == "s1q7"] <- "marital"
names(hh_w3_head)[names(hh_w3_head) == "s1q8"] <- "marital"
names(hh_w4_head)[names(hh_w4_head) == "s1q7"] <- "marital"
names(hh_w6_head)[names(hh_w6_head) == "s1q7"] <- "marital"

names(hh_w1_head)[names(hh_w1_head) == "s1q4"] <- "age"
names(hh_w2_head)[names(hh_w2_head) == "s1q4"] <- "age"
names(hh_w3_head)[names(hh_w3_head) == "s1q6"] <- "age"
names(hh_w4_head)[names(hh_w4_head) == "s1q4"] <- "age"
names(hh_w6_head)[names(hh_w6_head) == "s1q4"] <- "age"

names(hh_w1_head)[names(hh_w1_head) == "s1q2"] <- "gender"
names(hh_w2_head)[names(hh_w2_head) == "s1q2"] <- "gender"
names(hh_w3_head)[names(hh_w3_head) == "s1q2"] <- "gender"
names(hh_w4_head)[names(hh_w4_head) == "s1q2"] <- "gender"
names(hh_w6_head)[names(hh_w6_head) == "s1q2"] <- "gender"

hh_w1_head <- hh_w1_head[, c("hhid", "indiv", "marital","age","gender")]
hh_w2_head <- hh_w2_head[, c("hhid", "indiv", "marital","age","gender")]
hh_w3_head <- hh_w3_head[, c("hhid", "indiv", "marital","age","gender")]
hh_w4_head <- hh_w4_head[, c("hhid", "indiv", "marital","age","gender")]
hh_w6_head <- hh_w6_head[, c("hhid", "indiv", "marital","age","gender")]




hh_w1_head$date <- 1
hh_w2_head$date <- 2
hh_w3_head$date <- 3
hh_w4_head$date <- 4
hh_w6_head$date <- 6


hh_head <- bind_rows(hh_w1_head, hh_w2_head, hh_w3_head, hh_w4_head, hh_w6_head)






# 1 is male and 0 is female.
hh_head$gender[hh_head$gender == 2] <- 0


# making this variablble binary. 1= married (monogamous) , 2= married (polugamous), 3= informal union
hh_head$marital <- ifelse(hh_head$marital <= 3, 1, 0)


write.csv(hh_head, "hh_head.csv", row.names = FALSE)


# i couldn't add religion because no information in the ph_w1


# education

# ISSUE questions weren't asked in the post harvest surveys during waves 1 & 2. so i used post planting responses

pp2_w1 <-read.csv("sect2_plantingw1.csv")

ph2a_w1 <-read.csv("sect2a_harvestw1.csv")
ph2b_w1 <-read.csv("sect2b_harvestw1.csv")

pp2_w2 <-read.csv("sect2_plantingw2.csv")

ph2a_w2 <-read.csv("sect2a_harvestw2.csv")
ph2b_w2 <-read.csv("sect2a_harvestw2.csv")

ph2_w3 <-read.csv("sect2_harvestw3.csv")

#

names(pp2_w1)[names(pp2_w1) == "s2q3"] <- "read"
edu_w1 <- pp2_w1[, c("hhid", "indiv", "read")]

names(ph2a_w1)[names(ph2a_w1) == "s2aq5"] <- "read"
ph2a_w1 <- ph2a_w1[, c("hhid", "indiv", "read")]
ph2a_w1 <- ph2a_w1[complete.cases(ph2a_w1$read), ]
edu_w2 <- bind_rows(edu_w1, ph2a_w1)

#
names(pp2_w2)[names(pp2_w2) == "s2q4"] <- "read"
edu_w3 <- pp2_w2[, c("hhid", "indiv", "read")]

names(ph2a_w2)[names(ph2a_w2) == "s2aq5"] <- "read"
ph2a_w2 <- ph2a_w2[, c("hhid", "indiv", "read")]
ph2a_w2 <- ph2a_w2[complete.cases(ph2a_w2$read), ]
edu_w4 <- bind_rows(edu_w3, ph2a_w2)

#
names(ph2_w3)[names(ph2_w3) == "s2aq5"] <- "read"
edu_w6 <- ph2_w3[, c("hhid", "indiv", "read")]


edu_w1$date <- 1
edu_w2$date <- 2
edu_w3$date <- 3
edu_w4$date <- 4
edu_w6$date <- 6

hh_edu <- bind_rows(edu_w1, edu_w2, edu_w3, edu_w4, edu_w6)





# 1 is "can read and write" and 0 is "can't".
hh_edu$read[hh_edu$read == 2] <- 0


# write.csv(hh_edu, "hh_edu.csv", row.names = FALSE)

#pdata_x <- pdata.frame(hh_edu, index = c("hhid","indiv", "date"))

#duplicate_combinations <- table(index(pdata_x), useNA = "ifany")
#df_duplicate_combinations <- as.data.frame(duplicate_combinations)
#write.csv(df_duplicate_combinations, "duplicate_combinations.csv", row.names = FALSE)





#hh_head <- reduce(list(hh_head, hh_edu), left_join, by = c('hhid', 'indiv', 'date'))

#duplicate_rows <- subset(hh_head, duplicated(hhid) & duplicated(date))
#head(duplicate_rows)





write.csv(hh_head, "hh_head.csv", row.names = FALSE)









# checking
# grouped_data <- hh_size %>% group_by(date)


# hh_size_check_sum <- grouped_data %>% summarise( 
#  n = n(),
#  mean = mean(hh_size),
#  sd = sd(hh_size),
#  median = median(hh_size),
#  min = min(hh_size),
#  max = max(hh_size)
# )

# write.csv(hh_size_check_sum, "hh_size_check_sum.csv", row.names = FALSE)



