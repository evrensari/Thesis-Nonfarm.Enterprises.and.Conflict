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

dependent <-read.csv("dependent_ent_READY.csv")






# prevalence of new / currently operating / premanently closed / temporarily closed enterprise

dependent$new_enterprise <- ifelse(dependent$new == 1, 1, 0)
dependent_hh_new <- dependent %>%
  group_by(hhid, date) %>%
  summarize(new_enterprise = ifelse(any(new_enterprise == 1), 1, ifelse(all(new_enterprise == 0 | is.na(new_enterprise)), 0, NA)))
dependent_hh_new <- subset(dependent_hh_new, select = c('hhid', 'date','new_enterprise'))


dependent$currently_operating <- ifelse(dependent$status == "Currently Operating", 1, 0)
dependent_hh_currently <- dependent %>%
  group_by(hhid, date) %>%
  summarize(currently_operating = ifelse(any(currently_operating == 1), 1, ifelse(all(currently_operating == 0 | is.na(currently_operating)), 0, NA)))
dependent_hh_currently <- subset(dependent_hh_currently, select = c('hhid', 'date','currently_operating'))

dependent$closed_temporarily <- ifelse(dependent$status == "Closed, Temporarily", 1, 0)
dependent_hh_temporarily <- dependent %>%
  group_by(hhid, date) %>%
  summarize(closed_temporarily = ifelse(any(closed_temporarily == 1), 1, ifelse(all(closed_temporarily == 0 | is.na(closed_temporarily)), 0, NA)))
dependent_hh_temporarily <- subset(dependent_hh_temporarily, select = c('hhid', 'date','closed_temporarily'))

dependent$closed_permanently <- ifelse(dependent$status == "Closed, Permanently", 1, 0)
dependent_hh_permanently <- dependent %>%
  group_by(hhid, date) %>%
  summarize(closed_permanently = ifelse(any(closed_permanently == 1), 1, ifelse(all(closed_permanently == 0 | is.na(closed_permanently)), 0, NA)))
dependent_hh_permanently <- subset(dependent_hh_permanently, select = c('hhid', 'date','closed_permanently'))



# number of currently operating / new enterprise

dependent$num_operating_enterprises <- NA
grouped_df <- group_by(dependent, hhid, date)
dependent <- mutate(grouped_df, num_operating_enterprises = ifelse(any(currently_operating == 1), sum(currently_operating == 1), NA))
dependent_hh_currently_num <- dependent[!duplicated(dependent[, c('hhid', 'date','num_operating_enterprises')]), ]
dependent_hh_currently_num <- subset(dependent_hh_currently_num, select = c('hhid', 'date','num_operating_enterprises'))

dependent$num_new_enterprise <- NA
grouped_df <- group_by(dependent, hhid, date)
dependent <- mutate(grouped_df, num_new_enterprise = ifelse(any(new_enterprise == 1), sum(new_enterprise == 1), NA))
dependent_hh_new_num <- dependent[!duplicated(dependent[, c('hhid', 'date','num_new_enterprise')]), ]
dependent_hh_new_num <- subset(dependent_hh_new_num, select = c('hhid', 'date','num_new_enterprise'))




# merge

dependent_hh <- list( dependent_hh_new, dependent_hh_currently,dependent_hh_currently_num,dependent_hh_new_num,dependent_hh_permanently,dependent_hh_temporarily) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

# update

# merging prevalence of "currently operating" and "new"
# a "new" enterprise might also be a "currently operating" enterprise. In that case you can use the below variable:

dependent_hh$currently_operating_x <- ifelse(is.na(dependent_hh$currently_operating), dependent_hh$new_enterprise, dependent_hh$currently_operating)


# merging number of "currently operating" and "new"

dependent_hh$new_enterprise_x <- replace(dependent_hh$num_new_enterprise, dependent_hh$num_new_enterprise == 0, NA)
dependent_hh$num_operating_enterprises_x <- ifelse(!is.na(dependent_hh$num_operating_enterprises) & !is.na(dependent_hh$new_enterprise_x),dependent_hh$num_operating_enterprises + dependent_hh$new_enterprise_x,dependent_hh$num_operating_enterprises)
dependent_hh$num_operating_enterprises_x <- ifelse(is.na(dependent_hh$num_operating_enterprises_x), dependent_hh$new_enterprise_x, dependent_hh$num_operating_enterprises_x)



dependent_hh <- subset(dependent_hh, select = -new_enterprise_x)


# below data includes the households labeled as "exposed to conflict" 

geo_var_gps <-read.csv("geo_var_gps_111.csv")

analysis <- list( geo_var_gps, dependent_hh) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))



analysis <- as.data.frame(lapply(analysis, function(x) ifelse(x == TRUE, 1, x)))

# Here you aggregate the variables like credit use, cost, capital value to get an household level data.


totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_credit_amount = sum(credit_amount, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_value_capital = sum(value_capital, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_value_sale = sum(value_sale, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_wage = sum(cost_wage, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_inventory = sum(cost_inventory, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_transport = sum(cost_transport, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_insurance = sum(cost_insurance, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_rent = sum(cost_rent, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_interest = sum(cost_interest, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_raw = sum(cost_raw, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_cost_other = sum(cost_other, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_male = sum(male, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#

totals <- dependent %>%
  group_by(hhid, date) %>%
  summarise(total_female = sum(female, na.rm = TRUE))

analysis <- list( analysis, totals) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))

#



# add the waves back

analysis$date[analysis$date == "2010-08-14"] <- 1
analysis$date[analysis$date == "2011-03-02"] <- 2
analysis$date[analysis$date == "2012-09-28"] <- 3
analysis$date[analysis$date == "2013-03-15"] <- 4
analysis$date[analysis$date == "2016-03-27"] <- 6







# add the household control variables

hh_edu <-read.csv("hh_edu.csv")
hh_head <-read.csv("hh_head.csv")



#hh_head <- list( hh_head, hh_edu) %>% 
#  reduce(left_join, by = c('hhid'='hhid','indiv'='indiv','date'='date'))





hh_size <-read.csv("hh_size.csv")

analysis$date <- as.integer(analysis$date)

analysis <- list( analysis, hh_size, hh_head) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))



# add the geographical control varibles

geo_var <-read.csv("geo_var.csv")

colnames(geo_var)[colnames(geo_var) == "wave"] <- "date"

analysis <- list( analysis, geo_var) %>% 
  reduce(left_join, by = c('hhid'='hhid','date'='date'))




write.csv(analysis, "hh_dependent.csv", row.names = FALSE)

