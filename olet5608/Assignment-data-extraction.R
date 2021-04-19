#---------------------------------------
# This script sets out to pull SES data
# from the data warehouse
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 19 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(DBI)
library(nousutils)

con <- connect_dawn()

#--------------------- Data extraction ------------------

sql <- "
SELECT
postcode,
main_state,
ra_name_2016,
usual_resident_population,
median_mortgage_repayment,
educ_occ_score,
prop_dwellings_no_vehicle
FROM common.d_postcodes
GROUP BY postcode, main_state, ra_name_2016, usual_resident_population, median_mortgage_repayment, educ_occ_score, prop_dwellings_no_vehicle
"

tmp <- dbGetQuery(con, sql) %>%
  drop_na()

write.csv(tmp, file = "olet5608/data/med_mortgage_repay.csv")
