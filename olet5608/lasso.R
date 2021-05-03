#---------------------------------------
# This script sets out to pull the data
# and fit a Lasso regresison to identify
# a useful subset of predictors to retain
# for linear modelling
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 25 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(fitzRoy)
library(glmnet)

# Pull AFL data for 2010-2019
# NOTE: Ignoring 2020 as it was an anomalous season played almost entirely in QLD,
# meaning the consistent home-and-away game style did not exist and may make it
# heterogenous compared to previous seasons

years <- c(seq(from = 2010, to = 2019, by = 1))
store <- list()

for(i in years){
  
  start_date <- as.character(paste0(i,"-01-01"))
  end_date <- as.character(paste0(i,"-12-01"))
  
  tmp <- get_afltables_stats(start_date = start_date, end_date = end_date) %>%
    clean_names() %>%
    mutate(season = gsub("-.*", "\\1", date),
           season = as.numeric(season))
  
  store[[i]] <- tmp
}

all_seasons <- data.table::rbindlist(store, use.names = TRUE)

#---------------- Pre processing ---------------

# Removes finals as these matches are likely very different to season games and might bias analysis

the_finals <- c("EF", "SF", "QF", "PF", "GF")
'%ni%' <- Negate('%in%')

# Aggregate over each team and match for all key variables of interest

aggregated <- all_seasons %>%
  filter(round %ni% the_finals) %>%
  mutate(round = as.numeric(round)) %>%
  group_by(season, round, playing_for) %>%
  summarise(goals = sum(goals),
            marks = sum(marks),
            handballs = sum(handballs),
            hit_outs = sum(hit_outs),
            tackles = sum(tackles),
            rebounds = sum(rebounds),
            inside_50s = sum(inside_50s),
            clearances = sum(clearances),
            clangers = sum(clangers),
            frees_for = sum(frees_for),
            contested_possessions = sum(contested_possessions),
            contested_marks = sum(contested_marks),
            marks_inside_50 = sum(marks_inside_50)) %>%
  ungroup() %>%
  dplyr::select(-c(season, round, playing_for))

# Rescaling

#' Function to mean centre and standardise a numeric vector with optional log transforms
#' 
#' @param x a vector of numeric values
#' @param log a Boolean of whether to log-scale the vector or not. Defaults to FALSE
#' @return a vector of length(x)
#' @author Trent Henderson
#' 

standardCentre <- function(x, log = FALSE){
  
  if(log){
    x <- (log(x)-mean(log(x), na.rm = TRUE))/sd(log(x), na.rm = TRUE)
  } else{
    x <- (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  }
  return(x)
}

aflScaled <- aggregated %>%
  mutate(marks = standardCentre(marks, log = FALSE),
         handballs = standardCentre(handballs, log = FALSE),
         hit_outs = standardCentre(hit_outs, log = FALSE),
         tackles = standardCentre(tackles, log = FALSE),
         rebounds = standardCentre(rebounds, log = FALSE),
         inside_50s = standardCentre(inside_50s, log = FALSE),
         clearances = standardCentre(clearances, log = FALSE),
         clangers = standardCentre(clangers, log = FALSE),
         frees_for = standardCentre(frees_for, log = FALSE),
         contested_possessions = standardCentre(contested_possessions, log = FALSE),
         contested_marks = standardCentre(contested_marks, log = FALSE),
         marks_inside_50 = standardCentre(marks_inside_50, log = FALSE))

#---------------- Lasso regression -------------

m <- glmnet::glmnet(goals ~ ., data = aflScaled, alpha = 1)

#---------------- Save subset names ------------

# Retrieve model summary

summary(m)

# Retrieve weights


