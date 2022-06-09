#-------------------------------------
# This scipt pulls data from the web
# and calculates features for use in
# the report
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 9 June 2022
#-------------------------------------

library(dplyr)
library(tidyr)
library(theft)

#-------- Pull the dataset ---------

#' Function to load and process files into a single tidy dataframe
#' @param problem string specifying the problem to use
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

tidy_arff_file <- function(problem){
  
  # Pull the dataset
  
  temp <- tempfile()
  download.file(paste0("https://www.timeseriesclassification.com/Downloads/", problem, ".zip"), temp, mode = "wb")
  
  #-----------------------------
  # Grab the train and test data
  #-----------------------------
  
  train <- foreign::read.arff(unz(temp, paste0(problem, "_TRAIN.arff"))) %>%
    mutate(id = row_number()) %>%
    mutate(set_split = "Train")
  
  themax <- max(train$id) # To add in test set to avoid duplicate IDs
  
  test <- foreign::read.arff(unz(temp, paste0(problem, "_TEST.arff"))) %>%
    mutate(id = row_number() + themax) %>% # Adjust relative to train set to stop double-ups
    mutate(set_split = "Test")
  
  #----------------------------
  # Wrangle data to long format
  #----------------------------
  
  # Train
  
  thecolstr <- colnames(train)
  keepcolstr <- thecolstr[!thecolstr %in% c("target", "id", "set_split")]
  
  train <- train %>%
    pivot_longer(cols = all_of(keepcolstr), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  # Test
  
  thecolste <- colnames(test)
  keepcolste <- thecolste[!thecolste %in% c("target", "id", "set_split")]
  
  test <- test %>%
    pivot_longer(cols = all_of(keepcolste), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  #------
  # Merge
  #------
  
  outs <- bind_rows(train, test)
  return(outs)
}

tmp <- tidy_arff_file(problem = "Chinatown")

#-------- Calculate features ---------

# Fix Python environment to where the Python libraries are installed on my machine

init_theft("~/opt/anaconda3/bin/python")

# Extract time-series features

outs <- calculate_features(tmp, id_var = "id", time_var = "timepoint", 
                           values_var = "values", group_var = "target", 
                           feature_set = c("catch22", "tsfresh", "TSFEL", "Kats"), 
                           catch24 = TRUE, tsfresh_cleanup = FALSE, seed = 123)

save(outs, file = "olet5610/report/OLET5610 Report/outs.Rda")
