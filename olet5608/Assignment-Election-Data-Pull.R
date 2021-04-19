#---------------------------------------
# This script sets out to pull election
# data for the 2019 Federal Election
# and prep it into a tidy format for
# statistical analysis
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 19 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(janitor)

#----------------- Retrieve data ----------------

#--------------------
# Two-party vote data
#--------------------

# Demographic data by electorate

#' Function to pull all the two-party vote data by worksheet for each electorate
#' 
#' @param url a string specifying the URL to download the workbook from
#' @return an object of class dataframe in tidy format
#' @author Trent Henderson
#' @references https://results.aec.gov.au/24310/Website/HouseDownloadsMenu-24310-Csv.htm
#' 

grab_elec_votes <- function(url = "https://results.aec.gov.au/24310/Website/Downloads/HouseTppByDivisionByVoteTypeDownload-24310.csv"){
  
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  
  electorateData <- readr::read_csv(temp, skip = 1) %>%
    clean_names() %>%
    dplyr::select(c(division_nm, state_ab, liberal_national_coalition_declaration_pre_poll_percentage, 
                    liberal_national_coalition_ordinary_percentage)) %>%
    rename(electorate = division_nm,
           state = state_ab,
           lnc_prepoll_percentage = liberal_national_coalition_declaration_pre_poll_percentage,
           lnc_ordinary_percentage = liberal_national_coalition_ordinary_percentage)
  
  return(electorateData)
}

electorateData <- grab_elec_votes()

#--------------------
# Demographic data by 
# electorate
#--------------------

#' Function to pull all the separate demographic data by worksheet for each electorate
#' 
#' @param filepath a string specifying the file location to load the workbook from
#' @return an object of class dataframe in tidy format
#' @author Trent Henderson
#' @references https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2082.02019?OpenDocument
#' 

grab_elec_dems <- function(filepath = "olet5608/data/commonwealth electorate data.xls"){
  
  #-------------- Pull individual variables -----------
  
  # Proportion of population aged over 50
  
  prop_over_50 <- readxl::read_excel(filepath, sheet = "Table 2", skip = 5) %>%
    clean_names() %>%
    drop_na() %>%
    rename(electorate = 1) %>%
    pivot_longer(!electorate, names_to = "names", values_to = "values") %>%
    mutate(category = case_when(
      names == "x0_to_17_years_old"     ~ "prop_under_50",
      names == "x18_to_34_years_old"    ~ "prop_under_50",
      names == "x35_to_49_years_old"    ~ "prop_under_50",
      names == "x50_to_64_years_old"    ~ "prop_over_50",
      names == "x65_to_79_years_old"    ~ "prop_over_50",
      names == "x80_years_old_and_over" ~ "prop_over_50")) %>%
    group_by(electorate, category) %>%
    summarise(counter = sum(values)) %>%
    group_by(electorate) %>%
    mutate(props = counter/sum(counter)) %>%
    ungroup() %>%
    pivot_wider(id_cols = electorate, names_from = "category", values_from = "props") %>%
    dplyr::select(c(electorate, prop_over_50))
  
  # Proportion of families that are couple with children
  
  families <- readxl::read_excel(filepath, sheet = "Table 3", skip = 5) %>%
    clean_names() %>%
    drop_na() %>%
    rename(electorate = 1) %>%
    dplyr::select(c(electorate, couple_family_with_children))
  
  # Median weekly household income
  
  householdincome <- readxl::read_excel(filepath, sheet = "Table 4", skip = 5) %>%
    clean_names() %>%
    rename(electorate = 1) %>%
    dplyr::select(c(electorate, median_weekly_household_income)) %>%
    drop_na()
  
  # Proportion of residents born overseas
  
  overseas <- readxl::read_excel(filepath, sheet = "Table 5", skip = 5) %>%
    clean_names() %>%
    rename(electorate = 1) %>%
    dplyr::select(c(electorate, born_overseas)) %>%
    drop_na()
  
  #-------------- Merge all together -----------------
  
  electorateDemographics <- prop_over_50 %>%
    left_join(families, by = c("electorate" = "electorate")) %>%
    left_join(householdincome, by = c("electorate" = "electorate")) %>%
    left_join(overseas, by = c("electorate" = "electorate"))
  
  return(electorateDemographics)
}

electorateDemographics <- grab_elec_dems(filepath = "olet5608/data/commonwealth electorate data.xls")

# Merge into single dataframe

electionData <- electorateData %>%
  left_join(electorateDemographics, by = c("electorate" = "electorate"))
