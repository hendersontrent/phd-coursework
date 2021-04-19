#---------------------------------------
# This script sets out to visualise the
# 2019 election data
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 19 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(mgcv)
library(gratia)
library(Cairo)

source("olet5608/Assignment-Election-Data-Pull.R")

#----------------- Rescale data --------------

#' Function to mean centre and standardise a numeric vector
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

electionDataScaled <- electionData %>%
  mutate(median_weekly_household_income = rescale(median_weekly_household_income, to = c(0,1)), # Same range as proportion variables
         lnc_prepoll_percentage = lnc_prepoll_percentage/100)

#----------------- Graph data ----------------

# Draw plots of all variables and the response variable

#' Function to produce raw scatterplot with smoothed trendline for all covariates
#' 
#' @param data a dataframe containing the variables to visualise
#' @param cols a vector containing strings of column names to graph
#' @return an object of class ggplot containing the graphic
#' @author Trent Henderson
#' 

draw_plot <- function(data, cols){
  
  keepcols <- append(cols, "lnc_ordinary_percentage")
  
  longer <- data %>%
    dplyr::select(keepcols) %>%
    pivot_longer(cols = cols, names_to = "covariates", values_to = "values")
  
  p <- longer %>%
    ggplot(aes(x = values, y = lnc_ordinary_percentage)) +
    geom_point(alpha = 0.6) +
    geom_smooth(aes(group = covariates), formula = y ~ x, method = "lm") +
    labs(title = "Relationship between quantitative variables and percentage of votes for Liberal/National Coalition",
         subtitle = "Data is at the Commonwealth Electorate level for the 2019 election.",
         x = "Value",
         y = "% of Votes for Liberal/National Coalition",
         caption = "Source: Australian Electoral Commision and ABS.") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~covariates)
  
  return(p)
}

CairoPNG("olet5608/output/eda_scatter.png", 800, 600)
p <- draw_plot(data = electionDataScaled, cols = c("lnc_prepoll_percentage", "prop_over_50", 
                                             "couple_family_with_children", "median_weekly_household_income", 
                                             "owned_outright", "born_overseas", "fully_engaged"))
print(p)
dev.off()

#--------------------- Statistical modelling ------------

#-------------------
# Basic linear model
#-------------------

m1 <- lm(lnc_ordinary_percentage ~ lnc_prepoll_percentage + prop_over_50 + couple_family_with_children + 
          median_weekly_household_income + born_overseas, 
        data = electionDataScaled)

# Formal collinearity diagnostic using Variable Importance Factors

olsrr::ols_vif_tol(m1) # Values are almost 1 indicating very little correlation between predictors

# Retrieve model summary

summary(m1)

# Diagnostic plots

CairoPNG("olet5608/output/lm_diagnostics.png", 800, 600)
par(mfrow = c(2, 2))
plot(m1)
dev.off()

#----------
# GAM model
#----------

m2 <- gam(lnc_ordinary_percentage ~ s(lnc_prepoll_percentage) + s(prop_over_50) + s(couple_family_with_children) + 
           s(median_weekly_household_income) + s(born_overseas), 
         data = electionDataScaled)

# Retrieve model summary

summary(m2)

# Smoothed effects plots

CairoPNG("olet5608/output/gam_smooths.png", 800, 600)
draw(m2)
dev.off()

# Diagnostic plots

CairoPNG("olet5608/output/gam_diagnostics.png", 800, 600)
appraise(m2)
dev.off()
