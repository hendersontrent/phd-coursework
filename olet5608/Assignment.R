#---------------------------------------
# This script sets out to produce all
# analysis for the OLET5608 assignment
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

tmp <- readr::read_csv("olet5608/data/med_mortgage_repay.csv")

#--------------------- Pre processing -------------------

# Convert strings to factors and adjust levels to get Major Cities Remoteness Area
# a first level of the factor (makes for easy comparison of intercept terms in modelling)

tmp1 <- tmp %>%
  mutate(main_state = as.factor(main_state),
         ra_name_2016 = factor(ra_name_2016, levels = c("Major Cities of Australia", "Inner Regional Australia", 
                                                        "Outer Regional Australia", 
                                                        "Remote Australia", "Very Remote Australia")))

# Double check hard coding factor length is the same

if(length(tmp1$ra_name_2016) != length(tmp$ra_name_2016)){
  print("Length of Remoteness Area unique name vectors are not equal. Check coding again.")
}

#-------------------------------
# Centre and standardise numeric
# predictors
#-------------------------------

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

tmp2 <- tmp1 %>%
  mutate(usual_resident_population_log_c = standardCentre(usual_resident_population, log = TRUE),
         educ_occ_score_c = standardCentre(educ_occ_score, log = FALSE),
         prop_dwellings_no_vehicle_c = standardCentre(prop_dwellings_no_vehicle, log = FALSE))

#--------------------- Exploratory data analysis --------

# Draw plots of all variables and the response variable

#' Function to produce raw scatterplot with smoothed trendline for all covariates
#' 
#' @param data a dataframe containing the variables to visualise
#' @param cols a vector containing strings of column names to graph
#' @return an object of class ggplot containing the graphic
#' @author Trent Henderson
#' 

draw_plot <- function(data, cols){
  
  keepcols <- append(cols, "median_mortgage_repayment")
  
  longer <- data %>%
    dplyr::select(keepcols) %>%
    pivot_longer(cols = cols, names_to = "covariates", values_to = "values")
  
  p <- longer %>%
    ggplot(aes(x = values, y = median_mortgage_repayment)) +
    geom_point(alpha = 0.6) +
    geom_smooth(aes(group = covariates), formula = y ~ x, method = "lm") +
    labs(title = "Relationship between quantitative variables and median mortgage repayment",
         subtitle = "Data is at the postcode level.",
         x = "Value",
         y = "Median Mortgage Repayment",
         caption = "Source: Australia Post.") +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    facet_wrap(~covariates, dir = "v")
  
  return(p)
}

CairoPNG("olet5608/output/eda_centre_standard.png", 800, 600)
p <- draw_plot(data = tmp2, cols = c("usual_resident_population_log_c", "educ_occ_score_c", 
                                     "prop_dwellings_no_vehicle_c"))
print(p)
dev.off()

#--------------------- Check collinearity ---------------

# Formal collinearity diagnostic using Variable Importance Factors

m <- lm(median_mortgage_repayment ~ usual_resident_population_log_c + educ_occ_score_c + prop_dwellings_no_vehicle_c, data = tmp2)
olsrr::ols_vif_tol(m) # Values are almost 1 indicating very little correlation between predictors

#--------------------- Statistical modelling ------------

#-------------------
# Basic linear model
#-------------------

m1 <- lm(median_mortgage_repayment ~ 1 + usual_resident_population_log_c + educ_occ_score_c + prop_dwellings_no_vehicle_c + main_state + ra_name_2016,
         data = tmp2)

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

m2 <- gam(median_mortgage_repayment ~ 1 + s(usual_resident_population_log_c) + s(educ_occ_score_c) + s(prop_dwellings_no_vehicle_c) + main_state + ra_name_2016,
          data = tmp2)

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
