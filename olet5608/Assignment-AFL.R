#---------------------------------------
# This script sets out to pull the data
# needed for a classification algorithm
# and prep it for modelling
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 25 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(fitzRoy)
library(Cairo)

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

# Aggregate over each team and match for some key variables of interest

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

#---------------- Data visualisation -----------

#-------------------
# Summary statistics
#-------------------

table1::table1(~ marks + handballs + hit_outs + tackles + rebounds + inside_50s + 
                 clearances + clangers + frees_for + contested_possessions + contested_marks + marks_inside_50, 
               data = aggregated)

#--------------------
# Linear relationship
#--------------------

#' Function to produce raw scatterplot with smoothed trendline for all covariates
#' 
#' @param data a dataframe containing the variables to visualise
#' @param cols a vector containing strings of column names to graph
#' @param y a string denoting the response variable vector
#' @return an object of class ggplot containing the graphic
#' @author Trent Henderson
#' 

draw_plot <- function(data, cols, y){
  
  keepcols <- append(cols, y)
  
  longer <- data %>%
    dplyr::select(keepcols) %>%
    pivot_longer(cols = cols, names_to = "covariates", values_to = "values")
  
  p <- longer %>%
    ggplot(aes(x = values, y = goals)) +
    geom_point(alpha = 0.3, colour = "#003f5c") +
    geom_smooth(aes(group = covariates), formula = y ~ x, method = "lm") +
    labs(title = "Relationship between z-scored covariates and total goals kicked in AFL games",
         subtitle = "Data is at the team and match level for all non-finals games in seasons 2010-2019 inclusive.",
         x = "z-scored Predictor Value",
         y = "Total Goals per Team per Match",
         caption = "Data source: fitzRoy R package") +
    facet_wrap(~covariates, scales = "free_x")
  
  return(p)
}

CairoPNG("olet5608/output/afl_scatter.png", 800, 600)
p <- draw_plot(data = aflScaled, cols = c("marks", "handballs", "hit_outs", "tackles", 
                                          "rebounds", "inside_50s", "clearances", "clangers",
                                          "frees_for", "contested_possessions", "contested_marks",
                                          "marks_inside_50"), y = "goals")
print(p)
dev.off()

#-------------------
# Correlation matrix
#-------------------

# NOTE: Multicollinearity is numerically tested below using Variance Inflation Factors

reducedMatrix <- aflScaled[,c(2:13)]

# Calculate correlation and produce graphic

CairoPNG("olet5608/output/correlation_matrix.png", 800, 600)
corr <- round(cor(reducedMatrix), 2)
ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
                       title = "Correlation matrix of predictors")
dev.off()

#---------------- Model outputs ----------------

#----------
# Fit model
#----------

m <- lm(goals ~ ., data = aflScaled)

#------------------
# Multicollinearity
#------------------

olsrr::ols_vif_tol(m) # All values are close to 1 and far lower than common thresholds, suggesting no issue

#-----------------------
# Retrieve model summary
#-----------------------

summary(m)

# Publication-ready version

sjPlot::tab_model(m,
                  show.se = TRUE,
                  collapse.se = TRUE, 
                  digits = 3)

#-----------------
# Diagnostic plots
#-----------------

# Overall summary

CairoPNG("olet5608/output/afl_lm_diagnostics.png", 800, 600)
par(mfrow = c(2, 2))
plot(m)
dev.off()

# By predictor

covariate_list <- c("marks", "handballs", "hit_outs", "tackles", 
                    "rebounds", "inside_50s", "clearances", "clangers",
                    "frees_for", "contested_possessions", "contested_marks",
                    "marks_inside_50")
  
CairoPNG("olet5608/output/afl_residuals.png", 800, 600)
p1 <- aflScaled %>%
  mutate(residuals = m$residuals) %>%
  dplyr::select(covariate_list, residuals) %>%
  pivot_longer(cols = covariate_list, names_to = "covariates", values_to = "values") %>%
  ggplot(aes(x = values, y = residuals)) +
  geom_point(alpha = 0.3, colour = "#003f5c") +
  geom_hline(aes(yintercept = 0), colour = "red") +
  labs(title = "Residuals plots for each predictor",
       x = "z-scored Predictor Value",
       y = "Residuals") +
  facet_wrap(~covariates, scales = "free_x")
print(p1)
dev.off()
