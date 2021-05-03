#---------------------------------------
# This script sets out to pull the data
# needed for linear modelling and
# produce models and associated data
# visualisations
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 25 April 2021
#---------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(fitzRoy)
library(MASS)
library(mgcv)
library(mgcViz)
library(Cairo)
library(ggfortify)

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

#-----------------------
# Variable distributions
#-----------------------

CairoPNG("olet5608/output/densities.png", 800, 600)
aggregated %>%
  pivot_longer(everything(), names_to = "names", values_to = "values") %>%
  ggplot(aes(x = values)) +
  geom_density(alpha = 0.6, fill = "#d95f02") +
  labs(title = "Distribution of raw values for each variable",
       x = "Value",
       y = "Density") +
  facet_wrap(~names, scales = "free_x")
dev.off()

#--------------------
# Linear relationship
#--------------------

#' Function to produce raw scatterplot with smoothed trendline for all covariates
#' 
#' @param data a dataframe containing the variables to visualise
#' @param cols a vector containing strings of column names to graph
#' @param y a string denoting the response variable vector
#' @param robust Boolean whether to use robust linear regression using M-estimator
#' @return an object of class ggplot containing the graphic
#' @author Trent Henderson
#' 

draw_plot <- function(data, cols, y, robust = FALSE){
  
  keepcols <- append(cols, y)
  
  longer <- data %>%
    dplyr::select(keepcols) %>%
    pivot_longer(cols = cols, names_to = "covariates", values_to = "values")
  
  if(robust){
    p <- longer %>%
      ggplot(aes(x = values, y = goals)) +
      geom_point(alpha = 0.3, colour = "#003f5c") +
      geom_smooth(aes(group = covariates), formula = y ~ x, method = "rlm", method.args = list(method = "MM")) +
      labs(title = "Outlier robust relationship between covariates and total goals kicked in AFL games")
  } else{
    p <- longer %>%
      ggplot(aes(x = values, y = goals)) +
      geom_point(alpha = 0.3, colour = "#003f5c") +
      geom_smooth(aes(group = covariates), formula = y ~ x, method = "lm") +
      labs(title = "Relationship between covariates and total goals kicked in AFL games")
  }
  
  p <- p +
    labs(subtitle = "Data is at the team and match level for all non-finals games in seasons 2010-2019 inclusive.",
         x = "Predictor Value",
         y = "Total Goals per Team per Match",
         caption = "Data source: fitzRoy R package") +
    facet_wrap(~covariates, scales = "free_x")
  
  return(p)
}

CairoPNG("olet5608/output/afl_scatter.png", 800, 600)
draw_plot(data = aggregated, cols = c("marks", "handballs", "hit_outs", "tackles", 
                                     "rebounds", "inside_50s", "clearances", "clangers",
                                     "frees_for", "contested_possessions", "contested_marks",
                                     "marks_inside_50"), y = "goals", robust = FALSE)
dev.off()

CairoPNG("olet5608/output/afl_scatter_robust.png", 800, 600)
draw_plot(data = aggregated, cols = c("marks", "handballs", "hit_outs", "tackles", 
                                      "rebounds", "inside_50s", "clearances", "clangers",
                                      "frees_for", "contested_possessions", "contested_marks",
                                      "marks_inside_50"), y = "goals", robust = TRUE)
dev.off()

#-------------------
# Correlation matrix
#-------------------

# NOTE: Multicollinearity is numerically tested below using Variance Inflation Factors

reducedMatrix <- aflScaled[,c(2:13)]

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

# Plot version

coefs <- as.data.frame(coef(summary(m))) %>%
  tibble::rownames_to_column()

confints <- as.data.frame(confint(m)) %>%
  tibble::rownames_to_column()

coefs <- coefs %>%
  left_join(confints, by = c("rowname" = "rowname")) %>%
  rename(variable = rowname,
         lower = 6,
         upper = 7) %>%
  filter(variable != "(Intercept)") %>%
  mutate(category = case_when(
         lower < 0 & upper < 0 ~ "Significant & Negative",
         lower < 0 & upper > 0 ~ "Not Significant",
         lower > 0 & upper > 0 ~ "Significant & Positive")) %>%
  mutate(category = factor(category, levels = c("Significant & Negative", "Not Significant", "Significant & Positive")))

mypal <- c("Significant & Negative" = "#f95d6a",
           "Not Significant" = "#7D9DAC",
           "Significant & Positive" = "#ffa600")

CairoPNG("olet5608/output/afl_coefficients.png", 800, 600)
coefs %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.75, colour = "black") +
  geom_segment(aes(x = variable, xend = variable, y = lower, yend = upper, colour = category), size = 1.65) +
  geom_point(aes(x = reorder(variable, -Estimate), y = Estimate, colour = category), size = 3.5) +
  labs(title = "Linear model coefficients and 95% confidence intervals",
       x = "Variable",
       y = "Coefficient Value",
       colour = NULL) +
  scale_colour_manual(values = mypal) +
  scale_y_continuous(breaks = seq(from = -1, to = 2, by = 0.5)) +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.key = element_blank())
dev.off()

#-----------------
# Diagnostic plots
#-----------------

# Overall summary

CairoPNG("olet5608/output/afl_lm_diagnostics.png", 800, 600)
par(mfrow = c(2, 2))
plot(m)
dev.off()

CairoPNG("olet5608/output/afl_lm_diagnostics_gg.png", 800, 600)
autoplot(m, which = 1:4)
dev.off()

# Square root epsilon of residuals

CairoPNG("olet5608/output/sqrt_epsilon.png", 800, 600)
plot(fitted(m), sqrt(abs(residuals(m))), xlab = "Fitted", ylab = expression(sqrt(hat(epsilon))))
dev.off()

# Shapiro-Wilk for normality

shapiro.test(residuals(m))

#------------------
# Outlier detection
#------------------

# Calculate residuals on distribution

stud <- rstudent(m)

# Retrieve maximum residual

max_resid <- stud[which.max(abs(stud))]

# Compare maximum residual to Bonferroni-corrected critical value
# Procedure = alpha/2 (for two-sided test), n-p (df)

abs(qt(.05/(length(stud)*2), nrow(aflScaled)-ncol(aflScaled))) # Maximum doesn't exceed, so not worth doing p-values

#---------------- Alternate models -------------

#------------------
# Robust regression
#------------------

m2 <- rlm(goals ~ ., data = aflScaled)

# Look at range of weights (if close to 1, then OLS and robust results will be similar)

range(m2$w)

#---------------------------
# Generalised additive model
#---------------------------

thecols <- colnames(aflScaled)[-1]
gamformula <- as.formula(paste("goals ~ ", paste("s(",thecols, ", k = 27)", collapse = "+ ")))
gam_mod <- gam(formula = gamformula, data = aflScaled, method = "REML")

# Model outputs

summary(gam_mod)

# Diagnostic plots

CairoPNG("olet5608/output/gamDiagnostics.png", 800, 600)
viz <- getViz(gam_mod)
check(viz,
      gam_mod.qq = list(method = "tnorm", 
                        gam_mod.cipoly = list(fill = "light blue")), 
      gam_mod.respoi = list(size = 0.5), 
      gam_mod.hist = list(bins = 10))
dev.off()

# Predictor smooth plots

CairoPNG("olet5608/output/gamOutputs.png", 800, 600)
gratia::draw(gam_mod)
dev.off()
