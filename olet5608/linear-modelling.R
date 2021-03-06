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
library(lmtest)
library(sandwich)

# Pull AFL data for 2005-2019
# NOTE: Ignoring 2020 as it was an anomalous season played almost entirely in QLD,
# meaning the consistent home-and-away game style did not exist and may make it
# heterogenous compared to previous seasons

years <- c(seq(from = 2005, to = 2019, by = 1))
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
  mutate(score = ifelse(playing_for == home_team, home_score, away_score)) %>%
  group_by(season, round, playing_for) %>%
  summarise(score = mean(score),
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

table1::table1(~ score + marks + handballs + hit_outs + tackles + rebounds + inside_50s + 
                 clearances + clangers + frees_for + contested_possessions + contested_marks + marks_inside_50, 
               data = aggregated)

#-----------------------
# Variable distributions
#-----------------------

CairoPNG("olet5608/output/densities.png", 800, 600)
aggregated %>%
  pivot_longer(everything(), names_to = "names", values_to = "values") %>%
  ggplot(aes(x = values, y = ..density..)) +
  geom_histogram(alpha = 0.6, fill = "#d95f02", binwidth = 2) +
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
      ggplot(aes(x = values, y = score)) +
      geom_point(alpha = 0.3, colour = "#003f5c") +
      geom_smooth(aes(group = covariates), formula = y ~ x, method = "rlm", method.args = list(method = "MM")) +
      labs(title = "Outlier robust relationship between quantitative covariates and total score in AFL games")
  } else{
    p <- longer %>%
      ggplot(aes(x = values, y = score)) +
      geom_point(alpha = 0.3, colour = "#003f5c") +
      geom_smooth(aes(group = covariates), formula = y ~ x, method = "lm") +
      labs(title = "Relationship between quantitative covariates and total score in AFL games")
  }
  
  p <- p +
    labs(subtitle = "Data is at the team and match level for all non-finals games in seasons 2005-2019 inclusive.",
         x = "Predictor Value",
         y = "Total Score per Team per Match") +
    facet_wrap(~covariates, scales = "free_x")
  
  return(p)
}

CairoPNG("olet5608/output/afl_scatter.png", 800, 600)
draw_plot(data = aggregated, cols = c("marks", "handballs", "hit_outs", "tackles", 
                                     "rebounds", "inside_50s", "clearances", "clangers",
                                     "frees_for", "contested_possessions", "contested_marks",
                                     "marks_inside_50"), y = "score", robust = FALSE)
dev.off()

CairoPNG("olet5608/output/afl_scatter_robust.png", 800, 600)
draw_plot(data = aggregated, cols = c("marks", "handballs", "hit_outs", "tackles", 
                                      "rebounds", "inside_50s", "clearances", "clangers",
                                      "frees_for", "contested_possessions", "contested_marks",
                                      "marks_inside_50"), y = "score", robust = TRUE)
dev.off()

#-------------------
# Correlation matrix
#-------------------

# NOTE: Multicollinearity is numerically tested below using Variance Inflation Factors

reducedMatrix <- aflScaled[,-c(1)]

CairoPNG("olet5608/output/correlation_matrix.png", 800, 600)
corr <- round(cor(reducedMatrix), 2)
ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
                       title = "Correlation matrix of quantitative predictors")
dev.off()

#---------------- Model outputs ----------------

#-----------
# Fit models
#-----------

# Base OLS model

m <- lm(score ~ ., data = aflScaled)

# Base OLS model with dropped non-sig predictors

aflScaled2 <- aflScaled %>%
  dplyr::select(-c(contested_marks, hit_outs))

mAlt <- lm(score ~ ., data = aflScaled2)

# With polynomial second-order terms

m1 <- lm(score ~ marks + handballs + hit_outs + tackles + rebounds + inside_50s + I(inside_50s^2) +
          clearances + clangers + frees_for + contested_possessions + contested_marks + marks_inside_50 + I(marks_inside_50^2),
        data = aflScaled)

# Square-root transformed response

m2 <- lm(sqrt(score) ~ ., data = aflScaled)

# Weighted OLS

wt <- 1 / lm(abs(mAlt$residuals) ~ mAlt$fitted.values)$fitted.values^2
m3 <- lm(score ~ ., weights = wt, data = aflScaled)

# Heteroscedastic-robust standard errors of base OLS

lmtest::bptest(mAlt) # Breush-Pagan test for heteroscedasticity

# Robust test statistics

sjPlot::tab_model(mAlt, vcov.fun = "HC", show.se = TRUE)

#------------------
# Multicollinearity
#------------------

olsrr::ols_vif_tol(mAlt) # All values are far lower than common thresholds, suggesting no issue

#-----------------------
# Retrieve model summary
#-----------------------

summary(mAlt)

# Plot version

coefs <- as.data.frame(coef(summary(mAlt))) %>%
  tibble::rownames_to_column()

confints <- as.data.frame(confint(mAlt)) %>%
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
  geom_point(aes(x = reorder(variable, Estimate), y = Estimate, colour = category), size = 3.5) +
  geom_segment(aes(x = variable, xend = variable, y = lower, yend = upper, colour = category), size = 1.65) +
  labs(title = "Linear model coefficients and 95% confidence intervals",
       x = "Variable",
       y = "Coefficient Value",
       colour = NULL) +
  scale_colour_manual(values = mypal) +
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

# By covariate

df <- broom::augment(m) %>%
  clean_names() #%>%
  #mutate(i_inside_50s_2 = as.numeric(i_inside_50s_2),
         #i_marks_inside_50_2 = as.numeric(i_marks_inside_50_2))

df <- df %>% 
  pivot_longer(cols = c(marks:marks_inside_50), names_to = "covariate", values_to = "value")

#df <- df %>% 
#  pivot_longer(cols = c(marks:i_marks_inside_50_2), names_to = "covariate", values_to = "value")

CairoPNG("olet5608/output/afl_covariate_linearity.png", 800, 600)
ggplot(data = df, aes(x = fitted, y = value)) + 
  labs(title = "Relationship between model fitted and covariate values",
       subtitle = "Smooth lines estimated with GAMs to highlight existent nonlinearities") +
  geom_point() + 
  geom_smooth(formula = y ~ s(x), method = "gam") +
  facet_wrap(~covariate)
dev.off()

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

m4 <- rlm(goals ~ ., data = aflScaled)

# Look at range of weights (if close to 1, then OLS and robust results will be similar)

range(m2$w)

#---------------------------
# Generalised additive model
#---------------------------

thecols <- colnames(aflScaled2)[-c(1)]
gamformula <- as.formula(paste("score ~ ", paste("s(",thecols, ", k = 27)", collapse = "+ ")))
m5 <- gam(formula = gamformula, data = aflScaled2, method = "REML")

m5_het <- gam(formula = gamformula, data = aflScaled, method = "REML", family = gaulss())

# Model outputs

summary(m5)

# Diagnostic plots

CairoPNG("olet5608/output/gamDiagnostics.png", 800, 600)
viz <- getViz(m5)
check(viz,
      m5.qq = list(method = "tnorm", 
                        m5.cipoly = list(fill = "light blue")), 
      m5.respoi = list(size = 0.5), 
      m5.hist = list(bins = 10))
dev.off()

# Predictor smooth plots

CairoPNG("olet5608/output/gamOutputs.png", 800, 600)
gratia::draw(m5)
dev.off()
