#--------------------------------------
# This script sets out to calculate a 
# power analysis for the proposed
# study
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 24 June 2021
#--------------------------------------

library(dplyr)
library(magrittr)
library(pwr)
library(lmSupport)

# Simulate some data

stderror <- 150

tmp <- data.frame(ball = c("Old", "New", "Old", "New", "Old", "New"),
                  handicap = c("Low", "Low", "Mid", "Mid", "High", "High"),
                  spinrate = c(10200,10250,9900,10200,9350,10000)) %>%
  mutate(handicap = factor(handicap, levels = c("Low", "Mid", "High"))) %>%
  mutate(lower = spinrate-(2*stderror),
         upper = spinrate+(2*stderror))

# Fit basic GLM

mod <- glm(spinrate ~ age + sex + stiffness + handicap + ball + handicap*ball, data = tmp, family = "gamma")
summary(mod)

# Do power analysis

modelEffectSizes(mod)
modelPower(u = 1, v = 1284, alpha = 0.05, peta2 = 0.03)
