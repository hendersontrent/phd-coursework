#------------------------------------------
# This script sets out to produce a graphic
# of the hypothesised relationship discussed
# in the assignment
#------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 14 June 2021
#--------------------------------------

library(dplyr)
library(ggplot2)
library(Cairo)

# Simulate data

stderror <- 150

tmp <- data.frame(ball = c("Old", "New", "Old", "New", "Old", "New"),
                  handicap = c("Low", "Low", "Mid", "Mid", "High", "High"),
                  spinrate = c(10200,10250,9900,10200,9350,10100)) %>%
  mutate(handicap = factor(handicap, levels = c("Low", "Mid", "High"))) %>%
  mutate(lower = spinrate-(2*stderror),
         upper = spinrate+(2*stderror))

# Draw graphic

CairoPNG("olet5402/images/expectations.png", 600, 400)
p <- tmp %>%
  ggplot(aes(x = handicap, y = spinrate, colour = ball)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  labs(x = "Handicap Grouping",
       y = "Spin Rate (Revolutions per Minute)",
       colour = "Ball Type") +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = NA))
print(p)
dev.off()
