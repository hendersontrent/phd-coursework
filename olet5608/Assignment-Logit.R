#---------------------------------------
# This script sets out to produce a 
# simple win/loss analysis for goals
# scored
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 27 April 2021
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

tmp1 <- all_seasons %>%
  filter(round %ni% the_finals) %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE                    ~ "Remove")) %>%
  filter(winner != "Remove") %>%
  mutate(did_i_win = case_when(
    playing_for == winner ~ 1,
    TRUE                  ~ 0)) %>%
  mutate(round = as.numeric(round)) %>%
  group_by(season, round, did_i_win, playing_for) %>%
  summarise(goals = sum(goals)) %>%
  ungroup()

#---------------- Data visualisation -----------

# Fit statistical model

m <- glm(did_i_win ~ goals, data = tmp1)
summary(m)

# Extract log odds for coefficient (and 95% CI) and exponentiate to get odds

odds <- round(as.numeric(exp(m$coefficients)[2]), digits = 2)
confints <- confint(m)[2,]

# Draw graphic

CairoPNG("olet5608/output/logit.png", 800, 600)
p <- tmp1 %>%
  ggplot(aes(x = goals, y = did_i_win)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) +
  geom_jitter(alpha = 0.2, height = 0, width = 0.5, colour = "#003f5c") + # To make each point a little more visible
  labs(title = "Relationship between goals scored and match outcome",
       subtitle = paste0("For every one unit increase in goals scored, the odds of winning increase by ",odds," (95% CI = ",round(exp(confints[1]), digits = 2),"-",round(exp(confints[2]), digits = 2),", p < .001)"),
       x = "Goals Scored",
       y = "Match Outcome (0 = loss, 1 = win)",
       caption = "Data is at the team and match level for all non-finals games in seasons 2010-2019 inclusive.") +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,1))
print(p)
dev.off()
