### Proposed analysis

library(ggplot2)
library(dplyr)
library(reshape2)
theme_set(theme_bw())

## SETUP ####
#Load model functions
source("code/beds_needed_uclh_functions.R")

cov_curve_all = read.csv("data/200331_pietro_pred.csv")
colnames(cov_curve_all)[1] = "Date"

# cov_curve_all %>%
#   select(-day) %>%
#   mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
#   melt(., id.vars = "Date") %>%
#   ggplot(.) +
#   geom_line(aes(Date, value, group = variable, colour = variable)) +
#   scale_colour_discrete(breaks = c("basic", "X20pred", "X60pred"),
#                         labels = c("Basic", "20% reduction", "60% reduction")) +
#   labs(x = "Time (months)", y = "Forecasted number of COVID19 admissions", colour = "Scenario:") +
#   theme_bw()

run_duration = max(cov_curve_all$day) #run duration in days

params = list(
  # Pathways:
  #1: intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
  prop_path1 = 0.18,
  ICU_mortality = 0.5,
  #2: no intubation (7 days HDU with 50% mortality, then 10 days ward)
  prop_path2 = 0.075,
  HDU_mortality = 0.5,
  
  # LoS:
  average_LoS_ICU = 14, #average LoS in ICU in days
  average_LoS_ICU_death = 10, #average LoS in ICU in days if dies
  average_LoS_HDU = 7, #average LoS in HDU in days
  average_LoS_HDU_death = 6, #average LoS in HDU in days if die
  average_LoS_ward1 = 14, #average LoS in ward in days (pathway 1)
  average_LoS_ward2 = 10) #average LoS in ward in days (pathway 2)


## SIMULATIONS ####
#multiple simulations

## BASE SCENARIO ####

cov_curve = cov_curve_all$basic

## Values to vary
los_icu_vary <- seq(5,25,5)
prop_to_p1 <- seq(0,100,10) # Proportion go to pathway 1 of the 25% that go to critical care
to_cc <- sum(params$prop_path1, params$prop_path2)

results_all <- c()

for(i in 1:length(los_icu_vary)){
  for(j in 1:length(prop_to_p1)){
    
    params$average_LoS_ICU <- los_icu_vary[i]
    params$prop_path1 <- to_cc * prop_to_p1[j]/100
    params$prop_path2 <- to_cc * (1 - prop_to_p1[j]/100)
    print(c(params$average_LoS_ICU, params$prop_path1))
    
    results_base = multi_uclh_model(nruns = 20, cov_curve, params, run_duration)
    results_base$date <- seq(as.Date("2020/3/7"), by = "days", length.out = length(results_base[,1]))
    results_base$los <- los_icu_vary[i]
    results_base$pp1 <- prop_to_p1[j]
    
    results_all <- rbind(results_all, results_base)
  }
  
}

results_all <- as.data.frame(results_all)

grped <- results_all %>% group_by(los, pp1) %>% summarise(ICU.max = max(ICU_beds), HDU.max = max(HDU_beds))

ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = ICU.max)) + 
  ggtitle("ICU") + 
  scale_y_continuous("Proportion of all critical care to ICU on admission") + 
  scale_x_continuous("Length of stay in ICU") + 
  scale_fill_distiller(palette = "Spectral","Max. # beds")
## Prop to ICU doesn't affect max number of beds needed - all length of stay dependent... 

ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = HDU.max)) + 
  ggtitle("HDU") + 
  scale_y_continuous("Proportion of all critical care to ICU on admission") + 
  scale_x_continuous("Length of stay in ICU") + 
  scale_fill_distiller(palette = "Spectral","Max. # beds")

# proportion to ICU pathway has no impact - all driven by Length of stay
ggplot(results_all, aes(x=time, y = ICU_beds)) + 
  geom_line(aes(group = interaction(los,pp1),col = factor(los), linetype = factor(pp1)))

ggplot(results_all, aes(x=time, y = HDU_beds)) + 
  geom_line(aes(group = interaction(los,pp1),col = factor(los), linetype = factor(pp1)))


