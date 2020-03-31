library(ggplot2)
library(dplyr)
library(reshape2)

## SETUP ####
#Load model functions
source("beds_needed_uclh_functions.R")

run_duration = 90 #run duration in days

# Covid admission incidence:
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}
cov_curve = c(sigmoid(c(15,0.1,15),seq(1,run_duration,1))) #currently just a random curve

params = list(
  # Pathways:
  #1: intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
  prop_path1 = 0.08,
  ICU_mortality = 0.5,
  #2: no intubation (7 days HDU with 20% mortality, then 10 days ward)
  prop_path2 = 0.06,
  HDU_mortality = 0.2,
  
  # LoS:
  average_LoS_ICU = 8, #average LoS in ICU in days
  average_LoS_HDU = 10, #average LoS in HDU in days
  average_LoS_ward1 = 14, #average LoS in ward in days (pathway 1)
  average_LoS_ward2 = 10) #average LoS in ward in days (pathway 2)


## SIMULATIONS ####

#single simulation
results = uclh_model(cov_curve, params, run_duration)

results_beds = results %>%
  select(-deaths) %>%
  melt(., id.vars = "time")

ggplot(results_beds) +
  geom_line(aes(time,value, colour = variable))



#multiple simulations
results = multi_uclh_model(nruns = 500, cov_curve, params, run_duration)

#current bed capacity (don't know these, so just picked some)
ICU_capacity = 117
HDU_capacity = 8
ward_capacity = 150

ggplot(results) +
  geom_line(aes(time, ICU_beds, colour = "ICU")) +
  geom_ribbon(aes(time, ymin = ICU_beds - ICU_beds_sd,
                  ymax = ICU_beds + ICU_beds_sd, fill = "ICU"), alpha = 0.3) +
  geom_hline(aes(yintercept = ICU_capacity, colour = "ICU")) +
  geom_line(aes(time, HDU_beds, colour = "HDU")) +
  geom_ribbon(aes(time, ymin = HDU_beds - HDU_beds_sd,
                  ymax = HDU_beds + HDU_beds_sd, fill = "HDU"), alpha = 0.3) +
  geom_hline(aes(yintercept = HDU_capacity, colour = "HDU")) +
  geom_line(aes(time, ward_beds, colour = "Ward")) +
  geom_ribbon(aes(time, ymin = ward_beds - ward_beds_sd,
                  ymax = ward_beds + ward_beds_sd, fill = "Ward"), alpha = 0.3) +
  geom_hline(aes(yintercept = ward_capacity, colour = "Ward")) +
  labs(colour = "Bed category:", x = "Time (days)", y = "Beds needed") +
  guides(fill = FALSE)




