library(ggplot2)
library(dplyr)
library(reshape2)

## SETUP ####
#Load model functions
source("beds_needed_uclh_functions.R")

cov_curve_all = read.csv("200331_pietro_pred.csv")
run_duration = max(cov_curve_all$day) #run duration in days

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

## BASE SCENARIO ####

cov_curve = cov_curve_all$basic
results = multi_uclh_model(nruns = 500, cov_curve, params, run_duration)

results$date <- seq(as.Date("2020/3/7"), by = "days", length.out = length(results[,1]))

plot_multi(results, save = T, filename = "cov_pred_base")

## 20% REDUCTION SCENARIO ####

cov_curve = cov_curve_all$X20pred
results = multi_uclh_model(nruns = 500, cov_curve, params, run_duration)

results$date <- seq(as.Date("2020/3/7"), by = "days", length.out = length(results[,1]))

plot_multi(results, save = T, filename = "cov_pred_x20")


## 60% REDUCTION SCENARIO ####

cov_curve = cov_curve_all$X60pred
results = multi_uclh_model(nruns = 500, cov_curve, params, run_duration)

results$date <- seq(as.Date("2020/3/7"), by = "days", length.out = length(results[,1]))

plot_multi(results, save = T, filename = "cov_pred_x60")




## Some quick and not clean compilation of results ####

icu_peak = which.max(results$ICU_beds)
hdu_peak = which.max(results$HDU_beds)

icu_peak
results$ICU_beds[icu_peak]
results$ICU_beds[icu_peak] + results$ICU_beds_sd[icu_peak]
results$ICU_beds[icu_peak] - results$ICU_beds_sd[icu_peak]

hdu_peak
results$HDU_beds[hdu_peak]
results$HDU_beds[hdu_peak] + results$HDU_beds_sd[hdu_peak]
results$HDU_beds[hdu_peak] - results$HDU_beds_sd[hdu_peak]

