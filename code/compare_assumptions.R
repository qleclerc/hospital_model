### Compare to Tristan

library(ggplot2)
library(dplyr)
library(reshape2)
library(bookdown)
library(knitr)
library(kableExtra)
library(tidyverse)
source("beds_needed_uclh_functions.R")

#here specify the csv containing the predicted covid admission incidence
cov_curve_all = read.csv("data/200402_pietro_preds.csv")
colnames(cov_curve_all)[1] = "Date" #fix colnames
cov_curve_all = cov_curve_all %>% drop_na()

run_duration = max(cov_curve_all$day) #run duration in days
nruns = 500
params = list(
  # Pathways:
  #1: intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
  prop_path1 = 0.18,
  ICU_mortality = 0.5,
  #2: no intubation (7 days HDU with 20% mortality, then 10 days ward)
  prop_path2 = 0.075,
  HDU_mortality = 0.2,
  
  # LoS:
  average_LoS_ICU = 14, #average LoS in ICU in days
  average_LoS_HDU = 7, #average LoS in HDU in days
  average_LoS_ward1 = 14, #average LoS in ward in days (pathway 1)
  average_LoS_ward2 = 10) #average LoS in ward in days (pathway 2)

cov_curve = cov_curve_all$X73red
results_x73 = multi_uclh_model(nruns = nruns, cov_curve, params, run_duration)
results_x73$date <- seq(as.Date("2020/3/6"), by = "days", length.out = length(results_x73[,1]))
oldd <- melt(results_x73[,c("ICU_beds","HDU_beds","deaths","date")],id.vars = "date")
oldd$model <- "Old assumptions"

#### new
source("beds_needed_uclh_functions.R")

params = list(
  # Pathways:
  #1: intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
  prop_path1 = 0.18,
  ICU_mortality = 0.5,
  #2: no intubation (7 days HDU with 50% mortality, then 10 days ward)
  prop_path2 = 0.08,
  HDU_mortality = 0.5,
  
  # LoS:
  average_LoS_ICU = 14, #average LoS in ICU in days
  average_LoS_HDU = 7, #average LoS in HDU in days
  average_LoS_ward1 = 14, #average LoS in ward in days (pathway 1)
  average_LoS_ward2 = 10) #average LoS in ward in days (pathway 2)

cov_curve = cov_curve_all$X73red
results_x73 = multi_uclh_model(nruns = nruns, cov_curve, params, run_duration)
results_x73$date <- seq(as.Date("2020/3/6"), by = "days", length.out = length(results_x73[,1]))
newd <- melt(results_x73[,c("ICU_beds","HDU_beds","deaths","date")],id.vars = "date")
newd$model <- "New assumptions"

### COMPARE
ggplot() + geom_line(data = oldd, aes(x=date, y = value, colour = variable, linetype = model)) + 
  geom_line(data = newd,aes(x=date, y = value, colour = variable, linetype = model)) +
  ggtitle("73% reduction")
ggsave("outputs/new_assumptions_compare.pdf",width = 10)



