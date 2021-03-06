library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

## SETUP ####
#Load model functions
source("code/beds_needed_uclh_functions.R")

cov_curve_all = read.csv("data/200406_pietro_preds.csv")
colnames(cov_curve_all)[1] = "Date" #fix colnames
cov_curve_all = cov_curve_all %>% drop_na()
start_date = min(as.Date(cov_curve_all$Date))

cov_curve_all %>%
  select(-day) %>%
  mutate(Date = as.Date(Date)) %>%
  melt(., id.vars = "Date") %>%
  ggplot(.) +
  geom_line(aes(Date, value, group = variable, colour = variable)) +
  scale_colour_discrete(breaks = c("X0pred", "X20pred", "X40pred", "X50pred", "X60pred", "X70pred", "X80pred"),
                        labels = c("Basic (0%)", "20%", "40%", "50%", "60%", "70%", "80%"), "Reduction\nlevel") +
  labs(x = "Time (months)", y = "Forecasted number of COVID19 admissions", colour = "Scenario:") +
  theme_bw()

#ggsave("outputs/scenarios.png")



run_duration = max(cov_curve_all$day) #run duration in days

params = list(
  prop_critical_care = 0.255, #proportion of admissions going to critical care
  # Pathways in critical care:
  #1: intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
  prop_path1 = 0.5, #proportion of critical care patients going through pathway 1
  ICU_mortality = 0.5,
  #2: no intubation (7 days HDU with 50% mortality, then 10 days ward)
  prop_path2 = 0.5, #proportion of critical care patients going through pathway 2
  HDU_mortality = 0.5,
  
  # LoS:
  average_LoS_ICU = 14, #average LoS in ICU in days
  average_LoS_ICU_death = 10, #average LoS in ICU in days if dies
  average_LoS_HDU = 7, #average LoS in HDU in days
  average_LoS_HDU_death = 6, #average LoS in HDU in days if die
  average_LoS_ward1 = 14, #average LoS in ward in days (pathway 1)
  average_LoS_ward2 = 10) #average LoS in ward in days (pathway 2)


## SIMULATIONS ####

#single simulation
results = uclh_model(cov_curve_all$X0pred, params, run_duration)

results_beds = results %>%
  select(-deaths) %>%
  melt(., id.vars = "time")

ggplot(results_beds) +
  geom_line(aes(time,value, colour = variable))



#multiple simulations

## BASE SCENARIO ####

cov_curve = cov_curve_all$X0pred
results_base = multi_uclh_model(nruns = 10, cov_curve, params, run_duration)
results_base$date = seq(as.Date("2020/3/7"), by = "days", length.out = length(results_base[,1]))

plot_multi(results_base, save = F)

## 20% REDUCTION SCENARIO ####

cov_curve = cov_curve_all$X20pred
results_x20 = multi_uclh_model(nruns = 10, cov_curve, params, run_duration)
results_x20$date = seq(as.Date("2020/3/7"), by = "days", length.out = length(results_x20[,1]))

plot_multi(results_x20, save = F)


## 60% REDUCTION SCENARIO ####

cov_curve = cov_curve_all$X60pred
results_x60 = multi_uclh_model(nruns = 10, cov_curve, params, run_duration)
results_x60$date = seq(as.Date("2020/3/7"), by = "days", length.out = length(results_x60[,1]))

plot_multi(results_x60, save = F)


## Compilation of results ####

table_multi(results_base, results_x20, results_x60, save = F)


#plot deaths - not a dedicated function yet:
ggplot() +
  geom_line(data = results_base, aes(date, deaths, colour = "1")) +
  geom_ribbon(data = results_base, aes(date, ymin = deaths - deaths_sd,
                                       ymax = deaths + deaths_sd, fill = "1"), alpha = 0.3) +
  geom_line(data = results_x20, aes(date, deaths, colour = "2")) +
  geom_ribbon(data = results_x20, aes(date, ymin = deaths - deaths_sd,
                                      ymax = deaths + deaths_sd, fill = "2"), alpha = 0.3) +
  geom_line(data = results_x60, aes(date, deaths, colour = "3")) +
  geom_ribbon(data = results_x60, aes(date, ymin = deaths - deaths_sd,
                                      ymax = deaths + deaths_sd, fill = "3"), alpha = 0.3) +
  scale_colour_discrete(breaks = c("1", "2", "3"),
                        labels = c("Basic", "20% reduction", "60% reduction")) +
  labs(x = "Time (months)", y = "Incidence of COVID19 deaths in hospital", colour = "Scenario:") +
  guides(fill = FALSE) +
  theme_bw()


