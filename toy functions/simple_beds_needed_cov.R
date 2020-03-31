library(ggplot2)
library(dplyr)
library(reshape2)

sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}


#This applies the "simple" mechanism to a scenario where we have both normal incidence and covid19 incidence
#default value here re-generate your ICHNT results

run_duration = 90 #run duration in days

norm_curve = rpois(run_duration, 16) #average number of admissions per day
cov_curve = c(sigmoid(c(15,0.3,30),seq(1,run_duration,1)))

average_LoS = 8 #average LoS in days
average_LoS_cov = 10 #average covid LoS in days

beds_needed = rep(0, run_duration) #vector to store beds needed
#NOTE this starts the hospital empty, need to remove "burn-in" period
beds_needed_cov = rep(0, run_duration) #vector to store beds needed for cov patients

#run for X days
for (t in c(1:run_duration)) {
  
  #how many admissions at time t ?
  new_patients = norm_curve[t]
  new_patients_cov = cov_curve[t]
  
  #for each admission at time t:
  for (i in c(1:new_patients)) {
    
    #how long will that person need a bed for?
    LoS = rpois(1, average_LoS)
    
    #record that 1 bed will be needed between now (t) and until the time that patient is discharged (t+LoS)
    beds_needed[t:min(t + LoS, run_duration)] = beds_needed[t:min(t + LoS, run_duration)] + 1
    
    #ideally, split this to not store if bed capacity overwhelmed, store in second vector to show patients not admitted
    
    #if (any((beds_needed[t:min(t + LoS, run_duration)]+beds_needed_cov[t:min(t + LoS, run_duration)]) > current_bed_capacity)) missed_normal
  }
  
  #for each admission at time t:
  for (i in c(1:new_patients_cov)) {
    
    #how long will that person need a bed for?
    LoS_cov = rpois(1, average_LoS_cov)
    
    #record that 1 bed will be needed between now (t) and until the time that patient is discharged (t+LoS)
    beds_needed_cov[t:min(t + LoS_cov, run_duration)] = beds_needed_cov[t:min(t + LoS_cov, run_duration)] + 1
  }
  
}

current_bed_capacity = 157
summary_data = data.frame(time = c(1:run_duration),
                          normal = beds_needed,
                          cov = beds_needed_cov)

summary_data = summary_data %>% 
  mutate(total = normal+cov)

# 1) So, how many beds would we need each day?
gg = ggplot(summary_data) +
  geom_line(aes(time, normal, colour = "Normal")) +
  geom_line(aes(time, cov, colour = "COVID19")) +
  geom_line(aes(time, total, colour = "Total")) +
  ylab("beds needed") +
  labs(colour = "")

plot(gg)

# 2) Accounting for our current capacity, would we need extra beds, and when?
gg + geom_abline(intercept = current_bed_capacity, slope = 0,)

summary_data$extra = sapply(summary_data$total, function(x) max(0, x - current_bed_capacity)) #calculate extra need
summary_data$ratio = summary_data$normal/summary_data$total

summary_data$extra_normal = apply(summary_data, 1, function(x) rbinom(1, x["extra"], x["ratio"]))
summary_data$extra_cov = summary_data$extra - summary_data$extra_normal

summary_missed = summary_data %>%
  select(time, extra_normal, extra_cov) %>%
  melt(. , id.vars = "time")

ggplot(summary_missed) +
  geom_bar(aes(fill = variable, x = time, y = value), position = "stack", stat = "identity") +
  ylab("extra bed space needed per day") +
  labs(fill = "")

#3) So, how many more beds should we add to our ward to be sure we can deal with the forecasted need?
max(summary_data$extra)
sum(summary_data$extra_normal)
sum(summary_data$extra_cov)
