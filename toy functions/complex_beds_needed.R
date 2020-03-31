
#This illustrates a more complex scenario where patients can either be in standard beds or ICU beds

average_new_patients = 6 #average number of admissions per day
average_LoS_standard = 7 #average LoS in days
average_LoS_ICU = 7

proba_norm = 0.9025
proba_ICU_norm = 0.04
proba_norm_ICU_norm = 0.038
proba_ICU = 0.01
proba_norm_ICU = 0.0095

run_duration = 100 #run duration in days

beds_needed_standard = rep(0, run_duration) #vector to store beds needed
beds_needed_ICU = rep(0, run_duration) #vector to store beds needed

#run for X days
for (t in c(1:run_duration)) {
  
  #how many admissions at time t ?
  new_patients = rpois(1, average_new_patients)
  
  #for each admission at time t:
  for (i in c(1:new_patients)) {
    
  }
}


# 1) So, how many beds would we need each day?
#you see it tends towards an equilibrium value, which depends on average daily admission & LoS
plot(beds_needed, xlab = "Time (days)") #plot bed needs output from model


# 2) Accounting for our current capacity, would we need extra beds, and when?
current_bed_capacity = 50
abline(h = current_bed_capacity, col = "red") #plot current capacity

extra_beds_needed = sapply(beds_needed, function(x) max(0, x - current_bed_capacity)) #calculate extra need
plot(extra_beds_needed, xlab = "Time (days)") #plot extra need

#3) So, how many more beds should we add to our ward to be sure we can deal the forecasted need?
max(extra_beds_needed)
