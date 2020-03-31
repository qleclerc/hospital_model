
#This illustrates a simple mechanism to track number of beds required at each timepoint, accounting for the fact that beds are only needed for a certain period of time

average_new_patients = 16 #average number of admissions per day
average_LoS = 8 #average LoS in days

run_duration = 100 #run duration in days

beds_needed = rep(0, run_duration) #vector to store beds needed
#NOTE this starts the hospital empty, need to remove "burn-in" period

#run for X days
for (t in c(1:run_duration)) {
  
  #how many admissions at time t ?
  new_patients = rpois(1, average_new_patients)
  
  #for each admission at time t:
  for (i in c(1:new_patients)) {
    
    #how long will that person need a bed for?
    LoS = rpois(1, average_LoS)
    
    #record that 1 bed will be needed between now (t) and until the time that patient is discharged (t+LoS)
    beds_needed[t:min(t + LoS, run_duration)] = beds_needed[t:min(t + LoS, run_duration)] + 1
  }
}


# 1) So, how many beds would we need each day?
#you see it tends towards an equilibrium value, which depends on average daily admission & LoS
plot(beds_needed, xlab = "Time (days)", type = "l") #plot bed needs output from model


# 2) Accounting for our current capacity, would we need extra beds, and when?
current_bed_capacity = 157
abline(h = current_bed_capacity, col = "red") #plot current capacity

extra_beds_needed = sapply(beds_needed, function(x) max(0, x - current_bed_capacity)) #calculate extra need
barplot(extra_beds_needed, xlab = "Time (days)", ylab = "Extra bed space needed per day") #plot extra need

#3) So, how many more beds should we add to our ward to be sure we can deal with the forecasted need?
max(extra_beds_needed)
