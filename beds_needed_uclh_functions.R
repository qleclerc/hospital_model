
uclh_model = function(cov_curve, params, run_duration){
  
  #load params and run simulation
  with(params,{
    
    # Vectors to store beds needed:
    beds_needed_ICU = beds_needed_HDU = beds_needed_ward = rep(0, run_duration) 
    
    # Vector to store deaths
    deaths = rep(0, run_duration)
    
    #run for X days
    for (t in c(1:run_duration)) {
      
      #how many admissions at time t ?
      new_patients = cov_curve[t]
      new_patients = round(new_patients * (prop_path1 + prop_path2))
      
      #draw randomly to sort patients into pathways
      new_patients = rbinom(new_patients, 1, prop_path2/(prop_path1 + prop_path2))
      new_patients = new_patients + 1
      #so 1s will go in path 1, and 2s go in path 2 (for consistency)
      
      #for each admission at time t:
      for (i in new_patients) {
        
        if (i == 1) {
          ## PATHWAY 1 ####
          #intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
          
          #STEP 1) patient is in ICU
          #how long will that person need a bed for?
          LoS_ICU = rpois(1, average_LoS_ICU)
          #update the patient pathway time (capped at the simulation duration)
          step1_time = min(t + LoS_ICU, run_duration)
          #record that 1 bed will be needed between now (t) and the time of next move (step1_time)
          beds_needed_ICU[t:step1_time] = beds_needed_ICU[t:step1_time] + 1
          
          #IF we don't exceed simulation duration
          if (step1_time != run_duration) {
            
            #IF that patient does not die in ICU 
            if (runif(1) > ICU_mortality) {
              
              #STEP 2) patient is in HDU
              #how long will that person need a bed for?
              LoS_HDU = rpois(1, average_LoS_HDU)
              #update the pathway time (capped at the simulation duration)
              step2_time = min(step1_time + LoS_HDU, run_duration)
              #record that 1 bed will be needed between now (step1_time) and time of next move (step2_time)
              beds_needed_HDU[step1_time:step2_time] = beds_needed_HDU[step1_time:step2_time] + 1
              
              #IF we don't exceed simulation duration
              if (step2_time != run_duration) {
                
                #STEP 3) patient is in ward
                #how long will that person need a bed for?
                LoS_ward = rpois(1, average_LoS_ward1)
                #update the pathway time (capped at the simulation duration)
                step3_time = min(step2_time + LoS_ward, run_duration)
                #record that 1 bed will be needed between now (step2_time) and time of next move (step3_time)
                beds_needed_ward[step2_time:step3_time] = beds_needed_ward[step2_time:step3_time] + 1
                
              }
              
            } else {
              #patient dies in ICU
              deaths[step1_time] = deaths[step1_time] + 1
            }
            
          }
          
        } else {
          
          ## PATHWAY 2 ####
          #2: no intubation (7 days HDU with 20% mortality, then 10 days ward)
          
          #1) patient is in HDU
          #how long will that person need a bed for?
          LoS_HDU = rpois(1, average_LoS_HDU)
          #update the pathway time (capped at the simulation duration)
          step1_time = min(t + LoS_HDU, run_duration)
          #record that 1 bed will be needed between now (t) and time of next move (step1_time)
          beds_needed_HDU[t:step1_time] = beds_needed_HDU[t:step1_time] + 1
          
          #IF we don't exceed simulation duration
          if (step1_time != run_duration) {
            
            #IF that patient does not die in HDU
            if (runif(1) > HDU_mortality) {
              
              #2) patient is in ward
              #how long will that person need a bed for?
              LoS_ward = rpois(1, average_LoS_ward2)
              #update the pathway time (capped at the simulation duration)
              step2_time = min(step1_time + LoS_ward, run_duration)
              #record that 1 bed will be needed between now (step1_time) and time of next move (step2_time)
              beds_needed_ward[step1_time:step2_time] = beds_needed_ward[step1_time:step2_time] + 1
              
            } else {
              #patient dies in HDU
              deaths[step1_time] = deaths[step1_time] + 1
            }
          }
        }
      }
    }
    
    # Combine results
    results = data.frame(time = c(1:run_duration),
                         ICU_beds = beds_needed_ICU,
                         HDU_beds = beds_needed_HDU,
                         ward_beds = beds_needed_ward,
                         deaths = deaths)
    
    return(results)
    
  })
}


multi_uclh_model = function(nruns, cov_curve, params, run_duration) {
  
  summary_ICU_beds = matrix(0, run_duration, nruns)
  summary_HDU_beds = matrix(0, run_duration, nruns)
  summary_ward_beds = matrix(0, run_duration, nruns)
  summary_deaths = matrix(0, run_duration, nruns)
  
  for (run in c(1:nruns)) {
    
    results = uclh_model(cov_curve, params, run_duration)
    
    summary_ICU_beds[,run] = results$ICU_beds
    summary_HDU_beds[,run] = results$HDU_beds
    summary_ward_beds[,run] = results$ward_beds
    summary_deaths[,run] = results$deaths
    
  }
  
  summary_results = data.frame(time = c(1:run_duration),
                               ICU_beds = rowMeans(summary_ICU_beds),
                               ICU_beds_sd = apply(summary_ICU_beds, 1, sd),
                               HDU_beds = rowMeans(summary_HDU_beds),
                               HDU_beds_sd = apply(summary_HDU_beds, 1, sd),
                               ward_beds = rowMeans(summary_ward_beds),
                               ward_beds_sd = apply(summary_ward_beds, 1, sd),
                               deaths = rowMeans(summary_deaths),
                               deaths_sd = apply(summary_deaths, 1, sd))
  
  return(summary_results)
  
}
