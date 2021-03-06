
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
      #how many critical care admissions at time t ?
      new_patients = round(new_patients * prop_critical_care)
      
      #draw randomly to sort critical care admissions into pathways
      new_patients = rbinom(new_patients, 1, prop_path2)

      new_patients = new_patients + 1
      #so 1s will go in path 1, and 2s go in path 2 (for consistency)
      
      #for each admission at time t:
      for (i in new_patients) {
        
        if (i == 1) {
          ## PATHWAY 1 ####
          #intubation (14 days ICU with 50% mortality, then 7 days HDU, then 14 days ward bed)
          
          #STEP 1) patient is in ICU
          #how long will that person need a bed for?
          #this depends on whether that patient will die or not
          patient_mortality = ifelse(runif(1) > ICU_mortality, FALSE, TRUE)
          #if (patient_mortality == TRUE) LoS_ICU = round(runif(1, 0, LoS_ICU))
          if (patient_mortality == TRUE) {
            LoS_ICU = rpois(1, average_LoS_ICU_death)
          } else{
            LoS_ICU = rpois(1, average_LoS_ICU)
          }
          
          #update the patient pathway time (capped at the simulation duration)
          step1_time = min(t + LoS_ICU, run_duration)
          #record that 1 bed will be needed between now (t) and the time of next move (step1_time)
          beds_needed_ICU[t:step1_time] = beds_needed_ICU[t:step1_time] + 1
          
          #IF we don't exceed simulation duration
          if (step1_time != run_duration) {
            
            #IF that patient does not die in ICU 
            if (patient_mortality == FALSE) {
              
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
          #2: no intubation (7 days HDU with 50% mortality, then 10 days ward)
          
          #1) patient is in HDU
          #how long will that person need a bed for?
          #this depends on whether that patient will die or not
          patient_mortality = ifelse(runif(1) > HDU_mortality, FALSE, TRUE)
          #if (patient_mortality == TRUE) LoS_HDU = round(runif(1, 0, LoS_HDU))
          if (patient_mortality == TRUE) {
            LoS_HDU = rpois(1, average_LoS_HDU_death)
          } else {
            LoS_HDU = rpois(1, average_LoS_HDU)
          }
          
          #update the pathway time (capped at the simulation duration)
          step1_time = min(t + LoS_HDU, run_duration)
          #record that 1 bed will be needed between now (t) and time of next move (step1_time)
          beds_needed_HDU[t:step1_time] = beds_needed_HDU[t:step1_time] + 1
          
          #IF we don't exceed simulation duration
          if (step1_time != run_duration) {
            
            #IF that patient does not die in HDU
            if (patient_mortality == FALSE) {
              
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



plot_multi = function(results, save = F, filename = "plot"){
  
  #current bed capacity (don't know these, so just picked some)
  ICU_capacity = 117
  HDU_capacity = c(rep(0,20), # nothing until 27th March
                   rep(8,19), # 8 until 15th April
                   rep(116,20), # on 15th April until early May (5th?)
                   rep(155,length(results$time) - 59)) # jump to
  hdf <- tibble(time = seq_along(HDU_capacity), capacity = HDU_capacity)
  hdf$date <- results$date
  
  gg = ggplot(results) +
    geom_line(aes(date, ICU_beds, colour = "ICU")) +
    geom_ribbon(aes(date, ymin = ICU_beds - ICU_beds_sd,
                    ymax = ICU_beds + ICU_beds_sd, fill = "ICU"), alpha = 0.3) +
    geom_hline(aes(yintercept = ICU_capacity, colour = "ICU")) +
    geom_line(aes(date, HDU_beds, colour = "HDU")) +
    geom_ribbon(aes(date, ymin = HDU_beds - HDU_beds_sd,
                    ymax = HDU_beds + HDU_beds_sd, fill = "HDU"), alpha = 0.3) +
    geom_line(data = hdf, aes(x = date, y = capacity, colour = "HDU")) +
    # geom_line(aes(time, ward_beds, colour = "Ward")) +
    # geom_ribbon(aes(time, ymin = ward_beds - ward_beds_sd,
    #                 ymax = ward_beds + ward_beds_sd, fill = "Ward"), alpha = 0.3) +
    # geom_hline(aes(yintercept = ward_capacity, colour = "Ward")) +
    labs(colour = "Bed category:", x = "Time (months)", y = "Beds needed") +
    guides(fill = FALSE) +
    theme_bw()
  
  plot(gg)
  
  if (save == T) ggsave(paste0(filename,".png"))
  
}

#NOTE this function is flexible, but must be used sensibly
#it will take any number of results inputs and make a summary table
#however, there are no safety checks, so could break easily
#in addition, it expects the results arguments to be named in a specific manner
#i.e. to contain the percentage reduction (20, 40 etc...)
#if the name contains no number, the function assumes it is the "base 0%" scenario
table_multi = function(..., filename = NULL, save = F){
  
  res_table = c()
  res_list = lst(...)
  res_names = names(res_list)
  index = 0
  
  for (results in list(...)) {
    
    index = index + 1
    scenario = readr::parse_number(res_names[index])
    
    if (is.na(scenario)) {
      scenario = "Base (0%)"
    } else {
      scenario = paste0(scenario, "% reduction")
    }
    
    icu_peak = which.max(results$ICU_beds)
    hdu_peak = which.max(results$HDU_beds)
    ward_peak = which.max(results$ward_beds)
    
    res_vec = data.frame(scenario,
                         results$date[icu_peak],
                         round(results$ICU_beds[icu_peak]),
                         round(results$ICU_beds_sd[icu_peak]),
                         results$date[hdu_peak],
                         round(results$HDU_beds[hdu_peak]),
                         round(results$HDU_beds_sd[hdu_peak]),
                         results$date[ward_peak],
                         round(results$ward_beds[ward_peak]),
                         round(results$ward_beds_sd[ward_peak]),
                         round(sum(results$deaths)),
                         round(sum(results$deaths_sd)),
                         fix.empty.names = F)
    
    res_table = rbind(res_table, res_vec)
    
  }
  
  colnames(res_table) = c("Scenario",
                          "Peak ICU bed needs timing", "Mean peak ICU bed needs", "ICU SD",
                          "Peak HDU bed needs timing", "Mean peak HDU bed needs", "HDU SD",
                          "Peak ward bed needs timing", "Mean peak ward bed needs", "Ward SD",
                          "Cumulative deaths", "Deaths SD")
  
  if (save == T) openxlsx::write.xlsx(res_table, paste0(filename, ".xlsx"))
  
  return(res_table)
  
}
