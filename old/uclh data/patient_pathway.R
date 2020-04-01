#### UCLH patient pathways analysis 
library("janitor")
library("tidyverse")
library("ggplot2")
library(reshape2)
library(rlist)

## PREP DATA ####

# Load data
uclh_data = read_csv("200326_uclh_data.csv")
# Janitor makes names lowercase and "_" between
uclh_data = clean_names(uclh_data)

# Reformat to date
uclh_data$bed_status_end_dttm[which(uclh_data$bed_status_end_dttm == "NULL")] = NA
uclh_data$bed_status_end_dttm = as.POSIXct(uclh_data$bed_status_end_dttm, tz = "GMT")

#uclh_data = uclh_data %>% filter(covid_capacity_type != "Wait Bed")

# Take only covid patients
covid_patients = uclh_data %>% 
  group_by(patient_id) %>% 
  summarise(covid_positive = any(covid_status == "Positive")) %>% 
  filter(!(patient_id == "NULL") & covid_positive == TRUE) 

# Remove duplicates (when something other than the patient bed is changed e.g. ventilation)
covid_patients = uclh_data %>% 
  inner_join(covid_patients) %>%
  select(patient_id, bed_status_start_dttm, bed_status_end_dttm, covid_capacity_type) %>%
  group_by(patient_id) %>% 
  arrange(bed_status_start_dttm) %>%
  distinct()


## EXTRACT PATHWAYS ####

#create empty list to store patient pathways
patients_summary = list()

#loop through all patients
for (id in unique(covid_patients$patient_id)) {
  
  #filter to only have that patient's data
  patient_data = covid_patients %>% filter(patient_id == id)
  
  #calculate LoS for each step
  patient_data$LoS = difftime(patient_data$bed_status_end_dttm, patient_data$bed_status_start_dttm, units = "day")
  
  #BUT sometimes patients are recorded in the same place for more than one observation!
  #but can't just group_by(covid_capacity_type) because patients can later return to a previously occupied bed
  #so need a new "step" variable that changes everytime the patient is moved
  
  patient_data$step = 1
  step_count = 1
  
  #loop through patient record, adding a step whenever they're moved
  if(nrow(patient_data) > 1) {
    for (j in c(2:nrow(patient_data))) {
      if (patient_data$covid_capacity_type[j] != patient_data$covid_capacity_type[j - 1]) {
        step_count = step_count + 1
      }
      patient_data$step[j] = step_count
    }
  }
  
  #then, summarise the patient pathway
  patient_pathway = patient_data %>%
    group_by(step) %>%
    summarise(cum_LoS = sum(LoS), ward = unique(covid_capacity_type))
  
  #record the pathway as a list
  patient_pathway = list(id = id, path = patient_pathway$ward, LoS = patient_pathway$cum_LoS)
  
  #append to the summary list
  patients_summary = list.append(patients_summary, patient_pathway)
  
}



## ANALYSE PATHWAYS ####

#what are the possible pathways?
patients_summary %>%
  list.map(path) %>% 
  unique(.)
#35 different pathways!!

#But, only 20 completed pathways
patients_summary %>%
  list.filter(!is.na(sum(LoS))) %>%
  list.map(path) %>% 
  unique(.)

#total duration for completed pathways 
total_LoS = patients_summary %>%
  list.map(sum(LoS)) %>%
  list.filter(!is.na(.)) %>%
  unlist(.)
#only 53 completed pathways... rest are not yet discharged

hist(total_LoS)
#heavily skewed towards shorter durations, expected since a lot of pathways are still not completed?

#total durations for all pathways, including unfinished ones
patients_summary %>%
  list.map(sum(LoS, na.rm = T)) %>%
  unlist(.) %>%
  hist(., breaks = seq(0,140,10))

#Problem: some patients have been there for ages, even before covid was here
#suggest putting a minimum date after which we start looking at the patients

#what about ICU LoS?
#need to figure out how to filter this!

#investigate covid patients released from ICU
id_check = patients_summary %>%
  list.filter(!is.na(sum(LoS)), "ICU" %in% path) %>%
  list.mapv(id)

data_check = covid_patients %>%
  filter(patient_id %in% id_check)

Z1039681

#look for nosocomial transmission





#visualise hospital bed usage

#### UCLH patient pathways analysis 
library("janitor")
library("tidyverse")
library("ggplot2")
library(reshape2)
library(rlist)

## PREP DATA ####

# Load data
uclh_data = read_csv("200326_uclh_data.csv")
# Janitor makes names lowercase and "_" between
uclh_data = clean_names(uclh_data)

# Reformat to date
uclh_data$bed_status_end_dttm[which(uclh_data$bed_status_end_dttm == "NULL")] = NA
uclh_data$bed_status_end_dttm = as.POSIXct(uclh_data$bed_status_end_dttm, tz = "GMT")

uclh_beds = uclh_data %>%
  filter(covid_capacity_type == "ICU") %>%
  select(bed_name, bed_status_start_dttm, bed_status_end_dttm, covid_status) %>%
  distinct() %>%
  group_by(bed_name) %>%
  arrange(bed_status_start_dttm) 

table(uclh_beds$covid_status)


ggplot()