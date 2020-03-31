#### UCLH data analysis 
library("janitor")
library("tidyverse")
library("ggplot2")
library(reshape2)

# DATA
uclh_data <- read_csv("200326_uclh_data.csv")
# Janitor makes names lowercase and "_" between
uclh_data <- clean_names(uclh_data)

# Looking at one patient
pat1 <- unique(uclh_data$patient_id)[1]
w <- which(uclh_data$patient_id == pat1)
uclh_data[w,c("effective_start_date","effective_start_time","effective_end_date","effective_end_time")]

# Trying to find COVID patients
u <- unique(uclh_data$patient_id)
length(u)
table(uclh_data$covid_confirmed_yes_no) # 1015 COVID positive - but not by patient

## Data for COVID positive patients
uclh_data$covid_capacity_type %>% table # What are all these beds?
#think these are just the categories that each bed falls in

uclh_data %>% 
  group_by(patient_id) %>% 
  summarise(covid_positive = any(covid_confirmed_yes_no=="Yes")) %>% 
  filter(!(patient_id=="NULL") & covid_positive==TRUE) -> covid_pos 

covid_patients <- uclh_data %>% 
  inner_join(covid_pos)

# Age groups. 
covid_patients %>% 
  mutate(age=as.integer(patient_age)) %>%
  mutate(age_group = cut(age, seq(0, max(age), by = 5))) %>% 
  count(age_group)
# no  < 10. Lots of elderly. 

#### First day entry (hope dates are in order...)
covid_patients %>% group_by(patient_id) %>% summarise(date_admission=bed_status_start_dttm[1])

covid_patients %>% group_by(patient_id) %>% glimpse()
covid_patients %>% group_by(patient_id) -> cov

# Doesn't look like dates are in order
cov_play <- cov[1:20,c("patient_id", "bed_status_start_dttm")]
# check arrange works. Yep think so
cov_play %>% group_by(patient_id) %>% arrange(bed_status_start_dttm)

# Use to arrange all 
covid_patients %>% group_by(patient_id) %>% arrange(bed_status_start_dttm) %>%
  summarise(date_admission=bed_status_start_dttm[1], first_bed = covid_capacity_type[1],
            bed_icu = as.integer(any(covid_capacity_type == "ICU"))) -> dates

table(dates$first_bed) # only 1 ICU on admission
table(dates$bed_icu) # 42 end up in ICU

# Not sure why x axis limits doesn't work... and bin error
ggplot(dates, aes(x=date_admission)) + geom_histogram() + 
  scale_x_datetime(limits = c(as.POSIXct("2020-01-01 00:00:00", format="%Y-%m-%dT%H:%M:%OS"),
                              as.POSIXct("2020-04-01 00:00:00", format="%Y-%m-%dT%H:%M:%OS")))


## Tried to sort out histogram but not working... .
# split patient id into character and number for histogram?
dates[,c("pat_x","pat_n")]<-colsplit(dates$patient_id, "(?<=\\p{L})(?=[\\d+$])", c("char", "digit"))
### All have "Z" - potentially could remove. 
dates$pat_n <- as.numeric(dates$pat_n)

dates$day <- as.Date(dates$date_admission)
as.POSIXct(dates$day, format = "%Y-%m-%d")
# doesn't work
##h <- hist(as.POSIXct(dates$day, format = "%Y-%m-%d")) #, "days",format="%Y-%m-%dT%H:%M:%OS")

# Incidence of patient arrivals: ideally would want only icu?
incidence_covid <- dates %>% group_by(day) %>% count()
ggplot(incidence_covid, aes(day, n)) + geom_bar(stat="identity")

write.csv(incidence_covid, "UCLH/data/uclh_incidence_covid_all.csv")

# Really want if arrive go into ICU? This is just if they every went into ICU
incidence_covid_evericu <- dates %>% filter(bed_icu == 1) %>% group_by(day) %>% count()

# Compare entries
ggplot(incidence_covid, aes(x=day, y = n)) + geom_line() + 
  geom_line(data = incidence_covid_evericu, col = "red") 

## Filter out by ICU - when enter that
cov_icu <- covid_patients %>% filter(covid_capacity_type == "ICU")

cov_icu_dates <- cov_icu %>% group_by(patient_id) %>% arrange(bed_status_start_dttm) %>%
  summarise(date_icu=bed_status_start_dttm[1])
cov_icu_dates$day <- as.Date(cov_icu_dates$date_icu)

inc_cov_icu <- cov_icu_dates %>% group_by(day) %>% count()
write.csv(inc_cov_icu, "UCLH/data/uclh_incidence_covid_icu.csv")

# Compare entries
ggplot(incidence_covid, aes(x=day, y = n)) + geom_line() + 
  geom_line(data = inc_cov_icu, col = "red") 

