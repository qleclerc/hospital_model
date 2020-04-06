#quick script for converting pietro excel to useable csv

library(stringr)
library(dplyr)
library(readr)
library(openxlsx)

## open raw excel ####
raw_data = openxlsx::read.xlsx("data/UCLH_arrivals_0304.xlsx")


## extract scenarios ####
scenarios = unlist(raw_data[which(str_detect(do.call(paste, c(raw_data, sep="-")), "mitigation")),])
scenarios = scenarios[!is.na(scenarios)]
scenarios = readr::parse_number(scenarios)
scenarios = paste0(scenarios, "pred")


## extract dates ####
dates = unlist(raw_data[,which(str_detect(raw_data, "Date"))])
dates = dates[!is.na(dates)]
head(dates) #should have "Date" first
#ASSUMPTION that "Date" is first
dates = as.Date(as.numeric(dates[-1]), origin = "1899-12-30") 


## extract admissions ####
all_incidence = raw_data[,which(str_detect(raw_data, "All ages"))]
head(all_incidence) #first 3 rows should be rubbish
#ASSUMPTION that first 3 rows should be removed
#ASSUMPTION that rows past max date should be removed
all_incidence = all_incidence[c(4:(3+length(dates))),]
#break apart dataset and convert to numeric:
all_incidence = as.numeric(unlist(all_incidence)) 
#re-generate dataset:
all_incidence = matrix(all_incidence, nrow = length(dates), ncol = length(scenarios), byrow = F)
all_incidence = as.data.frame(all_incidence)
colnames(all_incidence) = scenarios


## build clean dataset ####
pred_summary = data.frame(Date = dates,
                          time = seq(1, length(dates), 1))
pred_summary = cbind(pred_summary, all_incidence)

filename = paste0("data/", format(Sys.Date(), "%y%d%m"), "_pietro_preds.csv")
write.csv(pred_summary, filename, row.names = F)
