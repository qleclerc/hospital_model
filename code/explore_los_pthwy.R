### Proposed analysis

library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)
theme_set(theme_bw())

## SETUP ####
#Load model functions
source("code/beds_needed_uclh_functions.R")

cov_curve_all = read.csv("data/200403_pietro_preds.csv")[1:242,]
colnames(cov_curve_all)[1] = "Date"
cov_curve_all[is.na(cov_curve_all)] = 0

# cov_curve_all %>%
#   select(-day) %>%
#   mutate(Date = as.Date(Date) %>%
#   melt(., id.vars = "Date") %>%
#   ggplot(.) +
#   geom_line(aes(Date, value, group = variable, colour = variable)) +
#   scale_colour_discrete(breaks = c("basic", "X20pred", "X60pred"),
#                         labels = c("Basic", "20% reduction", "60% reduction")) +
#   labs(x = "Time (months)", y = "Forecasted number of COVID19 admissions", colour = "Scenario:") +
#   theme_bw()




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
#multiple simulations

## BASE SCENARIO ####
for(ii in 1:6){
  name <- colnames(cov_curve_all[ii+2])
  
  cov_curve = cov_curve_all[,ii+2]
  
  ## Values to vary
  los_icu_vary <- seq(5,25,5)
  prop_to_p1 <- seq(0,100,10) # Proportion go to pathway 1 of the 25% that go to critical care
  
  results_all <- c()
  
  for(i in 1:length(los_icu_vary)){
    for(j in 1:length(prop_to_p1)){
      
      params$average_LoS_ICU <- los_icu_vary[i]
      params$prop_path1 <- prop_to_p1[j]/100
      params$prop_path2 <- (1 - prop_to_p1[j]/100)
      print(c(params$average_LoS_ICU, params$prop_path1))
      
      results_base = multi_uclh_model(nruns = 50, cov_curve, params, run_duration)
      results_base$date <- seq(as.Date("2020/3/7"), by = "days", length.out = length(results_base[,1]))
      results_base$los <- los_icu_vary[i]
      results_base$pp1 <- prop_to_p1[j]
      
      results_all <- rbind(results_all, results_base)
    }
    
  }
  
  results_all <- as.data.frame(results_all)
  
  grped <- results_all %>% group_by(los, pp1) %>%
    summarise(ICU.max = max(ICU_beds), HDU.max = max(HDU_beds),
              icu.max.when = which.max(ICU_beds), hdu.max.when = which.max(HDU_beds))
  
  g1<-ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = ICU.max)) + 
    ggtitle("ICU") + 
    scale_y_continuous("Proportion of all critical care to ICU on admission") + 
    scale_x_continuous("Length of stay in ICU") + 
    scale_fill_distiller(palette = "Spectral","Max. # beds",limits = c(0,2500))  
  scale_fill_manual()
  ggsave(paste0("explore/",name,"icu_fill.pdf"))
  
  g2<-ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = HDU.max)) + 
    ggtitle("HDU") + 
    scale_y_continuous("Proportion of all critical care to ICU on admission") + 
    scale_x_continuous("Length of stay in ICU") + 
    scale_fill_distiller("Max. # beds")  
  ggsave(paste0("explore/",name,"hdu_fill.pdf"))
  
  g3<-ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = icu.max.when)) + 
    ggtitle("ICU") + 
    scale_y_continuous("Proportion of all critical care to ICU on admission") + 
    scale_x_continuous("Length of stay in ICU") + 
    scale_fill_distiller(palette = "Spectral","Time to peak")
  ggsave(paste0("explore/",name,"icu_fill_time.pdf"))
  
  g4<-ggplot(grped, aes(x = los, y = pp1)) + geom_tile(aes(fill = hdu.max.when)) + 
    ggtitle("HDU") + 
    scale_y_continuous("Proportion of all critical care to ICU on admission") + 
    scale_x_continuous("Length of stay in ICU") + 
    scale_fill_distiller("Time to peak",limits = c(60,120))
  ggsave(paste0("explore/",name,"hdu_fill_time.pdf"))
  
  (g1+g2)/(g3+g4)
  ggsave(paste0("explore/",name,"square_fill.pdf"))
  
  
  ggplot(results_all, aes(x=time, y = ICU_beds)) + 
    geom_line(aes(group = interaction(los,pp1),col = factor(los), linetype = factor(pp1))) 
  ggsave(paste0("explore/",name,"icu_line.pdf"))
  
  ggplot(results_all%>%filter(los == 5), aes(x=time, y = ICU_beds)) + 
    geom_line(aes(group = interaction(los,pp1),col = factor(pp1)))
  ggsave(paste0("explore/",name,"icu_line_5.pdf"))
  
  ggplot(results_all, aes(x=time, y = HDU_beds)) + 
    geom_line(aes(group = interaction(los,pp1),col = factor(los), linetype = factor(pp1)))
  ggsave(paste0("explore/",name,"hdu_line.pdf"))
  
  ggplot(results_all%>%filter(los == 25), aes(x=time, y = HDU_beds)) + 
    geom_line(aes(group = interaction(los,pp1),col = factor(pp1)))
  ggsave(paste0("explore/",name,"hdu_line_25.pdf"))
  
  
  # What number of cases actually go to ICU? 
  
  cov_curve_all %>%
    select(-day) %>%
    mutate(Date = as.Date(Date)) %>%
    melt(., id.vars = "Date") %>%
    ggplot(.) +
    geom_line(aes(Date, 0.255*value, group = variable, colour = variable)) +
    labs(x = "Time (months)", y = "Forecasted number of COVID19 ICU admissions", colour = "Scenario:") +
    theme_bw() + 
    geom_line(aes(Date, 0.01*value, group = variable, colour = variable), linetype = "dashed") 
  #   big variation - must be the queuing that really fills beds... not the arriving
  
  ## Proportion change
  max.ICU <- max(grped$ICU.max)
  max.HDU <- max(grped$HDU.max)
  
  grped$propmax.icu <- grped$ICU.max / max.ICU
  grped$propmax.hdu <- grped$HDU.max / max.HDU
  
  ggplot(grped, aes(x=los,y=propmax.icu)) + geom_point(aes(col=factor(los)))
  ggplot(grped, aes(x=pp1,y=propmax.icu)) + geom_point(aes(col=factor(los)))
}


