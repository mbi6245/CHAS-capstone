# Visuals 

#ER 
colnames(did_visit_types_year)

# no! takes forever to load! 
# 
# did_visit_types_year %>% 
#   ggplot(aes(x = Date, y = ER, col = marsh))+
#   geom_jitter()+

# Best model Lines
ggplot()+
  geom_abline( intercept =(gee_mod_DID_ER_yr_best$coefficients[1] + gee_mod_DID_ER_yr_best$coefficients[2]),
               slope= (gee_mod_DID_ER_yr_best$coefficients[3] + gee_mod_DID_ER_yr_best$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_ER_yr_best$coefficients[1]),
               slope= gee_mod_DID_ER_yr_best$coefficients[3], col = "red")+ # line for Non-Hispanic White
  xlim(c(-0.5,4))+
  #ylim(c(0, 1))+
  labs(title= "Rates of Emergency Room Utilization")+
theme_bw()+
  xlab("Year (since 2019)")+
  ylab("ER visits per patient") #+
 # annotate("text", 
  #         x = 2, y = 0.5, label = ("Non-Hispanic White = red, Marshallese = blue") )
  #legend("right", col = c("Non-Hispanic White" = "red", "Marshallese" = "blue"))


ggplot()+
  geom_abline( intercept =(gee_mod_DID_PCP_yr_best$coefficients[1] + gee_mod_DID_PCP_yr_best$coefficients[2]),
               slope= (gee_mod_DID_PCP_yr_best$coefficients[3] + gee_mod_DID_PCP_yr_best$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_PCP_yr_best$coefficients[1]),
               slope= gee_mod_DID_PCP_yr_best$coefficients[3], col = "red")+ # line for Non-Hispanic White
  theme_bw()+
  xlim(c(-1,4))+
  labs(title= "Rates of Primary Care Provider Utilization")+
  xlab("Year (since 2019)")+
  ylab("PCP visits per patient")




# ggplot()+
#   geom_abline( intercept =(gee_mod_DID_ER_yr$coefficients[1] + gee_mod_DID_ER_yr$coefficients[2]), 
#                slope= (gee_mod_DID_ER_yr$coefficients[3] + gee_mod_DID_ER_yr$coefficients[4]), col = "blue")+ # line for Marshallese
#   geom_abline( intercept = (gee_mod_DID_ER_yr$coefficients[1]), 
#                slope= gee_mod_DID_ER_yr$coefficients[3], col = "red")+ # line for Non-Hispanic White
#   xlim(c(-1,4))+
#   labs(title= "Rates of Emergency Room Utilization")
#   
# 
# 
# ggplot()+
#   geom_abline( intercept =(gee_mod_DID_PCP_yr$coefficients[1] + gee_mod_DID_PCP_yr$coefficients[2]), 
#                slope= (gee_mod_DID_PCP_yr$coefficients[3] + gee_mod_DID_PCP_yr$coefficients[4]), col = "blue")+ # line for Marshallese
#   geom_abline( intercept = (gee_mod_DID_PCP_yr$coefficients[1]), 
#                slope= gee_mod_DID_PCP_yr$coefficients[3], col = "red")+ # line for Non-Hispanic White
#   xlim(c(-1,4))+
#   labs(title= "Rates of Primary Care Provider Utilization")



# graph number of all visits? 
#table_all_visit_types_wider <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/CHAS-capstone/Analysis Data/table_all_visit_types_wider.csv")


table_all_visit_types <- as.data.frame( with(all_visit_types, table(ServiceLine,year, marsh)) ) #, useNA = "always"


# this is a good visual idea but we need rates instead of total counts to show any Marshallese on the scale! 
table_all_visit_types %>% filter( year != 2025, year != 2016) %>%
  ggplot(aes(x = year, y = Freq, col = marsh))+
  geom_point()+
  facet_wrap(~ServiceLine)

# You can facet_wrap by 2 variables below, but it keeps the scale the same
# The problem is there are 2 different scales. 

# table_all_visit_types %>% filter( year != 2025, year != 2016) %>%
#   ggplot(aes(x = year, y = Freq, col = marsh))+
#   geom_point()+
#   facet_wrap(~ServiceLine + marsh)

# add a column with total population size for each year, then mutate to get the rate? 
# can adjust code from other DIDVisuals.R file

# nope just tells you how many rows we have for each, not the total from each column
#with(table_all_visit_types, table(year, marsh))



# NOPE TOTAL VISITS
# total_visit_count_marsh_year <- as.data.frame(with(all_visit_types, table(marsh, year) ))
# colnames(total_visit_count_marsh_year)[3] <- "total_pop"
# 
# table_all_visit_types <- left_join(table_all_visit_types, total_visit_count_marsh_year)
# table_all_visit_types <- table_all_visit_types %>% mutate(rate = Freq/total_pop)


total_pop_confirmed<- all_visit_types %>% group_by(marsh, year) %>% distinct(UniqueIdentifier) 
total_pop_confirmed <- with(total_pop_confirmed, table(marsh, year))
total_pop_confirmed <- as.data.frame(total_pop_confirmed)

write.csv(total_pop_confirmed, "Analysis Data/total_pop_confirmed_marsh_year.csv")
# confirmed= total population of unique patients each year
colnames(total_pop_confirmed)[3] <- "total_pop_confirmed"
table_all_visit_types <- left_join(table_all_visit_types, total_pop_confirmed)
 table_all_visit_types <- table_all_visit_types %>% mutate(rate_confirmed = Freq/total_pop_confirmed)
 colnames(table_all_visit_types)[5] <- "total_visits"


# 
# Marshallese_2016 <- all_visit_types %>% filter(marsh == 1, year == 2016) #
# length(unique(Marshallese_2016$UniqueIdentifier))
# 
# # 82
# 
# Marshallese_x <- all_visit_types %>% filter(marsh == 1, year == 2024) #
# length(unique(Marshallese_x$UniqueIdentifier))
# # 723
# 
# Control_x <- all_visit_types %>% filter(marsh == 0, year == 2016) #
# length(unique(Control_x$UniqueIdentifier))
# # 7465
# Control_x <- all_visit_types %>% filter(marsh == 0, year == 2024) #
# length(unique(Control_x$UniqueIdentifier))
# # Whoo-hoo! These match the table above
# 

# alternative way to try? 
# # remove duplicates then table? 
# check <- distinct(all_visit_types, UniqueIdentifier, .keep_all = TRUE)
# ?distinct
# with(check, table(marsh, year) 
# 
# # or !
# # nope can't figure out if M or NHW 
# # check <- as.data.frame(with(all_visit_types, table(UniqueIdentifier, year, marsh)))
# 
#  check <- as.data.frame(with(all_visit_types, table(UniqueIdentifier, year)))
# check <- check %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
#                              if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) )
# 
# check2 <- as.data.frame(with(check, table(marsh,  year)))

 # take out zeros beause they are actually missing, we shouldn't have them on the graph
  # table_all_visit_types$Freq[table_all_visit_types$Freq == 0 & table_all_visit_types$year == 2016] <- NA
  # table_all_visit_types$Freq[table_all_visit_types$Freq == 0 & table_all_visit_types$year == 2017] <- NA
   #table_all_visit_types <- table_all_visit_types %>% mutate(rate_confirmed = Freq/total_pop_confirmed)
   
   
 # Best, All Service Line Rates of NHW and M over a decade
table_all_visit_types %>% filter( year != 2025, year != 2016) %>% 
  ggplot(aes(x = year, y = rate_confirmed, col = marsh))+
  geom_point()+ # jitter optinos aes( alpha = 0.5), width= 0.25, height = 0
  facet_wrap(~ServiceLine)+
  theme_bw()+
  ylab("rate (number of visits per patient)")+
  labs(title= "Rates per Service Line \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinic")+
  scale_color_discrete(name = "Group", labels=c("Non-Hispanic White", "Marshallese"))+
  theme(legend.position = "bottom", legend.direction = "vertical")

# Best Marshallese over time 
table_all_visit_types %>% filter( year != 2025, year != 2016, marsh == 1) %>% 
  ggplot(aes(x = year, y = rate_confirmed, col = marsh))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ServiceLine)+
  theme_bw()+
  ylab("rate (number of visits per patient)")+
  labs(title= "Rates per Service Line \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinic")+
  scale_color_discrete(name = "Group", labels=c("Non-Hispanic White", "Marshallese"))+
  theme(legend.position = "bottom", legend.direction = "vertical")


# Best ER DID image
# table_all_visit_types %>% filter( year != 2025, year != 2016) %>% filter(ServiceLine == "Emergency") %>%
#   ggplot(aes(x = year, y = rate, col = marsh))+
#   geom_point()+
#   #facet_wrap(~ServiceLine)+
#   ylim(c(0, 0.2))+
#   theme_bw()+
#   ylab("ER rate (visits per patient)")+
#   labs(title= "Rates per Service Line \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinic")+
#   geom_vline(xintercept = 3.75, col = "goldenrod")


table_all_visit_types %>% filter( year != 2025, year != 2016, year != 2017) %>% filter(ServiceLine == "Emergency") %>%
  ggplot(aes(x = year, y = rate_confirmed, col = marsh))+
  geom_point()+
ylim(c(0, 1.25))+
  theme_bw()+
  ylab("ER Visits per Patient")+
  xlab("Year") +
 labs(title= "ER Rates Over Time by Race")+
  geom_vline(xintercept = 2.75, col = "black")+
  geom_vline(xintercept = 3.32, col = "grey", lty =2) +
  geom_vline(xintercept = 4.42, col = "grey", lty = 2) +
  scale_color_discrete(name = "Race", labels=c("Non-Hispanic White", "Marshallese"))+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size=11))


# Best PCP DID image

# table_all_visit_types %>% filter( year != 2025, year != 2016) %>% filter(ServiceLine == "Primary Care") %>%
#   ggplot(aes(x = year, y = rate, col = marsh))+
#   geom_point()+
#   #facet_wrap(~ServiceLine)+
#   #ylim(c(0, 0.2))+
#   theme_bw()+
#   ylab("PCP rate (visits per patient)")+
#   labs(title= "Rates per Service Line \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinic")+
#   geom_vline(xintercept = 3.75, col = "goldenrod")

table_all_visit_types %>% filter( year != 2025, year != 2016, year != 2017) %>% filter(ServiceLine == "Primary Care") %>%
  ggplot(aes(x = year, y = rate_confirmed, col = marsh))+
  geom_point()+
  ylim(c(0, 3.25))+
  theme_bw()+
  ylab("PC Visits per Patient)")+
  #labs(title= "Primary Care Rates Over Time by Race")+
  geom_vline(xintercept = 2.75, col = "black")+
  geom_vline(xintercept = 3.32, col = "grey", lty =2) +
  geom_vline(xintercept = 4.42, col = "grey", lty = 2) +
  scale_color_discrete(name = "Race", labels=c("Non-Hispanic White", "Marshallese"))+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size=11))

# lines
  ggplot()+
  geom_abline( intercept =(gee_mod_DID_ER_yr$coefficients[1] + gee_mod_DID_ER_yr$coefficients[2]), 
               slope= (gee_mod_DID_ER_yr$coefficients[3] + gee_mod_DID_ER_yr$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_ER_yr$coefficients[1]), 
               slope= gee_mod_DID_ER_yr$coefficients[3], col = "red")+ # line for Non-Hispanic White
  xlim(c(-1,4))+
    ylim(c(0, 0.2)) + 
  theme_bw()+
  labs(title= "Rates of Emergency Room Utilization")

# PCP DID images
  
#   # Stopped here
#   check <- table_all_visit_types %>% filter( year != 2025, year != 2016) %>% filter(ServiceLine == "Primary Care" | ServiceLine == "Health Equity") 
# check_wider <- check %>% pivot_wider(names_from = ServiceLine, 
#                       values_from = Freq)  
# check_wider <- check_wider %>% group_by(year) %>%summarize(sum("Primary Care", "Health Equity" , na.rm = TRUE)  )
  
# PCP and Health Equity ServiceLine == "Primary Care" | ServiceLine == "Health Equity") 
# import combined data from Excel
  fp_visits_combined = file.path(getwd(), "Analysis Data/table_all_visit_types_wider_combined.csv")
  table_all_visit_types_wider_combined <- read.csv(fp_visits_combined)   
  
  table_all_visit_types_wider_combined <- table_all_visit_types_wider_combined[20:21, ]
  table_all_visit_types_wider_combined <- t(table_all_visit_types_wider_combined)
  table_all_visit_types_wider_combined <- table_all_visit_types_wider_combined[-c(1:2), ] # get rid of junk from excel
  table_all_visit_types_wider_combined <- t(table_all_visit_types_wider_combined)
  class(table_all_visit_types_wider_combined)
  # "matrix" "array"
  table_all_visit_types_combined <- as.data.frame(table_all_visit_types_wider_combined) %>% pivot_longer(!marsh, 
                                                                                                         names_prefix = "X", 
                                                                                          values_to = "combined_visits", 
                                                                                          names_to = "year")
  colnames(table_all_visit_types_combined)
  colnames(total_pop_confirmed)
    
  
  table_all_visit_types_combined  <- left_join(table_all_visit_types_combined , total_pop_confirmed)
  
  class(table_all_visit_types_combined$combined_visits)
  table_all_visit_types_combined$combined_visits <- as.numeric(table_all_visit_types_combined$combined_visits)
    table_all_visit_types_combined  <- table_all_visit_types_combined  %>%  mutate(combined_rate = combined_visits/total_pop_confirmed)
  
  
  
  table_all_visit_types_combined %>% filter( year != 2025, year != 2016) %>%
    ggplot(aes(x = year, y = combined_rate, col = marsh))+
    geom_point()+
#    facet_wrap(~ServiceLine)+
  #  ylim(c(0, 0.2))+
    theme_bw()+
    labs(title= "Combined Rates PCP and Health Equity \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinic")
  
  
  
  
  
  
  
#### Lloyd's idea for correlation between the covariates
# Using Corr Plots from ML Class Classification.R script

#GGally
ggpairs(full[,c(1:5,13)])
# pairwise variables for all 6 columns, 
# diagonol is density or summary for that variable


targetpop_DID <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/CHAS-capstone/Analysis Data/targetpop_DID.csv")

colnames(targetpop_DID)
targetpop_pairs <- targetpop_DID %>% select(age, Sex, No.column.name, IncomeLevel, BLACERISK, Marsh, BMI)
library(GGally)
ggpairs(targetpop_pairs)
