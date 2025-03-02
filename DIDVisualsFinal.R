# Visuals 

#ER 
colnames(did_visit_types_year)

# no! takes forever to load! 
# 
# did_visit_types_year %>% 
#   ggplot(aes(x = Date, y = ER, col = marsh))+
#   geom_jitter()+

# Best model
ggplot()+
  geom_abline( intercept =(gee_mod_DID_ER_yr_best$coefficients[1] + gee_mod_DID_ER_yr_best$coefficients[2]),
               slope= (gee_mod_DID_ER_yr_best$coefficients[3] + gee_mod_DID_ER_yr_best$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_ER_yr_best$coefficients[1]),
               slope= gee_mod_DID_ER_yr_best$coefficients[3], col = "red")+ # line for Non-Hispanic White
  xlim(c(-1,4))+
  labs(title= "Rates of Emergency Room Utilization")



ggplot()+
  geom_abline( intercept =(gee_mod_DID_PCP_yr_best$coefficients[1] + gee_mod_DID_PCP_yr_best$coefficients[2]),
               slope= (gee_mod_DID_PCP_yr_best$coefficients[3] + gee_mod_DID_PCP_yr_best$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_PCP_yr_best$coefficients[1]),
               slope= gee_mod_DID_PCP_yr_best$coefficients[3], col = "red")+ # line for Non-Hispanic White
  xlim(c(-1,4))+
  labs(title= "Rates of Primary Care Provider Utilization")



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
