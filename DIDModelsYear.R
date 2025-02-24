# Repeat DID Models But Use Each Quarter as the Population, Not Each Year

# DID Models
#rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo) # to extract month out of the standard date format
library(rstudioapi) # to allow us to use file.path 
library(did) # package from other professors who require time change in start
library(geepack) #geeglm

# Import dataframes DataCleaningDID.R script


fp_visits = file.path(getwd(), "Analysis Data/all_visit_types.csv")
all_visit_types <- read.csv(fp_visits) 
# Already marked Visit Types with Marshallese and NHW
# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)


# start building dataframe
# Change to year

did_visit_types_year <- all_visit_types %>% filter(Date >= "2018-08-31", Date <= "2019-08-31"  | # pretreat
                                                Date >= "2021-08-31", Date <= "2022-08-31") # post treat

# start_date = "2018-08-31", end_date = "2019-08-31") # All service lines = total population
# 
# start_date = "2021-08-31", end_date = "2022-08-31") # All service lines = total population


# remove the NA, not our target pop
did_visit_types_year <- did_visit_types_year %>% filter(!is.na(marsh)) 
# removed 1/3 of visits not our target pop



did_visit_types_year <- did_visit_types_year %>% mutate(ER = if_else( ((ServiceLine == "Emergency") &
                                                                    (Date >= "2019-06-01" & Date <= "2019-08-31" |
                                                                       Date >= "2022-06-01" & Date <= "2022-08-31" ) ), 1, 0),
                                              PCP = if_else( ( ( ServiceLine == "Primary Care") &
                                                                 (Date >= "2019-06-01" & Date <= "2019-08-31" |
                                                                    Date >= "2022-06-01"& Date <= "2022-08-31" ) ), 1, 0))



######################################################
########## BUILD DATA FRAME FOR DID ##################
######################################################




# size of our populations in this version of the study

# 
Marshallese_yr <- did_visit_types_year %>% filter(marsh == 1) 
length(unique(Marshallese_yr$UniqueIdentifier))
# 476 Marshallese visited these 2 clinics this year

NHW_yr <- did_visit_types_year %>% filter(marsh == 0) 
length(unique(NHW_yr$UniqueIdentifier))
# 18586 NonHispanic Whites visited these 2 clinics this year


# we will have some of the same patients who appear in both time frames, but more in the 2nd time period

# and all Non-Hispanic White uniqueID for any type of appointment per year



# number of ER visits per month/ Population size per year

# numerator from the  ER2016_2025_yearmonth_marsh and 

# add months 
#all_visit_types <- all_visit_types %>% mutate(yearmonth = as.Date(zoo::as.yearmon(all_visit_types$Date, "%m/%d/%Y")))

# denominator from pop_size_year 




# Jan 2017 to Aug 2019 ER Pretrends (about 2.5 years)

# Sept 2019 to Sept 2022 DID ER Estimates for CHW 1 and 2

# Oct 2022 to Dec 2024 additional ER trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 


# ! Note that ER visits are recorded from 2017 forward. 

# But primary care provider (PCP) visits are only recorded from 2018 forward
# Jan 2018 to Aug 2019 PCP Pretrends (about 1.5 years)

# Sept 2019 to Sept 2022 DID PCP Estimates for CHW 1 and 2

# Oct 2022 to Dec 2024 additional PCP trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 



# How to we account for correlation with longitutindal data  
# using unbalanced panel in the DID package




################### GEEGLM #################
############################################

# Standford said they use GEEGLM for correlation 
# https://diff.healthpolicydatascience.org/


# Use GEEGLM for all



# Center the data so the intercept has better interpretation- Won't effect Slope or p-values

did_visit_types_year <- did_visit_types_year %>% mutate(year_center = (year - 2019) )


gee_mod_DID_PCP <- geeglm(PCP ~ marsh*year_center, 
                          data = did_visit_types_year,
                          id = UniqueIdentifier,
                          family = gaussian, 
                          scale.fix = T, # this sets phi = 1
                          corstr = "exchangeable")
#print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP)


# Call:
#   geeglm(formula = PCP ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_year, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate   Std.err    Wald Pr(>|W|)    
# (Intercept)        0.084075  0.000896 8813.17  < 2e-16 ***
#   marsh             -0.010641  0.007963    1.79     0.18    
# year_center        0.015998  0.000453 1247.24  < 2e-16 ***
#   marsh:year_center -0.017449  0.003240   29.01  7.2e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = exchangeable 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate Std.err
# alpha  -0.0172 0.00149
# Number of clusters:   186070  Maximum cluster size: 2 

gee_mod_DID_ER <- geeglm(ER ~ marsh*year_center, 
                         data = did_visit_types_year,
                         id = UniqueIdentifier,
                         family = gaussian, 
                         scale.fix = T, # this sets phi = 1
                         corstr = "exchangeable")
#print(gee_mod_DID_ER)
summary(gee_mod_DID_ER)

# Call:
#   geeglm(formula = ER ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_year, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate   Std.err    Wald Pr(>|W|)    
# (Intercept)        0.036872  0.000613 3623.86  < 2e-16 ***
#   marsh              0.012142  0.006521    3.47    0.063 .  
# year_center        0.003297  0.000295  125.11  < 2e-16 ***
#   marsh:year_center -0.011148  0.002491   20.03  7.6e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = exchangeable 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate Std.err
# alpha     0.04 0.00103
# Number of clusters:   186070  Maximum cluster size: 2
gee_mod_DID_ER$coefficients[1]

# ! how to extract pvalue? 


# Extract/ Calculate the rate with the predict function
# # this will give us the same response because it is the same data.
# but we could also use it to predict future years

 predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))
 
 
 

   
   
   Marshallese_yr_2019 <- did_visit_types_year %>% filter(marsh == 1, year == 2019) 
 length(unique(Marshallese_yr_2019$UniqueIdentifier))
 # 157 Marshallese visited these 2 clinics this year
 
 # # compare to actual counts
 # predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))* length(unique(Marshallese_yr_2019$UniqueIdentifier))
# 5.79 
  # sum(Marshallese_yr_2019$ER)
 # # 58
 # 
 
 rate <- predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))
 rate*nrow(Marshallese_yr_2019)
 # 22.5 
 sum(Marshallese_yr_2019$ER)
# still doesn't match... !
 
 # NHW_yr <- did_visit_types_year %>% filter(marsh == 0, year == 2019) 
 # length(unique(NHW_yr$UniqueIdentifier))
 # #  NonHispanic Whites visited these 2 clinics this year
 
# 
# predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))*100
# number of visits per Marshallese 100 patients in year 2019


# From Machine Learning HW
# when you supply a new 1 line dataframe it will give me the 1 predicted response for covariates in the





###############################################################
# Only Correlated Data

# filter the model for only patients who were there in both years
# the opposite of an LM model where we assume all independent, these are all correlated. 


did_visit_types_year_2019 <- did_visit_types_year %>% filter(year == 2019)
did_visit_types_year_2022 <- did_visit_types_year %>% filter(year == 2022)

UID_balanced_panel_year <- intersect(did_visit_types_year_2019$UniqueIdentifier, did_visit_types_year_2022$UniqueIdentifier)

did_visit_types_year_balanced <- did_visit_types_year %>% filter(UniqueIdentifier %in% UID_balanced_panel_year)




# size of our populations in this version of the study

# 
Marshallese_yr_bal <- did_visit_types_year_balanced %>% filter(marsh == 1) 
length(unique(Marshallese_yr_bal$UniqueIdentifier))
# 118 Marshallese visited these 2 clinics this year

NHW_yr_bal <- did_visit_types_year_balanced %>% filter(marsh == 0) 
length(unique(NHW_yr_bal$UniqueIdentifier))
# 8906 NonHispanic Whites visited these 2 clinics this year




gee_mod_DID_ER_bal <- geeglm(ER ~ marsh*year_center, 
                             data = did_visit_types_year_balanced,
                             id = UniqueIdentifier,
                             family = gaussian, 
                             scale.fix = T, # this sets phi = 1
                             corstr = "exchangeable")
# print(gee_mod_DID_ER)
summary(gee_mod_DID_ER_bal)

# Call:
#   geeglm(formula = ER ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_year_balanced, id = UniqueIdentifier, 
#          corstr = "exchangeable", scale.fix = T)
# 
# Coefficients:
#   Estimate   Std.err    Wald Pr(>|W|)    
# (Intercept)        0.037391  0.000671 3105.06  < 2e-16 ***
#   marsh              0.025770  0.008986    8.22  0.00413 ** 
#   year_center        0.003522  0.000344  104.63  < 2e-16 ***
#   marsh:year_center -0.012391  0.003628   11.67  0.00064 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = exchangeable 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate Std.err
# alpha   0.0425 0.00115
# Number of clusters:   132115  Maximum cluster size: 2 
gee_mod_DID_ER_bal$coefficients[1]


gee_mod_DID_PCP_bal <- geeglm(PCP ~ marsh*year_center, 
                              data = did_visit_types_year_balanced,
                              id = UniqueIdentifier,
                              family = gaussian, 
                              scale.fix = T, # this sets phi = 1
                              corstr = "exchangeable")
# print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP_bal)

# Call:
#   geeglm(formula = PCP ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_year_balanced, id = UniqueIdentifier, 
#          corstr = "exchangeable", scale.fix = T)
# 
# Coefficients:
#   Estimate   Std.err   Wald Pr(>|W|)    
# (Intercept)        0.092133  0.001019 8179.7  < 2e-16 ***
#   marsh              0.012383  0.011320    1.2     0.27    
# year_center        0.012858  0.000535  577.2  < 2e-16 ***
#   marsh:year_center -0.024194  0.004713   26.4  2.8e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = exchangeable 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate Std.err
# alpha  0.00487 0.00153
# Number of clusters:   132115  Maximum cluster size: 2 


# !are total interactions growing? Even if ER rates are going up.
