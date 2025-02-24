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
# Mark Visit Types with Marshallese and NHW
# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)


# start building dataframe
did_visit_types_qtr <- all_visit_types %>% filter(Date >= "2019-06-01", Date <= "2019-08-31"  | # pretreat
                                                Date >= "2022-06-01", Date <= "2022-08-31") # post treat
# remove the NA, not our target pop
did_visit_types_qtr <- did_visit_types_qtr %>% filter(!is.na(marsh)) 


Marshallese_qtr <- did_visit_types_qtr %>% filter(marsh == 1) 
length(unique(Marshallese_qtr$UniqueIdentifier))
# 315 Marshallese visited these 2 clinics this year

NHW_qtr <- did_visit_types_qtr %>% filter(marsh == 0) 
length(unique(NHW_qtr$UniqueIdentifier))
# 13804 NonHispanic Whites visited these 2 clinics this year

######################################################
########## BUILD DATA FRAME FOR DID ##################
######################################################



# we will have some of the same patients who appear in both time frames, but more in the 2nd time period

# and all Non-Hispanic White uniqueID for any type of appointment per quarter



# number of ER visits per month/ Population size per quarter! 
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




# !are total interactions growing? Even if ER rates are going up.



# How to we account for correlation with longitutindal data  
################### GEEGLM #################
############################################

# Standford said they use GEEGLM for correlation 
# https://diff.healthpolicydatascience.org/

# to use normal linear model need to limit to those who are in only 1 time frames !! 
# Change data source? 
# attempt2 <- lm(ER ~ marsh*year  , data = did_visit_types_qtr)
# summary(attempt2)
# # all significant! 
# 
# attempt3 <- lm(PCP ~ marsh*year  , data = did_visit_types_qtr)
# summary(attempt3)

# Instead use GEEGLM for all



# From Longitudinal lectures
# mod1 <- geeglm(distance ~ Sex*age8, data = Orthodont,
#                + id = Subject, corstr = "independence")

# gee_mod_x <- geeglm(x_binary ~ y + minutes_in_transport + sex + ega_cat , #TimePoint Being replaced by minutes_in_transport, not TimeDiff, not real time stamp,
#                     data = mydata,
#                     id = PRID,
#                     family=binomial(link="logit"),
#                     # waves = time,
#                     scale.fix = T, # this sets phi = 1
#                     corstr = "exchangeable")
# print(gee_mod_x)



# gee_mod_DID_PCP <- geeglm(PCP ~ marsh*year, 
#                     data = did_visit_types_qtr,
#                     id = UniqueIdentifier,
#                     family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
#                     # waves = time, # we only have one wave of treatment
#                     scale.fix = T, # this sets phi = 1
#                     corstr = "exchangeable")
# print(gee_mod_DID_PCP)
# summary(gee_mod_DID_PCP)
# 
# Call:
#   geeglm(formula = PCP ~ marsh * year, family = gaussian, data = did_visit_types_qtr, 
#          id = UniqueIdentifier, corstr = "exchangeable", scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err  Wald Pr(>|W|)    
# (Intercept) 12.34366  3.11206 15.73 7.30e-05 ***
#   marsh       90.87614 22.65508 16.09 6.04e-05 ***
#   year        -0.00590  0.00154 14.68 0.000128 ***
#   marsh:year  -0.04505  0.01121 16.16 5.83e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = exchangeable 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate  Std.err
# alpha   -1.025 0.001474
# Number of clusters:   50601  Maximum cluster size: 2

# 
# gee_mod_DID_ER <- geeglm(ER ~ marsh*year, 
#                           data = did_visit_types_qtr,
#                           id = UniqueIdentifier,
#                           family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
#                           # waves = time, # we only have one wave of treatment
#                           scale.fix = T, # this sets phi = 1
#                           corstr = "exchangeable")
# print(gee_mod_DID_ER)
# summary(gee_mod_DID_ER)

# 
# Call:
#   geeglm(formula = ER ~ marsh * year, family = gaussian, data = did_visit_types_qtr, 
#          id = UniqueIdentifier, corstr = "exchangeable", scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err  Wald Pr(>|W|)    
# (Intercept) 23.38413  2.31464 102.1  < 2e-16 ***
#   marsh       82.33869 19.25319  18.3  1.9e-05 ***
#   year        -0.01149  0.00115 100.7  < 2e-16 ***
#   marsh:year  -0.04075  0.00952  18.3  1.9e-05 ***
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
# alpha   -0.153 0.00694
# Number of clusters:   50601  Maximum cluster size: 2


# This is intercept for year zero, we want to know baseline of 2019
# Center the data so the intercept has better interpretation- Won't effect Slope or p-values

did_visit_types_qtr <- did_visit_types_qtr %>% mutate(year_center = (year - 2019) )

did_visit_types_qtr <- did_visit_types_qtr %>% mutate(ER = if_else(ServiceLine == "Emergency", 1, 0), 
                                              PCP = if_else(ServiceLine == "Primary Care", 1, 0))


gee_mod_DID_PCP_qtr <- geeglm(PCP ~ marsh*year_center, 
                          data = did_visit_types_qtr,
                          id = UniqueIdentifier,
                          family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                          # waves = time, # we only have one wave of treatment
                          scale.fix = T, # this sets phi = 1
                          corstr = "exchangeable")
# print(gee_mod_DID_PCP_qtr)
summary(gee_mod_DID_PCP_qtr)
# 
# Call:
#   geeglm(formula = PCP ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_qtr, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err     Wald Pr(>|W|)    
# (Intercept)        0.43243  0.00367 13855.33  < 2e-16 ***
#   marsh             -0.07211  0.03077     5.49  0.01910 *  
#   year_center       -0.00590  0.00154    14.68  0.00013 ***
#   marsh:year_center -0.04505  0.01121    16.16  5.8e-05 ***
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
# alpha    -1.02 0.00147
# Number of clusters:   50601  Maximum cluster size: 2 


gee_mod_DID_ER_qtr <- geeglm(ER ~ marsh*year_center, 
                         data = did_visit_types_qtr,
                         id = UniqueIdentifier,
                         family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                         # waves = time, # we only have one wave of treatment
                         scale.fix = T, # this sets phi = 1
                         corstr = "exchangeable")
#print(gee_mod_DID_ER_qtr)
summary(gee_mod_DID_ER_qtr)


# Call:
#   geeglm(formula = ER ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_qtr, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err    Wald Pr(>|W|)    
# (Intercept)        0.17617  0.00281 3936.18  < 2e-16 ***
#   marsh              0.05865  0.02712    4.68    0.031 *  
#   year_center       -0.01149  0.00115  100.73  < 2e-16 ***
#   marsh:year_center -0.04075  0.00952   18.31  1.9e-05 ***
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
# alpha   -0.153 0.00694
# Number of clusters:   50601  Maximum cluster size: 2 


# Extract/ Calculate the rate with the predict function
predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))
# this will give us the same response because it is the same data.

predict(gee_mod_DID_ER, type =  "response", newdata = data.frame( marsh = 0, year_center = 0))*100
# number of visits per Marshallese 100 patients in year 2019


# From Machine Learning HW
# when you supply a new 1 line dataframe it will give me the 1 predicted response for covariates in the
# glm.prob.train_10_12 = predict(glm.model, type = "response", data.frame(ave_rad = 10, ave_texture= 12))





###############################################################
# Only Correlated Data

# filter the model for only patients who were there in both years
# the opposite of an LM model where we assume all independent, these are all correlated. 


did_visit_types_qtr_2019 <- did_visit_types_qtr %>% filter(year == 2019)
did_visit_types_qtr_2022 <- did_visit_types_qtr %>% filter(year == 2022)

UID_balanced_panel_year <- intersect(did_visit_types_qtr_2019$UniqueIdentifier, did_visit_types_qtr_2022$UniqueIdentifier)

did_visit_types_qtr_balanced <- did_visit_types_qtr %>% filter(UniqueIdentifier %in% UID_balanced_panel_year)


gee_mod_DID_ER_bal <- geeglm(ER ~ marsh*year_center, 
                         data = did_visit_types_qtr_balanced,
                         id = UniqueIdentifier,
                         family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                         # waves = time, # we only have one wave of treatment
                         scale.fix = T, # this sets phi = 1
                         corstr = "exchangeable")
# print(gee_mod_DID_ER)
summary(gee_mod_DID_ER_bal)

# Call:
#   geeglm(formula = ER ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_qtr_balanced, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err    Wald Pr(>|W|)    
# (Intercept)        0.16970  0.00336 2551.94  < 2e-16 ***
#   marsh              0.06107  0.03390    3.24    0.072 .  
# year_center       -0.00694  0.00150   21.32  3.9e-06 ***
#   marsh:year_center -0.03194  0.01377    5.38    0.020 *  
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
# alpha   -0.198 0.00596
# Number of clusters:   26851  Maximum cluster size: 2 


gee_mod_DID_PCP_bal <- geeglm(PCP ~ marsh*year_center, 
                          data = did_visit_types_qtr_balanced,
                          id = UniqueIdentifier,
                          family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                          # waves = time, # we only have one wave of treatment
                          scale.fix = T, # this sets phi = 1
                          corstr = "exchangeable")
# print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP_bal)

# Call:
#   geeglm(formula = PCP ~ marsh * year_center, family = gaussian, 
#          data = did_visit_types_qtr_balanced, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err    Wald Pr(>|W|)    
# (Intercept)        0.40601  0.00439 8542.11  < 2e-16 ***
#   marsh             -0.00217  0.03953    0.00     0.96    
# year_center        0.00244  0.00202    1.47     0.23    
# marsh:year_center -0.07184  0.01645   19.08  1.3e-05 ***
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
# alpha   -0.719 0.00347
# Number of clusters:   26851  Maximum cluster size: 2
