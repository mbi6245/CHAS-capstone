# DID Models
#rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(rstudioapi)
library(did)
library(geepack) #geeglm

# Import dataframes DataCleaningDID.R script


fp_visits = file.path(getwd(), "Analysis Data/all_visit_types.csv")
all_visit_types <- read.csv(fp_visits) 
# Mark Visit Types with Marshallese and NHW
# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)


# start building dataframe
did_visit_types <- all_visit_types %>% filter(Date >= "2019-06-01", Date <= "2019-08-31"  | # pretreat
                                                Date >= "2022-06-01", Date <= "2022-08-31") # post treat
# remove the NA, not our target pop
did_visit_types <- did_visit_types %>% filter(!is.na(marsh)) 


# trying to fit DID pattern, but I don't think we need these for GEEGLM
# mark post treatment (the indicator = 0 means its pretreat)
did_visit_types <- did_visit_types %>% mutate(after.treat = if_else(year == 2022, 1, 0))


did_visit_types <- did_visit_types %>% mutate(first.treat = if_else(marsh == 1, 2019, 0))
# 
# 
# 
# fp_tarpop = file.path(getwd(), "Analysis Data/targetpop_DID.csv")
# targetpop_DID <- read.csv(fp_tarpop) 


# 
# Marshallese <- targetpop_DID %>% filter(Race == 'Marshallese' | Language == 'Marshallese'| KOHParticipant == 1) #
# MarshalleseUniqueID <- Marshallese$UniqueID
# # length(MarshalleseUniqueID)
# # 4824 repeated uniqueID for multiple visits
# 
# length(unique(MarshalleseUniqueID))
# 
# Control <- targetpop_DID %>% filter((Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))
# ControlUniqueID <- Control$UniqueID
# #length(ControlUniqueID)
# # 136022
# length(unique(ControlUniqueID))
# # 25616



######################################################
########## BUILD DATA FRAME FOR DID ##################
######################################################



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




# !are total interactions growing? Even if ER rates are going up.



# How to we account for correlation with longitutindal data  
# using unbalanced panel in the DID package



# to use DID package

# Best Guess for DID! 
# mw.attgt <- att_gt( yname = "ER", # outcome name
#                     idname = "UniqueIdentifier", # each observation
#                     # gname = , # group name, first.treat # we don't have groups varying over time like treated in 2003, 2004...
#                     tname = "year", # time name
#                     xformla = "marsh",
#                     #, # we will not condition on any other covariates, or leave blank
#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                     data = did_visit_types)
# 
# 
# 
# mw.attgt <- att_gt(yname = "ER", # outcome name
#                    idname = "UniqueIdentifier", # each observation
#                    gname = "marsh", # group name, first.treat
#                    tname = "year", # time name
#                    xformla = ~1, 
#                    #, # we will not condition on any other covariates, or leave blank
#                    # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                    data = did_visit_types)
# 


# Example from DID package
# estimate group-time average treatment effects on the treated without covariates

# att_gt
# Group-Time Average Treatment Effects
# Description
# att_gt computes average treatment effects in DID setups where there are more than two periods of
# data and allowing for treatment to occur at different points in time and allowing for treatment effect
# heterogeneity and dynamics. See Callaway and Sant’Anna (2021) for a detailed description

# We don't want this one because we don't have groups varying over time 
mw.attgt <- att_gt(yname = "lemp", # the outcome is the log employment rate per county
                   gname = "first.treat", # year the group was first treated, from 2003-2007, 0 for untreated
                   idname = "countyreal", # unique identifier for county
                   tname = "year", # the column with time
                   xformla = ~1, # no additional covariates in the model
                   data = mpdta)




# doesn't work with forcing a group name
# mw.attgt <- att_gt( yname = "ER", # outcome name/ each row has 1 ER visit per patient in the time periods of interest
#                     idname = "UniqueIdentifier", # each patient/observation has its own unique ID
#                     gname = "first.treat", # group name, first.treat it is the same as ever treated/Marshallese = 2019
#                     tname = "year", # time name
#                     xformla = ~ marsh, 
#                     #, # we will not condition on any other covariates, or leave blank
#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                     data = did_visit_types)
# # Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
# #                            No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).
# #                          In addition: Warning messages:
# #                            1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
# #                                                    Dropped 315 units that were already treated in the first period.
# #                                                  2: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
# #                                                                          Dropped 10949 observations while converting to balanced panel.


# doesn't work without a group name.
# 
# mw.attgt <- att_gt( yname = "ER", # outcome name
#                     +                     idname = "UniqueIdentifier", # each observation
#                     +                    # gname = "first.treat", # group name, first.treat it is the same as ever treated/marsh = 2019
#                       +                     tname = "year", # time name
#                     +                     xformla = ~ marsh, 
#                     +                     #, # we will not condition on any other covariates, or leave blank
#                       +                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                       +                     data = did_visit_types)
# Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
#                            data[, gname] must be numeric

# mw.attgt <- att_gt( yname = "ER", # outcome name/ each row has 1 ER visit per patient in the time periods of interest 
# or 0 if the visit was another type of visit

#                     idname = "UniqueIdentifier", # each patient/observation has its own unique ID

#                     gname = "first.treat", # group name, first.treat it is the same as ever treated/Marshallese = 2019

#                     tname = "year", # Year 2019 (pretreatment) or 2022 (post treatment)

#                     xformla = ~ 1, # we will not condition on any other covariates, so leave blank

#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first

#                     data = did_visit_types)
# 
# Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
#                            No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).
#                          In addition: Warning messages:
#                            1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                                                    Dropped 315 units that were already treated in the first period.
#                                                  2: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                                                                          Dropped 10949 observations while converting to balanced panel.



################### GEEGLM #################
############################################

# Standford said they use GEEGLM for correlation 
# https://diff.healthpolicydatascience.org/

# for those who are in both time frames only!! 
# Change data source? 
attempt2 <- lm(ER ~ marsh*year  , data = did_visit_types)
summary(attempt2)
# all significant! 

attempt3 <- lm(PCP ~ marsh*year  , data = did_visit_types)
summary(attempt3)

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



gee_mod_DID_PCP <- geeglm(PCP ~ marsh*year, 
                    data = did_visit_types,
                    id = UniqueIdentifier,
                    family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                    # waves = time, # we only have one wave of treatment
                    scale.fix = T, # this sets phi = 1
                    corstr = "exchangeable")
print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP)

# Call:
#   geeglm(formula = PCP ~ marsh * year, family = gaussian, data = did_visit_types, 
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


gee_mod_DID_ER <- geeglm(ER ~ marsh*year, 
                          data = did_visit_types,
                          id = UniqueIdentifier,
                          family = gaussian, # previously I used in longitudinal class family=binomial(link="logit"),
                          # waves = time, # we only have one wave of treatment
                          scale.fix = T, # this sets phi = 1
                          corstr = "exchangeable")
print(gee_mod_DID_ER)
summary(gee_mod_DID_ER)

# 
# Call:
#   geeglm(formula = ER ~ marsh * year, family = gaussian, data = did_visit_types, 
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