# DID Script 4
# DID Models

# Repeat DID Models But Use Each Quarter as the Population, Not Each Year, since we also want that metric

#rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo) # to extract month out of the standard date format
library(rstudioapi) # to allow us to use file.path 
library(did) # package from other professors who require time change in start
library(geepack) #geeglm
library(gtsummary)
# Import dataframes DataCleaningDID.R script


fp_visits = file.path(getwd(), "Analysis Data/all_visit_types.csv")
all_visit_types <- read.csv(fp_visits)
unique(all_visit_types$year)

#all_visit_types 
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
# gabby got 495

NHW_yr <- did_visit_types_year %>% filter(marsh == 0) 
length(unique(NHW_yr$UniqueIdentifier))
# 18586 NonHispanic Whites visited these 2 clinics this year
# gabby got 20177


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




# I think we need to change the format of the data. 
# if we have the did_visit_types_year for the geeglm I think it will give us the number of expected ER visits per visit! 
# not per patient. 
# If we make a table with uniqueID, ER, marsh and year_center I think we get the data we need

# PCP_per_person <- as.data.frame(with(did_visit_types_year, table(UniqueIdentifier, marsh, PCP, year_center)))


did_visit_types_year <- did_visit_types_year %>% mutate(pre_year = if_else((year == 2018 | year == 2019), 1, 0),
                                                        post_year = if_else((year == 2021 | year == 2022), 1, 0) )
                                                                           

Marshallese <- did_visit_types_year %>% filter(marsh == 1) 
#colnames(Marshallese)
MarshalleseUniqueID <- unique(Marshallese$UniqueIdentifier)

#number of unique Marshallese
length(unique(MarshalleseUniqueID))
# 476
# gabby got 495

Control <-did_visit_types_year %>% filter(marsh == 0) 
ControlUniqueID <- unique(Control$UniqueIdentifier)

# Number of unique controls 
length(unique(ControlUniqueID))
# 18586
# gabby got 20177


PCP_per_person <- did_visit_types_year %>% group_by(UniqueIdentifier, post_year) %>% summarize( sum(PCP))
PCP_per_person <- PCP_per_person %>%  mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                             if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) )


ER_per_person <- did_visit_types_year %>% group_by(UniqueIdentifier, post_year) %>% summarize( sum(ER))
ER_per_person <- ER_per_person %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                       if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) )
colnames(PCP_per_person)[3] <- "sum_PCP"
colnames(ER_per_person)[3] <- "sum_ER"
# 
length(unique(did_visit_types_year$UniqueIdentifier))
# 19062

# # length is different than the nrow/2. Some not in both time periods? 
# 
# nrow(ER_per_person)
# nrow(PCP_per_person)
# # same
# length(unique(ER_per_person$UniqueIdentifier)) == length(unique(did_visit_types_year$UniqueIdentifier))
# 
# setdiff(ER_per_person$UniqueIdentifier, did_visit_types_year$UniqueIdentifier)
# # No difference
# 
# check <- did_visit_types_year %>% filter(UniqueIdentifier == 14012)


# did_visit_types_year_merge <- did_visit_types_year %>% select(UniqueIdentifier, marsh, pre_year)
# check <- left_join( did_visit_types_year_merge, ER_per_person,)
# ### Not working at all!!





gee_mod_DID_PCP_yr_best <- geeglm(sum_PCP ~ marsh*post_year, 
                             data = PCP_per_person,
                             id = UniqueIdentifier,
                             family = gaussian, 
                             scale.fix = T, # this sets phi = 1
                             corstr = "exchangeable")
#print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP_yr_best)

# Call:
#   geeglm(formula = sum_PCP ~ marsh * post_year, family = gaussian, 
#          data = PCP_per_person, id = UniqueIdentifier, corstr = "exchangeable", 
#          scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err    Wald Pr(>|W|)    
# (Intercept)      0.59361  0.00954 3872.74   <2e-16 ***
#   marsh           -0.12888  0.07324    3.10    0.078 .  
# post_year        0.14158  0.01293  119.86   <2e-16 ***
#   marsh:post_year -0.16577  0.08058    4.23    0.040 *  
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
# alpha    0.208  0.0118
# Number of clusters:   19062  Maximum cluster size: 2 

# gabby added titles here :)
tbl_regression(gee_mod_DID_PCP_yr_best, intercept = TRUE,
               label = list(
                 marsh ~ "Marshallese",
                 post_year ~ "Year")) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_table_styling(
    columns = label,
    rows = label == "Year",
    footnote = "Year relative to CHW hiring in 2019"
  )

gee_mod_DID_ER_yr_best <- geeglm(sum_ER ~ marsh*post_year, 
                                  data = ER_per_person,
                                  id = UniqueIdentifier,
                                  family = gaussian, 
                                  scale.fix = T, # this sets phi = 1
                                  corstr = "exchangeable")
#print(gee_mod_DID_PCP)
summary(gee_mod_DID_ER_yr_best)
coef(gee_mod_DID_ER_yr_best)
# confint(gee_mod_DID_ER_yr_best)

# geeglm(formula = sum_ER ~ marsh * post_year, family = gaussian, 
#        data = ER_per_person, id = UniqueIdentifier, corstr = "exchangeable", 
#        scale.fix = T)
# 
# Coefficients:
#   Estimate  Std.err    Wald Pr(>|W|)    
# (Intercept)      0.25948  0.00651 1589.51   <2e-16 ***
#   marsh            0.03425  0.04762    0.52    0.472    
# post_year        0.00164  0.00824    0.04    0.843    
# marsh:post_year -0.13055  0.05187    6.33    0.012 *  
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
# alpha    0.282    0.03
# Number of clusters:   19062  Maximum cluster size: 2 


# GTSummary table of results


# reg2 <- glm(Affected ~ DHW.MeanMax_Degree_Heating_Weeks_MO03 + DHW.MeanMax_Degree_Heating_Weeks_YR10YR01 + mean_weekly_range_SST_CRW_Daily_ALLB4 + Prop_BleachResTaxa + Depth_m_mn,
#             family=poisson,
#             data = hcbc,
#             offset = log(LiveCoral))
# reg2
# 
# tbl_regression(reg2, exponentiate=TRUE, label=list(
#   DHW.MeanMax_Degree_Heating_Weeks_MO03 ~ 'Maximum DHW (prior 3 months)',
#   DHW.MeanMax_Degree_Heating_Weeks_YR10YR01 ~ 'Maximum DHW (prior 10 years)',
#   mean_weekly_range_SST_CRW_Daily_ALLB4 ~ 'Mean Weekly SST Range',
#   Prop_BleachResTaxa ~ 'Proportion of Bleach Resistant Taxa',
#   Depth_m_mn ~ 'Bathymetric Depth (m)'
# ))
# confint(gee_mod_DID_ER_yr_best)

# gabby added titles here :)  

tbl_regression(gee_mod_DID_ER_yr_best, intercept = TRUE,
               label = list(
                 marsh ~ "Marshallese",
                 post_year ~ "Year")) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_table_styling(
    columns = label,
    rows = label == "Year",
    footnote = "Year relative to CHW hiring in 2019")

###############################################################
###############################################################
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




gee_mod_DID_ER_yr_bal <- geeglm(ER ~ marsh*year_center, 
                             data = did_visit_types_year_balanced,
                             id = UniqueIdentifier,
                             family = gaussian, 
                             scale.fix = T, # this sets phi = 1
                             corstr = "exchangeable")
# print(gee_mod_DID_ER)
summary(gee_mod_DID_ER_yr_bal)

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


gee_mod_DID_PCP_yr_bal <- geeglm(PCP ~ marsh*year_center, 
                              data = did_visit_types_year_balanced,
                              id = UniqueIdentifier,
                              family = gaussian, 
                              scale.fix = T, # this sets phi = 1
                              corstr = "exchangeable")
# print(gee_mod_DID_PCP)
summary(gee_mod_DID_PCP_yr_bal)

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

