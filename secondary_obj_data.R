library(dplyr)
library(tidyverse)
library(gtsummary)
library(lubridate)
library(naniar)
library(flextable)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_gdp = file.path(getwd(), "general_data_prep.R")
source(fp_gdp)

first_koh_date = ymd("2023-04-05")

##################################
# HYPERTENSION DATASET PREPARATION
##################################

# subset of Marshallese and NHW patient ids w/hypertension
htn_ids = table1 %>%
  filter(HTN == 1 & (Marsh == 1 | Race == "White")) %>%
  select("UniqueIdentifier")

# htn ptids and all BP readings/associated dates ONLY no other vars
all_htn_reads = left_join(htn_ids, bp.nona.18, by="UniqueIdentifier") %>%
  select(-age)

# set of htn ptids with readings before first KOH meeting
pre_htn_read_ids = all_htn_reads %>%
  filter(Date <= first_koh_date)

pre_htn_read_ids = pre_htn_read_ids$UniqueIdentifier %>%
  as.array() %>% 
  unique()

# set of htn ptids with readings after first KOH meeting
post_htn_read_ids = all_htn_reads %>% 
  filter(Date > first_koh_date)

post_htn_read_ids = post_htn_read_ids$UniqueIdentifier %>% 
  as.array() %>% 
  unique()

# intersection of above sets (yields ptids with readings before and after first KOH meeting)
pre_post_ptids = intersect(pre_htn_read_ids, post_htn_read_ids)

# filter for only patients in above intersection
htn_reads_LME = all_htn_reads %>% 
  filter(UniqueIdentifier %in% pre_post_ptids)

write.csv(htn_reads_LME, "Analysis Data/Obj2BP_LME.csv")

# drop all measurements not taken at earliest and most recent dates
htn_reads_pp = htn_reads_LME %>% 
  group_by(UniqueIdentifier) %>% 
  filter(Date == min(Date) | Date == max(Date)) %>% 
  ungroup()

write.csv(htn_reads_pp, "Analysis Data/Obj2BPPrePost.csv")

##########################
# T2DM DATASET PREPARATION
##########################

# subset of Marshallese and NHW patient ids w/hypertension
a1c_ids = table1 %>%
  filter(Diabetes == 1 & (Marsh == 1 | Race == "White")) %>%
  select("UniqueIdentifier")

# htn ptids and all BP readings/associated dates ONLY no other vars
all_a1c_reads = left_join(a1c_ids, a1c.nona.18, by="UniqueIdentifier") %>%
  select(-age)

# set of htn ptids with readings before first KOH meeting
pre_a1c_read_ids = all_a1c_reads %>%
  filter(Date <= first_koh_date)

pre_a1c_read_ids = pre_a1c_read_ids$UniqueIdentifier %>%
  as.array() %>% 
  unique()

# set of htn ptids with readings after first KOH meeting
post_a1c_read_ids = all_a1c_reads %>% 
  filter(Date > first_koh_date)

post_a1c_read_ids = post_a1c_read_ids$UniqueIdentifier %>% 
  as.array() %>% 
  unique()

# intersection of above sets (yields ptids with readings before and after first KOH meeting)
pre_post_ptids = intersect(pre_a1c_read_ids, post_a1c_read_ids)

# filter for only patients in above intersection
a1c_reads_LME = all_a1c_reads %>% 
  filter(UniqueIdentifier %in% pre_post_ptids)

write.csv(a1c_reads_LME, "Analysis Data/Obj2A1c_LME.csv")

# drop all measurements not taken at earliest and most recent dates
a1c_reads_pp = a1c_reads_LME %>% 
  group_by(UniqueIdentifier) %>% 
  filter(Date == min(Date) | Date == max(Date)) %>% 
  ungroup()

write.csv(a1c_reads_pp, "Analysis Data/Obj2A1cPrePost.csv")
