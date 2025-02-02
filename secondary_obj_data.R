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

# subset of patient ids w/hypertension
htn_ids = koh.table1 %>%
  filter(HTN == 1) %>%
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
