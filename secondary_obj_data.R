library(dplyr)
library(tidyverse)
library(gtsummary)
library(lubridate)
library(naniar)
library(flextable)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_gdp = file.path(getwd(), "general_data_prep.R")
source(fp_gdp)

first_koh_date = as.Date("2023-04-05")

##################################
# HYPERTENSION DATASET PREPARATION
##################################

# grab covariates from table 1 dataset
obj2.cov <- table1 %>% select(c(UniqueIdentifier, Marsh, Group, age, Sex, IncomeLevel, BLACERISK, avg.bmi))
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

htn_reads_LME$KOHDate <- first_koh_date
htn_reads_LME.pre <- htn_reads_LME %>%
  mutate(datediff = as.numeric(difftime(Date, KOHDate, units="days"))) %>% 
  filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-c(datediff, KOHDate))
htn_reads_LME.post <- htn_reads_LME %>%
  mutate(datediff = as.numeric(difftime(Date, KOHDate, units="days"))) %>% filter(datediff>0) %>% select(-c(datediff, KOHDate))
htn_LME_final <- rbind(htn_reads_LME.pre, htn_reads_LME.post) %>% group_by(UniqueIdentifier) %>% arrange(Date, .by_group=TRUE)
htn_LME_final <- left_join(htn_LME_final, obj2.cov, by = "UniqueIdentifier")

write.csv(htn_LME_final, "Analysis Data/Obj2BP_LME.csv") 

# drop all measurements not taken at earliest and most recent dates
htn_reads_pp.post = htn_LME_final %>% select(c(UniqueIdentifier, Date, Systolic, Diastolic)) %>% 
  group_by(UniqueIdentifier) %>% filter(Date == max(Date)) %>% ungroup()
htn_reads_pp <- rbind(htn_reads_LME.pre, htn_reads_pp.post) %>% group_by(UniqueIdentifier) %>% arrange(Date, .by_group=TRUE)
htn_reads_pp <- left_join(htn_reads_pp, obj2.cov, by = "UniqueIdentifier")

write.csv(htn_reads_pp, "Analysis Data/Obj2BPPrePost.csv")

##########################
# T2DM DATASET PREPARATION
##########################

# subset of Marshallese and NHW patient ids w/t2dm
a1c_ids = table1 %>%
  filter(Diabetes == 1 & (Marsh == 1 | Race == "White")) %>%
  select("UniqueIdentifier")

# t2dm ptids and all A1c readings/associated dates ONLY no other vars
all_a1c_reads = left_join(a1c_ids, a1c.nona.18, by="UniqueIdentifier") %>%
  select(-age)

# set of t2dm ptids with readings before first KOH meeting
pre_a1c_read_ids = all_a1c_reads %>%
  filter(Date <= first_koh_date)

pre_a1c_read_ids = pre_a1c_read_ids$UniqueIdentifier %>%
  as.array() %>% 
  unique()

# set of diab ptids with readings after first KOH meeting
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

a1c_reads_LME$KOHDate <- first_koh_date
a1c_reads_LME.pre <- a1c_reads_LME %>%
  mutate(datediff = as.numeric(difftime(Date, KOHDate, units="days"))) %>% 
  filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-c(datediff, KOHDate))
a1c_reads_LME.post <- a1c_reads_LME %>%
  mutate(datediff = as.numeric(difftime(Date, KOHDate, units="days"))) %>% filter(datediff>0) %>% select(-c(datediff, KOHDate))
a1c_LME_final <- rbind(a1c_reads_LME.pre, a1c_reads_LME.post) %>% group_by(UniqueIdentifier) %>% arrange(Date, .by_group=TRUE)
a1c_LME_final <- left_join(a1c_LME_final, obj2.cov, by = "UniqueIdentifier")

# find duplicate values
a1c.dt.dups <- a1c_LME_final %>% group_by(UniqueIdentifier, Date) %>% count(name="counts") %>% filter(counts>1)
a1c.dups <- left_join(a1c.dt.dups, a1c_LME_final, by = c("UniqueIdentifier", "Date")) %>% select(-counts)
write.csv(a1c.dups, "obj2.a1c.duplicates.csv")

write.csv(a1c_LME_final, "Analysis Data/Obj2A1c_LME.csv")

# drop all measurements not taken at earliest and most recent dates
a1c_reads_pp.post = a1c_LME_final %>% select(c(UniqueIdentifier, Date, A1c)) %>% 
  group_by(UniqueIdentifier) %>% filter(Date == max(Date)) %>% ungroup()
a1c_reads_pp <- rbind(a1c_reads_LME.pre, a1c_reads_pp.post) %>% group_by(UniqueIdentifier) %>% arrange(Date, .by_group=TRUE)
a1c_reads_pp <- left_join(a1c_reads_pp, obj2.cov, by = "UniqueIdentifier")

write.csv(a1c_reads_pp, "Analysis Data/Obj2A1cPrePost.csv")

# combine datasets to get patients for table 1
obj2 <- rbind(a1c_reads_pp, htn_reads_pp) %>% distinct(UniqueIdentifier)
obj2 <- left_join(obj2, table1, by = "UniqueIdentifier")
write.csv(obj2, "Analysis Data/Obj2_AllPts.csv")

no.bmi <- obj2 %>% filter(is.na(avg.bmi))
write.csv(no.bmi, "Obj2_noBMI.csv")
