######################################################################
################# UW/CHAS Capstone Analysis Project ##################
################### File 4: Longitudinal Analyses ####################
########### Runs pre-post analysis for objectives 1 and 2 ############
######################################################################

rm(list=ls())
# load packages
library(tidyverse)
library(nlme)
library(kableExtra)
options(digits=3)

KOH1 = as.Date("2023-04-05")
DAYS_IN_MONTH = 30.4

###################################
# Impact of KOH on Blood Pressure #
###################################

# read in data
o1_bp = read_csv("Analysis Data/Obj1BPPrePost.csv")
names(o1_bp)[1] = "row_id"

# standardizing time as days from baseline measurement
# attendees
#   measured before 1st KOH visit: change date to 1st KOH visit
#   measured after 1st KOH visit: keep date
# non-attendees
#   measured before KOH 1: change date to KOH 1
#   measured after KOH 1: DNE
std_times = o1_bp %>% 
  group_by(UniqueIdentifier) %>% 
  summarize(
    pre_Date = min(BPDate),
    post_Date = max(BPDate),
    std_time = ifelse(BPDate == pre_Date, 0, ifelse(
      KOH == 0,
      as.numeric(post_Date - KOH1),
      ifelse(pre_Date < KOH1, as.numeric(post_Date - KOH1), as.numeric(post_Date - KOHDate))
    ))
  ) %>% 
  select(std_time) %>% 
  ungroup() %>% 
  mutate(
    row_id = as.numeric(o1_bp[["row_id"]]),
    std_months = std_time / DAYS_IN_MONTH
  ) %>% 
  select(std_time, std_months, row_id)
o1_bp = left_join(o1_bp, std_times, by="row_id") 
o1_bp = o1_bp %>% mutate(
  Sex = ifelse(o1_bp$Sex == "M", 1, 0),
  koh_cat = as.factor(case_when(
    KOH.none == 1 ~ "None",
    KOH.one == 1 ~ "One",
    KOH.mult == 1 ~ "Multiple"
  ))
)

