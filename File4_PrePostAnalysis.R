######################################################################
################# UW/CHAS Capstone Analysis Project ##################
################### File 4: Longitudinal Analyses ####################
########### Runs pre-post analysis for objectives 1 and 2 ############
######################################################################

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
o1_bp$koh_cat <- relevel(o1_bp$koh_cat, ref = "None")
o1_bp_ids = o1_bp$UniqueIdentifier %>% unique() %>% sample(0.5 * 0.25 * nrow(o1_bp))
o1_bp_pdt = o1_bp %>% filter(UniqueIdentifier %in% o1_bp_ids)

# spaghetti plot of SBP over time by KOH attendance group
ggplot(data=o1_bp_pdt, mapping=aes(x=std_time, y=sys, group=UniqueIdentifier, color=koh_cat)) +
  geom_line() +
  geom_point() +
  labs(
    title="Spaghetti Plot of SBP Against Time",
    x="time since pre-measurement (days)",
    y="SBP (mm/Hg)"
  )

# run linear effects model
mod_o1_bp = lme(
  fixed = sys ~ std_months * koh_cat + age + Sex + IncomeLevel + BLACERISK + avg.bmi,
  random = ~ std_months | UniqueIdentifier,
  data = o1_bp,
  method = "REML"
)
# show regression results
summary(mod_o1_bp)

###################################
####### Impact of KOH on A1c ######
###################################

# read in data
o1_a1c = read_csv("Analysis Data/Obj1A1cPrePost.csv")
names(o1_a1c)[1] = "row_id"

# standardizing time as days from baseline measurement
std_times = o1_a1c %>% 
  group_by(UniqueIdentifier) %>% 
  summarize(
    pre_Date = min(A1cDate),
    post_Date = max(A1cDate),
    std_time = ifelse(A1cDate == pre_Date, 0, ifelse(
      KOH == 0,
      as.numeric(post_Date - KOH1),
      ifelse(pre_Date < KOH1, as.numeric(post_Date - KOH1), as.numeric(post_Date - KOHDate))
    ))
  ) %>% 
  select(std_time) %>% 
  ungroup() %>% 
  mutate(
    row_id = as.numeric(o1_a1c[["row_id"]]),
    std_months = std_time / DAYS_IN_MONTH
  ) %>% 
  select(std_time, std_months, row_id)

o1_a1c = left_join(o1_a1c, std_times, by="row_id") 
o1_a1c = o1_a1c %>% mutate(
  Sex = ifelse(o1_a1c$Sex == "M", 1, 0),
  koh_cat = as.factor(case_when(
    KOH.none == 1 ~ "None",
    KOH.one == 1 ~ "One",
    KOH.mult == 1 ~ "Multiple"
  ))
)
o1_a1c$koh_cat <- relevel(o1_a1c$koh_cat, ref = "None")

# spaghetti plot of A1c over time by KOH attendance group
o1_a1c_ids = o1_a1c$UniqueIdentifier %>% unique() %>% sample(0.5 * 0.25 * nrow(o1_a1c))
o1_a1c_pdt = o1_a1c %>% filter(UniqueIdentifier %in% o1_a1c_ids)
ggplot(data=o1_a1c_pdt, mapping=aes(x=std_time, y=A1c, group=UniqueIdentifier, color=koh_cat)) +
  geom_line() +
  geom_point() +
  labs(
    title="Spaghetti Plot of A1c Against Time",
    x="time since pre-measurement (days)",
    y="A1c (%)"
  )

# run linear mixed effects model
mod_o1_a1c = lme(
  fixed = A1c ~ std_months * koh_cat + age + Sex + IncomeLevel + BLACERISK + avg.bmi,
  random = ~ std_months | UniqueIdentifier,
  data = o1_a1c,
  method = "REML"
)
# show regression results
summary(mod_o1_a1c)

#########################################
# Association of Race on Blood Pressure #
#########################################

# read in data
o2_bp = read_csv("Analysis Data/Obj2BPPrePost.csv")
names(o2_bp)[1] = "row_id"

# standardizing time as days from baseline measurement
# measured before KOH 1: change date to KOH 1
# measured after KOH 1: keep date
std_times = o2_bp %>%
  group_by(UniqueIdentifier) %>%
  summarize(
    pre_Date = min(Date),
    post_Date = max(Date),
    std_time = ifelse(
      Date == pre_Date,
      0,
      # ifelse(pre_Date < KOH1, as.numeric(post_Date - KOH1), as.numeric(post_Date - pre_Date))
      as.numeric(post_Date - pre_Date)
    )
  ) %>%
  select(std_time) %>% 
  ungroup() %>%
  mutate(
    row_id = as.numeric(o2_bp[["row_id"]]),
    std_months = std_time / DAYS_IN_MONTH
  ) %>% 
  select(std_time, std_months, row_id)

o2_bp = left_join(o2_bp, std_times, by="row_id") 
o2_bp = o2_bp %>% mutate(Sex = ifelse(o2_bp$Sex == "M", 1, 0))

o2_bp_ids = o2_bp$UniqueIdentifier %>% unique() %>% sample(0.5 * 0.05 * nrow(o2_bp))
o2_bp_pdt = o2_bp %>% filter(UniqueIdentifier %in% o2_bp_ids) %>% mutate(Marsh = as.factor(Marsh))

# spaghetti plot of SBP over time by race
ggplot(data=o2_bp_pdt, mapping=aes(x=std_time, y=Systolic, group=UniqueIdentifier, color=Marsh)) +
  geom_line() +
  geom_point() +
  labs(
    title="Spaghetti Plot of SBP Against Time",
    x="time since pre-measurement (days)",
    y="SBP (mm/Hg)"
  )

# run linear mixed effects model
mod_o2_bp = lme(
  fixed = Systolic ~ std_months * Marsh + age + Sex + IncomeLevel + avg.bmi,
  random = ~ std_months | UniqueIdentifier,
  data = o2_bp,
  method = "REML"
)
# show regression results
summary(mod_o2_bp)

#########################################
###### Association of Race on A1c #######
#########################################

# read in data
o2_a1c = read_csv("Analysis Data/Obj2A1cPrePost.csv")
names(o2_a1c)[1] = "row_id"

# standardizing time as days from baseline measurement
std_times = o2_a1c %>%
  group_by(UniqueIdentifier) %>%
  summarize(
    pre_Date = min(Date),
    post_Date = max(Date),
    std_time = ifelse(
      Date == pre_Date,
      0,
      as.numeric(post_Date - KOH1)
    )
  ) %>%
  select(std_time) %>% 
  ungroup() %>%
  mutate(
    row_id = as.numeric(o2_a1c[["row_id"]]),
    std_months = std_time / DAYS_IN_MONTH
  ) %>% 
  select(std_time, std_months, row_id)

o2_a1c = left_join(o2_a1c, std_times, by="row_id") 
o2_a1c = o2_a1c %>% mutate(Sex = ifelse(o2_a1c$Sex == "M", 1, 0))

o2_a1c_ids = o2_a1c$UniqueIdentifier %>% unique() %>% sample(0.5 * 0.05 * nrow(o2_a1c))
o2_a1c_pdt = o2_a1c %>% filter(UniqueIdentifier %in% o2_a1c_ids) %>% mutate(Marsh = as.factor(Marsh))
# spaghetti plot of A1c over time by race
ggplot(data=o2_a1c_pdt, mapping=aes(x=std_time, y=A1c, group=UniqueIdentifier, color=Marsh)) +
  geom_line() +
  geom_point() +
  labs(
    title="Spaghetti Plot of A1c Against Time",
    x="time since pre-measurement (days)",
    y="A1c (%)"
  )

# run linear mixed effects model
mod_o2_a1c = lme(
  fixed = A1c ~ std_months * Marsh + age + Sex + IncomeLevel + avg.bmi,
  random = ~ std_months | UniqueIdentifier,
  data = o2_a1c,
  method = "REML"
)
# show regression results
summary(mod_o2_a1c)

###########################################
# format for regression results from models
###########################################

coef_names = c("std_months:koh_catOne", "std_months:koh_catMultiple", "koh_catOne", "koh_catMultiple", "std_months")
col_names = c("est.", "lower", "upper")

# primary objective
obj1_res = data.frame(
  rbind(
    cbind(intervals(mod_o1_a1c)$fixed[coef_names, col_names], summary(mod_o1_a1c)$tTable[coef_names, "p-value"]),
    cbind(intervals(mod_o1_bp, which="fixed")$fixed[coef_names, col_names], summary(mod_o1_bp)$tTable[coef_names, "p-value"])
  )
)

rownames(obj1_res) = c("months:KOHonce", "months:KOHmultiple", "KOHonce", "KOHmultiple", "months", "months:KOHonce ", "months:KOHmultiple ", "KOHonce ", "KOHmultiple ", "months ")
colnames(obj1_res)[4] = "p-value"

kable(obj1_res) %>% pack_rows("a1c_pre_post", 1, 5) %>% pack_rows("sbp_pre_post", 6, 10)

# secondary objective
coef_names = c("std_months:Marsh", "Marsh", "std_months")
col_names = c("est.", "lower", "upper")

obj1_res = data.frame(
  rbind(
    cbind(intervals(mod_o2_a1c, which="fixed")$fixed[coef_names, col_names], summary(mod_o2_a1c)$tTable[coef_names, "p-value"]),
    cbind(intervals(mod_o2_bp, which="fixed")$fixed[coef_names, col_names], summary(mod_o2_bp)$tTable[coef_names, "p-value"])
  )
)

rownames(obj1_res) = c("months:Marsh","Marsh", "months", "months:Marsh ","Marsh ", "months ")
colnames(obj1_res)[4] = "p-value"

kable(obj1_res) %>% pack_rows("a1c_pre_post", 1, 3) %>% pack_rows("sbp_pre_post", 4, 6)
