# DID Script 3
# Pretrends Parallel Trends Assumption and when we have a Balanced Panel

# Yes generally parallel, but Unbalanced Panel (more patients came at the end)


# look at pretrends. Per Ting we don't need to analyze the pre-treatment time periods formally 
#but looking at the two trends is a good indicator if they are stable

# don't want to actually test per Ting and others
# But the test of parallel trends is neither necessary nor sufficient to establish validity of diff-in-diff (Kahn-Lang and Lang 2020)
# https://diff.healthpolicydatascience.org/#regression
  
library(tidyverse)
library(lubridate)
library(zoo)
library(rstudioapi)
# library(did)


# Repeat these charts and rates with quarter on the denominator! 


# Import dataframes DataCleaningDID.R script


fp_visits = file.path(getwd(), "Analysis Data/all_visit_types.csv")
all_visit_types <- read.csv(fp_visits) 
# Mark Visit Types with Marshallese and NHW
# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)


fp_tarpop = file.path(getwd(), "Analysis Data/targetpop_DID.csv")
targetpop_DID <- read.csv(fp_tarpop) 


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

# for pretrends Parallel Trends
# group by quarter
# test <- lubridate::quarter(all_visit_types$Date, with_year = TRUE)
# the quarter is given by 2017.1 for first quarter, 2017.2 for 2nd quarter, ... 2017.4 for last quarter of the year
with(all_visit_types, table(marsh, useNA = "always"))
check <- all_visit_types %>% filter(is.na(marsh))


all_visit_types <- all_visit_types %>% filter(!is.na(marsh))
all_visit_types_quarter <- all_visit_types %>% mutate(quarter = quarter(Date, with_year = TRUE)) 

# according to R documentation for lubridate
# If we want to group in June/July/Aug for our DID timeline we can use 
# quarter(x, with_year = TRUE, fiscal_start = 6)


# all_visit_types_counts_per_quarter <- all_visit_types_quarter %>% group_by(quarter) %>% count()
with(all_visit_types_counts_per_quarter_marsh_SL, table(marsh, useNA = "always"))

all_visit_types_counts_per_quarter_marsh_SL <- all_visit_types_quarter %>% 
  filter( Date >= "2017-01-01", Date <= "2019-05-31" ) %>% 
  group_by(quarter, marsh, ServiceLine) %>% 
  count()

pretrendER <- all_visit_types_counts_per_quarter_marsh_SL %>% filter(ServiceLine == "Emergency")

pretrendPCP <- all_visit_types_counts_per_quarter_marsh_SL %>% filter(ServiceLine == "Primary Care")

pretrend <- all_visit_types_quarter %>% filter( Date >= "2017-01-01", Date <= "2019-05-31" ) %>% group_by(quarter, marsh) %>% count()
colnames(pretrend)[3] <- c("total_visits")

# 
pretrend <- full_join(pretrend, pretrendER )  # add all total patient visits that quarter to 
# will join by quarter and marsh

# Create ER rate 
pretrend <- pretrend %>% mutate(PCPrate = n/total_visits)

pretrendM <- pretrend %>% filter(marsh == 1)

# we don't have 4 previous quarters to estimate, so we have to impute our best guess for 2016.4
# I just copied the number for 2017.1. 
# Its not a perfect estimate but I figure it is pretty good for this pretrend that just needs to be visualized

est_val <- as.numeric(pretrendM[1, 3])
estimate <- as.data.frame(t(c(2016.4, 1, est_val))) # , NA, NA, NA
colnames(estimate) <- colnames(pretrendM[ , 1:3])
length(estimate)
length(pretrendM)
pretrendM <- full_join(estimate, pretrendM)

rm(estimate)
rm(est_val)

running_total_M <- c()
for(i in 4:nrow(pretrendM)){
  x <- sum(pretrendM[ (i-3), 3], pretrendM[ (i-2), 3], pretrendM[ (i-1), 3], pretrendM[ i, 3])
  running_total_M <- c(running_total_M, x)
}

running_total_M <- c( NA, NA, NA, running_total_M)
pretrendM <- cbind(pretrendM, running_total_M)

  
pretrendNHW <- pretrend %>% filter(marsh == 0)

est_val <- as.numeric(pretrendNHW[1, 3])
estimate <- as.data.frame(t(c(2016.4, 0, est_val))) # , NA, NA, NA # Need to change to Non Marsh == 0
colnames(estimate) <- colnames(pretrendNHW[ , 1:3])
length(estimate)
length(pretrendNHW)
pretrendNHW <- full_join(estimate, pretrendNHW)

rm(estimate)
rm(est_val)

running_total_NHW <- c()
for(i in 4:nrow(pretrendNHW)){
  x <- sum(pretrendNHW[ (i-3), 3], pretrendNHW[ (i-2), 3], pretrendNHW[ (i-1), 3], pretrendNHW[ i, 3])
  running_total_NHW <- c(running_total_NHW, x)
}
running_total_NHW <- c( NA, NA, NA, running_total_NHW)
pretrendNHW <- cbind(pretrendNHW, running_total_NHW)


colnames(pretrendNHW)[7] <- c("total_running")

colnames(pretrendM)[7] <- c("total_running")
pretrend <- rbind(pretrendM, pretrendNHW)


pretrend <- pretrend %>% mutate(PCPrate_running = n/total_running)


# graph ER Pretrends  


#plot ER trends


# pretrend %>%  filter(quarter > 2017.2) %>%
#   ggplot(aes(x = quarter, y = PCPrate*100, group = as.factor(marsh), color = as.factor(marsh)))+
#   geom_line()+
#  ylim(c(0,50))+
#   labs(y="Rate per 100 patient", x="Year",
#        title = "Pretrends Emergency Room Visits Rates for CHAS Patients by Quarter
#      \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics")
# 
pretrend %>%  filter(quarter > 2017.2) %>%
  ggplot(aes(x = quarter, y = PCPrate_running*100, group = as.factor(marsh), color = as.factor(marsh)))+
  geom_line()+
  ylim(c(0,20))+
  labs(y="Rate per 100 patient", x="Year",
       title = "Pretrends Emergency Room Visits Rates for CHAS Patients by Quarter
       \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics")



pretrend2 <- all_visit_types_quarter %>% filter( Date >= "2017-01-01", Date <= "2019-05-31" ) %>% group_by(quarter, marsh) %>% count()
colnames(pretrend2)[3] <- c("total_visits")

#colnames(pretrendPCP)[4] <- c("PCP") # keep this as "n" for later mutated rate

colnames(pretrendPCP)[3] <- c("ServiceLinePCP")
pretrend2 <- full_join(pretrend2, pretrendPCP )  # add all total patient visits that quarter to 

pretrend2 <- pretrend2 %>% mutate(PCPrate = n/total_visits)




pretrend2M <- pretrend2 %>% filter(marsh == 1)

pretrend2NHW <- pretrend2 %>% filter(marsh == 0)

pretrend2M[5, 3]
running_total_M <- c()
for(i in 5:nrow(pretrend2M)){
  x <- sum(pretrend2M[ (i-3), 3], pretrend2M[ (i-2), 3], pretrend2M[ (i-1), 3], pretrend2M[ i, 3])
  running_total_M <- c(running_total_M, x)
}

running_total_M <- c( NA, NA, NA, NA, running_total_M)
pretrend2M <- cbind(pretrend2M, running_total_M)



running_total_NHW <- c()
for(i in 5:nrow(pretrend2NHW)){
  x <- sum(pretrend2NHW[ (i-3), 3], pretrend2NHW[ (i-2), 3], pretrend2NHW[ (i-1), 3], pretrend2NHW[ i, 3])
  running_total_NHW <- c(running_total_NHW, x)
}
running_total_NHW <- c( NA, NA, NA, NA, running_total_NHW)
pretrend2NHW <- cbind(pretrend2NHW, running_total_NHW)


colnames(pretrend2NHW)[7] <- c("total_running")

colnames(pretrend2M)[7] <- c("total_running")
pretrend2 <- rbind(pretrend2M, pretrend2NHW)


pretrend2 <- pretrend2 %>% mutate(PCPrate_running = n/total_running)




# plot PCP pretrends

# pretrend2 %>%  filter(quarter > 2018) %>%
#   ggplot(aes(x = quarter, y = PCPrate*100, group = as.factor(marsh), color = as.factor(marsh)))+
#   geom_line()+
#   ylim(c(0,50))+
#   labs(y="Rate per 100 patient", x="Year",
#        title = "Pretrends Primary Care Provider Visits Rates for CHAS Patients by Quarter
#        \n Marshallese and Non-Hispanic White Patients  \n Maple and Market Clinics")


pretrend2 %>%  filter(quarter > 2018) %>%
  ggplot(aes(x = quarter, y = PCPrate_running*100, group = as.factor(marsh), color = as.factor(marsh)))+
  geom_line()+
  ylim(c(0,20))+
  labs(y="Rate per 100 patient", x="Year",
       title = "Pretrends Primary Care Provider Visits Rates for CHAS Patients by Quarter
       \n Marshallese and Non-Hispanic White Patients  \n Maple and Market Clinics")


# need to find out how many patients were there in the beginning and end of DID



panel_balance <- function(start_date, end_date, Service) {
  pretreat <- all_visit_types %>% filter( Date >= {{start_date}}, Date <= {{end_date}} , 
                                          ServiceLine == {{Service}})
  
  marsh <- pretreat %>% filter(marsh == 1) 
  UniqueIDMarsh <- unique(marsh$UniqueIdentifier)
  NHW <- pretreat %>% filter(marsh == 0) 
  UniqueIDNHW <- unique(NHW$UniqueIdentifier)
  return(list(NHW_count = length(unique(NHW$UniqueIdentifier)), 
              Marsh_count = length(unique(marsh$UniqueIdentifier)), 
              total_count =   length(unique(pretreat$UniqueIdentifier)),
              UniqueIDMarsh = UniqueIDMarsh, 
              UniqueIDNHW = UniqueIDNHW))
}

# I run the two time period and compare 

pretest_ER_qt <- panel_balance(start_date = "2019-06-01", end_date = "2019-08-31", Service = "Emergency")
posttest_ER_qt <- panel_balance(start_date = "2022-06-01", end_date = "2022-08-31", Service = "Emergency")

length(intersect(pretest_ER_qt$UniqueIDMarsh, posttest_ER_qt$UniqueIDMarsh))
# 9  Marshallese patients in the ER both quarters

length(intersect(pretest_ER_qt$UniqueIDNHW, posttest_ER_qt$UniqueIDNHW))
# 590 NHW patients in the ER both quarters

pretest_PCP_qt <- panel_balance(start_date = "2019-06-01", end_date = "2019-08-31", Service = "Primary Care")
posttest_PCP_qt <- panel_balance(start_date = "2022-06-01", end_date = "2022-08-31", Service = "Primary Care")

length(intersect(pretest_PCP_qt$UniqueIDMarsh, posttest_PCP_qt$UniqueIDMarsh))
# 17 M patients at PCP both quarters

length(intersect(pretest_PCP_qt$UniqueIDNHW, posttest_PCP_qt$UniqueIDNHW))
# 2092 NHW patients at PCP both quarters

# any service line
panel_balance_any <- function(start_date, end_date) { # , Service
  pretreat <- all_visit_types %>% filter( Date >= {{start_date}}, Date <= {{end_date}} ) # , ServiceLine == {{Service}}
  
  marsh <- pretreat %>% filter(marsh == 1) 
  UniqueIDMarsh <- unique(marsh$UniqueIdentifier)
  NHW <- pretreat %>% filter(marsh == 0) 
  UniqueIDNHW <- unique(NHW$UniqueIdentifier)
  return(list(NHW_count = length(unique(NHW$UniqueIdentifier)), 
              Marsh_count = length(unique(marsh$UniqueIdentifier)), 
              total_count =   length(unique(pretreat$UniqueIdentifier)),
              UniqueIDMarsh = UniqueIDMarsh, 
              UniqueIDNHW = UniqueIDNHW))
}

pretest_yr <- panel_balance_any(start_date = "2018-08-31", end_date = "2019-08-31") # , Service = .

posttest_yr <- panel_balance_any(start_date = "2021-08-31", end_date = "2022-08-31") # , Service = .

length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh))
# 154 Marshallese got any services during both pre and post DID times

length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))
# 10405 NHW got any services during both  pre and post DID times

# # this is about 0.1813899 of the M
# 154/849
#   
# # and 0.4061914 of the NHW
# 10405/ 25616

