# DID Models
library(tidyverse)
library(lubridate)
library(zoo)
library(rstudioapi)
library(did)


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
# 
# 
# Control <- targetpop_DID %>% filter((Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))
# ControlUniqueID <- Control$UniqueID




# for pretrends
# group by quarter
# test <- lubridate::quarter(all_visit_types$Date, with_year = TRUE)
# the quarter is given by 2017.1 for first quarter, 2017.2 for 2nd quarter, ... 2017.4 for last quarter of the year
with(all_visit_types, table(marsh, useNA = "always"))
check <- all_visit_types %>% filter(is.na(marsh))


all_visit_types <- all_visit_types %>% filter(!is.na(marsh))
all_visit_types_quarter <- all_visit_types %>% mutate(quarter = quarter(Date, with_year = TRUE)) 

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

# Stopped here ! 
  pretrend <- full_join(pretrend, pretrendER )  # add all total patient visits that quarter to 
# will join by quarter and marsh

# Create ER rate 
  pretrend <- pretrend %>% mutate(PCPrate = n/total_visits)
  
# graph ER Pretrends  
#plot ER trends
  pretrend %>%  filter(quarter > 2017.2) %>%
    ggplot(aes(x = quarter, y = PCPrate*100, group = as.factor(marsh), color = as.factor(marsh)))+
    geom_line()+
   ylim(c(0,50))+
    labs(y="Rate per 100 patient", x="Year",
         title = "Pretrends Emergency Room Visits Rates for CHAS Patients by Quarter
       \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics")
  
  
  
  
  
  pretrend2 <- all_visit_types_quarter %>% filter( Date >= "2017-01-01", Date <= "2019-05-31" ) %>% group_by(quarter, marsh) %>% count()
  colnames(pretrend2)[3] <- c("total_visits")
  
  colnames(pretrendPCP)[4] <- c("PCP")

  colnames(pretrendPCP)[3] <- c("ServiceLinePCP")
  pretrend2 <- full_join(pretrend2, pretrendPCP )  # add all total patient visits that quarter to 
  
pretrend2 <- pretrend2 %>% mutate(PCPrate = PCP/total_visits)


# plot PCP pretrends
pretrend2 %>%  filter(quarter > 2018) %>%
  ggplot(aes(x = quarter, y = PCPrate*100, group = as.factor(marsh), color = as.factor(marsh)))+
  geom_line()+
  ylim(c(0,50))+
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
# 9 

length(intersect(pretest_ER_qt$UniqueIDNHW, posttest_ER_qt$UniqueIDNHW))
# 590

pretest_PCP_qt <- panel_balance(start_date = "2019-06-01", end_date = "2019-08-31", Service = "Primary Care")
posttest_PCP_qt <- panel_balance(start_date = "2022-06-01", end_date = "2022-08-31", Service = "Primary Care")

length(intersect(pretest_PCP_qt$UniqueIDMarsh, posttest_PCP_qt$UniqueIDMarsh))
# 17 

length(intersect(pretest_PCP_qt$UniqueIDNHW, posttest_PCP_qt$UniqueIDNHW))
# 2092

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






# who was there in June, July and Aug 2019 (pre-treatment)
colnames(all_visit_types)
class(all_visit_types$Date)
# test <- all_visit_types_quarter %>% filter(Date >= "2017-01-01", Date <= "2017-01-31")


pretreat <- all_visit_types %>% filter( Date >= "2019-06-01", Date <= "2019-08-31" ) %>%
  mutate(PreTreat = 1)

test <- pretreat %>% filter(marsh == 1, 
                    ServiceLine == "Emergency") 
nrow(test)
# 58 Marshallese Visited ER in the pretreat quarter
# !check this number against my yearmonth table to make sure I did it right

test <- pretreat %>% filter(marsh == 0, 
                            ServiceLine == "Emergency") 
nrow(test)
# 3243 Control visited ER in the pretreat quarter


# who was there in June, July and Aug 2022 (post-treatment)
posttreat <- all_visit_types %>% filter( Date >= "2022-06-01", Date <= "2022-08-31" ) %>%
  mutate(PostTreat = 1)

test <- posttreat %>% filter(marsh == 1, 
                            ServiceLine == "Emergency") 
# 73

test <- posttreat %>% filter(marsh == 0, 
                            ServiceLine == "Emergency") 
nrow(test)
#4394



# set up rates for DID pretends and to estimate
# We decided on # of Marshallese ER visits per quarter/ total Marshallese population per quarter
# and  # of Marshallese ER visits per quarter/ total Marshallese population per year

# !
# make a function that lets me control the population size
# need to divide by Marsh and NHW

# !


# old function
pop_size <- function(marsh0_or_1, year) {
  x <- all_visit_types %>% filter(marsh == {{marsh0_or_1}}) %>% filter(year == {{year}}) 
  z<- length(unique(x$UniqueIdentifier))
  return(z)
}


pop_size_marsh <- c(pop_size(1, 2017), # marshallese in 2017
                    pop_size(1, 2018),  # marshallese in 2018...
                    pop_size(1, 2019),
                    pop_size(1, 2020),
                    pop_size(1, 2021),
                    pop_size(1, 2022),
                    pop_size(1, 2023),
                    pop_size(1, 2024))

pop_size_white <- c(pop_size(0, 2017), # Non-Hisp white  in 2017
                    pop_size(0, 2018),
                    pop_size(0, 2019),
                    pop_size(0, 2020),
                    pop_size(0, 2021),
                    pop_size(0, 2022),
                    pop_size(0, 2023),
                    pop_size(0, 2024))

pop_size_year <- rbind(pop_size_marsh , pop_size_white)
colnames(pop_size_year) <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)





# look at pretrends. Per Ting we don't need to analyze the pre-treatment time periods formally 
#but looking at the two trends is a good indicaotr if they are stable











# we will have some of the same patients who appear in both time frames, but more in the 2nd time period

colnames(all_visit_types)



########## Visualize Rates of ER visits ###########

# create average number of ER visits per group. We need to divide each time periods number by the number of patients...


# crude population size for total time
# n_controls <- nrow(Control)
# n_marsh <- nrow(Marshallese)
# 
# 
# ER2016_2025_year_marsh <- ER2016_2025_year_marsh %>% mutate(pop_size = if_else((marsh == 1), n_marsh,
#                                                                                if_else((marsh == 0),  n_controls, NA) ),
#                                   rate = Freq/pop_size)


# ! create rate per year by using changing population size...
# ER2016_2025_year_marsh <- ER2016_2025_year_marsh %>% mutate(pop_size = ...

ER2016_2025_year_marsh %>% filter(year != 2025) %>% # !note that 2025 is only just beginning so numbers are low
  ggplot(aes(x=year, y=rate, group=marsh, color=marsh))+
  geom_line()+
  #facet_wrap(~marsh)+
  labs(y="Count", x="Time", title = "Emergency Visits Rates for CHAS Patients by Year
       \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics") #+
#scale_x_continuous(breaks=c(0,1,4,6))




# y<- all_visit_types %>% filter(marsh == 1) %>% filter(year == 2017) 
# length(unique(y$UniqueIdentifier))










# and all Non-Hispanic White uniqueID for any type of appointment per year



# number of ER visits per month/ Population size per year
# numerator from the  ER2016_2025_yearmonth_marsh and 

# add months 
#all_visit_types <- all_visit_types %>% mutate(yearmonth = as.Date(zoo::as.yearmon(all_visit_types$Date, "%m/%d/%Y")))

# denominator from pop_size_year 

#! Stopped here

# Jan 2017 to Aug 2019 ER Pretrends (about 2.5 years)

# Sept 2019 to Sept 2022 DID ER Estimates for CHW 1 and 2

# Oct 2022 to Dec 2024 additional ER trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 


# ! Note that ER visits are recorded from 2017 forward. 

# But primary care provider (PCP) visits are only recorded from 2018 forward
# Jan 2018 to Aug 2019 PCP Pretrends (about 1.5 years)

# Sept 2019 to Sept 2022 DID PCP Estimates for CHW 1 and 2

# Oct 2022 to Dec 2024 additional PCP trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 




# !are total interactions growing? Even if ER rates are going up.



# !! How to we account for correlation with longitutindal data  