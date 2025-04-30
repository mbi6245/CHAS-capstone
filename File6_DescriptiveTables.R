######################################################################
################# UW/CHAS Capstone Analysis Project ##################
#################### File 6: Descriptive Tables ######################
############# Creates descriptive statistics & tables ################
######################################################################

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(dplyr)
library(gtsummary)
library(flextable)

##################################
# Table 1: Primary Objective (KOH)
##################################
## includes only patients that were eligible for analysis (i.e. Marshallese, diabetes/hypertension, + had at least 1 pre A1c or SBP measure and 1 post measure)

# read in data
obj1 <- read.csv('Analysis Data/Obj1_AllPts.csv')[,-1]
# discretize KOH attendance groups
obj1 <- obj1 %>% mutate(koh.gp = case_when(koh.counts == 0 ~ 'None',
                                           koh.counts == 1 ~ 'One',
                                           koh.counts > 1 ~ 'Multiple'))
obj1$koh.gp <- factor(obj1$koh.gp, levels = c('None', 'One', 'Multiple'))

# create table
obj1 %>% select(c('koh.gp','age','Sex.long','avg.bmi','HTN','PreDM','T2DM','both','ClinicLocation','No.column.name','IncomeLevel','BLACERISK')) %>%
  gtsummary::tbl_summary(
    by = koh.gp,
    include = c(age, Sex.long, avg.bmi, HTN, PreDM, T2DM, both, ClinicLocation, No.column.name, IncomeLevel, BLACERISK),
    label = list(
      age ~ 'Age',
      Sex.long ~ 'Sex',
      avg.bmi ~ 'BMI',
      HTN ~ 'Hypertension',
      PreDM ~ 'Pre-Diabetes',
      T2DM ~ 'Type II Diabetes',
      both ~ 'Hypertension and T2DM',
      ClinicLocation ~ 'Clinic Location',
      No.column.name ~ 'Housing Status',
      IncomeLevel ~ 'Income Level',
      BLACERISK ~ 'Risk Score'
    ),
    missing_text = "Missing",
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ c(
      "{median} ({min}, {max})"
    ),
    digits = all_continuous() ~ 1
  )  %>%
  bold_labels() %>%
  add_p() %>% # you can omit this line and the one below if you want to remove the p-values (add # in front of the line)
  bold_p(t = 0.05) %>% # the p-values tell us if any of these factors are associated with KOH attendance level (<0.05 is significant)
  modify_table_styling(
    columns = label,
    rows = label == "Income Level",
    footnote = "Percentage of participant's annual income relative to the federal poverty line"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Risk Score",
    footnote = "Risk scores are calculated based on the BLACE Index Scoring Tool. Scores range from 0-19; <8 is low risk, 8-11 is moderate risk, and 12+ is high risk."
  ) %>%
  modify_header(label ~ "**Baseline Characteristics**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>\nN = {n}") %>%
  modify_caption("Marshallese Participant Characteristics") %>%
  as_flex_table()

#####################################
# Table 1: Secondary Objective (Race)
#####################################
## includes only patients that were eligible for analysis (i.e. Diabetes/hypertension + had at least 1 pre A1c or SBP measure and 1 post measure)

# read in data
obj2 <- read.csv('Analysis Data/Obj2_AllPts.csv')[,-1]

# create table
obj2 %>% select(c('Group','age','Sex.long','avg.bmi','HTN','PreDM','T2DM','both','ClinicLocation','No.column.name','IncomeLevel','BLACERISK')) %>%
  gtsummary::tbl_summary(
    by = Group,
    include=c(age, Sex.long, avg.bmi, HTN, PreDM, T2DM, both, ClinicLocation, No.column.name, IncomeLevel, BLACERISK),
    list(
      age ~ 'Age',
      Sex.long ~ 'Sex',
      avg.bmi ~ 'BMI',
      HTN ~ 'Hypertension',
      PreDM ~ 'Pre-Diabetes',
      T2DM ~ 'Type II Diabetes',
      both ~ 'Hypertension and T2DM',
      ClinicLocation ~ 'Clinic Location',
      No.column.name ~ 'Housing Status',
      IncomeLevel ~ 'Income Level',
      BLACERISK ~ 'Risk Score'
    ),
    missing_text = "Missing",
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ c(
      "{median} ({min}, {max})"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_table_styling(
    columns = label,
    rows = label == "Income Level",
    footnote = "Percentage of participant's annual income relative to the federal poverty line."
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Risk Score",
    footnote = "Risk scores are calculated based on the BLACE Index Scoring Tool. Scores range from 0-19; <8 is low risk, 8-11 is moderate risk, and 12+ is high risk."
  ) %>%
  modify_header(label ~ "**Baseline Characteristics**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>\nN = {n}") %>%
  modify_caption("Marshallese and Non-Hispanic White Participant Characteristics") %>%
  as_flex_table()

##############################################################################
# Distribution of KOH meetings attended by those who only attended one meeting
##############################################################################
fp_gdp = file.path(getwd(), "File1_Prelim.R")
source(fp_gdp)
koh_1x = koh.counts %>% filter(koh.counts == 1)
koh_1x = koh_1x %>% 
  left_join(koh.attend.18, by="UniqueIdentifier") %>% 
  select(Date)
koh_1x = koh_1x %>% 
  group_by(Date) %>% 
  summarize(attendees=n(), .groups="drop")

 # create bar chart
ggplot(koh_1x, aes(x=Date, y=attendees)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label=Date, angle=90, vjust=-0.5)) +
  labs(title="Number of One-Time KOH Attendees by Date",
       x="KOH Meeting Date",
       y="Number of Attendees")

#########################################
# Visit/Encounters Table by Year and Race
#########################################

# getting the dataset in the proper form
pop.race <- targetpop %>% select(c('UniqueIdentifier','Group'))
# add race to encounters table
enc.race <- inner_join(pop.race, enc.nona.18, by='UniqueIdentifier')
# isolate year to use in table
enc.race$year <- as.numeric(format(enc.race$Date, "%Y"))
enc.race$ServiceLine[enc.race$ServiceLine == "Unmapped"] <- "Other"
enc.race$ServiceLine <- factor(enc.race$ServiceLine,
                               levels = c("Primary Care", "Emergency", "Urgent Care", "Behavioral Health",
                                          "Pharmacy", "Health Equity", "Dental", "Multispecialty", "Other"))
# calculate number of type of visit per year for each patient
enc.counts <- enc.race %>% group_by(UniqueIdentifier, year, ServiceLine) %>% count(name = "enc.counts")
enc.counts.race <- inner_join(pop.race, enc.counts, by='UniqueIdentifier')
enc.counts.race$Group <- factor(enc.counts.race$Group, levels = c("Marshallese","Non-Marshallese"))
enc.counts.wide <- enc.counts.race %>%
  pivot_wider(
    names_from = ServiceLine,
    values_from = enc.counts,
    values_fill = 0 # Fill missing values with 0
  )
enc.counts.wide <- enc.counts.wide %>% rename(PrimaryCare = 'Primary Care', UrgentCare = 'Urgent Care',
                                              Behavioral = 'Behavioral Health', Equity = 'Health Equity') %>%
  mutate(across(c(PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, 
                  Equity, Dental, Multispecialty, Other), as.numeric))

# we removed visit types (Health Equity and Other) that had 0 visits for multiple years. 
# 2016, 2017, and 2025 didn't have much data either so we removed them. 
# Then we removed years 2018 and 2019 just to make the table a bit smaller since it was so wide.

enc.marsh <- enc.counts.wide %>% filter(Group=='Marshallese' & year != "2016" & year != "2017" & year != "2018" & year != "2019" & year != "2025") %>%
  select(year, PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, Dental, Multispecialty) %>%
  mutate(across(c(PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, 
                  Dental, Multispecialty), as.numeric)) %>%
  gtsummary::tbl_summary(
    by = year,
    include = c(PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, Dental, Multispecialty),
    label = list(
      PrimaryCare ~ 'Primary Care',
      Emergency ~ 'Emergency',
      UrgentCare ~ 'Urgent Care',
      Behavioral ~ 'Behavioral Health',
      Pharmacy ~ 'Pharmacy',
      Dental ~ 'Dental',
      Multispecialty ~ 'Multispecialty'
    ),
    missing="no",
    type = list(PrimaryCare ~ "continuous", Emergency ~ "continuous", UrgentCare ~ "continuous", Behavioral ~ "continuous", Pharmacy ~ "continuous", Dental ~ "continuous", Multispecialty ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({min}, {max})"),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_footnote(c(all_stat_cols()) ~ "Mean (Minimum, Maximum)") %>%
  modify_header(label ~ "**Mean Visits per Patient**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>\nN = {n}")

enc.nhw <- enc.counts.wide %>% filter(Group=='Non-Marshallese' & year != "2016" & year != "2017" & year != "2018" & year != "2019" & year != "2025") %>%
  select(year, PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, Dental, Multispecialty) %>%
  mutate(across(c(PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, 
                  Dental, Multispecialty), as.numeric)) %>%
  gtsummary::tbl_summary(
    by = year,
    include = c(PrimaryCare, Emergency, UrgentCare, Behavioral, Pharmacy, Dental, Multispecialty),
    label = list(
      PrimaryCare ~ 'Primary Care',
      Emergency ~ 'Emergency',
      UrgentCare ~ 'Urgent Care',
      Behavioral ~ 'Behavioral Health',
      Pharmacy ~ 'Pharmacy',
      Dental ~ 'Dental',
      Multispecialty ~ 'Multispecialty'
    ),
    missing="no",
    type = list(PrimaryCare ~ "continuous", Emergency ~ "continuous", UrgentCare ~ "continuous", Behavioral ~ "continuous", Pharmacy ~ "continuous", Dental ~ "continuous", Multispecialty ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({min}, {max})"),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_footnote(c(all_stat_cols()) ~ "Mean (Minimum, Maximum)") %>%
  modify_header(label ~ "**Mean Visits per Patient**") %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>\nN = {n}")
enc.tbl <- tbl_merge(tbls = list(enc.marsh, enc.nhw), tab_spanner = c("**Marshallese**", "**Non-Marshallese**")) %>% 
  modify_caption("**Clinic Visits Over Time**")

# print encounters table
enc.tbl

################################
# KOH Attendance by Disease Type
################################
# read in data
obj1 <- read.csv('Analysis Data/Obj1_AllPts.csv')[,-1]
obj1.koh <- obj1 %>% filter(KOH==1)
obj1.koh$koh.counts.factor <- factor(obj1.koh$koh.counts)
obj1.koh$koh.counts <- as.numeric(obj1.koh$koh.counts)

obj1.koh.bp <- obj1.koh %>% filter(HTN==1)
koh.bp.tbl <- obj1.koh.bp %>% 
  select(c("koh.counts","koh.counts.factor")) %>%
  gtsummary::tbl_summary(
    include=c(koh.counts, koh.counts.factor),
    label = list(
      koh.counts ~ 'Average Attendance',
      koh.counts.factor ~ 'No. Meetings Attended'
    ),
    missing_text = "Missing",
    type = list(koh.counts ~ "continuous"),
    statistic = all_continuous() ~ c(
      "{median} ({min}, {max})"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Hypertension**<br>\nN = 42") # I had to hard code the number of pts with hypertension (N=42)
                                                                  # so you will have to change to the correct number with new data

obj1.koh.a1c <- obj1.koh %>% filter(Diabetes==1)
koh.a1c.tbl <- obj1.koh.a1c %>% 
  select(c("koh.counts","koh.counts.factor")) %>%
  gtsummary::tbl_summary(
    include=c(koh.counts, koh.counts.factor),
    label = list(
      koh.counts ~ 'Average Attendance',
      koh.counts.factor ~ 'No. Meetings Attended'
    ),
    missing_text = "Missing",
    type = list(koh.counts ~ "continuous"),
    statistic = all_continuous() ~ c(
      "{median} ({min}, {max})"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**Diabetes**<br>\nN = 69") # I had to hard code the number of pts with diabetes (N=69)
                                                              # so you will have to change to the correct number with new data

koh.tbl <- tbl_merge(tbls = list(koh.bp.tbl, koh.a1c.tbl), tab_spanner = FALSE) %>% 
  modify_caption("**'Know Our Health' Participation by Disease Type**")

# print table
koh.tbl 
