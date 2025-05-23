######################################################################
################# UW/CHAS Capstone Analysis Project ##################
######### File 1: Preliminary Data Manipulation and Cleaning #########
# performs general data preparation before analysis dataset creation #
######################################################################

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_a1c = file.path(getwd(), "Raw Data/UWDataA1cs.csv")
fp_bmi = file.path(getwd(), "Raw Data/UWDataBMIs.csv")
fp_bp = file.path(getwd(), "Raw Data/UWDataBP.csv")
fp_diag = file.path(getwd(), "Raw Data/UWDataDiagnoses.csv")
fp_enc = file.path(getwd(), "Raw Data/UWDataEncounters.csv")
fp_panel = file.path(getwd(), "Raw Data/UWDataPanel.csv")
fp_koh = file.path(getwd(), "Raw Data/UWDataKOHAttendance.csv")
fp_exenc = file.path(getwd(), "Raw Data/UWExtraEncounterData.csv")
fp_missing_bmi = file.path(getwd(), "Raw Data/UWBMIMissing.csv")

# load in packages
library(dplyr)
library(tidyverse)
library(lubridate)
library(naniar)

# load in all datasets
a1c <- read.csv(fp_a1c)
bmi <- read.csv(fp_bmi)
bp <- read.csv(fp_bp)
diag <- read.csv(fp_diag) %>% rename(UniqueIdentifier = uniqueidentifier, PreDM = Pre.diabetes, T2DM = Type.2.DM)
enc <- read.csv(fp_enc) %>% rename(UniqueIdentifier = UniqueID)
panel <- read.csv(fp_panel, na.strings = "") %>% rename(UniqueIdentifier = UniqueID)
koh.attend <- read.csv(fp_koh) %>% rename(UniqueIdentifier = uniqueidentifier)
enc.extra <- read.csv(fp_exenc)
missing_bmi <- read.csv(fp_missing_bmi)


# change cases where PreDM=1 and T2DM=1 to PreDM=0
diag$PreDM[diag$PreDM == 1 & diag$T2DM == 1] <- 0

# change date variable to date type and remove NA values in necessary fields
a1c$Date <- mdy(a1c$Date)
a1c.nona <- a1c %>% filter(!is.na(A1c) & !is.na(Date)) %>% distinct()

# calculating appropriate BMI based on height and weight for patients with extreme or missing values of BMI in original BMI dataset
bmi$Date <- mdy(bmi$Date)
bmi.nona <- bmi %>% filter(!is.na(Date) & !is.na(BMI)) %>% distinct() %>% filter(BMI<=250) # filter out extreme values
missing_bmi$LastBMIDate <- mdy(missing_bmi$LastBMIDate)
missing_bmi$LastHeightDate <- mdy(missing_bmi$LastHeightDate)
missing_bmi$LastWeightDate <- mdy(missing_bmi$LastWeightDate)
missing_bmi <- missing_bmi %>% select(-c(BMIRefusedReason, BMIRefusedDate)) %>%
  mutate(BMI = ifelse(LastBMIDate >= LastWeightDate, LastBMI, (LastWeight / (LastHeight)^2) * 703),
         Date = ifelse(LastBMIDate >= LastWeightDate, LastBMIDate, LastWeightDate))
missing_bmi <- missing_bmi %>% rename(UniqueIdentifier = uniqueIdentifier) %>% select(c(UniqueIdentifier, BMI, Date))
missing_bmi$BMI <- round(missing_bmi$BMI, digits=2)
bmi.nona <- rbind(bmi.nona, missing_bmi)

bp$Date <- mdy(bp$Date)
bp.nona <- bp %>% filter(!is.na(Date) & !is.na(Systolic)) %>% distinct() # removing duplicate entries

enc <- bind_rows(enc, enc.extra) # merge extra encounter data with original encounter data
enc$Date <- mdy(enc$Date)
enc.nona <- enc %>% filter(!is.na(Date) & !is.na(ServiceLine))

koh.attend$Date <- mdy(koh.attend$dateAttended)
koh.attend <- koh.attend %>% filter(!is.na(Date)) %>% distinct() # remove duplicate entries
koh.counts <- koh.attend %>% group_by(UniqueIdentifier) %>% count(name = "koh.counts")

diag.nona <- diag %>% filter(!is.na(HTN) & !is.na(PreDM) & !is.na(T2DM))

# create diabetes indicator for both pre and T2DM and indicator for if they have both htn and diabetes
diag.nona <- diag.nona %>% mutate(Diabetes = ifelse((PreDM == 1 | T2DM == 1), 1, 0),
                                  both = ifelse((Diabetes == 1 & HTN == 1), 1, 0))

# Remove anyone under the age of 18
age <- panel %>% select(c("UniqueIdentifier","age"))
# function to remove anyone under the age of 18
adult <- function(data) {
  newdata <- left_join(data, age, by = "UniqueIdentifier") %>% filter(age >= 18)
  return(newdata)
}
a1c.nona.18 <- adult(a1c.nona)
bmi.nona.18 <- adult(bmi.nona)
# taking average of all BMI measurements since we are not using it as a time-varying covariate
bmi.nona.18 <- bmi.nona.18 %>% select(-age)
avg.bmi <- bmi.nona.18 %>% group_by(UniqueIdentifier) %>% mutate(avg.bmi = mean(BMI)) %>% slice_head() %>% 
  select(c(UniqueIdentifier, avg.bmi)) 

bp.nona.18 <- adult(bp.nona)
enc.nona.18 <- adult(enc.nona)
diag.nona.18 <- adult(diag.nona)
koh.attend.18 <- adult(koh.attend)

# patients with KOHParticipant = 1 but are not in KOH attendance dataset
panel.koh <- panel %>% filter(KOHParticipant == 1 & age>=18)
k <- full_join(koh.counts, panel.koh, by = "UniqueIdentifier")
k %>% filter(is.na(koh.counts))

panel <- panel %>% mutate(Sex.long = case_when(Sex == 'M' ~ 'Male',
                                               Sex == 'F' ~ 'Female'))
panel.18 <- panel %>% filter(age >= 18)

# create our population of non-hispanic white and marshallese (language = marsh or race = marsh or KOH = 1) 
targetpop <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1 | (Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))

# remove homeless population
targetpop <- targetpop %>% filter(No.column.name != 'Homeless Shelter' & No.column.name != 'Street')

# make marshallese an indicator variable
targetpop <- targetpop %>% 
  mutate(Marsh = ifelse((Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1), 1, 0),
         Group = ifelse(Marsh == 1, "Marshallese", "Non-Marshallese"))

targetpop$No.column.name <- factor(targetpop$No.column.name, 
                                   levels = c('Housed','Doubling Up','Permanent Supportive Housing','Transitional','Other','Unknown'))

# get rid of the age variable we got from merging to filter out children since targetpop already has age
diag.nona.18 <- diag.nona.18 %>% select(-age)

# merge diagnosis data with other characteristics
table1 <- right_join(diag.nona.18, targetpop, by = "UniqueIdentifier")
table1 <- left_join(table1, avg.bmi, by = "UniqueIdentifier")

# create subset of only KOH participants
koh <- table1 %>% filter(KOHParticipant == 1)
koh.table1 <- inner_join(koh.counts, koh, by = "UniqueIdentifier")