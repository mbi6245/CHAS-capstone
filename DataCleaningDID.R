# data cleaning
#rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(rstudioapi) # for file path? 

# Demographics #
######################
fp_pnl = file.path(getwd(), "Raw Data/UWDataPanel.csv")
panel <- read.csv(fp_pnl, na.strings = "") # demographics

# from Gabby's EDA code

panel.18 <- panel %>% filter(age >= 18)

# just checking the possible races
#unique(panel.18$Race) #123 races, looks like open text option
# marsh <- panel.18 %>% filter(Race == 'Marshallese')
# lang <- panel.18 %>% filter(Language == 'Marshallese')
#there are a lot of patients with Race = Marshallese that don't have language Marshallese and vice versa


#According to our SAP 3.3.2 we will match by age, sex and poverty.
# We will also exclude homeless.
# So these are the patients we need to exclude from the target population.

colnames(panel.18)

targetpop <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1 | (Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))

# make marshallese indicator variable
targetpop <- targetpop %>%
  mutate(Marsh = ifelse((Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1), 1, 0),
         Group = ifelse(Marsh == 1, "Marshallese", "Non-Marshallese"))

# m <- targetpop %>% filter(Marsh == 1 ) #%>% summarize(max(BLACERISK))
# w <- targetpop %>% filter(Marsh == 0 )


############### OLDER ######################
MaxMarshAge <- targetpop %>% filter(Marsh == 1) %>% summarize(max(age))

older <- targetpop %>% filter(age > as.numeric(MaxMarshAge))
# 67 patients 

############### HOMELESS ######################
# No.column.name is housing status
# filtered housing status from panel.18 in the DataCleaningDID.R
homeless <-  targetpop %>% filter(No.column.name == "Homeless Shelter" |
                                    No.column.name == "Street" )
#colnames(targetpop)

#with(targetpop, table(No.column.name, Marsh))

############### DECEASED ######################


# ! Also what do we do with Deceased Date?
deceased <-targetpop %>% filter(DeceasedDate != "")

nrow(deceased)
#  344

class(deceased$DeceasedDate)
# "character"
deceased$DeceasedDate <- as.Date(deceased$DeceasedDate, format = "%m/%d/%Y")
class(deceased$DeceasedDate)

min(deceased$DeceasedDate)
# "2022-06-03"
# this is before the end of the DID analysis so I would remove it...
max(deceased$DeceasedDate)
# "2024-12-25"
#hist(deceased$DeceasedDate, breaks = 24)
with(deceased, table( Marsh))

# 20 marshallese and 316 NHW 

# A lot of them are after our DID model but I am still inclined to remove them all

############### RISK SCORES ######################
# I am surprised to see such high Risk Scores in our controls
# I don't think they make good controls if they are double the maximum Marshallese risk score.
# we will keep all people without risk scores in both groups

MaxMarshBLACERISK <-  targetpop %>% filter(Marsh == 1) %>% summarize(max(BLACERISK, na.rm = TRUE))


higher_risk <- targetpop %>% filter(BLACERISK > as.numeric(MaxMarshBLACERISK))
nrow(higher_risk)
# 59

############### INCOME ######################
# higher income

MaxMarshIncome <-  targetpop %>% filter(Marsh == 1) %>% summarize(max(IncomeLevel, na.rm = TRUE))

higher_income <- targetpop %>% filter(IncomeLevel > as.numeric(MaxMarshIncome))

nrow(higher_income)
# 641

############### BMI ######################

fp_bmi = file.path(getwd(), "Raw Data/UWDataBMIs.csv")
bmi <- read.csv(fp_bmi) # demographics

# gabby added
fp_missing_bmi = file.path(getwd(), "Raw Data/UWBMIMissing.csv")
missing_bmi <- read.csv(fp_missing_bmi)

# cindys old code
#fp_new_bmi = file.path(getwd(), "Raw Data/UW BMI Missing List.csv")
#UW.BMI.Missing.List. <- read.csv(fp_new_bmi) # demographics

# bmi <- read.csv("Raw Data/UWDataBMIs.csv")

# gabby added
bmi$Date <- mdy(bmi$Date)
bmi.nona <- bmi %>% filter(!is.na(Date) & !is.na(BMI)) %>% distinct() %>% filter(BMI<=250)
missing_bmi$LastBMIDate <- mdy(missing_bmi$LastBMIDate)
missing_bmi$LastHeightDate <- mdy(missing_bmi$LastHeightDate)
missing_bmi$LastWeightDate <- mdy(missing_bmi$LastWeightDate)
missing_bmi <- missing_bmi %>% select(-c(BMIRefusedReason, BMIRefusedDate)) %>%
  mutate(BMI = ifelse(LastBMIDate >= LastWeightDate, LastBMI, (LastWeight / (LastHeight)^2) * 703),
         Date = ifelse(LastBMIDate >= LastWeightDate, LastBMIDate, LastWeightDate))
missing_bmi <- missing_bmi %>% rename(UniqueIdentifier = uniqueIdentifier) %>% select(c(UniqueIdentifier, BMI, Date))
missing_bmi$BMI <- round(missing_bmi$BMI, digits=2)
full_bmi <- rbind(bmi.nona, missing_bmi) %>% select(c(UniqueIdentifier, BMI))

# cindys old code
#bmi.nona <- bmi %>% filter(!is.na(Date) & !is.na(BMI))
# update this with new BMI list
#UW.BMI.Missing.List. <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/CHAS-capstone/Raw Data/UW BMI Missing List .csv")
#colnames(bmi)
# match up the column names so we can merge better
#colnames(UW.BMI.Missing.List.)[1] <- c("UniqueIdentifier")
#colnames(UW.BMI.Missing.List.)[2] <- c("BMI")
#full_bmi <- full_join(bmi, UW.BMI.Missing.List.) # , by = c("UniqueIdentifier" , "uniqueIdentifier")



# mark who are Marshallese and see if there are NHW with higher BMI
colnames(targetpop)
colnames(full_bmi)
colnames(targetpop)[1] <- c("UniqueID")
colnames(full_bmi)[1] <- c("UniqueID")
targetpop <-  left_join(targetpop ,full_bmi, by = "UniqueID")

MaxMarshalleseBMI <- targetpop %>% filter(Marsh == 1) %>% summarize(max(BMI, na.rm = TRUE))
# 73
MaxNHWBMI <- targetpop %>% filter(Marsh == 0) %>% summarize(max(BMI, na.rm = TRUE))
# 111436.5
# gabby got 246

# ! note there are some zero BMI in each group as well but since they are in both I think we can accept these?

higher_BMI <- targetpop %>% filter(BMI > as.numeric(MaxMarshalleseBMI))

nrow(higher_BMI)
# 331 including BMI in the thousands which are clearly out of range





# we might have to merge this to targetpop
exclude <- c(higher_BMI$UniqueID, higher_income$UniqueID, higher_risk$UniqueID, deceased$UniqueID, homeless$UniqueID, older$UniqueID)
length(exclude)
# 2396
exclude <- unique(exclude)
length(exclude)
# 2187 which is 209 duplicates between the groups


# Remove these groups too

# Final Target Pop for our analysis
targetpop_DID <- targetpop %>% filter(!UniqueID %in% exclude) # not in exclude list is !UniqueID %in% exclude

intersect(targetpop_DID$UniqueID, exclude) # check there are no more excluded patients in the new targetpop_DID
# good it worked


# our total patient population for DID
length(unique(targetpop_DID$UniqueID))
# 26462
# gabby got 28502

targetpop_DID %>% group_by(Marsh) %>% nrow()
# 140843
# gabby got 151297
targetpop_DID %>% filter(Marsh == 1) %>% nrow()
# 4824
# gabby got 4936
targetpop_DID %>% filter(Marsh == 0) %>% nrow()
# 136019
# gabby got 146361


# length(unique(targetpop$UniqueID))

nrow(targetpop_DID) == nrow(targetpop) - length(exclude)
# !FALSE

#! We need to drop the duplicates for summaries

# Visually check the covariates now with histograms

#hist(targetpop_DID$age)
# 2 groups

# hist(targetpop_DID$age[Marsh == 1 ], freq = FALSE, col = "grey",
# border = NA, xlab = "",
# ylab = "", yaxt = "n", breaks = 30,
# main = "Distribution of Age between Marshallese and Non-Hispanic White patients in DID Model",
# xlim = c(0, 1), ylim = c(0, 4.5))
# hist(targetpop_DID$age[Marsh == 0], freq = FALSE,
# add = TRUE,
# # breaks = 30,
# col=NA)

visuals_DID <- function(demographic) {
  targetpop_DID %>% 
    ggplot(aes(x = {{demographic}}))+
    geom_density()+
    facet_wrap(~Marsh)
}

visuals_DID(age)
visuals_DID(BMI)
visuals_DID(IncomeLevel)
visuals_DID(Sex)
visuals_DID(BLACERISK)
visuals_DID(No.column.name) # homeless status shows the options 
visuals_DID(DeceasedDate) # totally blank all of the patients are still alive
visuals_DID(ClinicLocation) # lots of marshallese have no clinic assigned



# ! check table 1 of these new groups 





##### FROM NOW ON USE targetpop_DID INSTEAD OF panel.18 ##########



Marshallese <- targetpop_DID %>% filter(Race == 'Marshallese' | Language == 'Marshallese'| KOHParticipant == 1) #
MarshalleseUniqueID <- unique(Marshallese$UniqueID)

#number of unique Marshallese
length(unique(MarshalleseUniqueID))
# 849
# gabby got 876

Control <- targetpop_DID %>% filter((Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))
ControlUniqueID <- unique(Control$UniqueID)

# Number of unique controls 
length(unique(ControlUniqueID))
# 25616
# gabby got 27629


write.csv(targetpop_DID, "Analysis Data/targetpop_DID.csv")



########### Start With Encounter Types #############


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_ex_enc = file.path(getwd(), "Raw Data/UWExtraEncounterData.csv")
fp_enc = file.path(getwd(), "Raw Data/UWDataEncounters.csv")

all_visit_types_2016_2021 <- read.csv(fp_ex_enc)
# Jan 2016 to Dec 2021
all_visit_types_2021_2025 <- read.csv(fp_enc)
# Jan 2021 ro Jan 2025

all_visit_types_2021_2025$Date <- mdy(all_visit_types_2021_2025$Date)
all_visit_types_2016_2021$Date <- mdy(all_visit_types_2016_2021$Date)
# make sure the sets are mutually exclusive

#?lubridate
#
# min(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y"))
# # "2016-01-02"
# max(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y"))
# # "2021-12-31"
# min(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y"))
# # "2021-01-09"
# max(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y"))
# # "2025-01-08"
#
# check <- intersect(all_visit_types_2016_2021, all_visit_types_2021_2025 )
# # all 2021 dates are overlapping


# Remove 2021 from one set
#lubridate::year(x)
#all_visit_types_2016_2021 <- all_visit_types_2016_2021 %>% mutate(year = lubridate::year(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y")) )
all_visit_types_2016_2021 <- all_visit_types_2016_2021 %>% mutate(year = lubridate::year(all_visit_types_2016_2021$Date) )

all_visit_types_2016_2020 <- all_visit_types_2016_2021 %>% filter(year != 2021)

# add year to all_visit_types_2021_2025 as well
#all_visit_types_2021_2025 <- all_visit_types_2021_2025 %>% mutate(year = lubridate::year(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y")) )
all_visit_types_2021_2025 <- all_visit_types_2021_2025 %>% mutate(year = lubridate::year(all_visit_types_2021_2025$Date) )

colnames(all_visit_types_2021_2025) == colnames(all_visit_types_2016_2020)

# combine all visit types across years
all_visit_types <- rbind(all_visit_types_2016_2020, all_visit_types_2021_2025)

nrow(all_visit_types_2016_2020) + nrow(all_visit_types_2021_2025) == nrow(all_visit_types)

# check if we have race/lang data for all the uniqueIDs


#all_visit_types






########### Make Sure we have data on all patients ############
# figure out how many races we are missing in 2016 to 2020 data
all_races <- panel %>% select(UniqueIdentifier, KOHParticipant, Ethnicity, Race)
all_visit_types_race <- full_join(all_visit_types, all_races, by = "UniqueIdentifier")
all_visit_types_race %>% summary()

all <- as.data.frame(summary(as.factor(all_visit_types_race$Race)))


# Double check, everyone in 2016-2020 is still there in 2021-2025, but there are about 22000 more people too :)
how_many_stay <- intersect(all_visit_types_2016_2020$UniqueIdentifier, all_visit_types_2021_2025$UniqueIdentifier )
length(unique(how_many_stay))
# 30434
length(unique(all_visit_types$UniqueIdentifier))
# 52506
length(unique(all_visit_types_2016_2020$UniqueIdentifier))
# 30434
length(unique(all_visit_types_2021_2025$UniqueIdentifier))
# 52506

all_visit_types_2016_2021$Date <- as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y")
class(all_visit_types_2016_2021$Date)

all_visit_types$Date <- as.Date(all_visit_types$Date, format = "%m/%d/%Y")
class(all_visit_types$Date)

# figure out how many races we are missing  - This was excluding the kids because we used panel.18! 
#
# panel.18 %>% filter(is.na(Race)) %>% nrow()
# # 0
# panel.18 %>% filter(Race == "") %>% nrow()
# # 112
# panel.18 %>% filter(Race == "Patient Declined") %>% nrow()
# # 3144
# length(unique(all_visit_types_2021_2025$UniqueIdentifier))
# # 52506
# # about 6% are unrecorded
#
# # with(panel.18, table(Race))
#
#










############# Create table for all visit types for our targetpop_DID ##############
# colnames(all_visit_types)

all_visit_types <- all_visit_types %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                              if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) )



write.csv(all_visit_types, "Analysis Data/all_visit_types.csv")


table_all_visit_types <- as.data.frame( with(all_visit_types, table(ServiceLine,year, marsh)) ) #, useNA = "always"

# update when we get all of the Marshallese and White Unique IDs

table_all_visit_types_wider <- pivot_wider(table_all_visit_types,
                                           names_from = year,
                                           values_from = Freq)

# export for final report
#write.csv(table_all_visit_types_wider, "table_all_visit_types_wider.csv")

# table_all_visit_types_wider <- t(table_all_visit_types_wider)



######## ER VISIT TYPES ###############

#ER2021_2025 <- all_visit_types |> filter(ServiceLine == "Emergency") #!  | ServiceLine == "Urgent Care"
ER2016_2025 <- all_visit_types |> filter(ServiceLine == "Emergency") #!  | ServiceLine == "Urgent Care"


# make table of ER visits per year

# x <- as.Date(exonerees$Date.of.Exoneration, format = "%m/%d/%Y");
# #lubridate::year(x)
# exonerees <- exonerees %>% mutate(year_exonerated = lubridate::year(x))
# exonerations_state_year <- with(exonerees, table(State, year_exonerated, useNA = "always"))
# exonerations_state_year <- as.data.frame(exonerations_state_year) #need to filter out federal to match,


#lubridate::year(x)
#ER2016_2025 <- ER2016_2025 %>% mutate(year = lubridate::year(as.Date(ER2016_2025$Date, format = "%m/%d/%Y")) )
ER2016_2025_year <-  as.data.frame( with(ER2016_2025, table(year)))  # , useNA = "always"

# make table of ER visits per month

ER2016_2025 <- ER2016_2025 %>% mutate(yearmonth = zoo::as.yearmon(ER2016_2025$Date, "%m/%d/%Y"), # shows as text "Feb 2024"
                                      yearmonth_as_date =as.Date(as.yearmon(ER2016_2025$Date, "%m/%d/%Y"))) 
# as.Date(as.yearmon(ER2016_2025$Date, "%m/%d/%Y")) # "2023-01-01" sets day to first day of month
ER2016_2025_yearmonth <-  as.data.frame( with(ER2016_2025, table(yearmonth_as_date)))


# make tables by Marshallese and Non-Hispanic white

# By Year
#colnames(ER2016_2025)
ER2016_2025 <- ER2016_2025 %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                      if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) ) %>%
  filter(!is.na(marsh))

# which(is.na(ER2016_2025$marsh))

ER2016_2025_year_marsh <- as.data.frame( with(ER2016_2025, table(year, marsh)) ) #, useNA = "always"


ER2016_2025_year_marsh_wider <- pivot_wider(ER2016_2025_year_marsh,
                                                 names_from = year,
                                                 values_from = Freq)

ER2016_2025_year_marsh_wider <- as.data.frame(ER2016_2025_year_marsh_wider)
rownames(ER2016_2025_year_marsh_wider) <- c("Marshallese", "Non-Hispanic White")
ER2016_2025_year_marsh_wider <- ER2016_2025_year_marsh_wider[ , -1] # removing marsh column so just dates




# By Month
ER2016_2025_yearmonth_marsh <-  as.data.frame( with(ER2016_2025, table(yearmonth_as_date, marsh)))


ER2016_2025_yearmonth_marsh_wider <- pivot_wider(ER2016_2025_yearmonth_marsh, 
                 names_from = yearmonth_as_date, 
                 values_from = Freq)



ER2016_2025_yearmonth_marsh_wider <- as.data.frame(ER2016_2025_yearmonth_marsh_wider)
rownames(ER2016_2025_yearmonth_marsh_wider) <- c("Marshallese", "Non-Hispanic White")
ER2016_2025_yearmonth_marsh_wider <- ER2016_2025_yearmonth_marsh_wider[ , -1] # removing marsh column so just dates
#

# Visualize the Trends of these 2 groups over time see DIDVisuals.R


# as.Date(as.yearmon(ER2016_2025$Date, "%m/%d/%Y")) # "2023-01-01" sets day to first day of month

class(ER2016_2025_yearmonth_marsh$yearmonth_as_date)
# factor


#############################################

# Compare Primary Care Trends 
pcp_2016_2025 <- all_visit_types |> filter(ServiceLine == "Primary Care") #!  | ServiceLine == "Urgent Care"

pcp_2016_2025 <- pcp_2016_2025 %>% mutate(year = lubridate::year(as.Date(pcp_2016_2025$Date, format = "%m/%d/%Y")) )
 # total number pcp visits
#pcp_2016_2025_year <-  as.data.frame( with(pcp_2016_2025, table(year)))  # , useNA = "always"


pcp_2016_2025 <- pcp_2016_2025 %>% mutate(yearmonth = zoo::as.yearmon(pcp_2016_2025$Date, "%m/%d/%Y"), # shows as text "Feb 2024"
                                          yearmonth_as_date =as.Date(as.yearmon(pcp_2016_2025$Date, "%m/%d/%Y")))  # "Feb 2024"

# as.Date(as.yearmon(pcp_2016_2025$Date, "%m/%d/%Y")) # "2023-01-01" sets day to first day of month

# total pcp appts per month
#pcp_2016_2025_yearmonth <-  as.data.frame( with(pcp_2016_2025, table(yearmonth)))




# make table by Marshallese and Non-Hispanic white

#colnames(pcp_2016_2025)
pcp_2016_2025 <- pcp_2016_2025 %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                      if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) ) %>%
  filter(!is.na(marsh))

# which(is.na(pcp_2016_2025$marsh))

pcp_2016_2025_year_marsh <- as.data.frame( with(pcp_2016_2025, table(year, marsh)) ) #, useNA = "always"


# Make tables wider


pcp_2016_2025_year_marsh_wider <- pivot_wider(pcp_2016_2025_year_marsh,
                                            names_from = year,
                                            values_from = Freq)


pcp_2016_2025_year_marsh_wider <- as.data.frame(pcp_2016_2025_year_marsh_wider)
rownames(pcp_2016_2025_year_marsh_wider) <- c("Marshallese", "Non-Hispanic White")
pcp_2016_2025_year_marsh_wider <- pcp_2016_2025_year_marsh_wider[ , -1] # removing marsh column so just dates




# By Month
#pcp_2016_2025_yearmonth_marsh <-  as.data.frame( with(pcp_2016_2025, table(yearmonth, marsh)))

pcp_2016_2025_yearmonth_marsh <-  as.data.frame( with(pcp_2016_2025, table(yearmonth_as_date, marsh)))


pcp_2016_2025_yearmonth_marsh_wider <- pivot_wider(pcp_2016_2025_yearmonth_marsh, 
                                                 names_from = yearmonth_as_date, 
                                                 values_from = Freq)



pcp_2016_2025_yearmonth_marsh_wider <- as.data.frame(pcp_2016_2025_yearmonth_marsh_wider)
rownames(pcp_2016_2025_yearmonth_marsh_wider) <- c("Marshallese", "Non-Hispanic White")
pcp_2016_2025_yearmonth_marsh_wider <- pcp_2016_2025_yearmonth_marsh_wider[ , -1] # removing marsh column so just dates




#  Make a function for each type of visit? We have these numbers in the full table 
# #  Urgent Care
# Urgent2016_2025 <- all_visit_types |> filter( ServiceLine == "Urgent Care")
#  
# Urgent2016_2025 <- Urgent2016_2025 %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
#                                                               if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) ) %>%
#   filter(!is.na(marsh))
# Urgent2016_2025_year_marsh <-  as.data.frame( with(Urgent2016_2025, table(year, marsh)))  # , useNA = "always"
# 

