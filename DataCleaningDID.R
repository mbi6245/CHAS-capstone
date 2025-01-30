# data cleaning
#rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo)


all_visit_types_2016_2021 <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/Raw Data/UWExtraEncounterData.csv")
# Jan 2016 to Dec 2021
all_visit_types_2021_2025 <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/Raw Data/UWDataEncounters.csv")
# Jan 2021 ro Jan 2025


# make sure the sets are mutually exclusive

?lubridate

min(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y"))
# "2016-01-02"
max(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y"))
# "2021-12-31"
min(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y"))
# "2021-01-09"
max(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y"))
# "2025-01-08"

check <- intersect(all_visit_types_2016_2021, all_visit_types_2021_2025 )
# all 2021 dates are overlapping


# Remove 2021 from one set
#lubridate::year(x)
all_visit_types_2016_2021 <- all_visit_types_2016_2021 %>% mutate(year = lubridate::year(as.Date(all_visit_types_2016_2021$Date, format = "%m/%d/%Y")) )

all_visit_types_2016_2020 <- all_visit_types_2016_2021 %>% filter(year != 2021)

# add year to all_visit_types_2021_2025 as well
all_visit_types_2021_2025 <- all_visit_types_2021_2025 %>% mutate(year = lubridate::year(as.Date(all_visit_types_2021_2025$Date, format = "%m/%d/%Y")) )

colnames(all_visit_types_2021_2025) == colnames(all_visit_types_2016_2020)

all_visit_types <- rbind(all_visit_types_2016_2020, all_visit_types_2021_2025)

nrow(all_visit_types_2016_2020) + nrow(all_visit_types_2021_2025) == nrow(all_visit_types)

# check if we have race/lang data for all the uniqueIDs


all_visit_types



#ER2021_2025 <- all_visit_types |> filter(ServiceLine == "Emergency") #!  | ServiceLine == "Urgent Care"
ER2016_2025 <- all_visit_types |> filter(ServiceLine == "Emergency") #!  | ServiceLine == "Urgent Care"

panel <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/Raw Data/UWDataPanel.csv") # demographics

# from Gabby's EDA code

panel.18 <- panel %>% filter(age >= 18)

# just checking the possible races
#unique(panel.18$Race) #123 races, looks like open text option
# marsh <- panel.18 %>% filter(Race == 'Marshallese')
# lang <- panel.18 %>% filter(Language == 'Marshallese')
#there are a lot of patients with Race = Marshallese that don't have language Marshallese and vice versa


Marshallese <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese'| KOHParticipant == 1) #
MarshalleseUniqueID <- Marshallese$UniqueID
Control <- panel.18 %>% filter((Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))
ControlUniqueID <- Control$UniqueID

# From Gabby
# create our population of non-hispanic whites and marshallese (language = marsh or race = marsh or KOH = 1) - think there might be a KOH participant that doesn't have race or language as marshallese but I need to double check
# targetpop <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1 | (Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))
#
# # make marshallese indicator variable
# targetpop <- targetpop %>% mutate(Marsh = ifelse((Race == 'White' & Ethnicity == 'Not Hispanic or Latino'), 0, 1),
#                                   Group = ifelse(Marsh == 1, "Marshallese", "Non-Marshallese"))


# !remove unhoused from control? remove ages that exceed Marshallese?

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




ER2016_2025 <- ER2016_2025 %>% mutate(yearmonth = zoo::as.yearmon(ER2016_2025$Date, "%m/%d/%Y")) # "Feb 2024"

# as.Date(as.yearmon(ER2016_2025$Date, "%m/%d/%Y")) # "2023-01-01" sets day to first day of month

ER2016_2025_yearmonth <-  as.data.frame( with(ER2016_2025, table(yearmonth)))


# ER And Urgent Care
ERandUC2016_2025 <- all_visit_types |> filter(ServiceLine == "Emergency" | ServiceLine == "Urgent Care")

ERandUC2016_2025 <-  as.data.frame( with(ERandUC2016_2025, table(year)))  # , useNA = "always"


# make table by Marshallese and Non-Hispanic white


#colnames(ER2016_2025)
ER2016_2025 <- ER2016_2025 %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                      if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) ) %>%
  filter(!is.na(marsh))

# which(is.na(ER2016_2025$marsh))

ER2016_2025_year_marsh <- as.data.frame( with(ER2016_2025, table(year, marsh)) ) #, useNA = "always"
ER2016_2025_yearmonth_marsh <-  as.data.frame( with(ER2016_2025, table(yearmonth, marsh)))

# ER And Urgent Care

ERandUC2016_2025 <- ERandUC2016_2025 %>% mutate(marsh = if_else((UniqueIdentifier %in% MarshalleseUniqueID), 1,
                                                      if_else((UniqueIdentifier %in% ControlUniqueID), 0, NA) ) ) %>%
  filter(!is.na(marsh))
ERandUC2016_2025_marsh <-  as.data.frame( with(ERandUC2016_2025, table(year, marsh)))  # , useNA = "always"

# Visualize the Trends of these 2 groups over time
# Spaghetti plot
# tlc_long %>%
#   ggplot(aes(x=time, y=lead, group=id, color=tx))+
#   geom_line()+
#   facet_wrap(~tx)+
#   labs(y="Lead (mg/dL)", x="Time (Weeks)")+
#   scale_x_continuous(breaks=c(0,1,4,6))

# ! I need to see if we have a good number of patients in the first 5 years as the 2nd five years

# Visualize the Trends of these 2 groups over time
# Spaghetti plot
# tlc_long %>%
#   ggplot(aes(x=time, y=lead, group=id, color=tx))+
#   geom_line()+
#   facet_wrap(~tx)+
#   labs(y="Lead (mg/dL)", x="Time (Weeks)")+
#   scale_x_continuous(breaks=c(0,1,4,6))

#!
ER2016_2025_year_marsh %>% filter(marsh == 1) %>%
  ggplot(aes(x= as.numeric(year), y= Freq, col = marsh, group = marsh ))+
  geom_line()+
  labs(y = "Number of ER Visits", x = "Year", title = "Number of ER Visits by Group 2016-2025")

# ! I think we had other patients before?

#class(ER2016_2025_year_marsh$year)
# ER2016_2025_yearmonth_marsh %>%
#   ggplot(aes(x= as.Date(yearmonth), y= Freq, col = marsh, group = marsh ))+
#   geom_line()+
#   labs(y = "Number of ER Visits", x = "Year", title = "Number of ER Visits by Group \2016-2025")

class(ER2016_2025_yearmonth_marsh$yearmonth)

# create average number of ER visits per group. We need to divide each time periods number by the number of patients...
n_controls <- nrow(Control)
n_marsh <- nrow(Marshallese)


ER2016_2025_year_marsh <- ER2016_2025_year_marsh %>% mutate(pop_size = if_else((marsh == 1), n_marsh,
                                                                               if_else((marsh == 0),  n_controls, NA) ),
                                  rate = Freq/pop_size)

ER2016_2025_year_marsh %>%
  ggplot(aes(x=year, y=rate, group=marsh, color=marsh))+
  geom_line()+
  #facet_wrap(~marsh)+
  labs(y="Count", x="Time", title = "Emergency Visits Rates for CHAS Patients by Year
       \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics") #+
  #scale_x_continuous(breaks=c(0,1,4,6))

# !note that 2025 is only just beginning so numbers are low


# ER2016_2025_year_marsh %>%
#   ggplot(aes(x=year, y=Freq, group=marsh, color=marsh))+
#   geom_line()+
#   #facet_wrap(~marsh)+
#   labs(y="Count", x="Time", title = "Emergency Visits for CHAS Patients by Year") #+
#   #scale_x_continuous(breaks=c(0,1,4,6))
#
# ER2016_2025_yearmonth %>%
#   ggplot(aes(x=as.Date(yearmonth), y=Freq, group=marsh, color=marsh))+
#   geom_line()+
#   #facet_wrap(~marsh)+
#   labs(y="Count", x="Time", title = "Emergency Visits for CHAS Patients by Month") #+
# #scale_x_continuous(breaks=c(0,1,4,6))
#
#
# Compare Primary Care Trends with descriptive statistics
pcp_2016_2025 <- all_visit_types |> filter(ServiceLine == "Primary Care") #!  | ServiceLine == "Urgent Care"

pcp_2016_2025 <- pcp_2016_2025 %>% mutate(year = lubridate::year(as.Date(pcp_2016_2025$Date, format = "%m/%d/%Y")) )
 # total number pcp visits
#pcp_2016_2025_year <-  as.data.frame( with(pcp_2016_2025, table(year)))  # , useNA = "always"


pcp_2016_2025 <- pcp_2016_2025 %>% mutate(yearmonth = zoo::as.yearmon(pcp_2016_2025$Date, "%m/%d/%Y")) # "Feb 2024"

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
pcp_2016_2025_yearmonth_marsh <-  as.data.frame( with(pcp_2016_2025, table(yearmonth, marsh)))


# Visualize the Trends of these 2 groups over time
# Spaghetti plot
# tlc_long %>%
#   ggplot(aes(x=time, y=lead, group=id, color=tx))+
#   geom_line()+
#   facet_wrap(~tx)+
#   labs(y="Lead (mg/dL)", x="Time (Weeks)")+
#   scale_x_continuous(breaks=c(0,1,4,6))

# # create average number of ER visits per group. We need to divide each time periods number by the number of patients...
# n_controls <- nrow(Control)
# n_marsh <- nrow(Marshallese)
#
# mutate
#
# pcp_2016_2025_year_marsh <- pcp_2016_2025_year_marsh %>% mutate(pop_size = if_else((marsh == 1), n_marsh,
#                                                                                if_else((marsh == 0),  n_controls, NA) ),
#                                                             rate = Freq/pop_size)
#
pcp_2016_2025_year_marsh %>% filter(year != 2025) %>%
  ggplot(aes(x=year, y=Freq, group=marsh, color=marsh))+
  geom_line()+
  #facet_wrap(~marsh)+
  labs(y="Count", x="Time",
  title = "Primary Care Provider Visits Rates for CHAS Patients by Year
       \n Marshallese and Non-Hispanic White Patients \n Maple and Market Clinics") #+
#scale_x_continuous(breaks=c(0,1,4,6))

pcp_2016_2025_year_marsh %>% filter(year != 2025) %>% # only one month into the year
  filter(marsh == 1) %>%
  ggplot(aes(x=year, y=Freq, group=marsh, color=marsh))+
  geom_line()+
  #facet_wrap(~marsh)+
  labs(y="Count", x="Time",
       title = "Primary Care Provider Visits Rates for CHAS Patients by Year
       \n Marshallese Patients \n Maple and Market Clinics") #+
#Whoo-hoo! Look at this one!



# !note that 2025 is only just beginning so numbers are low


# pcp_2016_2025_year_marsh %>%
#   ggplot(aes(x=year, y=Freq, group=marsh, color=marsh))+
#   geom_line()+
#   #facet_wrap(~marsh)+
#   labs(y="Count", x="Time", title = "Emergency Visits for CHAS Patients by Year") #+
#   #scale_x_continuous(breaks=c(0,1,4,6))
#
# pcp_2016_2025_yearmonth %>%
#   ggplot(aes(x=as.Date(yearmonth), y=Freq, group=marsh, color=marsh))+
#   geom_line()+
#   #facet_wrap(~marsh)+
#   labs(y="Count", x="Time", title = "Emergency Visits for CHAS Patients by Month") #+
# #scale_x_continuous(breaks=c(0,1,4,6))
#
#





