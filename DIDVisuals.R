# DID visualizations

ER2016_2025_yearmonth_marsh %>% #not wider version
  filter(yearmonth_as_date != "2025-01-01") %>% # the year just started
  ggplot(aes( x= as.Date(yearmonth_as_date), y = Freq, col = marsh )) +
  geom_line()+
  labs(x = "Time", y = "Number of ER visits", title = "Number of ER Visits by Month \n Marshallese and Non-Hispanic White Patients \n CHAS Maple and Market Clinics")+
  theme_bw()


# Pure numbers of ER visits for Marshallese per year
ER2016_2025_year_marsh %>% filter(marsh == 1) %>% filter(year != 2025) %>%
  ggplot(aes(x= year, y= Freq, col = marsh, group = marsh ))+
  geom_line()+
  labs(y = "Number of ER Visits", x = "Year", title = "Number of ER Visits for Marshallese 2016-2024")
# Note there is a change in 2019 that is flat then uptick again in 2021

# Pure numbers of ER visits for Marshallese per year
ER2016_2025_year_marsh %>% filter(marsh == 0) %>% filter(year != 2025) %>%
  ggplot(aes(x= year, y= Freq, col = marsh, group = marsh ))+
  geom_line()+
  labs(y = "Number of ER Visits", x = "Year", title = "Number of ER Visits for Non-Hispanic White 2016-2024")
# Note there is a change in 2019 that is flat then uptick again in 2021

# ! I need to see if we have a good number of patients in the first 5 years as the 2nd five years
# ! I think we had other patients before?

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


# visualize how long M patients have been there
all_visit_types %>% filter(UniqueIdentifier %in% MarshalleseUniqueID ) %>% mutate(extra = if_else((year %in% 2016:2020), 1, 0, NA)) %>%  
  # arrange(extra, UniqueIdentifier) %>%  this worked to move them all to the top but it also made shifts in the lines that weren't as clear
  # mutate(Row_Number = row_number()) %>% 
  # y = Row_Number
  ggplot(aes(x = Date, y = UniqueIdentifier, group = UniqueIdentifier, col = extra)) +
  geom_line()

