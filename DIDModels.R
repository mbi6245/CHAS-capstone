# DID Models
library(did)

# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)

# we will have some of the same patients who appear in both time frames, but more in the 2nd time period

colnames(all_visit_types)


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


#whoo-hoo it works


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

# Sept 2019 to Dec 2022 DID ER Estimates for CHW 1 and 2

# Jan 2023 to Dec 2024 additional ER trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 


# ! Note that ER visits are recorded from 2017 forward. 

# But primary care provider (PCP) visits are only recorded from 2018 forward
# Jan 2018 to Aug 2019 PCP Pretrends (about 1.5 years)

# Sept 2019 to Dec 2022 DID PCP Estimates for CHW 1 and 2

# Jan 2023 to Dec 2024 additional PCP trends from CHW 3 and 4, KOH and Change to Control Pop Insurance (Medicaid Unwinding ~ Dec 2022) 




# !are total interactions growing? Even if ER rates are going up.



# !! How to we account for correlation with longitutindal data  