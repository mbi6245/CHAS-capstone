# Run DID manually

# Import dataframes DataCleaningDID.R script


fp_visits = file.path(getwd(), "Analysis Data/all_visit_types.csv")
all_visit_types <- read.csv(fp_visits) 
# Mark Visit Types with Marshallese and NHW
# all_visit_types  has M indicator from previous DataCleaningDID.R script, which shows 
# 1 = marshallese, 0 = Non-Hispanic White, NA = Other not in our analysis)


# fp_tarpop = file.path(getwd(), "Analysis Data/targetpop_DID.csv")
# targetpop_DID <- read.csv(fp_tarpop) 
# 



# start building dataframe
did_visit_types <- all_visit_types %>% filter(Date >= "2019-06-01", Date <= "2019-08-31"  | # pretreat
                                                Date >= "2022-06-01", Date <= "2022-08-31") # post treat
# remove the NA, not our target pop
did_visit_types <- did_visit_types %>% filter(!is.na(marsh)) 

# mark post treatment (the indicator = 0 means its pretreat)
did_visit_types <- did_visit_types %>% mutate(after.treat = if_else(year == 2022, 1, 0))

# add column with total populations for year for M and NHW


# make an outcome column



# to try to compute it by hand...


# For a straightforward estimate of the ATT, we could simply plug in the sample averages for the four expectations on the right-hand side:
#   
# The post-intervention average of the treated group for E[Y(2)∣A=1];
# The pre-intervention average of the treated group for E[Y(1)∣A=1];
# The post-intervention average of the control group for E[Y(2)∣A=0];
# The pre-intervention average of the control group for E[Y(1)∣A=0].

# https://diff.healthpolicydatascience.org/



# we can do this for the small panel group...

# any service line

# Total Panel population numbers from DIDParllelTRends.R

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


pretest_yr <- panel_balance_any(start_date = "2018-08-31", end_date = "2019-08-31") # All service lines = total population

posttest_yr <- panel_balance_any(start_date = "2021-08-31", end_date = "2022-08-31") # All service lines = total population



# population size for pretest year Marshallese
pretest_yr$Marsh_count
# population size for pretest year Non-Hispanic White
pretest_yr$NHW_count

# population size for post test year Marshallese
posttest_yr$Marsh_count

# population size for post test year Non-Hispanic White
posttest_yr$NHW_count


length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh))
# 154 Marshallese got any services during both pre and post DID times

length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))
# 10405 NHW got any services during both  pre and post DID times

# # this is about 0.1813899 of the M
# 154/849
#   
# # and 0.4061914 of the NHW
# 10405/ 25616


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
              UniqueIDNHW = UniqueIDNHW, 
              Marsh_visits = nrow(marsh), 
              NHW_visits = nrow(NHW)))
}

# I run the two time period and compare 


pretest_PCP_qt <- panel_balance(start_date = "2019-06-01", end_date = "2019-08-31", Service = "Primary Care")
posttest_PCP_qt <- panel_balance(start_date = "2022-06-01", end_date = "2022-08-31", Service = "Primary Care")



pretest_ER_qt <- panel_balance(start_date = "2019-06-01", end_date = "2019-08-31", Service = "Emergency")
posttest_ER_qt <- panel_balance(start_date = "2022-06-01", end_date = "2022-08-31", Service = "Emergency")




################ CALCULATE ##################
########## PCP ########
# The post-intervention average of the treated group for E[Y(2)∣A=1];


post_tx_pcp <- posttest_PCP_qt$Marsh_visits/ length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh)) 
  # number of PCP visits in 2022 for Marshallese /Balanced total Marsh for that year
 
 194/ 154

#The pre-intervention average of the treated group for E[Y(1)∣A=1];
pre_tx_pcp <- pretest_PCP_qt$Marsh_visits /length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh)) 
    # number of PCP visits in 2019 for Marshallese /Balanced total Marsh for that year
89/154

# The post-intervention average of the control group for E[Y(2)∣A=0];
  # number of PCP visits in 2022 for NHW /Balanced total NHW for that year
  
post_ct_pcp <- posttest_PCP_qt$NHW_visits /length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))

  
12463/  10405  
# The pre-intervention average of the control group for E[Y(1)∣A=0].
  # number of PCP visits in 2019 for NHW /Balanced total NHW for that year
  
pre_ct_pcp <- pretest_PCP_qt$NHW_visits /   length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))

7327/10405


# Average Treatement Effect on the Treated 
# PCP

post_tx_pcp - pre_tx_pcp - post_ct_pcp + pre_ct_pcp
# 0.1882093
# a positive effect on those current patients PCP rates, 

#! is it significant?

# need to bootstrap confidence intervals or use equations!



################ CALCULATE ##################
##### ER Visit ###############

# The post-intervention average of the treated group for E[Y(2)∣A=1];


post_tx_ER <- posttest_ER_qt$Marsh_visits/ length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh)) 
# number of ER visits in 2022 for Marshallese /Balanced total Marsh for that year

73/ 154

#The pre-intervention average of the treated group for E[Y(1)∣A=1];
pre_tx_ER <- pretest_ER_qt$Marsh_visits /length(intersect(pretest_yr$UniqueIDMarsh, posttest_yr$UniqueIDMarsh)) 
# number of ER visits in 2019 for Marshallese /Balanced total Marsh for that year
58/154

# The post-intervention average of the control group for E[Y(2)∣A=0];
# number of ER visits in 2022 for NHW /Balanced total NHW for that year

post_ct_ER <- posttest_ER_qt$NHW_visits /length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))


4394/  10405  
# The pre-intervention average of the control group for E[Y(1)∣A=0].
# number of ER visits in 2019 for NHW /Balanced total NHW for that year

pre_ct_ER <- pretest_ER_qt$NHW_visits /   length(intersect(pretest_yr$UniqueIDNHW, posttest_yr$UniqueIDNHW))

3243/10405


# Average Treatement Effect on the Treated 
# ER

#(post_tx_ER - pre_tx_ER) - (post_ct_ER - pre_ct_ER)
post_tx_ER - pre_tx_ER - post_ct_ER + pre_ct_ER
# -0.0132173
# a negative effect on those current patients ER rates, 




#! is it significant?

# need to bootstrap confidence intervals or use packages below!





# Try this manually find averages to compare
# select PCP visits (or ER visits later)
did_PCP <- did_visit_types %>% filter(ServiceLine == "Primary Care")

# add total pop for each year, then mutate to create rate

# tbdata = tbdata %>% mutate(roomNo = case_when(
#   roomNo == "1" ~ 1,
#   roomNo == "2" ~ 2,
#   roomNo == "3+" ~ 3
# ))


# from beginning of script 
# population size for pretest year Marshallese

# population size for pretest year Non-Hispanic White


# population size for post test year Marshallese


# population size for post test year Non-Hispanic White


did_PCP <- did_PCP %>% mutate(pop = case_when(
  (year == 2019 & marsh == 1) ~ pretest_yr$Marsh_count,
  (year == 2019 & marsh == 0) ~ pretest_yr$NHW_count,
  (year == 2022 & marsh == 1) ~ posttest_yr$Marsh_count,
  (year == 2022 & marsh == 0) ~  posttest_yr$NHW_count))

did_PCP <- did_PCP %>% mutate(PCP_rate = 1/pop)

sum(did_PCP$PCP_rate[did_PCP$marsh == 1 & did_PCP$year == 2019])
# 0.4611399
# is this right?
did_PCP %>% filter(marsh == 1, year == 2019) %>% summarize(sum(PCP_rate))
# same  

# ! but pre_tx_pcp
# [1] 0.5779221




###
# try with lm per https://diff.healthpolicydatascience.org/#regression


# do we average before or after? The LM output should give us the expected value/average of the groups
# should give 3 coefficients for pre and post NHW and pre and post M,
# we want to know if the interaction term (the coef for postMarsh) if significant


# ! but we need to figure out correlation - if DID package won't work should we do coreelated geeglm??

did_visit_types <- did_visit_types %>% mutate(ER = if_else(ServiceLine == "Emergency", 1, 0), 
                           PCP = if_else(ServiceLine == "Primary Care", 1, 0))


attempt2 <- lm(ER ~ marsh*year  , data = did_visit_types)
summary(attempt2)
# all significant! 




# Call:
#   lm(formula = ER ~ marsh * year, data = did_visit_types)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.2348 -0.1762 -0.1417 -0.1417  0.9219 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 23.391612   2.255812  10.369  < 2e-16 ***
#   marsh       82.331210  17.495453   4.706 2.53e-06 ***
#   year        -0.011498   0.001116 -10.301  < 2e-16 ***
#   marsh:year  -0.040749   0.008655  -4.708 2.51e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3599 on 50600 degrees of freedom
# Multiple R-squared:  0.003156,	Adjusted R-squared:  0.003097 
# F-statistic: 53.39 on 3 and 50600 DF,  p-value: < 2.2e-16


attempt3 <- lm(PCP ~ marsh*year  , data = did_visit_types)
summary(attempt3)
# Marsh and Marsh:year are significantly different! 

# Call:
#   lm(formula = PCP ~ marsh * year, data = did_visit_types)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.4018 -0.4018 -0.3980  0.5981  0.7925 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2.171018   3.061855  -0.709    0.478    
# marsh       105.390815  23.746894   4.438 9.10e-06 ***
#   year          0.001272   0.001515   0.840    0.401    
# marsh:year   -0.052218   0.011748  -4.445 8.81e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4885 on 50600 degrees of freedom
# Multiple R-squared:  0.002862,	Adjusted R-squared:  0.002803 
# F-statistic: 48.41 on 3 and 50600 DF,  p-value: < 2.2e-16

#####################################################
# But can't do this with correlation of the unbalanced panel....
# we need the DID package for that I think? 
# see DIDModels.R ...






#  who was there in June, July and Aug 2019 (pre-treatment). Made function above once I got it to work :)
#
# test <- all_visit_types_quarter %>% filter(Date >= "2017-01-01", Date <= "2017-01-31")


# pretreat <- all_visit_types %>% filter( Date >= "2019-06-01", Date <= "2019-08-31" ) %>%
#   mutate(PreTreat = 1)
# 
# test <- pretreat %>% filter(marsh == 1, 
#                             ServiceLine == "Emergency") 
# nrow(test)
# # 58 Marshallese Visited ER in the pretreat quarter
# # !check this number against my yearmonth table to make sure I did it right
# 
# 
# 
# 
# 
# test <- pretreat %>% filter(marsh == 0, 
#                             ServiceLine == "Emergency") 
# nrow(test)
# # 3243 Control visited ER in the pretreat quarter
# 
# 
# # who was there in June, July and Aug 2022 (post-treatment)
# posttreat <- all_visit_types %>% filter( Date >= "2022-06-01", Date <= "2022-08-31" ) %>%
#   mutate(PostTreat = 1)
# 
# test <- posttreat %>% filter(marsh == 1, 
#                              ServiceLine == "Emergency") 
# # 73
# 
# test <- posttreat %>% filter(marsh == 0, 
#                              ServiceLine == "Emergency") 
# nrow(test)
# #4394
# # ! Are these unique?
# 
# 
# # set up rates for DID pretends and to estimate
# # We decided on # of Marshallese ER visits per quarter/ total Marshallese population per quarter
# # and  # of Marshallese ER visits per quarter/ total Marshallese population per year
# 
# # !
# # make a function that lets me control the population size
# # need to divide by Marsh and NHW
# 
# !


# old functions
# pop_size <- function(marsh0_or_1, year) {
#   x <- all_visit_types %>% filter(marsh == {{marsh0_or_1}}) %>% filter(year == {{year}}) 
#   z<- length(unique(x$UniqueIdentifier))
#   return(z)
# }
# 
# 
# pop_size_marsh <- c(pop_size(1, 2017), # marshallese in 2017
#                     pop_size(1, 2018),  # marshallese in 2018...
#                     pop_size(1, 2019),
#                     pop_size(1, 2020),
#                     pop_size(1, 2021),
#                     pop_size(1, 2022),
#                     pop_size(1, 2023),
#                     pop_size(1, 2024))
# 
# pop_size_white <- c(pop_size(0, 2017), # Non-Hisp white  in 2017
#                     pop_size(0, 2018),
#                     pop_size(0, 2019),
#                     pop_size(0, 2020),
#                     pop_size(0, 2021),
#                     pop_size(0, 2022),
#                     pop_size(0, 2023),
#                     pop_size(0, 2024))
# 
# pop_size_year <- rbind(pop_size_marsh , pop_size_white)
# colnames(pop_size_year) <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
# 
# 
# 
# # can add the option to go forward 1 year with lubridate 
# library(lubridate)
# pop_size <- function(marsh0_or_1, start_date) {
#   x <- all_visit_types %>% filter(marsh == {{marsh0_or_1}}) %>% filter(Date >= {{start_date}}, 
#                                                                        Date <= {{start_date + %m% years(1)}})  # this should have the filter be 1 year more than our start data according to stack overflow
#   z<- length(unique(x$UniqueIdentifier))
#   return(z)
# }
# 
# 
# 
# 
# 
