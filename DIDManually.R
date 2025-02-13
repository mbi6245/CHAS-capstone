# Run DID package

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


pretest_yr <- panel_balance_any(start_date = "2018-08-31", end_date = "2019-08-31") # , Service = .

posttest_yr <- panel_balance_any(start_date = "2021-08-31", end_date = "2022-08-31") # , Service = .


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

# need to boostrap confidence intervals or use equations



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

# need to boostrap confidence intervals or use packages below




# try with lm per https://diff.healthpolicydatascience.org/#regression

# ER_df <- as.data.frame(rbind(c(pre_tx_ER, post_tx_ER, 1),
#   c(pre_ct_ER, post_ct_ER, 0) ))
# colnames(ER_df) <- c("pre", "post", "Marsh")
# 
# ER_lm <- lm(post ~ pre*Marsh, data = ER_df)
# summary(ER_lm)
# 
# Call:
# lm(formula = post ~ pre * Marsh, data = ER_df)
# 
# Residuals:
#   ALL 2 residuals are 0: no residual degrees of freedom!
#   
#   Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1740        NaN     NaN      NaN
# pre           0.7965        NaN     NaN      NaN
# Marsh             NA         NA      NA       NA
# pre:Marsh         NA         NA      NA       NA
# 
# Residual standard error: NaN on 0 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:    NaN 
# F-statistic:   NaN on 1 and 0 DF,  p-value: NA


# Try this! 
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




# do we average before or after? 
# stopped here! 

PCP_lm <- lm(PCP_rate ~ marsh*after.treat, data = did_PCP)
summary(PCP_lm)
# should give 3 coefficients for pre and post NHW and pre and post M,
# we want to know if the interaction term (the coef for postMarsh) if significant

###

# Trying something different

did_visit_types <- did_visit_types %>% mutate(ER = if_else(ServiceLine == "Emergency", 1, 0), 
                           PCP = if_else(ServiceLine == "Primary Care", 1, 0))


attempt2 <- lm(ER ~ marsh*year  , data = did_visit_types)
summary(attempt2)


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



#####################################################
# can't do this with correlation of the unbalanced panel....
# we need the DID package



# to use DID package


mw.attgt <- att_gt( yname = "ER", # outcome name
                    idname = "UniqueIdentifier", # each observation
                   # gname = , # group name, first.treat # we don't have groups varying over time like treated in 2003, 2004...
                   tname = "year", # time name
                   xformla = "marsh",
                     #, # we will not condition on any other covariates, or leave blank
                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
                   data = did_visit_types)



mw.attgt <- att_gt(yname = "ER", # outcome name
                  idname = "UniqueIdentifier", # each observation
                   gname = "marsh", # group name, first.treat
                   tname = "year", # time name
                   xformla = ~1, 
                     #, # we will not condition on any other covariates, or leave blank
                  # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
                  data = did_visit_types)

# from DID package
# estimate group-time average treatment effects on the treated without covariates

# att_gt
# Group-Time Average Treatment Effects
# Description
# att_gt computes average treatment effects in DID setups where there are more than two periods of
# data and allowing for treatment to occur at different points in time and allowing for treatment effect
# heterogeneity and dynamics. See Callaway and Sant’Anna (2021) for a detailed description

# We don't want this one because we don't have groups varying over time 
mw.attgt <- att_gt(yname = "lemp", # the outcome is the log employment rate per county
                   gname = "first.treat", # year the group was first treated, from 2003-2007, 0 for untreated
                   idname = "countyreal", # unique identifier for county
                   tname = "year", # the column with time
                   xformla = ~1, # no additional covariates in the model
                   data = mpdta)



did_visit_types <- did_visit_types %>% mutate(first.treat = if_else(marsh == 1, 2019, 0))


# doesn't work with forcing a group name
# mw.attgt <- att_gt( yname = "ER", # outcome name/ each row has 1 ER visit per patient in the time periods of interest
#                     idname = "UniqueIdentifier", # each patient/observation has its own unique ID
#                     gname = "first.treat", # group name, first.treat it is the same as ever treated/Marshallese = 2019
#                     tname = "year", # time name
#                     xformla = ~ marsh, 
#                     #, # we will not condition on any other covariates, or leave blank
#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                     data = did_visit_types)
# # Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
# #                            No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).
# #                          In addition: Warning messages:
# #                            1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
# #                                                    Dropped 315 units that were already treated in the first period.
# #                                                  2: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
# #                                                                          Dropped 10949 observations while converting to balanced panel.


# doesn't work without a group name.
# 
# mw.attgt <- att_gt( yname = "ER", # outcome name
#                     +                     idname = "UniqueIdentifier", # each observation
#                     +                    # gname = "first.treat", # group name, first.treat it is the same as ever treated/marsh = 2019
#                       +                     tname = "year", # time name
#                     +                     xformla = ~ marsh, 
#                     +                     #, # we will not condition on any other covariates, or leave blank
#                       +                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                       +                     data = did_visit_types)
# Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
#                            data[, gname] must be numeric

# mw.attgt <- att_gt( yname = "ER", # outcome name/ each row has 1 ER visit per patient in the time periods of interest 
                          # or 0 if the visit was another type of visit

#                     idname = "UniqueIdentifier", # each patient/observation has its own unique ID

#                     gname = "first.treat", # group name, first.treat it is the same as ever treated/Marshallese = 2019

#                     tname = "year", # Year 2019 (pretreatment) or 2022 (post treatment)

#                     xformla = ~ 1, # we will not condition on any other covariates, so leave blank

#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first

#                     data = did_visit_types)
# 
# Error in pre_process_did(yname = yname, tname = tname, idname = idname,  : 
#                            No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).
#                          In addition: Warning messages:
#                            1: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                                                    Dropped 315 units that were already treated in the first period.
#                                                  2: In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                                                                          Dropped 10949 observations while converting to balanced panel.