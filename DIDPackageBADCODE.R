## THIS SCRIPT WON'T RUN BUT I DON'T WANT TO LOSE MY CODE IN CASE WE CAN MAKE IT RUN LATER

# Trying to fit DID package data requirements.
# trying to fit DID pattern, but I don't think we need these for GEEGLM
# mark post treatment (the indicator = 0 means its pretreat)

did_visit_types <- did_visit_types %>% mutate(after.treat = if_else(year == 2022, 1, 0))


did_visit_types <- did_visit_types %>% mutate(first.treat = if_else(marsh == 1, 2019, 0))
# 
# 
# 
# fp_tarpop = file.path(getwd(), "Analysis Data/targetpop_DID.csv")
# targetpop_DID <- read.csv(fp_tarpop) 


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




# using unbalanced panel in the DID package



# to use DID package

# Best Guess for DID! 
# mw.attgt <- att_gt( yname = "ER", # outcome name
#                     idname = "UniqueIdentifier", # each observation
#                     # gname = , # group name, first.treat # we don't have groups varying over time like treated in 2003, 2004...
#                     tname = "year", # time name
#                     xformla = "marsh",
#                     #, # we will not condition on any other covariates, or leave blank
#                     # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                     data = did_visit_types)
# 
# 
# 
# mw.attgt <- att_gt(yname = "ER", # outcome name
#                    idname = "UniqueIdentifier", # each observation
#                    gname = "marsh", # group name, first.treat
#                    tname = "year", # time name
#                    xformla = ~1, 
#                    #, # we will not condition on any other covariates, or leave blank
#                    # allow_unbalanced_panel = TRUE, # test standard DID without unbalanced first
#                    data = did_visit_types)
# 


# Example from DID package
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


