# DID Examples from Ting's Causal Lecture and DID Package Documentation
library(did)
# https://cran.r-project.org/web/packages/did/vignettes/did-basics.html

data(mpdta)
# mpdta is a balanced panel with 2500 observations
head(mpdta)
colnames(mpdta)
# "year"        2003-2007
# "countyreal" county identifier
# "lpop"       log population
# "lemp"      log teen employment
# "first.treat" first treatment or subsequent treatment
# "treat"      treatment tx group 


# To really evaluate the effect of the minimum wage on teen employment, one would need to be more careful along a number of dimensions. Thus, results displayed here should be interpreted as illustrative only.
# # 
# Data Requirements
# 
# In particular applications, the dataset should look like this with the key parts being:
#   
# The dataset should be in long format – each row corresponds to a particular unit at a particular point in time. Sometimes panel data is in wide format – each row contains all the information about a particular unit in all time periods. To convert from wide to long in R, one can use the tidyr::pivot_longer function. Here is an example
# 
# There needs to be an id variable. In mpdta, it is the variable countyreal. This should not vary over time for particular units. The name of this variable is passed to methods in the did package by setting, for example, idname = "countyreal"
# 
# There needs to be a time variable. In mpdta, it is the variable year. The name of this variable is passed to methods in the did package by setting, for example, tname = "year"
# 
# In this application, the outcome is lemp. The name of this variable is passed to methods in the did package by setting, for example, yname = "lemp"
# 
# There needs to be a group variable. In mpdta, it is the variable first.treat. This is the time period when an individual first becomes treated. For individuals that are never treated, this variable should be set equal to 0. The name of this variable is passed to methods in the did package by setting, for example, gname = "first.treat"
# 
# The did package allows for incorporating covariates so that the parallel trends assumption holds only after conditioning on these covariates. In mpdta, lpop is the log of county population. 

# The did package requires that covariates be time-invariant. For time varying covariates, the did package sets the value of the covariate to be equal to the value of the covariate in the “base period” where, in post-treatment periods the base period is the period immediately before observations in a particular group become treated (when there is anticipation, it is before anticipation effects start too), and in pre-treatment periods the base period is the period right before the current period. Covariates are passed as a formula to the did package by setting, for example, xformla = ~lpop. For estimators under unconditional parallel trends, the xformla argument can be left blank or can be set as xformla = ~1 to only include a constant.

# estimate group-time average treatment effects without covariates


# 3 different treatment times
mw.attgt <- att_gt(yname = "lemp",
                   gname = "first.treat",
                   idname = "countyreal",
                   tname = "year",
                   xformla = ~1,
                   data = mpdta,
)

# summarize the results
summary(mw.attgt)
#> 
#> Call:
#> att_gt(yname = "lemp", tname = "year", idname = "countyreal", 
#>     gname = "first.treat", xformla = ~1, data = mpdta)
#> 
#> Reference: Callaway, Brantly and Pedro H.C. Sant'Anna.  "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230, 2021. <https://doi.org/10.1016/j.jeconom.2020.12.001>, <https://arxiv.org/abs/1803.09015> 
#> 
#> Group-Time Average Treatment Effects:
#>  Group Time ATT(g,t) Std. Error [95% Simult.  Conf. Band]  
#>   2004 2004  -0.0105     0.0248       -0.0777      0.0567  
#>   2004 2005  -0.0704     0.0319       -0.1566      0.0158  
#>   2004 2006  -0.1373     0.0399       -0.2453     -0.0292 *
#>   2004 2007  -0.1008     0.0365       -0.1995     -0.0021 *
#>   2006 2004   0.0065     0.0225       -0.0543      0.0674  
#>   2006 2005  -0.0028     0.0203       -0.0577      0.0522  
#>   2006 2006  -0.0046     0.0179       -0.0529      0.0437  
#>   2006 2007  -0.0412     0.0202       -0.0959      0.0135  
#>   2007 2004   0.0305     0.0156       -0.0117      0.0727  
#>   2007 2005  -0.0027     0.0160       -0.0460      0.0405  
#>   2007 2006  -0.0311     0.0177       -0.0789      0.0167  
#>   2007 2007  -0.0261     0.0172       -0.0725      0.0204  
#> ---
#> Signif. codes: `*' confidence band does not cover 0
#> 
#> P-value for pre-test of parallel trends assumption:  0.16812
#> Control Group:  Never Treated,  Anticipation Periods:  0
#> Estimation Method:  Doubly Robust

# plot the results
# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt, ylim = c(-.3,.3))


# There are a few things to notice in this case
# 
# There does not appear to be much evidence against the parallel trends assumption. One fails to reject using the Wald test reported in summary; likewise the uniform confidence bands cover 0 in all pre-treatment periods.
# 
# There is some evidence of negative effects of the minimum wage on employment. Two group-time average treatment effects are negative and statistically different from 0. These results also suggest that it may be helpful to aggregate the group-time average treatment effects.


mw.attgt.X <- att_gt(yname = "lemp",
                     gname = "first.treat",
                     idname = "countyreal",
                     tname = "year",
                     xformla = ~lpop, # covariates lpop
                     data = mpdta,
)

summary(mw.attgt.X)
# even worse p-value when include covariates X


######################################## 
# from Ting's slides

# Tin'gs slides call for a subset of mpdta, but I don't know which subset 
# from what is needed I assume it is 2003 and 2004, and this worked to get the same results
mpdta.sub <- mpdta %>% filter(year >= 2003, year <=2004)


mpdta.sub <- mpdta.sub %>% mutate(after.treat = 1*(year >= first.treat))
# hand-coded DID
mean(with(mpdta.sub, lemp[first.treat == 2004 & year == 2003])) - 
  mean(with(mpdta.sub, lemp[first.treat == 2004 & year == 2004])) - 
  mean(with(mpdta.sub, lemp[first.treat == 0 & year == 2004]))+
  mean(with(mpdta.sub, lemp[first.treat == 0 & year == 2003]))


# [1] 0.1357633 
# ! supposed to be -0.0105...

mean(with(dat, outcome[pretreat_tx_group])) - 
  mean(with(dat, outcome[posttreat_tx_group])) - 
  mean(with(dat, outcome[posttreat_ct_group]))+
  mean(with(dat, outcome[pretreat_ct_group]))




#TWFE version (Two way Fixed Effects Model)
twfe_sub <- lm(lemp ~ first.treat + after.treat, data = mpdta.sub)
twfe_sub

# cluster-robust variance estimator with CR2 small-sample correction 
library(clubSandwich)
coeftest.twfe <- coef_test(twfe_sub, 
                           vcov = "CR2", 
                           cluster = mpdta.sub$countyreal)
coeftest.twfe
# > coeftest.twfe
# Coef. Estimate       SE t-stat d.f. (Satt) p-val (Satt) Sig.
# (Intercept) 5.505542 1.29e-01  42.63         215      < 0.001  ***
#   first.treat 0.000229 7.57e-05   3.02         192      0.00286   **
#   after.treat 0.124789 9.71e-02   1.28         170      0.20061     

# non-significant p-value

coeftest.twfe[3, ] # to get after treatment effect
# estimate should be the same as above



########################################
# Our project 

# unbalanced panels? 
# allow_unbalanced_panel = TRUE, 

#  Repeated cross sections
# panel = FALSE

# Dynamic Effects and Event Studies


