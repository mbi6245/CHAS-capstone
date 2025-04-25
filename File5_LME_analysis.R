######################################################################
################# UW/CHAS Capstone Analysis Project ##################
################### File 5: Longitudinal Analyses ####################
######### Runs longitudinal analysis for objectives 1 and 2 ##########
######################################################################

# load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(nlme)
library(gtsummary)
library(gt)
library(kableExtra)

###################################
# Impact of KOH on Blood Pressure #
###################################

# read in data
bp <- read.csv('Analysis Data/Obj1BP_LME.csv')[,-1]

# standardizing time as days from baseline measurement
bp.first <- bp %>% group_by(UniqueIdentifier) %>% arrange(BPDate, .by_group=TRUE) %>% slice_head()
bp.first$BPDate <- bp.first$KOH.start.dt
bp <- bp %>% filter(BPDate>KOH.start.dt)

bp <- rbind(bp.first, bp) %>% arrange(UniqueIdentifier, BPDate)
bp <- bp %>% mutate(time.from.koh1 = round(as.numeric(difftime(BPDate, KOH.start.dt, units="days"))))
bp <- bp %>% mutate(koh.cat = case_when(KOH.none == 1 ~ 'None',
                                        KOH.one == 1 ~ 'One',
                                        KOH.mult == 1 ~ 'Multiple'))
bp$koh.cat <- factor(bp$koh.cat, levels=c("None","One","Multiple"))
bp$male <- ifelse(bp$Sex == 'M', 1, 0)
bp$month.exact <- bp$time.from.koh1 / 30.4

# plot with all patients systolic blood pressure over time
ggplot(data= bp, 
       aes(x=month.exact, y=Systolic, group=UniqueIdentifier, colour=koh.cat))+
  geom_line() + 
  xlab("Time from first KOH meeting (months)") + ylab("Systolic Blood Pressure (mmHg)") + theme_bw() +
  guides(colour = guide_legend(title = "KOH Attendance"))

# plot of the mean systolic blood pressure per KOH attendance group over time
summary_bp <- bp %>% group_by(koh.cat, month.exact) %>%
  summarise(mean_bp = mean(Systolic))

ggplot(data=summary_bp, 
       aes(x=month.exact, y=mean_bp, group=koh.cat, colour=koh.cat))+ 
  geom_smooth(method=loess, se=FALSE, linewidth=0.85) + geom_line(alpha=0.3) +
  xlab("Time from first KOH meeting (months)") + ylab("Systolic Blood Pressure (mmHg)") + theme_bw() +
  guides(colour = guide_legend(title = "KOH Attendance")) + scale_x_continuous(breaks=seq(0,12, by=3)) + 
  scale_y_continuous(breaks=seq(65,215, by=25)) + # you can adjust the y-axis if data is being cut off - we narrowed it down to 65-215 to see small differences over time
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size=11))

# run linear mixed effects model
bp.mod1 <- lme(
  fixed = Systolic ~ month.exact * koh.cat + age + male + IncomeLevel + BLACERISK + avg.bmi,
  random = ~ month.exact | UniqueIdentifier,
  data = bp,
  method = "REML"
)

### calculate 12-month change in SBP estimates ###
fixed_effects <- summary(bp.mod1)$tTable

# KOH non-participants
month_effect <- fixed_effects["month.exact", "Value"]
month_se <- fixed_effects["month.exact", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m

# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")

month_effect <- fixed_effects["koh.catOne", "Value"]
month_se <- fixed_effects["koh.catOne", "Std.Error"]

month_effect <- fixed_effects["koh.catMultiple", "Value"]
month_se <- fixed_effects["koh.catMultiple", "Std.Error"]

month_effect <- fixed_effects["month.exact:koh.catOne", "Value"]
month_se <- fixed_effects["month.exact:koh.catOne", "Std.Error"]

month_effect <- fixed_effects["month.exact:koh.catMultiple", "Value"]
month_se <- fixed_effects["month.exact:koh.catMultiple", "Std.Error"]
