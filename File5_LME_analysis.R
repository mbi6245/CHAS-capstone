######################################################################
################# UW/CHAS Capstone Analysis Project ##################
################### File 5: Longitudinal Analyses ####################
######### Runs longitudinal analysis for objectives 1 and 2 ##########
######################################################################

rm(list=ls())
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

#### calculate 12-month change in SBP estimates ####
fixed_effects <- summary(bp.mod1)$tTable

### KOH non-participants ###
month_effect <- fixed_effects["month.exact", "Value"]
month_se <- fixed_effects["month.exact", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")


### KOH one time participants ###
month_effect <- fixed_effects["koh.catOne", "Value"]
month_se <- fixed_effects["koh.catOne", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")

### KOH multiple meeting participants ###
month_effect <- fixed_effects["koh.catMultiple", "Value"]
month_se <- fixed_effects["koh.catMultiple", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")

### effect of KOH on one time participants (aka difference in rate of change of SBP over time between no KOH and one KOH meeting attended) ###
month_effect <- fixed_effects["month.exact:koh.catOne", "Value"]
month_se <- fixed_effects["month.exact:koh.catOne", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")

### effect of KOH on multiple meeting participants (aka difference in rate of change of SBP over time between no KOH and multiple KOH meeting attended) ###
month_effect <- fixed_effects["month.exact:koh.catMultiple", "Value"]
month_se <- fixed_effects["month.exact:koh.catMultiple", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (", ci_lower, ",", ci_upper, ")\n")

###################################
###### Impact of KOH on A1c ######
###################################

# read in data
a1c <- read.csv('Analysis Data/Obj1A1c_LME.csv')[,-1]

# standardizing time as days from baseline measurement
a1c.first <- a1c %>% group_by(UniqueIdentifier) %>% arrange(A1cDate, .by_group=TRUE) %>% slice_head()
a1c.first$A1cDate <- a1c.first$KOH.start.dt
a1c <- a1c %>% filter(A1cDate>KOH.start.dt)
a1c <- rbind(a1c.first, a1c) %>% arrange(UniqueIdentifier, A1cDate)
a1c <- a1c %>% mutate(time.from.koh1 = round(as.numeric(difftime(A1cDate, KOH.start.dt, units="days"))))
a1c <- a1c %>% mutate(koh.cat = case_when(KOH.none == 1 ~ 'None',
                                          KOH.one == 1 ~ 'One',
                                          KOH.mult == 1 ~ 'Multiple'))
a1c$koh.cat <- factor(a1c$koh.cat, levels=c("None","One","Multiple"))
a1c$male <- ifelse(a1c$Sex == 'M', 1, 0)
a1c$month.exact <- a1c$time.from.koh1 / 30.4

# plot with all patients A1c over time
ggplot(data= a1c, 
       aes(x=month.exact, y=A1c, group=UniqueIdentifier, colour=koh.cat))+
  geom_line() + 
  xlab("Time from first KOH meeting (months)") + ylab("A1c (%)") + theme_bw() +
  guides(colour = guide_legend(title = "KOH Attendance"))

# plot of the mean systolic blood pressure per KOH attendance group over time
summary_a1c <- a1c %>% group_by(koh.cat, month.exact) %>%
  summarise(mean_a1c = mean(A1c))

ggplot(data=summary_a1c, 
       aes(x=month.exact, y=mean_a1c, group=koh.cat, colour=koh.cat))+
  geom_smooth(method=loess, se=FALSE, linewidth=0.85) + geom_line(alpha=0.3) +
  xlab("Time from first KOH meeting (months)") + ylab("A1c (%)") + theme_bw() +
  guides(colour = guide_legend(title = "KOH Attendance")) + scale_x_continuous(breaks=seq(0,12, by=3)) + 
  scale_y_continuous(breaks=seq(5,15, by=2)) + # you can adjust the y-axis if data is being cut off - we narrowed it down to 5-15 to see small differences over time
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size=11))

# run linear mixed effects model
a1c.mod1 <- lme(
  fixed = A1c ~ month.exact * koh.cat + age + male + IncomeLevel + BLACERISK + avg.bmi,
  random = ~ month.exact | UniqueIdentifier,
  data = a1c,
  method = "REML"
)

#### calculate 12-month change in A1c estimates ####
fixed_effects <- summary(a1c.mod1)$tTable

### KOH non-participants ###
month_effect <- fixed_effects["month.exact", "Value"]
month_se <- fixed_effects["month.exact", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (",ci_lower,",", ci_upper,")\n")

### KOH one time participants ###
month_effect <- fixed_effects["koh.catOne", "Value"]
month_se <- fixed_effects["koh.catOne", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (",ci_lower,",", ci_upper,")\n")

### KOH multiple meeting participants ###
month_effect <- fixed_effects["koh.catMultiple", "Value"]
month_se <- fixed_effects["koh.catMultiple", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (",ci_lower,",", ci_upper,")\n")

### effect of KOH on one time participants (aka difference in rate of change of A1c over time between no KOH and one KOH meeting attended) ###
month_effect <- fixed_effects["month.exact:koh.catOne", "Value"]
month_se <- fixed_effects["month.exact:koh.catOne", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (",ci_lower,",", ci_upper,")\n")

### effect of KOH on multiple meeting participants (aka difference in rate of change of SBP over time between no KOH and multiple KOH meeting attended) ###
month_effect <- fixed_effects["month.exact:koh.catMultiple", "Value"]
month_se <- fixed_effects["month.exact:koh.catMultiple", "Std.Error"]
estimate_12m <- 12 * month_effect
se_12m <- 12 * month_se
ci_lower <- estimate_12m - 1.96 * se_12m
ci_upper <- estimate_12m + 1.96 * se_12m
# Display 12-month estimate and 95% CI
cat("12-month estimate:", estimate_12m, "\n")
cat("95% CI: (",ci_lower,",", ci_upper,")\n")

###########################################
# obj 1 regression result table
###########################################
options(digits=3)
coef_names = c("month.exact", "koh.catOne", "koh.catMultiple", "month.exact:koh.catOne", "month.exact:koh.catMultiple",
               "age", "male", "IncomeLevel", "BLACERISK", "avg.bmi")
col_names = c("est.", "lower", "upper")

a1c.ci1 <- intervals(a1c.mod1)$fixed[coef_names, col_names]
a1c.ci1 <- data.frame(a1c.ci1) %>%
  mutate(`95% CI` = paste0("(", round(lower, 3), ", ", round(upper, 3), ")"),
         est. = round(est., 3)) %>%
  select(-c(lower, upper))
colnames(a1c.ci1) <- c("estimate","95% CI")
a1c.p1 <- data.frame(summary(a1c.mod1)$tTable[coef_names, "p-value"])
colnames(a1c.p1) <- c("p-value")
a1c.p1 <- a1c.p1 %>% mutate(`p-value` = round(`p-value`, 3))

bp.ci1 <- intervals(bp.mod1)$fixed[coef_names, col_names]
bp.ci1 <- data.frame(bp.ci1) %>%
  mutate(`95% CI` = paste0("(", round(lower, 3), ", ", round(upper, 3), ")"),
         est. = round(est., 3)) %>%
  select(-c(lower, upper))
colnames(bp.ci1) <- c("estimate","95% CI")
bp.p1 <- data.frame(summary(bp.mod1)$tTable[coef_names, "p-value"])
colnames(bp.p1) <- c("p-value")
bp.p1 <- bp.p1 %>% mutate(`p-value` = round(`p-value`, 3))
obj1_res_a1c = data.frame(cbind(a1c.ci1, a1c.p1))
obj1_res_bp = data.frame(cbind(bp.ci1, bp.p1))
rownames(obj1_res_a1c) = c("Months ", "KOHOnce ", "KOHMultiple ", "Months:KOHOnce ", "Months:KOHMultiple ", "Age ", "Male ", "Income ", "Risk Score ", "BMI ")
rownames(obj1_res_bp) = c("Months ", "KOHOnce ", "KOHMultiple ", "Months:KOHOnce ", "Months:KOHMultiple ", "Age ", "Male ", "Income ", "Risk Score ", "BMI ")
colnames(obj1_res_a1c) = c("estimate ", "95% CI", "p-value")
colnames(obj1_res_bp) = c("estimate ", "95% CI", "p-value")

# Display the table (A1c)
kable(obj1_res_a1c, align = c("c", "c", "c", "c")) %>%
  pack_rows("A1c Longitudinal", 1, 10) %>%
  column_spec(2, width = "3cm") %>%  # Adjust width for estimate column
  column_spec(3, width = "3cm")
# Display the table (SBP)
kable(obj1_res_bp, align = c("c", "c", "c", "c")) %>%
  pack_rows("SBP Longitudinal", 1, 10) %>%
  column_spec(2, width = "3cm") %>%  # Adjust width for estimate column
  column_spec(3, width = "3cm")


