library(dplyr)
library(tidyverse)
library(gtsummary)
library(lubridate)
library(naniar)
library(flextable)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_gdp = file.path(getwd(), "general_data_prep.R")
source(fp_gdp)

# get pts with htn
koh.htn <- koh.table1 %>% filter(HTN == 1)
# isolate the patient ids
koh.htn.patlist <- koh.htn %>% select("UniqueIdentifier")
# select bp measures for only kohn htn pts
koh.bp <- left_join(koh.htn.patlist, bp.nona.18, by = "UniqueIdentifier") %>% select(-age)

# count number of bp readings per patient
koh.bp.counts <- koh.bp %>% group_by(UniqueIdentifier) %>% count(name = "bp.counts")
# remove any patients that only have 1 reading - i will filter out any that don't have reading before and after their KOH meeting but for now, we at least know they need to have at least 2 bp readings
koh.htn.elig.patlist <- koh.bp.counts %>% filter(bp.counts > 1) %>% select("UniqueIdentifier")
# now that we have the "eligible patients" we can subset the BP data for these patients
koh.bp.elig <- left_join(koh.htn.elig.patlist, koh.bp, by = "UniqueIdentifier")


# getting the date of first visit attended
koh.mtg1 <- koh.attend.18 %>% group_by(UniqueIdentifier) %>% arrange(Date, .by_group=TRUE) %>% slice_head() %>% select(c("UniqueIdentifier", "Date")) %>% rename(KOHDate = Date)

# merging with eligble bp table to find "pre" BP reading closest to first KOH meeting
koh.mtg1.bp <- right_join(koh.mtg1, koh.bp.elig, by="UniqueIdentifier") %>% 
  rename(BPDate = Date) %>%
  mutate(datediff = as.numeric(difftime(BPDate, KOHDate, units="days")))

# these are all the "pre" KOH BP measurements
koh.bp.pre <- koh.mtg1.bp %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, BPDate.pre = BPDate, sys.pre = Systolic, dia.pre = Diastolic)

# now finding the most recent BP measure for "post" KOH
koh.bp.post <- koh.mtg1.bp %>% filter(datediff>0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, BPDate.post = BPDate, sys.post = Systolic, dia.post = Diastolic)

# now merging together the pre and post datasets to find patients that have both timepoints
koh.bp.pre.post <- inner_join(koh.bp.pre, koh.bp.post, by="UniqueIdentifier")

marsh.non.htn <- table1 %>% filter(Marsh == 1 & KOHParticipant == 0 & HTN == 1)
# isolate the patient ids
marsh.non.htn.patlist <- marsh.non.htn %>% select("UniqueIdentifier")
# select bp measures for only non koh htn pts
marsh.non.bp <- left_join(marsh.non.htn.patlist, bp.nona.18, by = "UniqueIdentifier") %>% select(-age)

# count number of bp readings per patient
marsh.non.bp.counts <- marsh.non.bp %>% group_by(UniqueIdentifier) %>% count(name = "bp.counts")
# remove any patients that only have 1 reading - i will filter out any that don't have reading before and after first KOH meeting but for now, we at least know they need to have at least 2 bp readings
marsh.non.htn.elig.patlist <- marsh.non.bp.counts %>% filter(bp.counts > 1) %>% select("UniqueIdentifier")
# now that we have the "eligible patients" we can subset the BP data for these patients
marsh.non.bp.elig <- left_join(marsh.non.htn.elig.patlist, marsh.non.bp, by = "UniqueIdentifier")

# the earliest KOH meeting in the koh subsets is 4/5/2023, so we will use that as our starting point to find "pre" readings
marsh.non.bp.elig$KOHDate <- as.Date('2023-04-05')
# find "pre" BP reading closest to first KOH meeting
marsh.non.bp.elig <- marsh.non.bp.elig %>% rename(BPDate = Date) %>%
  mutate(datediff = as.numeric(difftime(BPDate, KOHDate, units="days")))

# these are all the "pre" KOH BP measurements
marsh.non.bp.pre <- marsh.non.bp.elig %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, BPDate.pre = BPDate, sys.pre = Systolic, dia.pre = Diastolic)

# now finding the most recent BP measure for "post" KOH
marsh.non.bp.post <- marsh.non.bp.elig %>% filter(datediff>0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, BPDate.post = BPDate, sys.post = Systolic, dia.post = Diastolic)

# now merging together the pre and post datasets to find patients that have both timepoints
marsh.non.bp.pre.post <- inner_join(marsh.non.bp.pre, marsh.non.bp.post, by="UniqueIdentifier")

# just looking at the pre and post means between groups
mean(marsh.non.bp.pre.post$sys.pre)
mean(marsh.non.bp.pre.post$sys.post)
mean(koh.bp.pre.post$sys.pre)
mean(koh.bp.pre.post$sys.post)

marsh.non.bp.pre.post <- marsh.non.bp.pre.post %>% select(-c(KOH.start.dt.x, KOH.start.dt.y))
marsh.non.bp.pre.post_long <- marsh.non.bp.pre.post %>%
  pivot_longer(
    cols = c(BPDate.pre, sys.pre, dia.pre, BPDate.post, sys.post, dia.post),
    names_to = c(".value", "TimePoint"),
    names_sep = "\\."
  ) %>%
  select(UniqueIdentifier, BPDate, sys, dia)

koh.bp.pre.post <- koh.bp.pre.post %>% select(-c(KOH.start.dt.x, KOH.start.dt.y))
koh.bp.pre.post_long <- koh.bp.pre.post %>%
  pivot_longer(
    cols = c(BPDate.pre, sys.pre, dia.pre, BPDate.post, sys.post, dia.post),
    names_to = c(".value", "TimePoint"),
    names_sep = "\\."
  ) %>%
  select(UniqueIdentifier, BPDate, sys, dia)
koh.bp.pre.post_long$KOH <- 1
obj1.bp.pre.post <- bind_rows(koh.bp.pre.post_long, marsh.non.bp.pre.post_long)
obj1.bp.pre.post$KOH[is.na(obj1.bp.pre.post$KOH)] <- 0
obj1.bp.pre.post <- left_join(obj1.bp.pre.post, koh.counts, by = "UniqueIdentifier")
obj1.bp.pre.post$koh.counts[is.na(obj1.bp.pre.post$koh.counts)] <- 0
# discretizing koh attendance
obj1.bp.pre.post <- obj1.bp.pre.post %>% mutate(KOH.none = ifelse(koh.counts == 0, 1, 0),
                                                KOH.one = ifelse(koh.counts == 1, 1, 0),
                                                KOH.mult = ifelse(koh.counts > 1, 1, 0))

write.csv(obj1.bp.pre.post, 'Analysis Data/Obj1BPPrePost.csv')

# these are all the "pre" KOH BP measurements
koh.bp.pre.all <- koh.mtg1.bp %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)

# now finding all BP measures for "post" KOH
koh.bp.post.all <- koh.mtg1.bp %>% filter(datediff>0) %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)
# combining baseline and subsequent measurements
koh.bp.all <- bind_rows(koh.bp.pre.all, koh.bp.post.all)
# making sure final list has both a pre and post timepoint
koh.bp.pre.post.pt <- koh.bp.pre.post %>% select(UniqueIdentifier)
koh.bp.all.elig <- left_join(koh.bp.pre.post.pt, koh.bp.all, by = "UniqueIdentifier")
koh.bp.all.elig$KOH <- 1

# these are all the "pre" KOH BP measurements
marsh.non.bp.pre.all <- marsh.non.bp.elig %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)

# now finding all BP measures for "post" KOH
marsh.non.bp.post.all <- marsh.non.bp.elig %>% filter(datediff>0) %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)
# combining baseline and subsequent measurements
marsh.non.bp.all <- bind_rows(marsh.non.bp.pre.all, marsh.non.bp.post.all)
# making sure final list has both a pre and post timepoint
marsh.non.bp.pre.post.pt <- marsh.non.bp.pre.post %>% select(UniqueIdentifier)
marsh.non.bp.all.elig <- left_join(marsh.non.bp.pre.post.pt, marsh.non.bp.all, by = "UniqueIdentifier")
marsh.non.bp.all.elig$KOH <- 0

# combining koh with non-koh
obj1.bp.lme <- bind_rows(koh.bp.all.elig, marsh.non.bp.all.elig)
obj1.bp.lme <- left_join(obj1.bp.lme, koh.counts, by = "UniqueIdentifier")
obj1.bp.lme$koh.counts[is.na(obj1.bp.lme$koh.counts)] <- 0
# discretizing koh attendance
obj1.bp.lme <- obj1.bp.lme %>% mutate(KOH.none = ifelse(koh.counts == 0, 1, 0),
                                      KOH.one = ifelse(koh.counts == 1, 1, 0),
                                      KOH.mult = ifelse(koh.counts > 1, 1, 0))

write.csv(obj1.bp.lme, 'Analysis Data/Obj1BP_LME.csv')

# get pts with diabetes
koh.t2dm <- koh.table1 %>% filter(Diabetes == 1)
# isolate the patient ids
koh.t2dm.patlist <- koh.t2dm %>% select("UniqueIdentifier")
# select a1c measures for only koh t2dm pts
koh.a1c <- left_join(koh.t2dm.patlist, a1c.nona.18, by = "UniqueIdentifier") %>% select(-age)

# count number of a1c readings per patient
koh.a1c.counts <- koh.a1c %>% group_by(UniqueIdentifier) %>% count(name = "a1c.counts")
# remove any patients that only have 1 reading - i will filter out any that don't have reading before and after their KOH meeting but for now, we at least know they need to have at least 2 a1c readings
koh.t2dm.elig.patlist <- koh.a1c.counts %>% filter(a1c.counts > 1) %>% select("UniqueIdentifier")
# now that we have the "eligible patients" we can subset the a1c data for these patients
koh.a1c.elig <- left_join(koh.t2dm.elig.patlist, koh.a1c, by = "UniqueIdentifier")


# find "pre" A1c reading closest to first KOH meeting
koh.mtg1.a1c <- right_join(koh.mtg1, koh.a1c.elig, by="UniqueIdentifier") %>% 
  rename(A1cDate = Date) %>%
  mutate(datediff = as.numeric(difftime(A1cDate, KOHDate, units="days")))

# these are all the "pre" KOH A1c measurements
koh.a1c.pre <- koh.mtg1.a1c %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, A1cDate.pre = A1cDate, A1c.pre = A1c)

# now finding the most recent A1c measure for "post" KOH
koh.a1c.post <- koh.mtg1.a1c %>% filter(datediff>0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, A1cDate.post = A1cDate, A1c.post = A1c)

# now merging together the pre and post datasets to find patients that have both timepoints
koh.a1c.pre.post <- inner_join(koh.a1c.pre, koh.a1c.post, by="UniqueIdentifier")

marsh.non.dia <- table1 %>% filter(Marsh == 1 & KOHParticipant == 0 & Diabetes == 1)
# isolate the patient ids
marsh.non.dia.patlist <- marsh.non.dia %>% select("UniqueIdentifier")
# select bp measures for only non koh diabetic pts
marsh.non.a1c <- left_join(marsh.non.dia.patlist, a1c.nona.18, by = "UniqueIdentifier") %>% select(-age)

# count number of a1c readings per patient
marsh.non.a1c.counts <- marsh.non.a1c %>% group_by(UniqueIdentifier) %>% count(name = "a1c.counts")
# remove any patients that only have 1 reading - i will filter out any that don't have reading before and after first KOH meeting but for now, we at least know they need to have at least 2 a1c readings
marsh.non.dia.elig.patlist <- marsh.non.a1c.counts %>% filter(a1c.counts > 1) %>% select("UniqueIdentifier")
# now that we have the "eligible patients" we can subset the A1c data for these patients
marsh.non.a1c.elig <- left_join(marsh.non.dia.elig.patlist, marsh.non.a1c, by = "UniqueIdentifier")

# the earliest KOH meeting in the koh subsets is 4/5/2023, so we will use that as our starting point to find "pre" readings
marsh.non.a1c.elig$KOHDate <- as.Date('2023-04-05')
# find "pre" A1c reading closest to first KOH meeting
marsh.non.a1c.elig <- marsh.non.a1c.elig %>% rename(A1cDate = Date) %>%
  mutate(datediff = as.numeric(difftime(A1cDate, KOHDate, units="days")))

# these are all the "pre" KOH A1c measurements
marsh.non.a1c.pre <- marsh.non.a1c.elig %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, A1cDate.pre = A1cDate, A1c.pre = A1c)

# now finding the most recent A1c measure for "post" KOH
marsh.non.a1c.post <- marsh.non.a1c.elig %>% filter(datediff>0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate, A1cDate.post = A1cDate, A1c.post = A1c)

# now merging together the pre and post datasets to find patients that have both timepoints
marsh.non.a1c.pre.post <- inner_join(marsh.non.a1c.pre, marsh.non.a1c.post, by="UniqueIdentifier")

# just looking at the pre and post means between groups
mean(marsh.non.a1c.pre.post$A1c.pre)
mean(marsh.non.a1c.pre.post$A1c.post)
mean(koh.a1c.pre.post$A1c.pre)
mean(koh.a1c.pre.post$A1c.post)

# PRIMARY OBJECTIVE A1C PRE-POST DATASET
marsh.non.a1c.pre.post <- marsh.non.a1c.pre.post %>% select(-c(KOH.start.dt.x, KOH.start.dt.y))
marsh.non.a1c.pre.post_long <- marsh.non.a1c.pre.post %>%
  pivot_longer(
    cols = c(A1cDate.pre, A1c.pre, A1cDate.post, A1c.post),
    names_to = c(".value", "TimePoint"),
    names_sep = "\\."
  ) %>%
  select(UniqueIdentifier, A1cDate, A1c)

koh.a1c.pre.post <- koh.a1c.pre.post %>% select(-c(KOH.start.dt.x, KOH.start.dt.y))
koh.a1c.pre.post_long <- koh.a1c.pre.post %>%
  pivot_longer(
    cols = c(A1cDate.pre, A1c.pre, A1cDate.post, A1c.post),
    names_to = c(".value", "TimePoint"),
    names_sep = "\\."
  ) %>%
  select(UniqueIdentifier, A1cDate, A1c)
koh.a1c.pre.post_long$KOH <- 1
obj1.a1c.pre.post <- bind_rows(koh.a1c.pre.post_long, marsh.non.a1c.pre.post_long)
obj1.a1c.pre.post$KOH[is.na(obj1.a1c.pre.post$KOH)] <- 0
obj1.a1c.pre.post <- left_join(obj1.a1c.pre.post, koh.counts, by = "UniqueIdentifier")
obj1.a1c.pre.post$koh.counts[is.na(obj1.a1c.pre.post$koh.counts)] <- 0
# discretizing koh attendance
obj1.a1c.pre.post <- obj1.a1c.pre.post %>% mutate(KOH.none = ifelse(koh.counts == 0, 1, 0),
                                                  KOH.one = ifelse(koh.counts == 1, 1, 0),
                                                  KOH.mult = ifelse(koh.counts > 1, 1, 0))

write.csv(obj1.a1c.pre.post, 'Analysis Data/Obj1A1cPrePost.csv')

# these are all the "pre" KOH A1c measurements
koh.a1c.pre.all <- koh.mtg1.a1c %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)

# now finding all A1c measures for "post" KOH
koh.a1c.post.all <- koh.mtg1.a1c %>% filter(datediff>0) %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)
# combining baseline and subsequent measurements
koh.a1c.all <- bind_rows(koh.a1c.pre.all, koh.a1c.post.all)
# making sure final list has both a pre and post timepoint
koh.a1c.pre.post.pt <- koh.a1c.pre.post %>% select(UniqueIdentifier)
koh.a1c.all.elig <- left_join(koh.a1c.pre.post.pt, koh.a1c.all, by = "UniqueIdentifier")
koh.a1c.all.elig$KOH <- 1

# these are all the "pre" KOH A1c measurements
marsh.non.a1c.pre.all <- marsh.non.a1c.elig %>% filter(datediff<=0) %>% group_by(UniqueIdentifier) %>% arrange(desc(datediff), .by_group=TRUE) %>% slice_head() %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)

# now finding all A1c measures for "post" KOH
marsh.non.a1c.post.all <- marsh.non.a1c.elig %>% filter(datediff>0) %>% select(-(datediff)) %>%
  rename(KOH.start.dt = KOHDate)
# combining baseline and subsequent measurements
marsh.non.a1c.all <- bind_rows(marsh.non.a1c.pre.all, marsh.non.a1c.post.all)
# making sure final list has both a pre and post timepoint
marsh.non.a1c.pre.post.pt <- marsh.non.a1c.pre.post %>% select(UniqueIdentifier)
marsh.non.a1c.all.elig <- left_join(marsh.non.a1c.pre.post.pt, marsh.non.a1c.all, by = "UniqueIdentifier")
marsh.non.a1c.all.elig$KOH <- 0

obj1.a1c.lme <- bind_rows(koh.a1c.all.elig, marsh.non.a1c.all.elig)
obj1.a1c.lme <- left_join(obj1.a1c.lme, koh.counts, by = "UniqueIdentifier")
obj1.a1c.lme$koh.counts[is.na(obj1.a1c.lme$koh.counts)] <- 0
# discretizing koh attendance
obj1.a1c.lme <- obj1.a1c.lme %>% mutate(KOH.none = ifelse(koh.counts == 0, 1, 0),
                                        KOH.one = ifelse(koh.counts == 1, 1, 0),
                                        KOH.mult = ifelse(koh.counts > 1, 1, 0))

write.csv(obj1.a1c.lme, 'Analysis Data/Obj1A1c_LME.csv')
