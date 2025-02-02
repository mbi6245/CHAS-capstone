######################################################################################
# README: run this file before running any analyses to create analysis dataset objects
######################################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fp_a1c = file.path(getwd(), "Raw Data/UWDataA1cs.csv")
fp_bmi = file.path(getwd(), "Raw Data/UWDataBMIs.csv")
fp_bp = file.path(getwd(), "Raw Data/UWDataBP.csv")
fp_diag = file.path(getwd(), "Raw Data/UWDataDiagnoses.csv")
fp_enc = file.path(getwd(), "Raw Data/UWDataEncounters.csv")
fp_panel = file.path(getwd(), "Raw Data/UWDataPanel.csv")
fp_koh = file.path(getwd(), "Raw Data/UWDataKOHAttendance.csv")
fp_exenc = file.path(getwd(), "Raw Data/UWExtraEncounterData.csv")

a1c <- read.csv(fp_a1c)
bmi <- read.csv(fp_bmi)
bp <- read.csv(fp_bp)
diag <- read.csv(fp_diag)
enc <- read.csv(fp_enc)
panel <- read.csv(fp_panel, na.strings = "")
koh.attend <- read.csv(fp_koh)
enc.extra <- read.csv(fp_exenc)

# change cases where PreDM=1 and T2DM=1 to PreDM=0
diag$PreDM[diag$PreDM == 1 & diag$T2DM == 1] <- 0

# change date variable to date type and remove NA values in necessary fields
a1c$Date <- mdy(a1c$Date)
a1c.nona <- a1c %>% filter(!is.na(A1c) & !is.na(Date))

bmi$Date <- mdy(bmi$Date)
bmi.nona <- bmi %>% filter(!is.na(Date) & !is.na(BMI))

bp$Date <- mdy(bp$Date)
bp.nona <- bp %>% filter(!is.na(Date) & !is.na(Systolic))

enc <- bind_rows(enc, enc.extra)
enc$Date <- mdy(enc$Date)
enc.nona <- enc %>% filter(!is.na(Date) & !is.na(ServiceLine))

koh.attend$Date <- mdy(koh.attend$dateAttended)
koh.attend <- koh.attend %>% filter(!is.na(Date))
koh.counts <- koh.attend %>% group_by(UniqueIdentifier) %>% count(name = "koh.counts")

diag.nona <- diag %>% filter(!is.na(HTN) & !is.na(PreDM) & !is.na(T2DM))

# create diabetes indicator for both pre and T2DM and indicator for if they have both htn and diabetes
diag.nona <- diag.nona %>% mutate(Diabetes = ifelse((PreDM == 1 | T2DM == 1), 1, 0),
                                  both = ifelse((Diabetes == 1 & HTN == 1), 1, 0))

# Remove anyone under the age of 18
age <- panel %>% select(c("UniqueIdentifier","age"))
a1c.nona.18 <- left_join(a1c.nona, age, by = "UniqueIdentifier")
a1c.nona.18 <- a1c.nona.18 %>% filter(age >= 18)

bmi.nona.18 <- left_join(bmi.nona, age, by = "UniqueIdentifier")
bmi.nona.18 <- bmi.nona.18 %>% filter(age >= 18)

bp.nona.18 <- left_join(bp.nona, age, by = "UniqueIdentifier")
bp.nona.18 <- bp.nona.18 %>% filter(age >= 18)

enc.nona.18 <- left_join(enc.nona, age, by = "UniqueIdentifier")
enc.nona.18 <- enc.nona.18 %>% filter(age >= 18)

diag.nona.18 <- left_join(diag.nona, age, by = "UniqueIdentifier")
diag.nona.18 <- diag.nona.18 %>% filter(age >= 18)

koh.attend.18 <- left_join(koh.attend, age, by = "UniqueIdentifier")
koh.attend.18 <- koh.attend.18 %>% filter(age >= 18)

# patients with KOHParticipant = 1 but are not in KOH attendance dataset
panel.koh <- panel %>% filter(KOHParticipant == 1 & age>=18)
k <- full_join(koh.counts, panel.koh, by = "UniqueIdentifier")
k %>% filter(is.na(koh.counts))

panel <- panel %>% mutate(Sex.long = case_when(Sex == 'M' ~ 'Male',
                                               Sex == 'F' ~ 'Female'))
panel.18 <- panel %>% filter(age >= 18)

marsh <- panel.18 %>% filter(Race == 'Marshallese')
lang <- panel.18 %>% filter(Language == 'Marshallese')

# create our population of non-hispanic whites and marshallese (language = marsh or race = marsh or KOH = 1) 
targetpop_temp <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1 | (Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))

# remove any homeless population
targetpop_temp <- targetpop_temp %>% filter(No.column.name != 'Homeless Shelter' & No.column.name != 'Street')

# make marshallese indicator variable
targetpop_temp <- targetpop_temp %>% 
  mutate(Marsh = ifelse((Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1), 1, 0),
         Group = ifelse(Marsh == 1, "Marshallese", "Non-Marshallese"))

targetpop_temp$No.column.name <- factor(targetpop_temp$No.column.name, 
                                   levels = c('Housed','Doubling Up','Permanent Supportive Housing','Transitional','Other','Unknown'))

# replace all the blanks in the character variables with NAs so we can count the missings in the table
# targetpop <- targetpop %>% replace_with_na_all(condition = ~.x == "")
targetpop_temp = targetpop_temp %>% mutate(Marsh = if_else(UniqueIdentifier %in% c(94813, 402761, 50526), 0, Marsh),
                                           Group = if_else(UniqueIdentifier %in% c(94813, 402761, 50526), "Non-Marshallese", Group))
# targetpop_temp %>% filter(UniqueIdentifier %in% c(94813, 402761, 50526)) %>% mutate(Marsh = 0, Group = 0)
targetpop_temp = tibble(targetpop_temp)

identical(targetpop, targetpop_temp)

# get rid of the age variable we got from merging to filter out children since targetpop already has age
diag.nona.18 <- diag.nona.18 %>% select(-age)

# merge diagnosis data with other characteristics
table1 <- right_join(diag.nona.18, targetpop, by = "UniqueIdentifier")

# create subset of only KOH participants
koh <- table1 %>% filter(KOHParticipant == 1)
koh.table1_temp <- inner_join(koh.counts, koh, by = "UniqueIdentifier")


