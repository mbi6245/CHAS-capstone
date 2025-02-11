#According to our SAP 3.3.2 we will match by age, sex and poverty.
# We will also exclude homeless.
# So these are the patients we need to exclude from the target population.

colnames(panel.18)

targetpop <- panel.18 %>% filter(Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1 | (Race == 'White' & Ethnicity == 'Not Hispanic or Latino'))

# make marshallese indicator variable
targetpop <- targetpop %>%
  mutate(Marsh = ifelse((Race == 'Marshallese' | Language == 'Marshallese' | KOHParticipant == 1), 1, 0),
         Group = ifelse(Marsh == 1, "Marshallese", "Non-Marshallese"))

# m <- targetpop %>% filter(Marsh == 1 ) #%>% summarize(max(BLACERISK))
# w <- targetpop %>% filter(Marsh == 0 )


############### OLDER ######################

older <-targetpop %>% filter(age > max(Marshallese$age))
# 67 patients 

############### HOMELESS ######################
# No.column.name is housing status
# filtered housing status from panel.18 in the DataCleaningDID.R
homeless <-  targetpop %>% filter(No.column.name == "Homeless Shelter" |
                                    No.column.name == "Street" )
#colnames(targetpop)

#with(targetpop, table(No.column.name, Marsh))

############### DECEASED ######################


# ! Also what do we do with Deceased Date?
deceased <-targetpop %>% filter(DeceasedDate != "")

nrow(deceased)
#  344

class(deceased$DeceasedDate)
# "character"
deceased$DeceasedDate <- as.Date(deceased$DeceasedDate, format = "%m/%d/%Y")
class(deceased$DeceasedDate)

min(deceased$DeceasedDate)
# "2022-06-03"
# this is before the end of the DID analysis so I would remove it...
max(deceased$DeceasedDate)
# "2024-12-25"
hist(deceased$DeceasedDate, breaks = 24)
with(deceased, table( Marsh))

# 20 marshallese and 316 NHW 

# A lot of them are after our DID model but I am still inclined to remove them all

############### RISK SCORES ######################
# I am surprised to see such high Risk Scores in our controls
# I don't think they make good controls if they are double the maximum Marshallese risk score.
# we will keep all people without risk scores in both groups
higher_risk <- targetpop %>% filter(BLACERISK > max(Marshallese$BLACERISK, na.rm = TRUE))
nrow(higher_risk)
# 59

############### INCOME ######################
# higher income
colnames(targetpop)
higher_income <- targetpop %>% filter(IncomeLevel > max(Marshallese$IncomeLevel, na.rm = TRUE))

nrow(higher_income)
# 641

############### BMI ######################

fp_bmi = file.path(getwd(), "Raw Data/UWDataBMIs.csv")
bmi <- read.csv(fp_bmi) # demographics

fp_new_bmi = file.path(getwd(), "Raw Data/UW BMI Missing List .csv")
UW.BMI.Missing.List. <- read.csv(fp_new_bmi) # demographics

# bmi <- read.csv("Raw Data/UWDataBMIs.csv")
bmi$Date <- mdy(bmi$Date)
#bmi.nona <- bmi %>% filter(!is.na(Date) & !is.na(BMI))
# update this with new BMI list

#UW.BMI.Missing.List. <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/CHAS-capstone/Raw Data/UW BMI Missing List .csv")

colnames(bmi)
# match up the column names so we can merge better
colnames(UW.BMI.Missing.List.)[1] <- c("UniqueIdentifier")
colnames(UW.BMI.Missing.List.)[2] <- c("BMI")


full_bmi <- full_join(bmi, UW.BMI.Missing.List.) # , by = c("UniqueIdentifier" , "uniqueIdentifier")



# mark who are Marshallese and see if there are NHW with higher BMI
colnames(targetpop)
colnames(full_bmi)
colnames(full_bmi)[1] <- c("UniqueID")
targetpop <-  left_join(targetpop ,full_bmi)

Max_Marshallese_BMI <- targetpop %>% filter(Marsh == 1) 

max(Max_Marshallese_BMI$BMI, na.rm = TRUE)
# 73
Max_NHW_BMI <- targetpop %>% filter(Marsh == 0) 
max(Max_NHW_BMI$BMI, na.rm = TRUE)
# 111436.5

# ! note there are some zero BMI in each group as well but since they are in both I think we can accept these?

higher_BMI <- targetpop %>% filter(BMI >max(Max_Marshallese_BMI$BMI, na.rm = TRUE))

nrow(higher_BMI)
# 331 including BMI in the thousands which are clearly out of range





# we might have to merge this to targetpop
exclude <- c(higher_BMI$UniqueID, higher_income$UniqueID, higher_risk$UniqueID, deceased$UniqueID, homeless$UniqueID, older$UniqueID)
length(exclude)
# 2396
exclude <- unique(exclude)
length(exclude)
# 2187 which is 209 duplicates between the groups


# Remove these groups too

# Final Target Pop for our analysis
targetpop_DID <- targetpop %>% filter(!UniqueID %in% exclude) # not in exclude list is !UniqueID %in% exclude

intersect(targetpop_DID$UniqueID, exclude)
# good it worked

nrow(targetpop_DID) == nrow(targetpop) - length(exclude)
# !FALSE


# Visually check the covariates now with histograms

hist(targetpop_DID$age)
# 2 groups

# hist(targetpop_DID$age[Marsh == 1 ], freq = FALSE, col = "grey",
# border = NA, xlab = "",
# ylab = "", yaxt = "n", breaks = 30,
# main = "Distribution of Age between Marshallese and Non-Hispanic White patients in DID Model",
# xlim = c(0, 1), ylim = c(0, 4.5))
# hist(targetpop_DID$age[Marsh == 0], freq = FALSE,
# add = TRUE,
# # breaks = 30,
# col=NA)

visuals_DID <- function(demographic) {
targetpop_DID %>%
ggplot(aes(x = {{demographic}}))+
  geom_density()+
  facet_wrap(~Marsh)
}

visuals_DID(age)
visuals_DID(BMI)
visuals_DID(IncomeLevel)
visuals_DID(Sex)
visuals_DID(BLACERISK)
visuals_DID(No.column.name) # homeless status shows the options 
visuals_DID(DeceasedDate) # totally blank all of the patients are still alive
visuals_DID(ClinicLocation) 


  
# ! check table 1 of these new groups 
