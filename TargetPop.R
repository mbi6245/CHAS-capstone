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


older <-targetpop %>% filter(age > max(Marshallese$age))

# No.column.name is housing status
homeless <-  targetpop %>% filter(No.column.name == "Homeless Shelter" |
                                    No.column.name == "Street" )
colnames(targetpop)

with(targetpop, table(No.column.name, Marsh))

# Final Target Pop for our analysis?
targetpop <- anti_join(targetpop, older)
targetpop <- anti_join(test, homeless)

nrow(test) == nrow(targetpop) - nrow(older) - nrow(homeless)


# 2 New Problems
# !  I am surprised to see such high Risk Scores in our controls
# I don't think they make good controls if they are double the maximum Marshallese risk score.
higher_risk <- targetpop %>% filter(BLACERISK > max(Marshallese$BLACERISK, na.rm = TRUE))



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
max(deceased$DeceasedDate)
# "2024-12-25"

# Remove these 2 groups too? Or some of them?
