# Visuals 

#ER 
colnames(did_visit_types_year)

# no! takes forever to load! 
# 
# did_visit_types_year %>% 
#   ggplot(aes(x = Date, y = ER, col = marsh))+
#   geom_jitter()+


ggplot()+
  geom_abline( intercept =(gee_mod_DID_ER$coefficients[1] + gee_mod_DID_ER$coefficients[2]), 
               slope= (gee_mod_DID_ER$coefficients[3] + gee_mod_DID_ER$coefficients[4]), col = "blue")+ # line for Marshallese
  geom_abline( intercept = (gee_mod_DID_ER$coefficients[1]), 
               slope= gee_mod_DID_ER$coefficients[3], col = "red") # line for Non-Hispanic White
  
  


# graph number of all visits? 
table_all_visit_types_wider <- read.csv("~/BIOST CLASSES/597 Capstone with Lloyd Mancl/597 Capstone/CHAS-capstone/Analysis Data/table_all_visit_types_wider.csv")