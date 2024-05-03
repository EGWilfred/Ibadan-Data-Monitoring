library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
install.packages("anytime")
library(anytime)
install.packages("formattable")
library(formattable)
install.packages("stringdist")
library(stringdist)
library(scales)


setwd("C:/Users/Dell Latitude/Documents/Longitudinal")

lg<-read_csv('UrbanMalariaLongitud_DATA_LABELS_2024-04-15_1506.csv')

##Data cleaning
colnames(lg) <- iconv(colnames(lg), from = "UTF-8", to = "ASCII", sub = "byte")
# Replace any non-alphanumeric characters and trim spaces
colnames(lg) <- gsub("[^a-zA-Z0-9]", "_", colnames(lg))
colnames(lg) <- trimws(colnames(lg))

columns_to_prefill <- c(
  "LOCAL_GOVT__AREA", "WARD", "SETTLEMENT_TYPE", "COMMUNITY_NAME",
  "ENUMERATION_AREA_CLUSTER_NUMBER", "HOUSEHOLD_NUMBER___10", "HOUSEHOLD_COORDINATE__Longitude___11",
  "HOUSEHOLD_COORDINATE__Latitude___12", "NAME_OF_HOUSEHOLD_HEAD___13", "DATE___14",
  "INTERVIEWER_S_NAME___15", "INTERVIEWERS_PHONE_NO___16", "NTERVIEWER_VISIT_1_result___17",
  "Visit_1_Date___18",
  "SUPERVISORS_NAME", "FIELD_EDITOR", "Complete____25"
)
# Group by Serial Number and fill missing values within specified columns for each group

lg_filled <- lg %>%
  group_by(`Serial_Number`) %>%
  fill(!!!syms(columns_to_prefill)) %>%
  ungroup()





##follow up 1
# create a dataframe for children who have been followed up
follow_up1 <- lg_filled %>%
  filter(Event_Name == "Follow up 1" & !is.na(q704__Malaria_RDT_done))
# create a data frame for children not followed
not_followed1 <- anti_join(lg_filled, follow_up1, by = "Serial_Number")
write.csv(not_followed1, "children yet to be followed up1.csv", row.names = FALSE) 
# group by serial numbers, interviwers and visit date
unfollowed1 <- not_followed1 %>%
  select(Serial_Number, Visit_1_Date___18, INTERVIEWER_S_NAME___15, WARD) %>%
  distinct()  # Keep only unique rows
write.csv(unfollowed1, "unfollowed1.csv", row.names = FALSE) 




##follow up 2
# create a dataframe for children who have been followed up
follow_up2 <- lg_filled %>%
  filter(Event_Name == "Follow up 2" & !is.na(q704__Malaria_RDT_done))
 # create a data frame for children not followed
not_followed2 <- anti_join(lg_filled, follow_up2, by = "Serial_Number")

# group by serial numbers, interviwers and visit date
unfollowed2 <- not_followed2 %>%
  select(Serial_Number, Visit_1_Date___18, INTERVIEWER_S_NAME___15, WARD) %>%
  distinct()  # Keep only unique rows
write.csv(unfollowed2, "unfollowed2.csv", row.names = FALSE) 

 
 
 
 
 
 
##plot of households with follow up 1 against others without

 # Filter data for rows with Follow up 1 and count unique serial numbers by ward
 with_followup <- lg_filled %>%
   filter(Event_Name == "Follow up 1") %>%
   group_by(WARD) %>%
   summarise(followup = n_distinct(Serial_Number))
 
 # Filter data for rows without Follow up 1 and count unique serial numbers by ward
 without_followup <- filtered_lg %>%
   group_by(WARD) %>%
   summarise(no_followup = n_distinct(Serial_Number))

 
combined_data <- full_join(with_followup, without_followup, by = "WARD", suffix = c("_with_followup", "_without_followup"))
 

 
 # Reshape the data to have one row per ward, with columns for counts with and without follow-up
 combined_data_long <- combined_data %>%
   pivot_longer(cols = c(followup, no_followup), 
                names_to = "follow_up", values_to = "count")
 
 #filter out missing values from the ward column
 combined_data_long <- combined_data_long %>%
  filter(!is.na(WARD))
 

 
 # Plot
 ggplot(combined_data_long, aes(x = WARD, y = count, fill = follow_up)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Add count labels
  labs(title = "Number of Follow up 1 vs Expected",
      x = "Ward",
      y = "Number",
      fill = "Follow up") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 
 
 


 
#Creating a follow up tracking sheet
unique_data <- lg_filled %>%
  distinct(Serial_Number, .keep_all = TRUE) %>%
  select(Serial_Number, WARD, COMMUNITY_NAME, INTERVIEWER_S_NAME___15, INTERVIEWERS_PHONE_NO___16, HOUSEHOLD_COORDINATE__Longitude___11, HOUSEHOLD_COORDINATE__Latitude___12, Visit_1_Date___18, SETTLEMENT_TYPE, NAME_OF_HOUSEHOLD_HEAD___13, q300i__Name_of_picked_household_member)


#Add dates for follow up 1 to 12
lg_filled_with_followups <- unique_data %>%
  mutate(
    Follow_up_1 = Visit_1_Date___18 + days(30),
    Follow_up_2 = Visit_1_Date___18 + days(60),
    Follow_up_3 = Visit_1_Date___18 + days(90),
    Follow_up_4 = Visit_1_Date___18 + days(120),
    Follow_up_5 = Visit_1_Date___18 + days(150),
    Follow_up_6 = Visit_1_Date___18 + days(180),
    Follow_up_7 = Visit_1_Date___18 + days(210),
    Follow_up_8 = Visit_1_Date___18 + days(240),
    Follow_up_9 = Visit_1_Date___18 + days(270),
    Follow_up_10 = Visit_1_Date___18 + days(300),
    Follow_up_11 = Visit_1_Date___18 + days(330),
    Follow_up_12 = Visit_1_Date___18 + days(360)
  )


write.csv(lg_filled_with_followups, "follow up tracking sheet.csv", row.names = FALSE)
