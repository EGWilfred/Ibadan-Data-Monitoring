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
library(sf)
install.packages("gridExtra")
library(gridExtra)

#load data
hh<-read_csv('UrbanMalariaHousehol_DATA_LABELS_2024-02-05_1413.csv')
ws<-read_csv('UrbanMalariaWomenSur_DATA_LABELS_2024-02-05_1421.csv')
ms<-read_csv('UrbanMalariaMenSurve_DATA_LABELS_2024-02-05_1422.csv')

colnames(hh)
colnames(ws)
colnames(ms)
view(ms)

##Data cleaning
#household
colnames(hh) <- iconv(colnames(hh), from = "UTF-8", to = "ASCII", sub = "byte")
# Replace any non-alphanumeric characters and trim spaces
colnames(hh) <- gsub("[^a-zA-Z0-9]", "_", colnames(hh))
colnames(hh) <- trimws(colnames(hh))

#men's survey
colnames(ms) <- iconv(colnames(ms), from = "UTF-8", to = "ASCII", sub = "byte")
# Replace any non-alphanumeric characters and trim spaces
colnames(ms) <- gsub("[^a-zA-Z0-9]", "_", colnames(ms))
colnames(ms) <- trimws(colnames(ms))

#women's survey
colnames(ws) <- iconv(colnames(ws), from = "UTF-8", to = "ASCII", sub = "byte")
# Replace any non-alphanumeric characters and trim spaces
colnames(ws) <- gsub("[^a-zA-Z0-9]", "_", colnames(ws))
colnames(ws) <- trimws(colnames(ws))


##prefill empty cells
#household
# Replace empty strings with NA
hh[hh == ""] <- NA

columns_to_prefill <- c(
  "LOCAL_GOVT__AREA", "Ward", "Settlement_Type", 
  "Community_Name", "Enumeration_Area_Cluster_Number", 
  "Household_Number", "HOUSEHOLD_COORDINATE__Longitude", 
  "HOUSEHOLD_COORDINATE__Latitude", "Name_of_Household_Head", 
  "Date", "INTERVIEWER_S_NAME___14", "INTERVIEWERS_PHONE_NO", "NTERVIEWER_VISIT_1_result",	
  "Visit_1_Date",	"NTERVIEWER_VISIT_2_result",	"Visit_2_Date",
  "NTERVIEWER_VISIT_3_result",	"Visit_3_Date",	"SUPERVISORS_NAME",	"FIELD_EDITOR"
  
)
# Group by Serial Number and fill missing values within specified columns for each group
hh_filled <- hh %>%
  group_by(`Serial_Number`) %>%
  fill(!!!syms(columns_to_prefill)) %>%
  ungroup()

colnames(ws)
#women
# Replace empty strings with NA
ws[ws == ""] <- NA

columns_to_prefill <- c(
  "LOCAL_GOVT__AREA", "WARD", "SETTLEMENT_TYPE", 
  "COMMUNITY_STREET_NAME", "ENUMERATION_AREA_CLUSTER_NUMBER", 
  "HOUSEHOLD_NUMBER", "HOUSEHOLD_COORDINATE__Longitude", 
  "HOUSEHOLD_COORDINATE__Latitude", "NAME_OF_HOUSEHOLD_HEAD", 
  "DATE", "INTERVIEWER_S_NAME", "INTERVIEWERS_PHONE_NO", "NTERVIEWER_VISIT_1_result",	
  "Visit_1_Date",	"NTERVIEWER_VISIT_2_result",	"Visit_2_Date",
  "NTERVIEWER_VISIT_3_result",	"Visit_3_Date",	"SUPERVISORS_NAME",	"FIELD_EDITOR"
  
)
# Group by Serial Number and fill missing values within specified columns for each group
ws_filled <- ws %>%
  group_by(`Serial_Number`) %>%
  fill(!!!syms(columns_to_prefill)) %>%
  ungroup()




##weekly targets
#household

#weekly targets
# Group the data by week
##Extracting weeks
hh_filled <- hh_filled %>% 
  filter(Date >= as.Date("2024-01-20"))

hh_filled$week <- week(hh_filled$Date)
# Adjust week numbers to start from Week 1
min_week <- min(hh_filled$week, na.rm = TRUE)
hh_filled$week <- hh_filled$week - min_week + 1

view(hh_filled$week)



# Group by serial number, week and ward, and summarize total interviews
# Counting unique serial numbers per week
week_summary <- hh_filled %>%
  group_by(week, Ward) %>%
  summarise(total_households = n_distinct(Serial_Number))

view(week_summary)


# Number of interviews weekly
# For Agugu
agugu_week_summary <- week_summary %>%
  filter(Ward == "Agugu")

# Create the plot with annotations
agugu_p_week <- ggplot(data = agugu_week_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 180, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Agugu for CS") +
  scale_x_continuous(breaks = unique(agugu_week_summary$week))

# Print the plot
print(agugu_p_week)


# For Agugu
basorun_week_summary <- week_summary %>%
  filter(Ward == "Basorun")

# Create the plot with annotations
basorun_p_week <- ggplot(data = basorun_week_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 180, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Basorun for CSS") +
  scale_x_continuous(breaks = unique(basorun_week_summary$week))

# Print the plot
print(basorun_p_week)


Challenge_week_summary <- week_summary %>%
  filter(Ward == "Challenge")

# Create the plot with annotations
Challenge_p_week <- ggplot(data = Challenge_week_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 180, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Challenge for CSS") +
  scale_x_continuous(breaks = unique(Challenge_week_summary$week))

# Print the plot
print(Challenge_p_week)



Olopomewa_week_summary <- week_summary %>%
  filter(Ward == "Olopomewa")

# Create the plot with annotations
Olopomewa_p_week <- ggplot(data = Olopomewa_week_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 180, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Olopomewa for CSS") +
  scale_x_continuous(breaks = unique(Olopomewa_week_summary$week))

# Print the plot
print(Olopomewa_p_week)






# Define the plots for each ward
library(gridExtra)

# Remove legend for clarity
agugu_p_week <- agugu_p_week + theme(legend.position = "none")
basorun_p_week <- basorun_p_week + theme(legend.position = "none")
Olopomewa_p_week <- Olopomewa_p_week + theme(legend.position = "none")
Challenge_p_week <- Challenge_p_week + theme(legend.position = "none")

# Arrange the plots in a grid layout
combined_plots <- grid.arrange(agugu_p_week, basorun_p_week, Olopomewa_p_week, Challenge_p_week, nrow = 2)

# Print the combined plot
print(combined_plots)

