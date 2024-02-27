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


lg<-read_csv('UrbanMalariaLongitud_DATA_LABELS_2024-02-05_1409.csv')

view(lg)

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

#check for column names
colnames(lg_filled)
#check for total NA in a column
print(sum(is.na(lg_filled$week)))
#check for data type
str(lg_filled$week)
#exact row with NA
missing_week_row <- which(is.na(lg_filled$week))
missing_week_data <- lg_filled[missing_week_row, ]



#weekly targets
# Group the data by week
##Extracting weeks
lg_filled$week <- week(lg_filled$DATE___14)
# Adjust week numbers to start from Week 1
min_week <- min(lg_filled$week, na.rm = TRUE)
lg_filled$week <- lg_filled$week - min_week + 1

view(lg_filled$week)



# Group by serial number, week and ward, and summarize total interviews
# Counting unique serial numbers per week
weekly_summary <- lg_filled %>%
  group_by(week, WARD) %>%
  summarise(total_households = n_distinct(Serial_Number))


view(weekly_summary)

# Number of interviews weekly
# For Agugu
agugu_weekly_summary <- weekly_summary %>%
  filter(WARD == "Agugu")

# Create the plot with annotations
agugu_p_week <- ggplot(data = agugu_weekly_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 72, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Agugu") +
  scale_x_continuous(breaks = unique(agugu_weekly_summary$week))

# Print the plot
print(agugu_p_week)



# For Bashorun
# Filter out weeks greater than 4
Basorun_weekly_summary <- weekly_summary %>%
  filter(WARD == "Basorun", week <= 4)

# Create the plot with annotations
Basorun_p_week <- ggplot(data = Basorun_weekly_summary, aes(x = week, y = total_households)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 72, linetype = "dashed", color = "red") +
  geom_text(aes(label = total_households), vjust = -0.5, size = 3, color = "black") +  # Add annotation
  theme_minimal() +
  ylab("Number of households") +
  xlab("Week of Visit") +
  labs(title = "Weekly Interviews Conducted in Basorun") +
  scale_x_continuous(breaks = unique(Basorun_weekly_summary$week))

# Print the plot
print(Basorun_p_week)




#proportion of positive and negative in each settlement and ward

# Remove NA values from specific columns
lg_filled_no_na <- na.omit(lg_filled[, c("WARD", "q705__RESULT", "SETTLEMENT_TYPE")])

#result by ward
ggplot(lg_filled_no_na, aes(x = `WARD`, fill = `q705__RESULT`)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Results Across Wards in Longitudinal Survey",
       x = "WARD",
       y = "PROPORTION") +
  scale_fill_manual(values = c("NEGATIVE" = "grey", "POSITIVE" = "maroon"), name = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#results by settlement
ggplot(lg_filled_no_na, aes(x = `SETTLEMENT_TYPE`, fill = `q705__RESULT`)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Results Across Settlements in Longitudinal Survey",
       x = "SETTLEMENT_TYPE",
       y = "PROPORTION") +
  scale_fill_manual(values = c("NEGATIVE" = "grey", "POSITIVE" = "maroon"), name = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#Age distribution of children
#create the data frame
your_data_frame <- lg_filled %>%
  group_by(q300iii__What_was__NAME__age_on_their_last_birthday__Years__0_if_less_than_1, q705__RESULT) %>%
  summarise(total = n())
view(your_data_frame)

# Create a plot of age distribution
ggplot(your_data_frame, aes(x = q300iii__What_was__NAME__age_on_their_last_birthday__Years__0_if_less_than_1, y = total)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Children in Longitudinal Survey", x = "Age", y = "Frequency") +
  theme_minimal()


#Results per age group
ggplot(your_data_frame, aes(x = `q300iii__What_was__NAME__age_on_their_last_birthday__Years__0_if_less_than_1`, fill = `q705__RESULT`)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Results Across Age Distribution in Longitudinal Survey",
       x = "Age",
       y = "PROPORTION") +
  scale_fill_manual(values = c("NEGATIVE" = "grey", "POSITIVE" = "maroon"), name = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))









##PERFORMANCE OF RA BY INTERVIEWS TARGET MET

# Create a Data Frame with Unique Households and Interviewers
unique_combinations <- lg_filled %>%
  distinct(`Serial_Number`, `INTERVIEWER_S_NAME___15`)

# Determine RA Active Periods
lg_filled <- lg_filled %>%
  mutate(Date = anytime(DATE___14))

ra_active_period <- lg_filled %>%
  group_by(`INTERVIEWER_S_NAME___15`) %>%
  summarise(start_date = min(Date), end_date = max(Date),
            total_days = as.numeric(difftime(max(Date), min(Date), units = "days")) +1)
view(ra_active_period)

# Count Unique Households per Day for Each RA
unique_households <- lg_filled %>%
  inner_join(ra_active_period, by = c("INTERVIEWER_S_NAME___15" = "INTERVIEWER_S_NAME___15")) %>%
  filter(Date >= start_date & Date <= end_date) %>%
  group_by(`INTERVIEWER_S_NAME___15`, Date) %>%
  summarise(unique_household_count = n_distinct(`Serial_Number`))
view(unique_households)

# Count Total Number of Days each Interviewer worked
total_days_not_met <- unique_households %>%
  group_by(`INTERVIEWER_S_NAME___15`) %>%
  summarise(total_days = sum(Date))

colnames(ra_active_periods)
colnames(total_days_not_met)

#merge the two tables
merged_table <- left_join(ra_active_periods %>% select(INTERVIEWER_S_NAME, total_days),
                          total_days_not_met %>% select(INTERVIEWER_S_NAME, total_days_not_met),
                          by = "INTERVIEWER_S_NAME")

#  Calculate the percentage in each row of merged_table
merged_table$percentage <- (merged_table$total_days_not_met / merged_table$total_days) * 100

#visualizing the percentage of days not met
# Create a bar plot
# Filter out NAs
merged_table <- merged_table[complete.cases(merged_table$INTERVIEWER_S_NAME), ]

my_plot<- ggplot(merged_table, aes(x = percentage, y = INTERVIEWER_S_NAME)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 2.5) +
  labs(title = "Percentage Of Target Not Met",
       x = "Percentage",
       y = "Interviewer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("my_plot.png", plot = my_plot, height = 15, width = 10)
