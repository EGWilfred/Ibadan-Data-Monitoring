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


df<-read_csv('ib_hh_data_2911.csv')

##DATA CLEANING

# Clean up the column names
colnames(df) <- iconv(colnames(df), from = "UTF-8", to = "ASCII", sub = "byte")

# Replace any non-alphanumeric characters and trim spaces
colnames(df) <- gsub("[^a-zA-Z0-9]", "_", colnames(df))
colnames(df) <- trimws(colnames(df))

colnames(df)

#Cleaning up names of data collectors
# Create an empty mapping data structure
manual_mapping <- list(
  "/LOLA" = "LOLA",
  "ADEMOLA"="ADEMOLA",
  "ADEWALE" = "ADEWALE",
  "ADEWALE ADEBLOLA" = "ADEWALE",
  "ADEWALE ADEBOLA" = "ADEWALE",
  "ADEWALEADEBOLA" = "ADEWALE",
  "ADEYEMO COMFORT ADERONKE" = "ADEYEMO COMFORT ADERONKE",
  "AKINDELE" = "AKINDELE",
  "AKINDELE AYOMIDELE" = "AKINDELE",
  "AKINTUNDE" = "AKINTUNDE",
  "ALADE SOLOMON" = "ALADE SOLOMON",
  "AMINAT" = "AMINAT",
  "ANIBIJUWON OLUWATIMILEHIN" = "ANIBIJUWON OLUWATIMILEHIN",
  "AYINDE FOLASHADE" = "FOLASHADE",
  "AYINDE FOLASHADE ADEOLA" = "FOLASHADE",
  "AYODELE" = "AYODELE",
  "DOLAPO" = "DOLAPO",
  "EMEKA" = "Emeka",
  "EMMANUEL" = "EMMANUEL",
  "FAWOLE DAVID" = "FAWOLE DAVID",
  "FOLASHADE" = "FOLASHADE",
  "FRANCIS" = "FRANCIS",
  "FUNMITO ABIOLA" = "FUNMITO ABIOLA",
  "GBEMISOLA" = "GBEMISOLA",
  "GLORY" = "GLORY",
  "KEHINDE" = "KEHINDE",
  "KEJI" = "KEJI",
  "LOLA" = "LOLA",
  "MICHAEL" = "MICHAEL",
  "MISOLA" = "MISOLA",
  "MORENIKEJI" = "MORENIKEJI",
  "MRS ADEBAYO" = "MRS ADEBAYO",
  "OLAOLU" = "OLAOLU",
  "OLUWABUNMI" = "OLUWABUNMI",
  "OLUWABUNMI AREMO" = "OLUWABUNMI",
  "OLUWAFEMI" = "OLUWAFEMI",
  "OLUWAFEMI AKINSETE" = "OLUWAFEMI",
  "OLUWAPELUMI" = "OLUWAPELUMI",
  "OLUWATOSIN" = "OLUWATOSIN",
  "OPEMIPO" = "OPEMIPO",
  "OYEKANMI" = "OYEKANMI",
  "OYEKANMI VICTORIA" = "OYEKANMI",
  "Samuel" = "SAMUEL",
  "SAMUEL" = "SAMUEL",
  "TEMIDAYO" = "TEMIDAYO",
  "TOBILOBA" = "TOBILOBA",
  "TOMISIN" = "TOSIN",
  "TOSIN" = "TOSIN",
  "UDEME" = "UDEME",
  "ZAINAB" = "ZAINAB"
  
)

# Replace names in the original dataframe using the manual mapping
for (i in seq_along(df$`INTERVIEWER'S NAME`)) {
  name <- df$`INTERVIEWER'S NAME`[i]
  if (!is.na(name) && name %in% names(manual_mapping)) {
    df$`INTERVIEWER'S NAME`[i] <- manual_mapping[[name]]
  }
}

# see the 'INTERVIEWER'S NAME' 
unique_names <- unique(df$`INTERVIEWER'S NAME`)
unique_names <- sort(unique_names)  # Sort names alphabetically
# Displaying unique names alphabetically
print(unique_names)



#prefill empty cells
# Replace empty strings with NA
df[df == ""] <- NA

columns_to_prefill <- c(
  "LOCAL_GOVT__AREA", "Ward", "Settlement_Type", 
  "Community_Name", "Enumeration_Area_Cluster_Number", 
  "Household_Number", "HOUSEHOLD_COORDINATE__Longitude", 
  "HOUSEHOLD_COORDINATE__Latitude", "Name_of_Household_Head", 
  "Date", "INTERVIEWER_S_NAME", "INTERVIEWERS_PHONE_NO", "NTERVIEWER_VISIT_1_result",	
  "Visit_1_Date",	"NTERVIEWER_VISIT_2_result",	"Visit_2_Date",
  "NTERVIEWER_VISIT_3_result",	"Visit_3_Date",	"SUPERVISORS_NAME",	"FIELD_EDITOR"
  
)
# Group by Serial Number and fill missing values within specified columns for each group
df_filled <- df %>%
  group_by(`Serial_Number`) %>%
  fill(!!!syms(columns_to_prefill)) %>%
  ungroup()


# Replacing Ologuneru with Olopomewa
df_filled <- df_filled %>%
  mutate(Ward = ifelse(Ward == "Ologuneru", "Olopomewa", Ward))

print(df_filled)



##ERROR CHECK

# interviwers who selected complete, but with missing data
incomplete <- df_filled %>%
  filter(`Complete?...24` == "Complete" &
           (is.na(`LOCAL GOVT. AREA`) |
              is.na(Ward) |
              is.na(`Settlement Type`) |
              is.na(`Household Number`))) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(incomplete_data = n())

# Print the result
view(incomplete)



# Interviewers who entered wrong data type
# Define the columns to be checked
columns_to_check <- c(
  'Usual Residents, Please give me the names of the persons who usually live here',
  'Enumeration Area/Cluster Number',
  'Name of Household Head'
)

wrongtype <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(wrong_dtype = sum(across(all_of(columns_to_check), 
                                     ~ sum(as.numeric(grepl("^\\d+$", .))),
                                     .names = "{.col}_Count")))

#print the result
view(wrongtype)



#Blank date
# Count occurrences of NA values in the 'Date' column for each interviewer
result3 <- df_filled %>%
  filter(!is.na(`INTERVIEWER'S NAME`)) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(missing_date = sum(is.na(Date)))
# Exclude interviewers with 0 count
blankdate <- result3
# Print the result
view(blankdate)



# DOB QC
# Check if age and date of birth correspond with a difference greater than 1
dobcheck <- df_filled %>%
  filter(!is.na(`INTERVIEWER'S NAME`) & !is.na(`Age: How old was   (NAME) as at last birthday?...29`) & !is.na(`Date of Birth (day/month/year)`)) %>%
  mutate(
    dob = as.Date(`Date of Birth (day/month/year)`, format = "%d/%m/%Y"),
    age_calculated = as.numeric(format(dob, "%Y")),
    age_entered = as.numeric(`Age: How old was   (NAME) as at last birthday?...29`),
    difference_in_years = 2023 - as.numeric(format(dob, "%Y"))
  ) %>%
  filter(age_entered - difference_in_years > 1) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(dob = n())

# Print the result
view(dobcheck)



# Merge the errors by 'INTERVIEWER'S NAME'
merged_result <- Reduce(function(x, y) merge(x, y, by = 'INTERVIEWER\'S NAME', all = TRUE), list(dobcheck, blankdate, wrongtype, incomplete))
# Replace missing counts with 0
merged_result[is.na(merged_result)] <- 0
# Sum the counts for each interviewer
merged_result$sum_count <- rowSums(merged_result[, -1], na.rm = TRUE)
# Print the merged result
view(merged_result)



# Calculate total interviews conducted by each interviewer in df_filled
total_interviews <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(total_interviews = n())

# Merge total_interviews with merged_result
merged_result1 <- merge(merged_result, total_interviews, by.x = 1, by.y = 1)
view(merged_result1)

#  Calculate the percentage in each row of merged_result
merged_result1$percentage <- (merged_result1$sum_count / merged_result1$total_interviews) * 100

# Print the result
print(merged_result1)



#visualizing the errors
# Create a bar plot
ggplot(merged_result1, aes(x = percentage, y = `INTERVIEWER'S NAME`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Percentage of Errors by Interviewer",
       x = "Percentage",
       y = "Interviewer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("plot.png", plot = last_plot(), width = 20, height = 15)





##PERFORMANCE OF RA BY INTWEVIEWS TARGET MET

# Create a Data Frame with Unique Households and Interviewers
unique_combinations <- df_filled %>%
  distinct(`Serial_Number`, `INTERVIEWER_S_NAME`)

# Determine RA Active Periods
df_filled <- df_filled %>%
  mutate(Date = anytime(Date))

ra_active_periods <- df_filled %>%
  filter(!is.na(Date)) %>%
  group_by(`INTERVIEWER_S_NAME`) %>%
  summarise(start_date = min(Date), end_date = max(Date),
            total_days = as.numeric(difftime(max(Date), min(Date), units = "days")) +1)


# Count Unique Households per Day for Each RA
unique_households_per_day <- df_filled %>%
  inner_join(ra_active_periods, by = c("INTERVIEWER_S_NAME" = "INTERVIEWER_S_NAME")) %>%
  filter(Date >= start_date & Date <= end_date) %>%
  group_by(`INTERVIEWER_S_NAME`, Date) %>%
  summarise(unique_household_count = n_distinct(`Serial_Number`))

# Count Total Number of Days an Interviewer Did Not Meet the Target
total_days_not_met <- unique_households_per_day %>%
  group_by(`INTERVIEWER_S_NAME`) %>%
  summarise(total_days_not_met = sum(unique_household_count < 5))

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