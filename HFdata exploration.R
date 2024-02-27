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

df<-read_csv('UrbanMalariaHFSIbada_DATA_LABELS_2024-01-29_1831.csv')


# Clean up the column names
colnames(df) <- iconv(colnames(df), from = "UTF-8", to = "ASCII", sub = "byte")

# Replace any non-alphanumeric characters and trim spaces
colnames(df) <- gsub("[^a-zA-Z0-9]", "_", colnames(df))
colnames(df) <- trimws(colnames(df))


#checks
class(df$Date)
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
any(is.na(df$Date))
class(df$Date)

# Number of interviews per LGA
plot_data <- df %>%
  group_by(`LOCAL_GOVT__AREA___3`) %>%
  summarise(No_of_interviews = n())

# Create a bar plot
ggplot(plot_data, aes(x = `LOCAL_GOVT__AREA___3`, y = No_of_interviews)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = No_of_interviews), vjust = 1, color = "black", size = 4) +  # Add count labels
  labs(title = "Number of Interviews Conducted per LGA",
       x = "Local Government Area",
       y = "Number of Interviews") +
  theme_minimal()


##test result by health facility
#number of respondents
ggplot(df, aes(x = `NAME_OF_HEALTH_FACILITY___5`, fill = `q503__RESULT`)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Distribution of Results in Each Health Facility",
       x = "Health Facility",
       y = "Number of Respondents") +
  scale_fill_manual(values = c("NEGATIVE" = "grey", "POSITIVE" = "maroon"), name = "Result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#proportion of respondents
ggplot(df, aes(x = `NAME_OF_HEALTH_FACILITY___5`, fill = `q503__RESULT`)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Results in Each Health Facility",
       x = "Health Facility",
       y = "Percentage of Respondents") +
  scale_fill_manual(values = c("NEGATIVE" = "grey", "POSITIVE" = "maroon"), name = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Age distribution of respondents
plot_data <- df %>%
  group_by(`q101__How_old_were_you_on_your_last_birthday__c2__a0_AGE_AT_LAST_BIRTHDAY__IN_YEARS_`) %>%
  summarise(No_of_interviews = n())
# Create a bar plot
ggplot(plot_data, aes(x = `q101__How_old_were_you_on_your_last_birthday__c2__a0_AGE_AT_LAST_BIRTHDAY__IN_YEARS_`, y = No_of_interviews)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = No_of_interviews), vjust = -0.5, color = "black", size = 3) +  # Add count labels
  labs(title = "Age Distribution of Respondents",
       x = "Age",
       y = "Number of Interviews") +
  theme_minimal()


#Number of pregnancies
plot_data <- df %>%
  group_by(`q124b__How_many_pregnancies_have_you_had`) %>%
  summarise(No_of_interviews = n())
# Create a bar plot
ggplot(plot_data, aes(x = `q124b__How_many_pregnancies_have_you_had`, y = No_of_interviews)) +
  geom_bar(stat = "identity", fill = "maroon") +
  geom_text(aes(label = No_of_interviews), vjust = -0.5, color = "black", size = 3) +  # Add count labels
  labs(title = "Number of Pregnancies of Respondents",
       x = "No. of Pregnancies",
       y = "Number of Interviews") +
  theme_minimal()







#weekly targets

# Group the data by week and count the number of patients seen each day
##Extracting weeks
df$week <- week(df$Date)
# Adjust week numbers to start from Week 1
df$week <- df$week - min(df$week) + 1

# Group by week and health facility name, and summarize total patients
weekly_summary <- df %>%
  group_by(week, NAME_OF_HEALTH_FACILITY___5) %>%
  summarise(total_patients = n())

view(weekly_summary)

# Create the plot
# all HF
ag_p_dayy <- ggplot(data = daily_summary, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Week of Visit") +
  labs(title = "Weekly interviews conducted in All Health Facilities") +
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_dayy)




#For Adeoyo
daily_summaryy <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Adeoyo MTH")
ag_p_day <- ggplot(data = daily_summaryy, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 23, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Weekly interviews conducted in Adeoyo MTH")+
  scale_x_continuous(breaks = unique(daily_summary$week))
# Print the plot
print(ag_p_day)



#for Agbongbon PHC

daily_summary1 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Agbongbon PHC")
ag_p_day1 <- ggplot(data = daily_summary1, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 9, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Weekly interviews conducted in Agbongbon PHC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day1)


#Alafara
daily_summary2 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Alafara PHC")
ag_p_day2 <- ggplot(data = daily_summary2, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Alafara PHC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day2)



#Idi Ogungun PHC
daily_summary3 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Idi Ogungun PHC")
ag_p_day3 <- ggplot(data = daily_summary3, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Idi Ogungun PHC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day3)




#Jericho SH
daily_summary4 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Jericho SH")
ag_p_day4 <- ggplot(data = daily_summary4, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 13, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Jericho SH")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day4)





#Naomi Medical Centre
daily_summary5 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Naomi Medical Centre")
ag_p_day5 <- ggplot(data = daily_summary5, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 6, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Naomi Medical Centre")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day5)





#Oke Adu PHC
daily_summary6 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Oke Adu PHC")
ag_p_day6 <- ggplot(data = daily_summary6, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Oke Adu PHC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day6)





#Oniyanrin Comp HC
daily_summary7 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "Oniyanrin Comp HC")
ag_p_day7 <- ggplot(data = daily_summary7, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in Oniyanrin Comp HC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day7)







#ORANYAN PHC
daily_summary8 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "ORANYAN PHC")
ag_p_day8 <- ggplot(data = daily_summary8, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in ORANYAN PHC")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day8)




#RingRoad SSH
daily_summary9 <- daily_summary %>%
  filter(NAME_OF_HEALTH_FACILITY___5 == "RingRoad SSH")
ag_p_day9 <- ggplot(data = daily_summary9, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 11, linetype = "dashed", color = "red")+
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Date of Visit") +
  labs(title = "Daily interviews conducted in RingRoad SSH")+
  scale_x_continuous(breaks = unique(daily_summary$week))

# Print the plot
print(ag_p_day9)


# combining all plots
combined_plot <- grid.arrange(ag_p_day, ag_p_day1, ag_p_day2, ag_p_day3, ag_p_day4, ag_p_day5, ag_p_day6, ag_p_day7, ag_p_day8, ag_p_day9, ncol = 2)

# Print the combined plot
print(combined_plot)


















#map plotting

##Number of respondents across each ward
shapefile_folder <- "Ibadan_metro_ward_fiveLGAs"
shapefile <- st_read(dsn = shapefile_folder)
view(shapefile)
# Perform left join with filtered_shapefile
merged_data <- left_join(shapefile, df, by = "WardName")
view(merged_data)

ward_counts <- merged_data %>%
  group_by(WardName) %>%
  summarise(Wards_total = sum(State == "Ibadan (Oyo)"))
view(ward_counts)


# Plot the map
# Randomly select a subset of wards for labeling
num_selected_wards <- 18  # Number of wards to select
selected_wards <- sample(unique(ward_counts$WardName), num_selected_wards)

# Filter ward_counts data frame to include only selected wards
selected_ward_counts <- ward_counts[ward_counts$WardName %in% selected_wards, ]

# Plot with randomly selected wards as labels
ggplot(width = 20, height = 18) +
  geom_sf(data = ward_counts, aes(fill = Wards_total)) +
  geom_sf_text(data = selected_ward_counts, aes(label = WardName), size = 3, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey", name = "Number of pregnant women") +
  labs(title = "Number of Pregnant Women Across Wards") +
  theme_minimal() +
  coord_sf(expand = TRUE, xlim = range(ward_counts$geometry$x), ylim = range(ward_counts$geometry$y), lims_method = "geometry_bbox") +
  theme(axis.text = element_blank(),   # Remove axis text
        axis.title = element_blank())  # Remove axis titles






#proportion of positive cases by ward
# Calculate the proportion in percentages
ward_proportions <- merged_data %>%
  group_by(WardName) %>%
  summarise(total_cases = n(), 
            positive_cases = sum(q503__RESULT == "POSITIVE"),
            proportion_positive = (positive_cases / total_cases) * 100)

# Create categorical labels for the proportions based on groups of 10
# Define breaks and labels
breaks <- c(-Inf, seq(0, 90, by = 10), Inf)
labels <- c("0%", paste(seq(10, 90, by = 10), "-", seq(20, 100, by = 10), "%"), "100%")

# Apply cut function to create categorical_proportion groups
ward_proportions$categorical_proportion <- cut(ward_proportions$proportion_positive, 
                                               breaks = breaks,
                                               labels = labels,
                                               include.lowest = TRUE)



# Plot the map with color-coded proportions
# Randomly select a subset of wards for labeling
ggplot(data = ward_proportions) +
  geom_sf(aes(fill = categorical_proportion)) +
  scale_fill_manual(values = c("0%" = "white", "0 - 10 %" = "lightblue", "10 - 20 %" = "lightgreen", 
                               "20 - 30 %" = "yellow", "30 - 40 %" = "orange", "40 - 50 %" = "pink", 
                               "50 - 60 %" = "red", "60 - 70 %" = "purple", "70 - 80 %" = "blue", 
                               "80 - 90 %" = "darkblue", "90 - 100 %" = "black"),
                    na.value = "grey", 
                    name = "Proportion Positive") +
  labs(title = "Proportion of Positive Cases Across Wards in HFS") +
  theme_minimal() +
  coord_sf(expand = TRUE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
