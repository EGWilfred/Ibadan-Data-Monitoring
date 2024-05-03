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
install.packages("leaflet")
library(leaflet)
install.packages("cowplot")
library(cowplot)
install.packages("ggtext")


setwd("C:/Users/Dell Latitude/Documents/all about Health facility")
df <- read_csv('HealthFacilty.csv')
hf <- read_csv('UrbanMalariaHFSIbada_DATA_LABELS_2024-04-12_1637 (1).csv')
#extracting new data for categorization
extra_data <- anti_join(hf, df, by = "Serial Number")
# save the extra data
write.csv(extra_data, "extra_data.csv", row.names = FALSE)




##Data cleaning
colnames(df) <- iconv(colnames(df), from = "UTF-8", to = "ASCII", sub = "byte")
# Replace any non-alphanumeric characters and trim spaces
colnames(df) <- gsub("[^a-zA-Z0-9]", "_", colnames(df))
colnames(df) <- trimws(colnames(df))



##check if number of pregnancies carried to term is greater than number of pregnancies
filtered_data <- df %>%
  filter(`q124c: How many pregnancies were carried to term?` > `q124b: How many pregnancies have you had`)

write.csv(filtered_data, "number of pregnancies discrepancy.csv", row.names = FALSE)

##check if age is same as date of birth
df <- df %>%
  mutate(
    date_of_birth = as.Date(`q102__Can_you_tell_us_your_date_of_birth_`, format = "%m/%d/%Y"),
    age_at_last_birthday = year(date_of_birth) - `q101__How_old_were_you_on_your_last_birthday__c2__a0_AGE_AT_LAST_BIRTHDAY__IN_YEARS_`
  )
age <- df %>%
  filter(age_at_last_birthday == 1)



# create a new column: eligibility of addresses
df <- df %>%
  mutate(Eligible = ifelse(WardName %in% c("Bashorun", "Agugu", "Olopomewa", "Challenge", "Outside", "Not found"), "No", "Yes"))
write.csv(df, "df.csv", row.names = FALSE)

#plot the number of eligibles in total
# First, calculate the counts of "Yes" and "No" in the Eligible column
eligible_counts <- table(df$Eligible)
# Convert the counts to a data frame
eligible_counts_df <- as.data.frame(eligible_counts)
names(eligible_counts_df) <- c("Eligible", "Count")
# Plot the counts
ggplot(eligible_counts_df, aes(x = Eligible, y = Count, fill = Eligible)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3) + # Add labels
  labs(title = "Total Number of Eligible data",
       x = "Eligible",
       y = "Number",
       fill = "Eligible") +
  theme_minimal()



#number of pregnant women in each health facility without target
ggplot(df, aes(x = `NAME_OF_HEALTH_FACILITY___5`, fill = Eligible)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(title = "Number of Eligible and Ineligible Wards by Health Facility",
       x = "Health Facility",
       y = "Number",
       fill = "Eligible") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#By health facility with target
# Define the target points for each health facility
targets <- data.frame(
  NAME_OF_HEALTH_FACILITY___5 = c("Adeoyo MTH", "Agbongbon PHC", "Jericho SH", "Naomi Medical Centre", "Oke Adu PHC", "Oniyanrin Comp HC", "ORANYAN PHC", "RingRoad SSH", "Alafara PHC", "Idi Ogungun PHC"),
  Target = c(329, 72, 186, 78, 26, 32, 215, 88, 84, 127)
)

# Merge df with targets based on the health facility names
df_with_targets <- merge(df, targets, by.x = "NAME_OF_HEALTH_FACILITY___5", by.y = "NAME_OF_HEALTH_FACILITY___5", all.x = TRUE)
# Plot the bar plot
ggplot(df_with_targets, aes(x = `NAME_OF_HEALTH_FACILITY___5`, fill = Eligible)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +
  # Plot the target points
  geom_point(aes(y = Target), color = "black", size = 3) +
  # Connect the points with lines
  geom_line(aes(y = Target, group = 1), color = "black") +  # Use group = 1 to draw a single line
  labs(title = "Number of Eligible and Ineligible Wards by Health Facility",
       x = "Health Facility",
       y = "Number",
       fill = "Eligible") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))







#weekly targets
#convert to date type
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
#create week column
df$week <- week(df$`Date`)
# Adjust week numbers to start from Week 1
df$week <- df$week - min(df$week) + 1

# Group by week and Ward, and summarize total children
weekly_summary <- df %>%
  group_by(week, `WARD`) %>%
  summarise(total_patients = n())

# Group by week and health facility name, and summarize total patients
weekly_summary <- df %>%
  group_by(week, `NAME_OF_HEALTH_FACILITY___5`) %>%
  summarise(total_patients = n())

##Number of interviews conducted weekly
# Create the plot
# all HF
all <- df %>%
  group_by(week) %>%
  summarise(total_patients = n())
p_week <- ggplot(data = all, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +  # Add a line connecting the points
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") +
  theme_minimal() +
  ylab("Number of Patients") +
  xlab("Week of Visit") +
  labs(title = "Weekly interviews conducted in All Health Facilities") +
  scale_x_continuous(breaks = unique(all$week))


#For Adeoyo
weekly_summaryy <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Adeoyo MTH")
ade_p_week <- ggplot(data = weekly_summaryy, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Adeoyo MTH")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#for Agbongbon PHC
weekly_summary1 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Agbongbon PHC")
agb_p_week <- ggplot(data = weekly_summary1, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 9, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Agbongbon PHC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Alafara
weekly_summary2 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Alafara PHC")
ala_p_week <- ggplot(data = weekly_summary2, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 9, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Alafara PHC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Idi Ogungun PHC
weekly_summary3 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Idi Ogungun PHC")
idi_p_week <- ggplot(data = weekly_summary3, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Idi Ogungun PHC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Jericho SH
weekly_summary4 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Jericho SH")
jer_p_week <- ggplot(data = weekly_summary4, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 18, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Jericho SH")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Naomi Medical Centre
weekly_summary5 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Naomi Medical Centre")
nao_p_week <- ggplot(data = weekly_summary5, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 7, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Naomi Medical Centre")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Oke Adu PHC
weekly_summary6 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Oke Adu PHC")
oke_p_week <- ggplot(data = weekly_summary6, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Oke Adu PHC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#Oniyanrin Comp HC
weekly_summary7 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "Oniyanrin Comp HC")
oni_p_week <- ggplot(data = weekly_summary7, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in Oniyanrin Comp HC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#ORANYAN PHC
weekly_summary8 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "ORANYAN PHC")
ora_p_week <- ggplot(data = weekly_summary8, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in ORANYAN PHC")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


#RingRoad SSH
weekly_summary9 <- weekly_summary %>%
  filter(`NAME_OF_HEALTH_FACILITY___5` == "RingRoad SSH")
rin_p_week <- ggplot(data = weekly_summary9, aes(x = week, y = total_patients)) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_line(col = "tomato1") +
  geom_hline(yintercept = 9, linetype = "dashed", color = "blue")+
  theme_minimal() +
  ylab("Number") +
  xlab("Week") +
  labs(title = "Weekly interviews conducted in RingRoad SSH")+
  scale_x_continuous(breaks = unique(weekly_summary$week))


# Combine the plots into a grid with unified axis labels
combined_plot <- plot_grid(
  rin_p_week + labs(title = "RingRoad SSH"),
  ora_p_week + labs(title = "ORANYAN PHC"),
  oni_p_week + labs(title = "Oniyanrin Comp HC"),
  oke_p_week + labs(title = "Oke Adu PHC"),
  nao_p_week + labs(title = "Naomi Medical Centre"),
  jer_p_week + labs(title = "Jericho SH"),
  idi_p_week + labs(title = "Idi Ogungun PHC"),
  ala_p_week + labs(title = "Alafara PHC"),
  agb_p_week + labs(title = "Agbongbon PHC"),
  ade_p_week + labs(title = "Adeoyo MTH"),
  ncol = 2
)












#map plotting

##Number of respondents across each ward
shapefile_folder <- "Ibadan_metro_ward_fiveLGAs"
shapefile <- st_read(dsn = shapefile_folder)

# Perform left join with filtered_shapefile
merged_data <- left_join(shapefile, df, by = "WardName")

#count interviews per ward
ward_counts_non_blank <- merged_data %>%
  filter(!is.na(`Serial Number`)) %>%
  group_by(WardName) %>%
  summarise(Wards_total = n())
ward_counts_blank <- merged_data %>%
  filter(is.na(`Serial Number`)) %>%
  group_by(WardName) %>%
  summarise(Wards_total = NA_integer_)
ward_counts <- bind_rows(ward_counts_non_blank, ward_counts_blank)


# Plot the map
# Randomly select a subset of wards for labeling
num_selected_wards <-  30 # Number of wards to select
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
            positive_cases = sum(`q503: RESULT` == "POSITIVE", na.rm = TRUE),
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



ggplot(data = ward_proportions) +
  geom_sf(aes(fill = categorical_proportion)) +
  geom_text(aes(label = total_cases), size = 3, color = "black") +  # Add labels for total_cases
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
