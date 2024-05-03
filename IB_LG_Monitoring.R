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
library(ggrepel)  

setwd("C:/Users/Dell Latitude/Documents/Longitudinal")

lg<-read_csv('UrbanMalariaLongitud_DATA_LABELS_2024-04-15_1506.csv')
lg1<- read_excel('Agugu and Basorun Description sheet.xlsx')
shapefile_folder <- "Ibadan_metro_ward_fiveLGAs"
shapefile <- st_read(dsn = shapefile_folder)

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



##plot the expected data
ward_counts <- lg1 %>%
  group_by(Ward, Settlement) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(ward_counts, aes(x = Ward, y = count, fill = Settlement)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(title = "Number of Settlement Types in Each Ward (Expected)",
       x = "Ward",
       y = "Number",
       fill = "Settlement Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##plot for the obtained
# Filter out NA values from the WARD variable
ward_counts <- lg %>%
  filter(!is.na(WARD)) %>%
  group_by(WARD, `SETTLEMENT TYPE`) %>%
  summarise(count = n()) %>%
  ungroup()
# Create the plot
ggplot(ward_counts, aes(x = WARD, y = count, fill = `SETTLEMENT TYPE`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(title = "Number of Children Surveyed by Ward and Settlement",
       x = "Ward",
       y = "Number",
       fill = "Settlement Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Overall Malaria prevalence rate 
baseline_data <- filter(lg, `Event Name` == "Baseline" & !is.na(`q705: RESULT`))
baseline_positive_results <- filter(baseline_data, `q705: RESULT` == "POSITIVE")
# Calculate the prevalence rate
prevalence_rate <- nrow(baseline_positive_results) / nrow(baseline_data) * 100

##Prevalence by ward
# Filter the data for baseline and non-missing results
#and create a summary table with proportions of positives and negatives across each ward
baseline_data <- filter(lg, `Event Name` == "Baseline" & !is.na(`q705: RESULT`) & !is.na(WARD))%>%
  group_by(`WARD`, `q705: RESULT`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
#proportion of results within each ward
ggplot(baseline_data, aes(x = WARD, y = percentage, fill = `q705: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "black") +  # Label each stack with percentage
  labs(title = "Proportion of Malaria Test Results in Each Ward",
       x = "Ward",
       y = "Percentage",
       fill = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_manual(values = c("POSITIVE" = "darkred", "NEGATIVE" = "darkgreen")) +  # Manually specify colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


##Prevalence by settlement types
# Filter the data for baseline and non-missing results
#and create a summary table with proportions of positives and negatives across each settlement
baseline_data1 <- filter(lg, `Event Name` == "Baseline" & !is.na(`q705: RESULT`) & !is.na(`SETTLEMENT TYPE`))%>%
  group_by(`SETTLEMENT TYPE`, `q705: RESULT`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
#proportion of results within each ward
ggplot(baseline_data1, aes(x = `SETTLEMENT TYPE`, y = percentage, fill = `q705: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "black") +  # Label each stack with percentage
  labs(title = "Proportion of Malaria Test Results in Each Settlement",
       x = "Settlement Type",
       y = "Percentage",
       fill = "Result") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_manual(values = c("POSITIVE" = "darkred", "NEGATIVE" = "darkgreen")) +  # Manually specify colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



##Number of interviews conducted weekly
#create week column
baseline_week <- filter(lg, `Event Name` == "Baseline" & !is.na(`Visit 1 Date...18`) & !is.na(`WARD`))
#create a week column
baseline_week$week <- week(baseline_week$`Visit 1 Date...18`)
# Adjust week numbers to start from Week 1
baseline_week$week <- baseline_week$week - min(baseline_week$week) + 1

# Group by week and Ward, and summarize total children
weekly_summary <- baseline_week %>%
  group_by(week, `WARD`) %>%
  summarise(total_patients = n())



# Create the plot
# all wards
ggplot(data = weekly_summary, aes(x = week, y = total_patients, color = `WARD`)) +
  geom_point(size = 4.0) +
  geom_line(linewidth = 1.0, aes(group = `WARD`)) +  # Connect points with lines
  geom_text(aes(label = total_patients), size = 3, vjust = -0.5, hjust = -0.5) +  # Label points with counts
  geom_hline(yintercept = c(120, 125), linetype = "dashed", color = c("#40E0D0", "red")) +  # Two separate dashed lines for each ward
  theme_minimal() +
  ylab("Number of Children") +
  xlab("Week of Survey") +
  labs(title = "Weekly Surveys Conducted in All Wards at Baseline") +
  scale_x_continuous(breaks = unique(weekly_summary$week))



##PERFORMANCE OF RA BY INTERVIEWS TARGET MET

# Create a Data Frame with Unique Households and Interviewers
unique_combinations <- lg %>%
  distinct(`Serial Number`, `INTERVIEWER'S NAME...15`)

# Determine RA Active Periods
lg_filled <- lg %>%
  mutate(Date = anytime(`Visit 1 Date...18`))

ra_active_period <- lg_filled %>%
  group_by(`INTERVIEWER'S NAME...15`) %>%
  summarise(start_date = min(Date), end_date = max(Date),
            total_days = as.numeric(difftime(max(Date), min(Date), units = "days")) +1)
#total number of days each RA worked
ra_total_days_worked <- lg_filled %>%
  filter(!is.na(`INTERVIEWER'S NAME...15`))%>%
  group_by(`INTERVIEWER'S NAME...15`) %>%
  summarise(total_days = n_distinct(Date))

#Plot of total number of days each RA has worked
ggplot(ra_total_days_worked, aes(x = total_days, y = `INTERVIEWER'S NAME...15`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.8, position = "dodge") +
  geom_text(aes(label = total_days), vjust = 0.5, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  labs(title = "Total Number of Days on Field",
       x = "Number",
       y = "Interviewer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 2))



# Count number of interviews conducted by each RA
unique_households <- lg_filled %>%
  filter(!is.na(`INTERVIEWER'S NAME...15`))%>%
  inner_join(ra_active_period, by = c("INTERVIEWER'S NAME...15" = "INTERVIEWER'S NAME...15")) %>%
  group_by(`INTERVIEWER'S NAME...15`) %>%
  summarise(unique_household_count = n_distinct(`Serial Number`))
view(unique_households)



#Plot of total interviews by RA
ggplot(unique_households, aes(x = unique_household_count, y = `INTERVIEWER'S NAME...15`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.8, position = "dodge") +
  geom_text(aes(label = unique_household_count), vjust = 0.5, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  labs(title = "Total interviews conducted per RA",
       x = "Number",
       y = "Interviewer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 2))




##check for replacement
extra_data1 <- anti_join(lg, lg1, by = "Serial Number")

##check for children not captured
extra_data <- anti_join(lg1, lg, by = "Serial Number")


##plot of EAs not captured at baseline
#define the map theme function
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}


#Agugu
#filter out bashorun and na values
agug <- extra_data%>%
  filter(Ward == "AGUGU" & !is.na(longitude) & !is.na(latitude))
#remove other wards from the shape file
df_ib_a <- shapefile %>%
  dplyr::filter(WardName == 'Agugu')
#define the coordinates
agu_d <- sf::st_as_sf(agug, coords=c("longitude", "latitude"), crs=4326)
# Perform spatial intersection
st_crs(agu_d) <- 4326
st_crs(df_ib_a) <- 4326
intersects_a <- st_intersection(agu_d, df_ib_a)
#plot
ggplot(df_ib_a)+
  geom_sf(fill = NA) +
  geom_point(data = intersects_a,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_a,
    aes(label =  `EA.Cluster`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward showing EAs not captured")+
  coord_sf()



#Bashorun
#filter out Agugu and na values
bash <- extra_data%>%
  filter(Ward == "BASHORUN" & !is.na(longitude) & !is.na(latitude))
#remove other wards from the shape file
df_ib_b <- shapefile %>%
  dplyr::filter(WardName == 'Bashorun')
#define the coordinates
bash_d <- sf::st_as_sf(bash, coords=c("longitude", "latitude"), crs=4326)
# Perform spatial intersection
st_crs(bash_d) <- 4326
st_crs(df_ib_b) <- 4326
intersects_b <- st_intersection(bash_d, df_ib_b)
#plot
ggplot(df_ib_b)+
  geom_sf(fill = NA) +
  geom_point(data = intersects_b,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_b,
    aes(label =  `EA.Cluster`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward showing EAs not captured")+
  coord_sf()




##plot of children replaced at baseline
#Agugu
#filter out bashorun and na values
agug1 <- extra_data1%>%
  filter(WARD == "Agugu" & !is.na(`HOUSEHOLD COORDINATE- Longitude...11`) & !is.na(`HOUSEHOLD COORDINATE- Latitude...12`))
#remove other wards from the shape file
df_ib_a1 <- shapefile %>%
  dplyr::filter(WardName == 'Agugu')
#define the coordinates
agu_d1 <- sf::st_as_sf(agug1, coords=c("HOUSEHOLD COORDINATE- Longitude...11", "HOUSEHOLD COORDINATE- Latitude...12"), crs=4326)
# Perform spatial intersection
st_crs(agu_d1) <- 4326
st_crs(df_ib_a1) <- 4326
intersects_a1 <- st_intersection(agu_d1, df_ib_a1)
#plot
ggplot(df_ib_a1)+
  geom_sf(fill = NA) +
  geom_point(data = intersects_a1,  aes(geometry = geometry, size = 2.0, col = SETTLEMENT.TYPE), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_a1,
    aes(label =  `ENUMERATION.AREA.CLUSTER.NUMBER`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Agugu Ward Showing Replaced EAs")+
  coord_sf()






#Bashorun
#Remove Agugu and missing values
bash1 <- extra_data1%>%
  filter(WARD == "Basorun" & !is.na(`HOUSEHOLD COORDINATE- Longitude...11`) & !is.na(`HOUSEHOLD COORDINATE- Latitude...12`))
#remove other wards from the shape file
df_ib_b1 <- shapefile %>%
  dplyr::filter(WardName == 'Bashorun')
#define the coordinates
bash_d1 <- sf::st_as_sf(bash1, coords=c("HOUSEHOLD COORDINATE- Longitude...11", "HOUSEHOLD COORDINATE- Latitude...12"), crs=4326)
# Perform spatial intersection
st_crs(bash_d1) <- 4326
st_crs(df_ib_b1) <- 4326
intersects_b1 <- st_intersection(bash_d1, df_ib_b1)
#plot
ggplot(df_ib_b1)+
  geom_sf(fill = NA) +
  geom_point(data = intersects_b1,  aes(geometry = geometry, size = 2.0, col = SETTLEMENT.TYPE), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = intersects_b1,
    aes(label =  `ENUMERATION.AREA.CLUSTER.NUMBER`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Bashorun Ward Showing Replaced EAs")+
  coord_sf()
