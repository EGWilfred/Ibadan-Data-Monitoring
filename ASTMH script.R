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

setwd("C:/Users/Dell Latitude/Desktop/UMTask/ASTMH")
#read data
wash<-read_csv('household_wash_index.csv')
wealth<-read_csv('household_wealth_index.csv')
all<-read_csv('all_malaria_data.csv')

names(wash)[which(names(wash) == "pca")] <- "pca_wash"
names(wealth)[which(names(wealth) == "pca")] <- "pca_wealth"





## PCA for hygiene
hygiene_data <- wash[, c("stagnant_water_nearby", "vessels_with_stagnant_water",
                                          "bushes_nearby", "open_drainages", "overgrown_vegetation",
                                          "dumpsite_nearby")]
pca_result <- prcomp(hygiene_data, center = TRUE, scale = TRUE)
#view the PCA output
pc_scores <- pca_result$x 

wash$pca_hygiene= pc_scores[,1]



# Join to add age, gender and test result
# Selecting the required columns from 'all' dataframe
selected_cols <- select(all, unique_id, age, agebin, gender, itn_presence, rdt_test_result)
# Joining the selected columns from 'all' dataframe to the 'wash' dataframe
wash <- wash %>%
  left_join(selected_cols, by = "unique_id")
# Join to add wealth pca
merged_table <- wash %>%
  left_join(select(wealth, unique_id, pca_wealth), by = "unique_id")




#recoding variables
# Define a function to convert numeric values to alphabets
convert_to_alphabets <- function(value) {
  ifelse(value == 1, "No", "Yes")
}

# Convert numeric values to alphabets for specified variables
transformed_df <- merged_table %>%
  mutate(
    stagnant_water_nearby = convert_to_alphabets(stagnant_water_nearby),
    vessels_with_stagnant_water = convert_to_alphabets(vessels_with_stagnant_water),
    bushes_nearby = convert_to_alphabets(bushes_nearby),
    open_drainages = convert_to_alphabets(open_drainages),
    overgrown_vegetation = convert_to_alphabets(overgrown_vegetation),
    dumpsite_nearby = convert_to_alphabets(dumpsite_nearby),
    final_drinking_water_source_code = ifelse(final_drinking_water_source_code == 1, "Improved water", "Unimproved water"),
    final_toilet_facility = ifelse(final_toilet_facility == 1, "Improved facility", "Unimproved facility")
  )



# Calculate the median of PCA_wealth values
median_pca <- median(transformed_df$pca_wealth, na.rm = TRUE)
# Recode pca_wealth values based on the median
transformed_df <- transformed_df %>%
  mutate(socio_economic_status = ifelse(pca_wealth <= median_pca, "Poor", "Non-Poor"))


# Calculate the median of PCA_hygiene values
median_pcah <- median(transformed_df$pca_hygiene, na.rm = TRUE)
# Recode pca_hygiene values based on the median
transformed_df <- transformed_df %>%
  mutate(hygiene_status = ifelse(pca_hygiene <= median_pcah, "Ineffective", "Effective"))



#transformed data
wash_ses <- transformed_df %>% 
  select(unique_id, serial_number, Ward, longitude, latitude, unique_id, settlement_type_new,
         age, gender, final_drinking_water_source_code, final_toilet_facility,
         hygiene_status, socio_economic_status, itn_presence, rdt_test_result)%>%
  distinct(unique_id, .keep_all = TRUE)
  
#remove duplicates




# a summary table

summary_table <- wash_ses %>%
  group_by(settlement_type_new) %>%
  summarise(
    total_wards = n(),                               # Total number of wards
    mean_age = mean(age, na.rm = TRUE),              # Mean age
    percent_male = mean(gender == "Male", na.rm = TRUE) * 100,  # Percentage of males
    percent_ITN_presence = mean(itn_presence =="Yes", na.rm = TRUE) * 100,  # Percentage of ITN presence
    percent_poor = mean(socio_economic_status == "Poor", na.rm = TRUE) * 100,  # Percentage of poor in socioeconomic status
    percent_positive_cases = mean(rdt_test_result == "POSITIVE", na.rm = TRUE) * 100  # Percentage of positive cases in result
  )





#Recoding rdt result to binomial

# Convert "NEGATIVE" to 1 and "POSITIVE" to 0 in the result column, filtering out "INDETERMINATE"
# Filter out rows with "INDETERMINATE" result
wash_ses <- filter(wash_ses, rdt_test_result != "INDETERMINATE")
# Convert "NEGATIVE" to 1 and "POSITIVE" to 0 in the result column
wash_ses$result <- ifelse(wash_ses$rdt_test_result == "NEGATIVE", 0, ifelse(wash_ses$rdt_test_result == "POSITIVE", 1, NA))





##logistic regression
# Fit logistic regression model with adjustment for age, gender, and ITN presence
model <- glm(result ~ final_toilet_facility + final_drinking_water_source_code + hygiene_status +
               age + gender + itn_presence,
             data = wash_ses, family = binomial)
# Obtain odds ratios and confidence intervals
summary(model)
# Extract coefficients and standard errors
coef_summary <- summary(model)$coefficients
# Calculate odds ratios
odds_ratios <- exp(coef_summary[, "Estimate"])
# Calculate confidence intervals for odds ratios
lower_ci <- exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"])
upper_ci <- exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])
#p value
summary_model <- summary(model)
p_values <- summary_model$coefficients[, "Pr(>|z|)"]

# Combine results into a data frame
results <- data.frame(
  Exposure = rownames(coef_summary),
  adjusted_Odds_Ratio = odds_ratios,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  P = p_values
)



##temporal
ggplot(results, aes(x = adjusted_Odds_Ratio, y = Exposure)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = Upper_CI, xmin = Lower_CI), size = .5, height =.2) +
  geom_point(size = 3.5) +
  #facet_wrap(~settlement_tpye_new)+ 
  xlab("odds ratio")+
  #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  theme_bw(base_size = 20, base_family = "") +labs(color ="")+
  theme(panel.grid.minor = element_blank(), )



#wash_red <- wash_ses %>%
 # filter(final_drinking_water_source_code != "Unimproved water",
  #       final_toilet_facility != "Unimproved facility",
   #      hygiene_status != "Ineffective")

#%reduction
# Convert WASH variables to a factor variable
# List of variables to convert to factors
wash_red<-wash_ses
wash_variables <- c("final_drinking_water_source_code", "final_toilet_facility", "hygiene_status")
# Convert variables to factor variables
wash_red[wash_variables] <- lapply(wash_red[wash_variables], factor)

# Change the reference level for final_drinking_water_source_code to "Improved water"
wash_red$final_drinking_water_source_code <- relevel(wash_red$final_drinking_water_source_code, ref = "Unimproved water")
# Change the reference level for hygiene_status to "Effective"
wash_red$hygiene_status <- relevel(wash_red$hygiene_status, ref = "Ineffective")
# Change the reference level for final_toilet_facility to "Improved facility"
wash_red$final_toilet_facility <- relevel(wash_red$final_toilet_facility, ref = "Unimproved facility")


model1 <- glm(result ~ final_toilet_facility + final_drinking_water_source_code + hygiene_status +
               age + gender + itn_presence,
             data = wash_red, family = binomial)
# Obtain odds ratios and confidence intervals
summary(model1)
# Extract coefficients and standard errors
coef_summary <- summary(model1)$coefficients
# Calculate odds ratios
odds_ratios1 <- exp(coef_summary[, "Estimate"])
#Pvalues
summary_model1 <- summary(model1)
p_values1 <- summary_model1$coefficients[, "Pr(>|z|)"]

percentage_reduction1 <- (1 - odds_ratios1) * 100
reduction <- data.frame(
  Exposure = rownames(coef_summary),
  Adjusted_OR = odds_ratios1,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  Percentage_Reduction = percentage_reduction1,
  p = p_values1
)





# Descriptive Analysis
# Calculate proportions of poor WASH practices by settlement type
# Calculate proportions of poor WASH practices by settlement type
wash_practices <- wash_ses %>%
  group_by(settlement_type_new) %>%
  summarise(poor_wash_prop = mean(final_drinking_water_source_code == "Unimproved water" |
                                    final_toilet_facility == "Unimproved facility" |
                                    hygiene_status == "Ineffective", na.rm = TRUE) * 100)

# Calculate malaria prevalence by settlement type
malaria_prevalence <- wash_ses %>%
  group_by(settlement_type_new) %>%
  summarise(malaria_prevalence = mean(rdt_test_result == "POSITIVE", na.rm = TRUE) * 100)







# Perform logistic regression analysis for formal settlement and poor socioeconomic class
wash1<-wash_ses%>%
  filter(!is.na(socio_economic_status))
model2 <- glm(result ~ final_drinking_water_source_code + final_toilet_facility + 
               hygiene_status + socio_economic_status + settlement_type_new,
             data = wash1, family = binomial)


summary(model2)
# Extract coefficients and standard errors
coef_summary <- summary(model2)$coefficients
# Calculate odds ratios
odds_ratios2 <- exp(coef_summary[, "Estimate"])
#Pvalues
summary_model2 <- summary(model2)
p_values2 <- summary_model1$coefficients[, "Pr(>|z|)"]

percentage_reduction2 <- (1 - odds_ratios2) * 100
reduction2 <- data.frame(
  Exposure = rownames(coef_summary),
  Adjusted_OR = odds_ratios2,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci,
  Percentage_Reduction = percentage_reduction2,
  p = p_values2
)

view(reduction2)












##proportion of malaria infection in formal settlements by their wash practices: rich and poor
wash_prop <- wash_ses %>%
  filter(rdt_test_result == "POSITIVE", 
         settlement_type_new == "Formal",
         !is.na(socio_economic_status)) %>%
  group_by(socio_economic_status, 
           final_drinking_water_source_code, 
           final_toilet_facility, 
           hygiene_status) %>%
  summarise(count = n()) %>%
  mutate(Proportion = count / sum(count) * 100)



##proportion of malaria infection in slum settlements by their wash practices: rich and poor
wash_prop1 <- wash_ses %>%
  filter(rdt_test_result == "POSITIVE", 
         settlement_type_new == "Slum",
         !is.na(socio_economic_status)) %>%
  group_by(socio_economic_status, 
           final_drinking_water_source_code, 
           final_toilet_facility, 
           hygiene_status) %>%
  summarise(count = n()) %>%
  mutate(Proportion = count / sum(count) * 100)
view(wash_prop1)










# Calculate proportions of results in different settlements
result_proportions <- wash_ses %>%
  filter(rdt_test_result != "Undeterminate")%>%
  group_by(settlement_type_new, rdt_test_result) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Bar plot of proportions with text labels
bar_plot <- ggplot(result_proportions, aes(x = settlement_type_new, y = Proportion, fill = rdt_test_result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Proportion), "%"), group = rdt_test_result), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "black") +  # Add text labels with percentage values
  labs(title = "Proportion of Results in Different Settlements",
       x = "Settlement Type",
       y = "Percentage") +
  scale_fill_manual(values = c("POSITIVE" = "red", "NEGATIVE" = "grey")) +  # Customize fill colors
  theme_minimal()











#number of interviews done per ward
unique_serial_counts <- malaria_cases %>%
  group_by(Ward) %>%
  summarise(unique_serial_count = n_distinct(`serial_number`))

ggplot(unique_serial_counts, aes(x = Ward, y = unique_serial_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = unique_serial_count), vjust = -0.5, size = 3) +
  labs(title = "Number of HouseHolds per Ward", x = "Ward", y = "Number") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#plot settlement types
unique_settlement <- malaria_cases %>%
  filter(settlement_type != "")%>%
  group_by(settlement_type) %>%
  summarise(unique_settlements = n_distinct(`serial_number`))

ggplot(unique_settlement, aes(x = settlement_type, y = unique_settlements)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = unique_settlements), vjust = -0.5, size = 3) +
  labs(title = "Number of HouseHolds per Settlement", x = "Settlement", y = "Number") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




