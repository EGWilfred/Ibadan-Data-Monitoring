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
num_selected_wards <-  10 # Number of wards to select
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
breaks <- c(-Inf, seq(0, 100, by = 10))
labels <- c("0%", paste(seq(1, 100, by = 10), "-", seq(10, 100, by = 10), "%"))

# Apply cut function to create categorical_proportion groups
ward_proportions$categorical_proportion <- cut(ward_proportions$proportion_positive, 
                                               breaks = breaks,
                                               labels = labels,
                                               include.lowest = TRUE)
view(ward_proportions)
# Plot the map with color-coded proportions
# Randomly select a subset of wards for labeling
ggplot(data = ward_proportions) +
  geom_sf(aes(fill = categorical_proportion)) +
  geom_sf_text(aes(label = total_cases), size = 3, color = "black", nudge_x = 0, nudge_y = 0) +  
  scale_fill_manual(values = c("0%" = "white", "1 - 10 %" = "lightblue", "11 - 20 %" = "lightgreen", 
                               "21 - 30 %" = "yellow", "31 - 40 %" = "orange", "41 - 50 %" = "pink", 
                               "51 - 60 %" = "red", "61 - 70 %" = "purple", "71 - 80 %" = "blue", 
                               "81 - 90 %" = "darkblue", "91 - 100 %" = "black"),
                    na.value = "grey", 
                    name = "Proportion Positive") +
  labs(title = "Proportion of Positive Cases Across Wards in HFS") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())



