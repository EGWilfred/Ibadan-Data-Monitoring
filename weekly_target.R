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


