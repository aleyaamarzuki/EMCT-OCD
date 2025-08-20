library(tidyverse)
library(psych)
library(openxlsx)
library(dplyr)
library(reshape)
library(sjPlot)
library(ggcorrplot)
library(ggpubr)
library(car)
library(Hmisc)
library(PupillometryR)
library(ggpp)
library(ggdist)
library(gghalves)
library(cowplot)
library(viridis)
library(lubridate)
library(RColorBrewer)
library(forcats)
library(lme4)
library(sjPlot)
library(ggplot2)
library(broom.mixed)
library(blmeco)

set.seed(123)

# remove environment (optional)
rm(list = ls())

setwd('D:/Observational Study/BE EMA/')
set_theme(base = theme_bw()) # for plots

#### Read in data ####
OCI<-read_csv("OCI.csv")
TraitQ <- read_csv("TraitQ.csv")
EMA_data_new <- read_csv("EMA_data_new.csv")
task_data<-read_csv("task_data.csv")


#### Filter out repeat notifications (1 hour apart) ####

# filter out symptom log for now
symptom_log_only<- EMA_data_new[EMA_data_new$SymptomLog == 1, ]
EMA_withoutSymptomLog<- EMA_data_new[EMA_data_new$SymptomLog == 0, ]


# Ensure EMA_data_new is sorted by dataID and DateTime
EMA_withoutSymptomLog<- EMA_withoutSymptomLog %>%
  arrange(dataID, DateTime)

# Define a function to filter rows with repeat notifications within each group
filter_time_diff <- function(data) {
  # Initialize the result dataframe with the first row
  result <- data[1, ]
  
  # Iterate through the data starting from the second row
  for (i in 2:nrow(data)) {
    # Calculate the time difference with the last kept row
    time_diff <- difftime(data$DateTime[i], result$DateTime[nrow(result)], units = "hours")
    
    # Check if SymptomLog is 0 for the current row
    #if (data$SymptomLog[i] == 0) {
    # If the time difference is more than 1 hour, keep the row
    if (time_diff > 1) {
      result <- rbind(result, data[i, ])
    }
    #} else {
    # If SymptomLog is not 0, skip to the next row without updating result
    # next
  }
  
  return(result)
}

# Apply the function to each group
EMA_data_filtered <- EMA_withoutSymptomLog %>%
  group_by(dataID) %>%
  do(filter_time_diff(.))

# Ungroup the dataframe
EMA_data_filtered <- ungroup(EMA_data_filtered)


#### Add times of day to EMA file to aggregate later on ####

# number the days 

# Convert DateTime column to Date format
EMA_data_filtered <- EMA_data_filtered %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

# Filter out dates before and after study period
start_date <- as.POSIXct("2024-11-27", format = "%Y-%m-%d")
end_date <- as.POSIXct("2025-02-05", format = "%Y-%m-%d")
EMA_data_filtered <- EMA_data_filtered %>%
  filter(DateTime >= start_date & DateTime <= end_date)

# Calculate the day number
EMA_data_filtered <- EMA_data_filtered %>%
  group_by(dataID) %>%
  mutate(Day = as.integer(as.factor(as.Date(DateTime))))

#remove >14
EMA_data_filtered<-EMA_data_filtered[EMA_data_filtered$Day < 15,]


# count number of notifications per participant per day
notifs_per_day<-EMA_data_filtered %>%
  group_by(dataID, beID, Day) %>%
  summarise(Count = n(), .groups = 'drop')


#### Filter participants who completed less than 50% notifications ####

##### EMA #####

# Summarise the notifications answered per day per dataID
notifs_answered_perday <- EMA_data_filtered %>%
  group_by(dataID, Day) %>%
  summarise(Number_notifs_answered = n(), .groups = 'drop')

# Create a complete dataset with all dataID and Day combinations
all_days <- expand.grid(dataID = unique(EMA_data_filtered$dataID), 
                        Day = 1:14)

# Join the complete dataset with the summarised dataset
complete_data <- all_days %>%
  left_join(notifs_answered_perday, by = c("dataID", "Day")) %>%
  replace_na(list(Number_notifs_answered = 0))


# plot % answered per participant

complete_data$perc_answered <- ifelse(complete_data$Number_notifs_answered >= 4, 1,complete_data$Number_notifs_answered/4)

mean_notif_perSub <- complete_data %>%
  group_by(dataID) %>%
  summarise(Number_notifs_answered = mean(perc_answered))

to_remove<-mean_notif_perSub[mean_notif_perSub$Number_notifs_answered < 0.5,]

to_remove <- to_remove$dataID

##### Create histogram for paper  #####
mean_notif_perSub$bin <- ifelse(mean_notif_perSub$Number_notifs_answered < 0.5, "<0.5", "≥0.5")

ggplot(mean_notif_perSub, aes(x = Number_notifs_answered, fill = bin)) +
  theme_bw() +
  geom_histogram(color = "black", bins = 30) +  # You can adjust 'bins' as needed
  xlab("Completion Rate") + 
  ylab("Num. Participants") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  ggtitle(label = 'N Subjects Removed (<0.5 completion rate) = 38') +
  scale_fill_manual(values = c("<0.5" = "grey90", "≥0.5" = "slateblue4")) +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.position = "none"  # remove if you want the legend
  )

# remove filtered participants from main datasets
EMA_data_filtered<-EMA_data_filtered[!(EMA_data_filtered$dataID %in% to_remove),] #remove from EMA-only dataset
symptom_log_only<-symptom_log_only[!(symptom_log_only$dataID %in% to_remove),] #remove from symptom log dataset
task_data<-task_data[!(task_data$dataID %in% to_remove),] #remove from task dataset


# Calculate mean completion rate
mean_notif_filtered<-mean_notif_perSub[!(mean_notif_perSub$dataID %in% to_remove),]

mean(mean_notif_filtered$Number_notifs_answered)
sd(mean_notif_filtered$Number_notifs_answered)

# Calculate mean notifications answered per subject
notif_total<-aggregate(. ~ dataID, notifs_answered_perday, sum, na.rm = TRUE)

mean(notif_total$Number_notifs_answered)
sd(notif_total$Number_notifs_answered)

#####  Task #####
notifs_answered_perday <- task_data %>%
  group_by(dataID) %>%
  summarise(Number_notifs_answered = n(), .groups = 'drop')

#mean notifs answered (%) per subject
ggplot(notifs_answered_perday, aes(x = as.factor(dataID), y = Number_notifs_answered)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

# Calculate average number of games played per subject
notif_total<-aggregate(. ~ dataID, notifs_answered_perday, sum, na.rm = TRUE)

mean(notif_total$Number_notifs_answered)
sd(notif_total$Number_notifs_answered)

#### Add time of day to data ####

# create breaks
breaks <- lubridate::hour(hm("00:00", "06:00", "12:00", "16:00", "20:00", "23:00"))
#breaks <- hm("08:00", "12:00", "16:00", "20:00", "23:00")
# labels for the breaks
labels <- c("Late-Night", "Morning", "Afternoon", "Evening", "Night")

EMA_data_filtered$Time_of_day <- cut(x=hour(EMA_data_filtered$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

#how many are reported late night
length(which(EMA_data_filtered$Time_of_day == "Late-Night"))

# label days of the week
EMA_data_filtered$Day_of_week <- weekdays(EMA_data_filtered$DateTime)
#label weekend or weekday
EMA_data_filtered$dayType <- ifelse(EMA_data_filtered$Day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#### Label data based on which notification its associated with (1,2,3,4) ####

notif_times<-read_csv("Notif_times.csv")


#names(notif_times)[names(notif_times) == "?..Times"] <- "Times"

# Remove the double quotes and comma
df <- notif_times %>%
  mutate(Times = gsub('[",]', '', Times))

# Extract the day number
df <- df %>%
  mutate(Day = as.integer(sub("P(\\d+)DT.*", "\\1", Times)))

# Parse out the time
df <- df %>%
  mutate(Time = sub("P\\d+DT(.*)", "\\1", Times))

# Convert Time to POSIXct format
df <- df %>%
  mutate(Time = as.POSIXct(Time, format="%H:%M:%S"))

# Adjust time by subtracting 1 hour
#df <- df %>%
#  mutate(Time = Time - hours(1))

# Convert Time back to character format (optional)
#df <- df %>%
# mutate(Time = format(Time, "%H:%M:%S"))

# Create notif_num within each day
notifs <- df %>%
  group_by(Day) %>%
  arrange(Time) %>%
  mutate(notif_num = row_number())

notifs<- notifs[order(notifs$Day),]

# Function to find the matching notif_num
find_notif_num <- function(day, time, df) {
  # Filter df for the specific day
  df_day <- df %>% filter(Day == day)
  
  # Ensure df_day is sorted by time
  df_day <- df_day %>% arrange(Time)
  
  # Find the closest time in df_day that comes before the time in EMA_data_filtered
  closest_time <- NA
  for (i in 1:nrow(df_day)) {
    if (time >= df_day$Time[i]) {
      closest_time <- df_day$Time[i]
    } else {
      break
    }
  }
  
  # Check if time is within 2 hours after the closest time
  if (!is.na(closest_time) & difftime(time, closest_time, units = "hours") <= 2) {
    return(df_day$notif_num[df_day$Time == closest_time])
  }
  
  return(NA)  # Return NA if no match is found
}


# Create notif_num column in EMA_data_filtered
EMA_data_filtered <- EMA_data_filtered %>%
  mutate(Time = format(DateTime, "%H:%M:%S")) %>%
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>%
  mutate(notif_num = ifelse(Time >= as.POSIXct("07:00:00", format = "%H:%M:%S") & 
                              Time <= as.POSIXct("12:00:00", format = "%H:%M:%S"), 1, NA)) #automatically assign notif_num = 1 if notification answered before noon

# Assign notif_num based on proximity to times in df if not already assigned
EMA_data_filtered <- EMA_data_filtered %>%
  rowwise() %>%
  mutate(notif_num = ifelse(is.na(notif_num), find_notif_num(Day, Time, notifs), notif_num)) %>%
  ungroup() #If this is not working, check that there are not days outside of 1-14 in dataframe.


#### Conduct within-person z-scoring #####

EMA_data_filtered <-EMA_data_filtered %>% 
  group_by(dataID) %>% 
  mutate(zConfidence = scale(Confidence),
         zAnxiety = scale(Anxiety),
         zHappy =scale(Happy),
         zOCD = scale(OCD), 
         zSleep = scale(Sleep), 
         zThink_clear = scale(Think_clear),
         na.rm = TRUE)

task_data <-task_data %>% 
  group_by(dataID) %>% 
  mutate(zabs_evdiff = scale(abs_evdiff),
         zmeanConf = scale(meanConf),
         zconfCorr = scale(confCorr),
         zconfInc = scale (confInc),
         zmeanChoiceRT = scale(meanChoiceRT),
         zmeanConfRT = scale(meanChoiceRT),
         zConf_diff = scale(conf_diff),
         zAccuracy = scale(accuracy),
         z_d = scale(d),
         zmeta_d = scale(meta_d),
         zm_diff = scale(m_diff),
         zm_ratio = scale(m_ratio),
         na.rm = TRUE)


#### Process symptom log rows (add time-of-day and day-of-week) ####
symptom_log_filtered<-symptom_log_only

#symptom_log_filtered$DateTime <- dmy_hm(symptom_log_filtered$DateTime)

symptom_log_filtered <- symptom_log_only %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

start_date <- as.POSIXct("2024-11-27", format = "%Y-%m-%d")
end_date <- as.POSIXct("2025-02-05", format = "%Y-%m-%d")


symptom_log_filtered <-symptom_log_filtered %>%
  filter(DateTime >= start_date & DateTime <= end_date)


# add time of day to symptom logs

# create breaks
breaks <- lubridate::hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

symptom_log_filtered$Time_of_day <- cut(x=hour(symptom_log_filtered$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

# label days of the week
symptom_log_filtered$Day_of_week <- weekdays(symptom_log_filtered$DateTime)
#label weekend or weekday
symptom_log_filtered$dayType <- ifelse(symptom_log_filtered$Day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#Add these factors for combining data later

symptom_log_filtered$Day <- NA
symptom_log_filtered$Time <- NA
symptom_log_filtered$notif_num <- NA


# Calculate the day number
symptom_log_filtered <- symptom_log_filtered %>%
  group_by(dataID) %>%
  mutate(Day = as.integer(as.factor(as.Date(DateTime))))

#remove >14 days
symptom_log_filtered<-symptom_log_filtered[symptom_log_filtered$Day != 15,]

#### Process task data (add time-of-day and day-of-week) ####

task_data <- task_data %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

start_date <- as.POSIXct("2024-11-27", format = "%Y-%m-%d")
end_date <- as.POSIXct("2025-02-05", format = "%Y-%m-%d")


task_data <-task_data %>%
  filter(DateTime >= start_date & DateTime <= end_date)


# add time of day to task_data

# create breaks
breaks <- lubridate::hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

task_data$Time_of_day <- cut(x=hour(task_data$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

# label days of the week
task_data$Day_of_week <- weekdays(task_data$DateTime)
#label weekend or weekday
task_data$dayType <- ifelse(task_data$Day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#combine task_data and ema data

task_data$Day <- NA
task_data$Time <- NA
task_data$notif_num <- NA

# Calculate the day number
task_data <- task_data %>%
  group_by(dataID) %>%
  mutate(Day = as.integer(as.factor(as.Date(DateTime))))

#remove more than 14
task_data<-task_data[task_data$Day < 15,]

#combine task_data with ema_data_filtered
EMA_task<-dplyr::bind_rows(EMA_data_filtered, task_data) # bind rows while adding NAs for non-matching columns
EMA_task<-EMA_task[order(EMA_task$dataID, EMA_task$DateTime),]

#combine new symptom_log_filtered with ema_data_filtered
combined<-dplyr::bind_rows(EMA_task, symptom_log_filtered)
combined<-combined[order(combined$dataID, combined$DateTime),]

#### Plot symptom Logs ####

##### Time of Day #####

# show time of day when symptom logs are more prevalent
symLogs_perTime<-symptom_log_filtered %>%
  group_by(dataID, Time_of_day) %>%
  summarise(num_sympLogs_perTime = n())


# Calculate the mean and standard error for each dimension
num_symp_summary <- symLogs_perTime %>%
  group_by(Time_of_day) %>%
  summarise(
    mean_Score = mean(num_sympLogs_perTime, na.rm = TRUE),
    se_Score = sd(num_sympLogs_perTime, na.rm = TRUE) / sqrt(n())
  )

# Plot symptom logs by time of day (simple bar chart)
ggplot(num_symp_summary, aes(x=Time_of_day, y=mean_Score, fill=Time_of_day)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), width=0.2) +
  theme_bw() +
  labs(y = "Mean Num Symptom Logs", title = "Num. Symp Logs by Time of Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

# Plot symptom logs per time of day (more detailed)
ggplot(symLogs_perTime, aes(x=Time_of_day, y=num_sympLogs_perTime, fill=Time_of_day)) +
  geom_flat_violin(aes(fill = Time_of_day), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  geom_point(aes(x = Time_of_day, y = num_sympLogs_perTime, colour = Time_of_day), position = position_jitter(width = .01), size = 5, alpha = 0.5) +
  geom_boxplot(aes(x = Time_of_day, y = num_sympLogs_perTime, colour = Time_of_day), position = position_dodgenudge(width = 0.3, x = -.2), outlier.shape = NA, alpha = .5, width = .3, colour = "black") +
  geom_line(data = num_symp_summary, aes(x = Time_of_day, y = mean_Score, group = Time_of_day, colour = Time_of_day), linetype = 3, position = position_nudge(x = 0.1)) +
  geom_point(data = num_symp_summary, aes(x = Time_of_day, y = mean_Score, group = Time_of_day, colour = Time_of_day), shape = 18, position = position_nudge(x = 0.1)) +
  geom_errorbar(data = num_symp_summary, aes(x = Time_of_day, y = mean_Score, group = Time_of_day, colour = Time_of_day, ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), width = .05, position = position_nudge(x = 0.1)) +
  #scale_y_continuous(limits = c(40, 100)) +  
  labs(y = expression(paste ("Number of Symptoms Logged"))) + 
  labs(linetype = "Time_of_day", color = "Time_of_day", shape = "dataID") +
  theme_half_open() +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.title.x = element_blank(),
    legend.position = 'none'
  ) +
  ggtitle("Number of Symptoms Logged per Time") +
  theme(plot.title = element_text(size = 13, face = "bold"))


##### Day of week #####

# show day of week when symptom logs are more prevalent
symLogs_perDay<-symptom_log_filtered %>%
  group_by(dataID, Day_of_week) %>%
  summarise(num_sympLogs_perDay = n())


symLogs_perDay <- symLogs_perDay %>% arrange(ordered(Day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")))


# Calculate the mean and standard error for each dimension
num_symp_summary <- symLogs_perDay %>%
  group_by(Day_of_week) %>%
  summarise(
    mean_Score = mean(num_sympLogs_perDay, na.rm = TRUE),
    se_Score = sd(num_sympLogs_perDay, na.rm = TRUE) / sqrt(n())
  )

num_symp_summary <- num_symp_summary %>% arrange(ordered(Day_of_week, 
                                                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")))


# Plot symptom logs by day of week (simple bar chart)
num_symp_summary %>%
  ggplot(aes(x=Day_of_week, y=mean_Score, fill=Day_of_week)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), width=0.2) +
  theme_bw() +
  labs(y = "Num Symptom Logs", title = "Num. Symp Logs by Day of Week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12)
  )


ggplot(symLogs_perDay, aes(x= forcats::fct_inorder(Day_of_week), y=num_sympLogs_perDay, fill=Day_of_week)) +
  geom_flat_violin(aes(fill = Day_of_week), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  geom_point(aes(x = Day_of_week, y = num_sympLogs_perDay, colour = Day_of_week), position = position_jitter(width = .01), size = 5, alpha = 0.5) +
  geom_boxplot(aes(x = Day_of_week, y = num_sympLogs_perDay, colour = Day_of_week), position = position_dodgenudge(width = 0.3, x = -.2), outlier.shape = NA, alpha = .5, width = .3, colour = "black") +
  geom_line(data = num_symp_summary, aes(x = Day_of_week, y = mean_Score, group = Day_of_week, colour = Day_of_week), linetype = 3, position = position_nudge(x = 0.1)) +
  geom_point(data = num_symp_summary, aes(x = Day_of_week, y = mean_Score, group = Day_of_week, colour = Day_of_week), shape = 18, position = position_nudge(x = 0.1)) +
  geom_errorbar(data = num_symp_summary, aes(x = Day_of_week, y = mean_Score, group = Day_of_week, colour = Day_of_week, ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), width = .05, position = position_nudge(x = 0.1)) +
  labs(y = expression(paste ("Number of Symptoms Logged"))) + 
  labs(linetype = "Time_of_day", color = "Time_of_day", shape = "dataID") +
  theme_half_open() +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.title.x = element_blank(),
    legend.position = 'none'
  ) +
  ggtitle("Number of Symptoms Logged per Day") +
  theme(plot.title = element_text(size = 13, face = "bold"))

#### Statistical Test for Time of Day for Symptom Logs ####
library(rstatix)
symLogs_perTime <- symLogs_perTime %>%
  convert_as_factor(dataID, Time_of_day)

# need to ungroup to get anova to work
symLogs_perTime<-symLogs_perTime%>%ungroup() 

# time of day main ANOVA
res.aov <- anova_test(data = symLogs_perTime, 
                      dv = num_sympLogs_perTime, wid = dataID, 
                      within = Time_of_day)
get_anova_table(res.aov)

#posthoc tests
pwc <- symLogs_perTime %>%
  pairwise_t_test(
    num_sympLogs_perTime ~ Time_of_day, 
    p.adjust.method = "bonferroni"
  )
pwc

#### Statistical Test for Day of Week for Symptom Logs ####
library(rstatix)
symLogs_perDay <- symLogs_perDay %>%
  convert_as_factor(dataID, Day_of_week)

# need to ungroup to get anova to work
symLogs_perDay<-symLogs_perDay%>%ungroup() 

# time of day main ANOVA
res.aov <- anova_test(data = symLogs_perDay, 
                      dv = num_sympLogs_perDay, wid = dataID, 
                      within = Day_of_week)
get_anova_table(res.aov)

#posthoc tests
pwc <- symLogs_perDay %>%
  pairwise_t_test(
    num_sympLogs_perDay ~ Day_of_week, 
    p.adjust.method = "bonferroni"
  )
pwc


#### Quantify variability ####

# for EMA only
# Create an empty list to store results
rmssd_list <- list()
mssd_list <- list()

# Specify the variable names
variables <- c("zConfidence", "zAnxiety", "zHappy", "zOCD", "zThink_clear")

# Calculate RMSSD for each variable
for (var in variables) {
  rmssd_list[[var]] <- rmssd(EMA_data_filtered[[var]], group = EMA_data_filtered$dataID, lag = 1, na.rm = TRUE)
}

# Calculate MSSD for each variable
for (var in variables) {
  mssd_list[[var]] <- mssd(EMA_data_filtered[[var]], group = EMA_data_filtered$dataID, lag = 1, na.rm = TRUE)
}

# for task only
rmssd_list_task <- list()
mssd_list_task <- list()

# Specify the variable names
variables <- c("zabs_evdiff", "zmeanConf", "zconfCorr", "zconfInc", 
               "zmeanChoiceRT", "zmeanConfRT", "zConf_diff", "zAccuracy",
               "z_d", "zmeta_d", "zm_diff","zm_ratio")

# Calculate RMSSD for each variable
for (var in variables) {
  rmssd_list_task[[var]] <- rmssd(task_data[[var]], group = task_data$dataID, lag = 1, na.rm = TRUE)
}

# Calculate MSSD for each variable
for (var in variables) {
  mssd_list_task[[var]] <- mssd(task_data[[var]], group = task_data$dataID, lag = 1, na.rm = TRUE)
}

# for sleep only
sleep_only <- EMA_data_filtered %>%
  filter(!is.na(zSleep))

sleep_rmssd<-as.data.frame(rmssd(sleep_only[["zSleep"]], group = sleep_only$dataID, lag = 1, na.rm = TRUE))

sleep_mssd<-as.data.frame(mssd(sleep_only[["zSleep"]], group = sleep_only$dataID, lag = 1, na.rm = TRUE))

sleep_rmssd<-cbind(sleep_rmssd,unique(sleep_only$dataID))
sleep_mssd<-cbind(sleep_mssd,unique(sleep_only$dataID))

# make colname consistent with EMA dataset
names(sleep_rmssd)[names(sleep_rmssd) == 'unique(sleep_only$dataID)'] <- 'dataID'
names(sleep_mssd)[names(sleep_mssd) == 'unique(sleep_only$dataID)'] <- 'dataID'

names(sleep_rmssd)[names(sleep_rmssd) == 'V1'] <- 'sleeprmssd'
names(sleep_mssd)[names(sleep_mssd) == 'V1'] <- 'sleepmssd'

# get averages of EMA variables
summary_ema<-EMA_data_filtered %>%
  group_by(dataID, beID) %>%
  summarise(Confidence = mean(Confidence, na.rm = TRUE),
            Anxiety = mean(Anxiety, na.rm = TRUE),
            Happy = mean(Happy, na.rm = TRUE),
            OCD = mean(OCD, na.rm = TRUE),
            Sleep = mean (Sleep, na.rm = TRUE),
            Think_clear = mean(Think_clear, na.rm = TRUE))


summary_sympLog<-symptom_log_filtered %>%
  group_by(dataID, beID) %>%
  summarise(Symptom_count = sum(SymptomLog))



summary_ema_task<-task_data %>%
  group_by(dataID) %>%
  summarise(abs_evdiff = mean(abs_evdiff, na.rm = TRUE),
            meanConf = mean(meanConf, na.rm = TRUE),
            confCorr = mean(confCorr, na.rm = TRUE),
            confInc = mean(confInc, na.rm = TRUE),
            meanChoiceRT = mean(meanChoiceRT,  na.rm = TRUE),
            meanConfRT = mean(meanConfRT,  na.rm = TRUE), 
            conf_diff = mean(conf_diff, na.rm = TRUE),
            accuracy = mean(accuracy, na.rm = TRUE),
            d = mean(d, na.rm = TRUE),
            meta_d = mean(meta_d, na.rm = TRUE),
            m_diff = mean(m_diff, na.rm = TRUE),
            m_ratio = mean(m_ratio, na.rm = TRUE))



#Filter traitQ and OCI data using summary EMA

summary_ema_OCI<- merge (summary_ema, OCI, by = 'dataID')
summary_ema_OCI_traitQ <- merge(summary_ema_OCI, TraitQ, by = 'dataID', all.x = TRUE)
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, sleep_rmssd, by = 'dataID', all.x = TRUE) # sleep rmssd
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, sleep_mssd, by = 'dataID', all.x = TRUE) # sleep mssd
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, summary_ema_task, by = 'dataID', all.x = TRUE) 
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, summary_sympLog, by = 'dataID', all.x = TRUE) 

#order by orderID
summary_ema_OCI_traitQ<-summary_ema_OCI_traitQ[order(summary_ema_OCI_traitQ$dataID),]

# add the variability (rmssd) variables to datafile for correlations
# ema vars
rsmssd_table <- unique(EMA_data_filtered$dataID)
rmssd_table_ema<-as.data.frame(cbind(rsmssd_table, rmssd_list[[1]], rmssd_list[[2]], 
                                     rmssd_list[[3]], rmssd_list[[4]], rmssd_list[[5]], mssd_list[[1]],
                                     mssd_list[[2]], mssd_list[[3]], mssd_list[[4]], mssd_list[[5]]))

oldnames = c("rsmssd_table", "V2","V3","V4","V5","V6","V7","V8","V9","V10","V11")
newnames = c("dataID","rmssdConfidence","rmssdAnxiety", "rmssdHappy", "rmssdOCD", "rmssdThink_clear",
             "mssdConfidence","mssdAnxiety", "mssdHappy", "mssdOCD", "mssdThink_clear")

rmssd_table_ema<-rmssd_table_ema %>% rename_at(vars(oldnames), ~ newnames)

#task vars
rsmssd_table <- unique(task_data$dataID)
rmssd_table_task<-as.data.frame(cbind(rsmssd_table, rmssd_list_task[[1]], rmssd_list_task[[2]], 
                                      rmssd_list_task[[3]], rmssd_list_task[[4]], rmssd_list_task[[5]],rmssd_list_task[[6]],
                                      rmssd_list_task[[7]],rmssd_list_task[[8]],rmssd_list_task[[9]], rmssd_list_task[[10]],
                                      rmssd_list_task[[11]],rmssd_list_task[[12]],
                                      mssd_list_task[[1]], mssd_list_task[[2]], mssd_list_task[[3]], mssd_list_task[[4]],
                                      mssd_list_task[[5]], mssd_list_task[[6]], mssd_list_task[[7]], mssd_list_task[[8]],
                                      mssd_list_task[[9]], mssd_list_task[[10]],
                                      mssd_list_task[[11]],mssd_list_task[[12]]))

oldnames = c("rsmssd_table","V2","V3","V4","V5","V6","V7","V8","V9",
             "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17",
             "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25")
newnames = c("dataID","rmssdabs_evdiff","rmssdmeanConf", "rmssdconfCorr", "rmssdconfInc", 
             "rmssdmeanChoiceRT", "rmssdmeanConfRT", "rmssdConf_diff", "rmssd_accuracy","rmssd_d","rmssd_meta_d",
             "rmssd_m_diff", "rmssd_m_ratio",
             "mssdabs_evdiff","mssdmeanConf", "mssdconfCorr", "mssdconfInc", "mssdmeanChoiceRT", "mssdmeanConfRT", 
             "mssdConf_diff", "mssd_accuracy","mssd_d","mssd_meta_d",
             "mssd_m_diff", "mssd_m_ratio")


rmssd_table_task<-rmssd_table_task %>% rename_at(vars(oldnames), ~ newnames)

summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, rmssd_table_ema, by = 'dataID', all.x = TRUE) 

summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, rmssd_table_task, by = 'dataID', all.x = TRUE) 

#merge with number notifs completed
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, mean_notif_perSub, by = 'dataID', all.x = TRUE) 


#### Plot Distributions of Trait Data ####

##### Histograms #####

colnames(summary_ema_OCI_traitQ) <- make.unique(names(summary_ema_OCI_traitQ)) # to make histograms work

# Calculate mean and standard deviation
mean_oci <- mean(summary_ema_OCI_traitQ$OCI_total)
sd_oci <- sd(summary_ema_OCI_traitQ$OCI_total)


##### OCI Plot for paper #####
oci<-ggplot(summary_ema_OCI_traitQ, aes(x=OCI_total)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkorange2") + xlab ("OCI-R Total") + ylab("Density")+
  ggtitle (label = 'OCI-R Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15)
  )+
  # Add mean and standard deviation annotations
  annotate("text", x = mean_oci+1, y = 0.057, 
           label = paste("Mean:", round(mean_oci, 2)), vjust = -1, color = "black", size = 5) +
  annotate("text", x = mean_oci+1, y = 0.054, 
           label = paste("SD:", round(sd_oci, 2)), vjust = -1, color = "black", size = 5)

#tiff(file="D:/BE Pilot/changedPlots/oci_hist.tiff",width = 15, height = 8, units = "in", res = 300)
oci
#dev.off()


#STAI
stai<-ggplot(summary_ema_OCI_traitQ, aes(x=stai_sum)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkseagreen2") + xlab ("STAI") + ylab("")+
  ggtitle (label = 'STAI-T Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

#tiff(file="D:/BE Pilot/changedPlots/stai_hist.tiff",width = 15, height = 8, units = "in", res = 300)
#stai
# dev.off()


#PHQ
phq<-ggplot(summary_ema_OCI_traitQ, aes(x=phq_sum)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkseagreen2") + xlab ("PHQ") + ylab("")+
  ggtitle (label = 'PHQ Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

#tiff(file="D:/BE Pilot/changedPlots/phq_hist.tiff",width = 15, height = 8, units = "in", res = 300)
#phq
# dev.off()

#IUS
ius<-ggplot(summary_ema_OCI_traitQ, aes(x=ius_sum)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkseagreen2") + xlab ("IUS-F1") + ylab("")+
  ggtitle (label = 'IUS-F1 Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

# #tiff(file="D:/BE Pilot/changedPlots/ius1_hist.tiff",width = 15, height = 8, units = "in", res = 300)
# ius1
# dev.off()
# 


#BIS
bis<-ggplot(summary_ema_OCI_traitQ, aes(x=bis_sum)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkseagreen2") + xlab ("BIS") + ylab("")+
  ggtitle (label = 'BIS Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

#tiff(file="D:/BE Pilot/changedPlots/bis_hist.tiff",width = 15, height = 8, units = "in", res = 300)
# bis
# dev.off()


#CAOIC
caoic<-ggplot(summary_ema_OCI_traitQ, aes(x=caoic_sum)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="darkseagreen2") + xlab ("Cog Functioning") + ylab("")+
  ggtitle (label = 'CAOIC Scores of all Participants')+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

#tiff(file="D:/BE Pilot/changedPlots/caoic_hist.tiff",width = 15, height = 8, units = "in", res = 300)
#caoic
# dev.off()

#ses
freq_table <- as.data.frame(table(summary_ema_OCI_traitQ$ses_1))
colnames(freq_table) <- c("Number", "Frequency")

ses<-ggplot(freq_table, aes(x = Number, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = Frequency), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency Distribution of SES", 
       x = "Rung on McArthur Ladder", 
       y = "Frequency")+
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )

#tiff(file="D:/BE Pilot/changedPlots/ses_hist.tiff",width = 15, height = 8, units = "in", res = 300)
#ses
# dev.off()

##### Bar graphs of OCI dimensions #####

# Convert the data to long format
OCI_long <- gather(summary_ema_OCI, Dimension, Score, Hoarding:Checking, factor_key=TRUE)
OCI_long$Score <- as.double(OCI_long$Score)

# Calculate the mean and standard error for each dimension
OCI_summary <- OCI_long %>%
  group_by(Dimension) %>%
  summarise(
    mean_Score = mean(Score, na.rm = TRUE),
    se_Score = sd(Score, na.rm = TRUE) / sqrt(n())
  )

# Plot the data with ggplot2
ggplot(OCI_summary, aes(x=Dimension, y=mean_Score, fill=Dimension)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), width=0.2) +
  theme_bw() +
  labs(y = "Mean Score", title = "Mean Score by OC Dimension") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Find average time difference between symptom logs (a measure of density) ####

#doing this across all logs instead of by day because many people dont have enough logs
# Sort by dataID and DateTime
symptom_log_filtered2 <- symptom_log_filtered %>%
  arrange(dataID, DateTime)

# Calculate the time difference between consecutive rows for each subject
symptom_log_filtered2 <- symptom_log_filtered2 %>%
  group_by(dataID) %>%
  mutate(TimeDiff = difftime(DateTime, lag(DateTime), units = "hours"))

# Remove the first row of each group which has NA for TimeDiff
symptom_log_filtered2 <- symptom_log_filtered2 %>%
  filter(!is.na(TimeDiff))

# Calculate the mean time difference per day per subject
mean_diff_per_subject <- symptom_log_filtered2 %>%
  group_by(dataID) %>%
  summarise(MeanTimeDiff = mean(TimeDiff))

# add column to main dataframe
summary_ema_OCI_traitQ <- merge(summary_ema_OCI_traitQ, mean_diff_per_subject, by = "dataID", all.x = TRUE)

summary_ema_OCI_traitQ$MeanTimeDiff <-as.double(summary_ema_OCI_traitQ$MeanTimeDiff)

summary_ema_OCI_traitQ$MeanTimeDiff[is.na(summary_ema_OCI_traitQ$MeanTimeDiff)] <- NaN #convert NAs to NaN so it can be processed by rcorr later


#### Plotting trajectories ####

##### EMA #####
EMA_data_filtered <- EMA_data_filtered %>%
  mutate(Day_of_week = factor(Day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) 

# for day of week
EMA_data_filtered$day_of_week_num<-as.integer(factor(EMA_data_filtered$Day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')))

weekday_labels <- c("Mon", "Tues", "Weds", "Thurs", "Fri", "Sat", "Sun")

# for time of day
EMA_data_filtered$time_of_day_num<-as.integer(factor(EMA_data_filtered$Time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night", "Late-Night")))

time_labels <- c("Morning", "Afternoon", "Evening", "Night", "Late-Night")

##### Sleep #####
EMA_data_filtered  %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zSleep, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Sleep Quality") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)


##### Confidence #####

#across labelled time of day

EMA_data_filtered_2<-EMA_data_filtered %>%  filter(time_of_day_num!=5)

conf_time<-EMA_data_filtered_2  %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zConfidence, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Time of Day", 
       y="Self-Confidence") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text( size=18), legend.position = "none")+
  scale_x_discrete(labels = time_labels)


#across 14 days
EMA_data_filtered  %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zConfidence, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Self-Confidence") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)



##### Anxiety #####

#across labelled time of day

EMA_data_filtered  %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zAnxiety, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Anxiety Score") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=14), legend.position = "none")+
  scale_x_discrete(labels = time_labels)

#across 14 days
EMA_data_filtered  %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zAnxiety, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Anxiety Severity") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)

##### OCD Severity #####

#across labelled time of day
EMA_data_filtered_2<-EMA_data_filtered %>%  filter(time_of_day_num!=5)

ocd_time<-EMA_data_filtered_2 %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zOCD, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Time of Day", 
       y="OCD Severity") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(size=18), legend.position = "none")+
  scale_x_discrete(labels = time_labels)

#across 14 days
ocd_day<-EMA_data_filtered  %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zOCD, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekday 
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="OCD Severity") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)


##### Happiness #####

#across labelled time of day
EMA_data_filtered  %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zHappy, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Happiness Score") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=14), legend.position = "none")+
  scale_x_discrete(labels = time_labels)


#across 14 days
EMA_data_filtered %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zHappy, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Happiness") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)



##### Difficulty thinking clearly (Brain fog) #####

#across labelled time of day
EMA_data_filtered  %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zThink_clear, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Difficulty Thinking Clearly Score") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=14), legend.position = "none")+
  scale_x_discrete(labels = time_labels)


#across 14 days
EMA_data_filtered %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zThink_clear, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Difficulty Thinking Clearly") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)

#### Statistical tests for day of week effect on state variables ####

library(rstatix)

##### OCD Severity #####

new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = c("Day_of_week","dayType"))



model<-lmerTest::lmer(zOCD~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work +
                        (1 | dataID), data = new)
vif(model) #variance inflation - dropped Sunday
confint(model) #confidence intervals

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))


#with slopes (model not converging)
model<-lmerTest::lmer(zOCD~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + 
                        (Day_of_week_Monday | dataID) + (Day_of_week_Tuesday | dataID) + (Day_of_week_Wednesday | dataID) + 
                        (Day_of_week_Thursday | dataID) + (Day_of_week_Friday | dataID) + (Day_of_week_Saturday | dataID), data = new) 

#controlling for gender and age
model<-lmerTest::lmer(zOCD~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday
                      + work + zAge + Gender_2 +
                        (1 | dataID), data = new)

vif(model)
confint(model)

qqnorm(resid(model))
qqline(resid(model))


##### Self-confidence #####
model<-lmerTest::lmer(zConfidence~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work +
                        (1 | dataID), data = new)
vif(model)
confint(model)

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#with slopes (converged)
model<-lmerTest::lmer(zConfidence~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + 
                        (Day_of_week_Monday | dataID) + (Day_of_week_Tuesday | dataID) + (Day_of_week_Wednesday | dataID) + 
                        (Day_of_week_Thursday | dataID) + (Day_of_week_Friday | dataID) + (Day_of_week_Saturday | dataID), data = new)
vif(model)
confint(model)

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#controlling for age and gender
model<-lmerTest::lmer(zConfidence~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work + zAge + Gender_2 +
                        (1 | dataID), data = new)
vif(model)
confint(model)

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))


##### Day of Week effects on other state variables #####

model<-lmerTest::lmer(zThink_clear~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + 
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zAnxiety~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zHappy~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work +  
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zSleep~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + work + 
                        (1 | dataID), data = new)

plot_model(m, type = "est", title = "Day of Week Predicting EMA",
           show.values = TRUE, show.p = TRUE, value.offset = .3, vline.color = "black") 


#### Statistical tests for time of day effect on state variables ####


##### All times in one model #####
new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = "Time_of_day")

#ocd
model<-lmerTest::lmer(zOCD~ Time_of_day_Morning + Time_of_day_Afternoon + 
                        Time_of_day_Evening + Time_of_day_Night +  
                        (1 | dataID), data = new)
vif(model) #VIF is very high for all variables
confint(model)

qqnorm(resid(model))
qqline(resid(model))

#confidence
model<-lmerTest::lmer(zConfidence~ Time_of_day_Morning + Time_of_day_Afternoon + 
                        Time_of_day_Evening + Time_of_day_Night + 
                        (1 | dataID), data = new)
vif(model) #VIF is very high for all variables
confint(model)

qqnorm(resid(model))
qqline(resid(model))

##### Times in separate models to avoid trading off #####

#OCD

model<-lmerTest::lmer(zOCD~ Time_of_day_Morning +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Afternoon +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Evening +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Night +
                        (1 | dataID), data = new)

#for ocd with slopes
model<-lmerTest::lmer(zOCD~ Time_of_day_Morning +
                        (Time_of_day_Morning | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Afternoon +
                        (Time_of_day_Afternoon | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Evening +
                        (Time_of_day_Evening | dataID), data = new)

#night model failed to converge
model<-lmerTest::lmer(zOCD~Time_of_day_Night +
                        (Time_of_day_Night | dataID), data = new)

#for ocd with age and gender
model<-lmerTest::lmer(zOCD~ Time_of_day_Morning + zAge + Gender_2 +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Afternoon + zAge + Gender_2 + 
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Evening + zAge + Gender_2 +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zOCD~Time_of_day_Night + zAge + Gender_2 +
                        (1 | dataID), data = new)


#Confidence
model<-lmerTest::lmer(zConfidence~ Time_of_day_Morning +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Afternoon +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Evening +
                        (1 | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Night +
                        (1 | dataID), data = new)

#for confidence with slopes
model<-lmerTest::lmer(zConfidence~ Time_of_day_Morning +
                        (Time_of_day_Morning | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Afternoon +
                        (Time_of_day_Afternoon | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Evening +
                        (Time_of_day_Evening | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Night +
                        (Time_of_day_Night | dataID), data = new)

#for confidence with age and gender
model<-lmerTest::lmer(zConfidence~ Time_of_day_Morning +  zAge + Gender_2 +
                        (Time_of_day_Morning | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Afternoon + zAge + Gender_2 +
                        (Time_of_day_Afternoon | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Evening + zAge + Gender_2 +
                        (Time_of_day_Evening | dataID), data = new)

model<-lmerTest::lmer(zConfidence~Time_of_day_Night + zAge + Gender_2 +
                        (Time_of_day_Night | dataID), data = new)


#### Plotting Distribution of mean state measures ####
colnames(summary_ema_OCI_traitQ) <- make.unique(names(summary_ema_OCI_traitQ)) # to make histograms work

##### Symptom Log Counts #####

#filter out people who did not answer any symptom logs
high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

high_symp <- subset(summary_ema_OCI_traitQ, dataID %in% highSymp_id)

#barplot

symplog_count<-high_symp %>% count(Symptom_count)

ggplot(as.data.frame(symplog_count), aes(factor(Symptom_count), n)) +     
  geom_col(color="black", fill="tan1", position = 'dodge')+
  xlab ("Symptoms Logged") + ylab("Counts")+
  theme_bw()+
  ggtitle (label = 'Symptoms Logged Per Participant; N = 118 with >= 1 symptom log')+
  font("xlab", size = 15)+
  font("ylab", size = 15)+
  font("title", size = 15)+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

##### Other States #####

ggplot(summary_ema_OCI_traitQ, aes(x=Confidence)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Conf EMA") + ylab("Density")+
  ggtitle (label = 'EMA Confidence Score Per Participant')


ggplot(summary_ema_OCI_traitQ, aes(x=OCD)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean OCD EMA") + ylab("Density")+
  ggtitle (label = 'OCD Score Per Participant')


ggplot(summary_ema_OCI_traitQ, aes(x=Anxiety)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Anxiety EMA") + ylab("Density")+
  ggtitle (label = 'Anxiety Score Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=Happy)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Happiness EMA") + ylab("Density")+
  ggtitle (label = 'Happy Score Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=Think_clear)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Diff. Thinking EMA") + ylab("Density")+
  ggtitle (label = 'Brain Fog Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=Sleep)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Sleep EMA") + ylab("Density")+
  ggtitle (label = 'Sleep Score Per Participant')


#### Spearman Correlation Plots ####

##### Trait vs State #####

# averaged EMA data against trait data
forCorr = subset(summary_ema_OCI_traitQ, 
                 select = c(Symptom_count, Confidence, Anxiety, Happy, OCD, Sleep, 
                            Think_clear, OCI_total, Hoarding, Neutral, Obsessing, Ordering,
                            Checking, stai_sum, 
                            phq_sum, ius_sum,
                            ses_1, bis_sum, caoic_sum, rosenberg_sum ,abs_evdiff, meanConf, confCorr, confInc, meanChoiceRT, meanConfRT,
                            accuracy, conf_diff, d, meta_d, m_diff, m_ratio,
                            MeanTimeDiff, Number_notifs_answered))

correlations <- rcorr(as.matrix(forCorr), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("Confidence", "Anxiety", "Happy", "OCD", "Sleep", "Think_clear"), 
                   c("OCI_total", "stai_sum", 
                     "phq_sum", "ius_sum", "bis_sum", "caoic_sum", "rosenberg_sum")]
corr_all_p<-correlations$P
corr_all_p<-corr_all_p[c("Confidence", "Anxiety", "Happy", "OCD", "Sleep", "Think_clear"),  
                       c("OCI_total", "stai_sum", 
                         "phq_sum", "ius_sum", "bis_sum", "caoic_sum", "rosenberg_sum")]


colnames(corr_all) <- c("Total OCI-R", "STAI-T",
                        "PHQ-9", "IUS", "BIS", "CAIOC-13", "Rosenberg Self-Esteem")
rownames(corr_all) <- c("Self-confidence", "Anxiety", "Happiness", "OCD", "Sleep Quality", "Brain Fog")


colnames(corr_all_p) <- c("Total OCI-R", "STAI-T",
                          "PHQ-9", "IUS", "BIS", "CAIOC-13", "Rosenberg Self-Esteem")
rownames(corr_all_p) <- c("Self-confidence", "Anxiety", "Happiness", "OCD", "Sleep Quality", "Brain Fog")


#Plot state-trait correlations
ggcorrplot(corr_all, method = "circle", p.mat = corr_all_p, insig = "pch")+
  theme(
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15,angle = 90)
  ) +
  theme(
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size=15)
  )+
  labs(x = "Mean State", y = "Mean Trait")

#correct p-values

#for rosenberg
p.adjust(corr_all_p[1:6,ncol(corr_all_p)], method="fdr")

#for caioc-13
p.adjust(corr_all_p[1:6,ncol(corr_all_p)-1], method="fdr")

#for oci
p.adjust(corr_all_p[1:6,1], method="fdr")


##### partial correlations for state-trait correlations #####

# partialling out the effects of mean state variables aside from OCD and confidence

library(ppcor)


#partial correlations for confidence and rosenberg 

pcor_results<-list()

dvList<-c("Anxiety", "OCD", "Sleep", "Think_clear", "Happy")

for (m in 1:length(dvList)) {
  
  partCorr <- summary_ema_OCI_traitQ[, c("Confidence", dvList[[m]], "rosenberg_sum")]
  
  
  pcor_results[[m]] <- pcor(partCorr, method = "spearman")
  
}

#partial correlations for ocd and rosenberg 

pcor_results<-list()

dvList<-c("Anxiety", "Confidence", "Sleep", "Think_clear", "Happy")

for (m in 1:length(dvList)) {
  
  partCorr <- summary_ema_OCI_traitQ[, c("OCD", dvList[[m]], "rosenberg_sum")]
  
  
  pcor_results[[m]] <- pcor(partCorr, method = "spearman")
  
}


#partial correlations for confidence and caioc
pcor_results<-list()

dvList<-c("Anxiety", "OCD", "Sleep", "Think_clear", "Happy")

for (m in 1:length(dvList)) {
  
  partCorr <- summary_ema_OCI_traitQ[, c("Confidence", dvList[[m]], "caoic_sum")]
  
  
  pcor_results[[m]] <- pcor(partCorr, method = "spearman")
  
}


#partial correlations for ocd and caioc
pcor_results<-list()

dvList<-c("Anxiety", "Confidence", "Sleep", "Think_clear", "Happy")

for (m in 1:length(dvList)) {
  
  partCorr <- summary_ema_OCI_traitQ[, c("OCD", dvList[[m]], "caoic_sum")]
  
  
  pcor_results[[m]] <- pcor(partCorr, method = "spearman")
  
}

#partial correlations for ocd and oci

pcor_results<-list()

dvList<-c("Anxiety", "Confidence", "Sleep", "Think_clear", "Happy")

for (m in 1:length(dvList)) {
  
  partCorr <- summary_ema_OCI_traitQ[, c("OCD", dvList[[m]], "OCI_total")]
  
  
  pcor_results[[m]] <- pcor(partCorr, method = "spearman")
  
}


##### Plots of important State-Trait relationships #####

# correlating confidence with caioc

plot <- ggscatter(forCorr, x = "caoic_sum", y = "Confidence",
                  color = "black", fill = "mediumpurple2", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  ylab = "Mean State Self-Confidence", 
                  xlab = "Trait Perceived Functional Impairment (CAIOC-13)", 
                  title = "")+
  # title = "a) Self-Confidence (state) and Functional Impairment Associated with OCD (trait)") +
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 14)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

# confidence with rosenberg self-esteem

plot<-ggscatter(forCorr, x = "rosenberg_sum", y = "Confidence",color = "black", fill = "salmon",
                shape = 21, stroke = 1, size = 3,
                add = "reg.line",                                 # Add regression line
                conf.int = TRUE,                                  # Add confidence interval
                add.params = list(color = "black",
                                  fill = "lightgray"), 
                ylab = "Mean State Self-Confidence", 
                xlab = "Trait Rosenberg Self-Esteem Scale", title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 7)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

#caioc-sum and OCD severity

plot <- ggscatter(forCorr, x = "caoic_sum", y = "OCD",
                  color = "black", fill = "salmon", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  ylab = "Mean State OCD Severity", 
                  xlab = "Trait Perceived Functional Impairment (CAIOC-13)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  font("title", size = 15)+
  coord_fixed(ratio = 10)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

# ocd severity with rosenberg
plot<-ggscatter(forCorr, x = "rosenberg_sum", y = "OCD",color = "black", fill = "mediumpurple2",
                shape = 21, stroke = 1, size = 3,
                add = "reg.line",                                 # Add regression line
                conf.int = TRUE,                                  # Add confidence interval
                add.params = list(color = "black",
                                  fill = "lightgray"), 
                ylab = "Mean State OCD Severity", 
                xlab = "Trait Rosenberg Self-Esteem Scale", title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  font("title", size = 15)+
  coord_fixed(ratio = 5)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

#state confidence and ocd
plot<-ggscatter(forCorr, x = "OCD", y = "Confidence",color = "black", fill = "mediumpurple2",
                shape = 21, stroke = 1, size = 3,
                add = "reg.line",                                 # Add regression line
                conf.int = TRUE,                                  # Add confidence interval
                add.params = list(color = "black",
                                  fill = "lightgray"), 
                ylab = "Mean State Self-Confidence", 
                xlab = "Mean State OCD Severity", title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  font("title", size = 15)+
  coord_fixed(ratio = 0.8)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )


plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 


#state ocd and oci
plot<-ggscatter(forCorr, x = "OCI_total", y = "OCD",color = "black", fill = "salmon",
                shape = 21, stroke = 1, size = 3,
                add = "reg.line",                                 # Add regression line
                conf.int = TRUE,                                  # Add confidence interval
                add.params = list(color = "black",
                                  fill = "lightgray"), 
                ylab = "Mean State OCD Severity", 
                xlab = "Trait OCD Score (OCI-R)", title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  font("title", size = 15)+
  coord_fixed(ratio = 11)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )


plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 



##### State vs State #####

#mean EMA
forCorr = subset(summary_ema_OCI_traitQ, 
                 select = c(Symptom_count, OCD, Confidence, Anxiety, Happy,
                            Sleep, Think_clear, MeanTimeDiff, abs_evdiff, 
                            meanConf, confCorr, confInc, meanChoiceRT,
                            meanConfRT, conf_diff, accuracy, d, meta_d, m_diff, m_ratio, Number_notifs_answered))


#forCorr  <- forCorr  %>% replace(is.na(.), NaN)


correlations <- rcorr(as.matrix(forCorr), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("OCD", "Confidence", "Anxiety", 
                     "Happy", "Sleep", "Think_clear"), 
                   c("OCD", "Confidence", "Anxiety", 
                     "Happy", "Sleep", "Think_clear")]


corr_all_p<-correlations$P

#adjust p-value for mean time diff between logs (density) and states
p.adjust(corr_all_p[8, 2:7], method="fdr")


corr_all_p<-corr_all_p[c("OCD", "Confidence", "Anxiety", 
                         "Happy", "Sleep", "Think_clear"), 
                       c("OCD", "Confidence", "Anxiety", 
                         "Happy", "Sleep", "Think_clear")]

corr_all_p <- corr_all_p %>% replace(is.na(.), 0)


colnames(corr_all) <- c("OCD", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog")
rownames(corr_all) <- c("OCD", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog")

colnames(corr_all_p) <- c("OCD", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog")
rownames(corr_all_p) <- c("OCD", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog")

#make correlations only on bottom shown
ggcorrplot(corr_all, method = "circle",  type = "lower", p.mat = corr_all_p, insig = "pch")+ 
  theme(
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size=15)
  ) 

#correct p-values

#for ocd vs other states
p.adjust(corr_all_p[1, 2:6], method="fdr")



##### Plot of number of symptoms logged against Metacognitive bias #####

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

forCorr <- subset(summary_ema_OCI_traitQ, dataID %in% highSymp_id)

plot<-ggscatter(forCorr, x = "meanConf", y = "Symptom_count",color = "black", fill = "lightblue4",
                shape = 23, stroke = 1, size = 3,
                add = "reg.line",                                 # Add regression line
                conf.int = TRUE,                                  # Add confidence interval
                add.params = list(color = "black",
                                  fill = "lightgray"), 
                ylab = "Symptoms Logged", 
                xlab = "Mean Metacognitive Bias", title = "Number of Symptoms Logged against Mean Meta-Cognitive Bias")+
  font("xlab", size = 15)+
  font("ylab", size = 15)+
  font("title", size = 15)+
  coord_fixed(ratio = 0.4)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )



plot + stat_cor(method = "spearman",
                cor.coef.name = c("rho"), size = 5)


##### overall plot of mean time difference (frequency) #####

#mean EMA
forCorr = subset(summary_ema_OCI_traitQ, 
                 select = c(Symptom_count, OCD, Confidence, Anxiety, Happy,
                            Sleep, Think_clear, MeanTimeDiff, abs_evdiff, 
                            meanConf, confCorr, confInc, meanChoiceRT,
                            meanConfRT, conf_diff, accuracy, d, meta_d, m_diff, m_ratio, Number_notifs_answered))


#forCorr  <- forCorr  %>% replace(is.na(.), NaN)


correlations <- rcorr(as.matrix(forCorr), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("OCD", "Confidence", "Anxiety", 
                     "Happy", "Sleep", "Think_clear", "MeanTimeDiff"), 
                   c("OCD", "Confidence", "Anxiety", 
                     "Happy", "Sleep", "Think_clear", "MeanTimeDiff")]


corr_all_p<-correlations$P

#adjust p-value for mean time diff between logs (density) and states
p.adjust(corr_all_p[8, 2:7], method="fdr")


corr_all_p<-corr_all_p[c("OCD", "Confidence", "Anxiety", 
                         "Happy", "Sleep", "Think_clear", "MeanTimeDiff"), 
                       c("OCD", "Confidence", "Anxiety", 
                         "Happy", "Sleep", "Think_clear", "MeanTimeDiff")]

corr_all_p <- corr_all_p %>% replace(is.na(.), 0)


colnames(corr_all) <- c("OCD Severity Rating", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog", "Inter-symptom Interval")
rownames(corr_all) <- c("OCD Severity Rating", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog", "Inter-symptom Interval")

colnames(corr_all_p) <- c("OCD Severity Rating", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog", "Inter-symptom Interval")
rownames(corr_all_p) <- c("OCD Severity Rating", "Self-confidence", "Anxiety", "Happiness",  "Sleep Quality", "Brain Fog", "Inter-symptom Interval")

#make correlations only on bottom shown
ggcorrplot(corr_all, method = "circle",  type = "lower", p.mat = corr_all_p, insig = "pch")+ 
  theme(
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size=15)
  ) 

ggcorrplot(corr_all, method = "circle",  type = "lower")+ 
  theme(
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size=15)
  ) 

#correct p-values

#for ocd vs other states
p.adjust(corr_all_p[1, 2:6], method="fdr")


##### Correlate mean time difference between symptom logs with other measures #####

forCorr = subset(summary_ema_OCI_traitQ, 
                 select = c(Symptom_count, Confidence, Anxiety, Happy, OCD, Sleep, 
                            Think_clear, OCI_total, Hoarding, Neutral, Obsessing, Ordering,
                            Checking, stai_sum, 
                            phq_sum, ius_sum,
                            ses_1, bis_sum, caoic_sum, rosenberg_sum ,abs_evdiff, meanConf, confCorr, confInc, meanChoiceRT, meanConfRT,
                            accuracy, conf_diff, d, meta_d, m_diff, m_ratio,
                            MeanTimeDiff, Number_notifs_answered))


plot <- ggscatter(forCorr, x = "Confidence", y = "MeanTimeDiff",
                  color = "black", fill = "salmon", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  xlab = "Mean State Self-Confidence", 
                  ylab = "Mean Inter-symptom Interval (Minutes)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 0.008)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

plot <- ggscatter(forCorr, x = "Happy", y = "MeanTimeDiff",
                  color = "black", fill = "salmon", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  xlab = "Mean State Happiness", 
                  ylab = "Mean Inter-symptom Interval (Minutes)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 0.007)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )



plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 


plot <- ggscatter(forCorr, x = "Anxiety", y = "MeanTimeDiff",
                  color = "black", fill = "mediumpurple2", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  xlab = "Mean State Anxiety", 
                  ylab = "Mean Inter-symptom Interval (Minutes)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 0.009)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 


plot <- ggscatter(forCorr, x = "OCD", y = "MeanTimeDiff",
                  color = "black", fill = "mediumpurple2", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  xlab = "Mean State OCD Severity", 
                  ylab = "Mean Inter-symptom Interva (Minutes)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  coord_fixed(ratio = 0.01)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 


plot <- ggscatter(forCorr, x = "phq_sum", y = "MeanTimeDiff",
                  color = "black", fill = "mediumpurple2", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  xlab = "Trait Depression (PHQ-9)", 
                  ylab = "Mean Time Between Symptom Logs (Minutes)", 
                  title = "")+
  font("xlab", size = 20)+
  font("ylab", size = 20)+
  #coord_fixed(ratio = 14)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

plot + stat_cor(method = "spearman", cor.coef.name = c("rho"), size = 7, aes(label = ..r.label..)) 

#adjust p-value for mean time diff between logs (density) and traits

corr_all_p<-corr_all_p[c("MeanTimeDiff"),  
                       c("OCI_total", "stai_sum", 
                         "phq_sum", "ius_sum", "bis_sum", "caoic_sum", "rosenberg_sum")]


p.adjust(corr_all_p, method="fdr") #actually phq-9 no longer significant


##### Correlate metacognitive bias with state and trait data #####

# averaged EMA data against trait data
forCorr = subset(summary_ema_OCI_traitQ, 
                 select = c(Symptom_count, Confidence, Anxiety, Happy, OCD, Sleep, 
                            Think_clear, OCI_total, Hoarding, Neutral, Obsessing, Ordering,
                            Checking, stai_sum, 
                            phq_sum, ius_sum,
                            ses_1, bis_sum, caoic_sum, rosenberg_sum ,abs_evdiff, meanConf, confCorr, confInc, meanChoiceRT, meanConfRT,
                            accuracy, conf_diff, d, meta_d, m_diff, m_ratio,
                            MeanTimeDiff, Number_notifs_answered))

correlations <- rcorr(as.matrix(forCorr), type=c("spearman"))
sig_corr <- correlations$P<.05
corr_all<-correlations$r
corr_all<-corr_all[c("meanConf"), 
                   c("OCI_total", "stai_sum", 
                     "phq_sum", "ius_sum", "bis_sum", "caoic_sum", "rosenberg_sum","OCD", "Confidence", "Anxiety", 
                     "Happy", "Sleep", "Think_clear")]
corr_all_p<-correlations$P
corr_all_p<-corr_all_p[c("meanConf"),  
                       c("OCI_total", "stai_sum", 
                         "phq_sum", "ius_sum", "bis_sum", "caoic_sum", "rosenberg_sum","OCD", "Confidence", "Anxiety", 
                         "Happy", "Sleep", "Think_clear")]


corr_all_p #check for any sig corr (none)


#### Match task data to state EMA data ####

library(data.table)

# Make copies of DateTime column
EMA_data_filtered$dateTime_EMA <- EMA_data_filtered$DateTime

# Convert to data.tables
new <- data.table(EMA_data_filtered)
task<-data.table(task_data)
# Set keys including dataID and DateTime
setkey(new, dataID, DateTime)
setkey(task, dataID, DateTime)

# Perform the join within each dataID
EMA_task <- new[task, roll = "nearest"]
EMA_task<-EMA_task[order(EMA_task$dataID, EMA_task$DateTime),]

#remove duplicates (tasks that are matched to EMAs that already have a task matched to it)

#EMA_task<-EMA_task[!duplicated(EMA_task$dateTime_EMA)]


#just to check time matchings
compare = subset(EMA_task, 
                 select = c(times_ID, DateTime, dataID, dateTime_EMA))

# take out rows where the task is not done within one hour of the EMA

EMA_task$mins_apart<-with(EMA_task, difftime(DateTime,dateTime_EMA,units="mins"))

EMA_task$mins_apart<-abs(EMA_task$mins_apart)

EMA_task<-EMA_task[EMA_task$mins_apart <= 240,] #match state to task within 4 hour window


# merge ema rows not matched to task with task matched rows

EMA_task_matching_full <- EMA_task %>%
  dplyr::select(...1:dateTime_EMA) # get columns that match EMA_data_filtered (now called 'new') 

EMA_task_matching_full$DateTime<-EMA_task_matching_full$dateTime_EMA #currently the date-times in EMA_task correspond to the task. Add back the date times from the EMA
EMA_task_matching_full<-EMA_task_matching_full[order(EMA_task_matching_full$dataID, EMA_task_matching_full$DateTime),]

ema_without_taskRows = setdiff(new, EMA_task_matching_full) #get EMA rows that don't match the task ones

#combine ema_without_taskRows with EMA_task

EMA_task_new<-dplyr::bind_rows(ema_without_taskRows,EMA_task)
EMA_task_new<-EMA_task_new[order(EMA_task_new$dataID, EMA_task_new$DateTime),]


##### Plot distribution of task measures #####

ggplot(summary_ema_OCI_traitQ, aes(x=abs_evdiff)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Signal Strength") + ylab("Density")+
  ggtitle (label = 'Signal Strength Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=meanConf)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Mean Task Confidence") + ylab("Density")+
  ggtitle (label = 'Metacognitive bias Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=accuracy)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Staircased Accuracy") + ylab("Density")+
  ggtitle (label = 'Staircased Accuracy Per Participant')

ggplot(summary_ema_OCI_traitQ, aes(x=m_ratio)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Meta-d'/d'") + ylab("Density")+
  ggtitle (label = "Meta-d'/d'Per Participant")

ggplot(summary_ema_OCI_traitQ, aes(x=meanChoiceRT)) + theme_bw() +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+
  geom_density(alpha=.2, fill="lightpink") + xlab ("Choice RT") + ylab("Density")+
  ggtitle (label = "Choice RT Per Participant")


#### Plot overall trajectory of task measures ####

task_data_new <-EMA_task %>%
  group_by(dataID) %>%
  dplyr::mutate( notif_num=row_number())


task_data_new<-task_data_new[task_data_new$notif_num < 9,]

#abs ev diff (signal strength)
task_data_new %>% group_by(dataID, notif_num) %>%
  summarise(zMeanState = mean(zabs_evdiff, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.numeric(notif_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Notification Number", 
       y="Absolute Ev Diff") +
  ylim(-2, 2)+
  theme_classic(base_size=24) + 
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")

#confidence (metacognitive bias)
task_data_new %>% group_by(dataID, notif_num) %>%
  summarise(zMeanState = mean(zmeanConf, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.numeric(notif_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Notification Number", 
       y="Mean Confidence") +
  theme_classic(base_size=24) + 
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")

#meanChoiceRT
task_data_new %>% group_by(dataID, notif_num) %>%
  summarise(zMeanState = mean(zmeanChoiceRT, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.numeric(notif_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Notification Number", 
       y="Mean Confidence RT") +
  theme_classic(base_size=24) + 
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")


#meanConfRT
task_data_new %>% group_by(dataID, notif_num) %>%
  summarise(zMeanState = mean(zmeanConfRT, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.numeric(notif_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Notification Number", 
       y="Mean Confidence RT") +
  theme_classic(base_size=24) + 
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")

#meta-d'/d'
task_data_new %>% group_by(dataID, notif_num) %>%
  summarise(zMeanState = mean(zm_ratio, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays (1=thurs as first day in the study; 7=weds)
  mutate(d = as.numeric(notif_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(notif_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Notification Number", 
       y="Meta-d'/d'") +
  theme_classic(base_size=24) + 
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")


##### Check metacognitive bias trajectory by time of day and day of week ######
task_data <- task_data %>%
  mutate(Day_of_week = factor(Day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) 

# for day of week
task_data$day_of_week_num<-as.integer(factor(task_data$Day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')))

weekday_labels <- c("Mon", "Tues", "Weds", "Thurs", "Fri", "Sat", "Sun")

# for time of day
task_data$time_of_day_num<-as.integer(factor(task_data$Time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night", "Late-Night")))

time_labels <- c("Morning", "Afternoon", "Evening", "Night", "Late-Night")

#task mean conf

#across labelled time of day

task_data2<-task_data %>%  filter(time_of_day_num!=5)
task_data2<-task_data2 %>%  filter(time_of_day_num!=4)

task_data2  %>% group_by(dataID, time_of_day_num) %>%
  summarise(zMeanState = mean(zmeanConf, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(time_of_day_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(time_of_day_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Meta-Cognitive Bias") +
  theme_classic(base_size=24) + 
  ylim(-1.0, 1.0)+
  theme(axis.text.x = element_text( size=18), legend.position = "none")+
  scale_x_discrete(labels = time_labels)


#across 14 days
task_data2   %>% group_by(dataID, day_of_week_num) %>%
  summarise(zMeanState = mean(zmeanConf, na.rm=T)) %>% ungroup() %>%
  # create a numeric vector for the weekdays day_of_week_num
  mutate(d = as.factor(day_of_week_num))%>%
  ggplot(aes(x = d, y=zMeanState)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="point",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  stat_summary_bin(aes(group=dataID, color=dataID), color="darkgray",
                   fun.data=mean_se, geom="line",size=0.6, alpha=0.6,
                   position=position_dodge(width=0.1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="errorbar",size=1.25, alpha=0.8) +
  stat_summary_bin(aes(x=as.numeric(day_of_week_num)), fun.data=mean_se, geom="line",size=1.25, alpha=0.8) +
  labs(x="Day", 
       y="Meta-Cognitive Bias") +
  theme_classic(base_size=24) + 
  ylim(-1.5, 1.5)+
  theme(axis.text.x = element_text(angle = 30, size=18), legend.position = "none")+
  scale_x_discrete(labels = weekday_labels)



##### Statistical Test for day of week effects on metacognitive bias #####

new<-fastDummies::dummy_cols(task_data, select_columns = c("Day_of_week","dayType"))


model<-lmerTest::lmer(zmeanConf~ Day_of_week_Monday + Day_of_week_Tuesday + 
                        Day_of_week_Wednesday + Day_of_week_Thursday +  
                        Day_of_week_Friday + Day_of_week_Saturday + Day_of_week_Sunday + 
                        (1 | dataID), data = new)

vif(model)

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))


##### checking time of day #####

new<-fastDummies::dummy_cols(task_data, select_columns = c("Time_of_day"))

model<-lmerTest::lmer(zmeanConf~ Time_of_day_Afternoon + Time_of_day_Morning + Time_of_day_Evening +
                        (1 | dataID), data = new)
vif(model)

model<-lmerTest::lmer(zmeanConf~ Time_of_day_Afternoon + 
                        (1 | dataID), data = new)
vif(model)

model<-lmerTest::lmer(zmeanConf~ Time_of_day_Morning + 
                        (1 | dataID), data = new)
vif(model)

model<-lmerTest::lmer(zmeanConf~ Time_of_day_Evening + 
                        (1 | dataID), data = new)
vif(model)

#### Rough check of autocorrelations ####

# Lagging taking into account the nested structure
# by grouping by day, makes sure that only autocorrelations within single day are accounted for (i.e., lag value for 1st notif of the day is always NA)
df <- EMA_data_filtered %>%
  group_by(dataID, Day) %>%
  mutate(across(c(zConfidence:zThink_clear), ~lag(.), .names = "{.col}lag")) %>% # Lag the variables
  ungroup()

df_task <- EMA_task %>%
  group_by(dataID) %>%
  mutate(across(c(zmeanConf,zm_ratio), ~lag(.), .names = "{.col}lag")) %>% # Lag the variables
  ungroup()

# for looking at effects of zsleep on zOCD per day and vice versa
df_sleep<-EMA_data_filtered %>%
  group_by(dataID, Day) %>%
  summarise(meanzOCD = mean(zOCD, na.rm = TRUE),
            meanzAnxiety = mean(zAnxiety, na.rm = TRUE),
            meanzHappy = mean(zHappy, na.rm = TRUE),
            sleep_by_day = mean(zSleep, na.rm = TRUE))

df_sleep <- df_sleep %>%
  mutate(
    lagged_meanzOCD = lag(meanzOCD, 1),
    lagged_meanzAnxiety = lag(meanzAnxiety, 1),
    lagged_meanzHappy = lag(meanzHappy, 1),
    lagged_sleep_by_day = lag(sleep_by_day, 1)
  )

# Replace NA with NaN in specified columns
df <- df %>%
  mutate(across(zConfidencelag:zThink_clearlag, ~ replace_na(., NaN)))

df_task <- df_task %>%
  mutate(across(zmeanConf:zm_ratio, ~ replace_na(., NaN)))


df_sleep <- df_sleep %>%
  mutate(across(meanzOCD:lagged_sleep_by_day, ~ replace_na(., NaN)))


# function for autocorr scatterplot

autocorr_plot <- function(data, var1, var2, print) {
  
  # Scatter plot of PA1 and PA1lag
  x = data[[var1]]
  y = data[[var2]]
  
  p <- data %>%
    ggplot(aes(x = .data[[var1]], y = .data[[var2]])) +
    geom_point(color='black', fill='orange', shape=21, size = 2) +
    theme_bw() + 
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    annotate("text", x = -1.5, y = 3, 
             label = paste("Corr. =",round(cor(x, y, use="complete"),3), "\nP-value =", round(cor.test(x, y, use = "complete")$p.value,3)), 
             size = 4)+
    theme(
      plot.title = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text = element_text(size = 12)
    )
  
  print(p)
  
  if (print == TRUE) {
    ggsave(paste0("Corrplot_", var1, "_", var2, ".png"), p)
  }
  
  
}

##### autocorr, effects on OCD #####

# OCD and confidence t + 1 
autocorr_plot(df, "zConfidencelag", "zOCD", print = TRUE)

# confidence and confidence t + 1 
autocorr_plot(df, "zConfidencelag", "zConfidence", print = TRUE)

# OCD and OCD t + 1 
autocorr_plot(df, "zOCDlag", "zOCD", print = TRUE)

# OCD and anxiety t + 1
autocorr_plot(df, "zAnxietylag", "zOCD", print = TRUE)

# OCD and think_clear t + 1 
autocorr_plot(df, "zThink_clearlag", "zOCD", print = TRUE)

#OCD and happiness t + 1
autocorr_plot(df, "zHappylag", "zOCD", print = TRUE)

#OCD and sleep for day after
autocorr_plot(df_sleep, "lagged_meanzOCD", "sleep_by_day", print = TRUE)

#anxiety and sleep for day after
autocorr_plot(df_sleep, "lagged_meanzAnxiety", "sleep_by_day", print = TRUE)

#happiness and sleep for day after
autocorr_plot(df_sleep, "lagged_meanzHappy", "sleep_by_day", print = TRUE)

##### autocorr, effects of OCD on logged variables #####

# OCD and anxiety t + 1 
autocorr_plot(df, "zOCDlag", "zAnxiety", print = TRUE)

# OCD and think_clear t + 1
autocorr_plot(df, "zOCDlag", "zThink_clear", print = TRUE)

#OCD and happiness t + 1
autocorr_plot(df, "zOCDlag", "zHappy", print = TRUE)

##### same time point corr #####

# OCD and sleep
autocorr_plot(df, "zSleep", "zOCD", print = TRUE)

# OCD and anxiety 
autocorr_plot(df, "zAnxiety", "zOCD", print = TRUE)

# OCD and think_clear  
autocorr_plot(df, "zThink_clear", "zOCD", print = TRUE)

#OCD and happiness
autocorr_plot(df, "zHappy", "zOCD", print = TRUE)

#sleep and anxiety
autocorr_plot(df, "zSleep", "zAnxiety", print = TRUE)

#sleep and happy
autocorr_plot(df, "zSleep", "zHappy", print = TRUE)

#mean OCD from whole day and sleep
autocorr_plot(df_sleep, "sleep_by_day", "meanzOCD", print = TRUE)

#mean anxiety from whole day and sleep
autocorr_plot(df_sleep, "sleep_by_day", "meanzAnxiety", print = TRUE)

#mean OCD from whole day and sleep
autocorr_plot(df_sleep, "sleep_by_day", "meanzHappy", print = TRUE)

#checking task autocorr
autocorr_plot(df_task, "zmeanConflag", "zmeanConf", print = TRUE)
autocorr_plot(df_task, "zmeta_d", "zmeanConf", print = TRUE)


#### Effect of Activities and Social Context on States and Symptom Log ####

##### OCD severity ratings #####

#model without social context because of vif, results are the same as with

model<-lmerTest::lmer(zOCD ~ work + resting + errands + leisure + sports + travel + selfCare 
                      + with_others + (1 | dataID), data = EMA_data_filtered)

vif(model) # alone and with others very high vif ()

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#checking with slopes (model not converging)
summary(lmerTest::lmer(zOCD ~ work + resting + errands + leisure + sports + travel + selfCare + 
                         alone + with_others + (work  | dataID) + (resting | dataID) + (errands | dataID) +
                         (leisure | dataID) + (sports | dataID) +(travel | dataID) + (selfCare | dataID) + 
                         (alone | dataID) + (with_others | dataID), data = EMA_data_filtered)) 


#checking with gender and age
summary(lmerTest::lmer(zOCD ~ work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + zAge + Gender_2 + (1 | dataID), data = EMA_data_filtered))

# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "work" ~ "Working/Studying",
    term == "resting" ~ "Resting",
    term == "errands" ~ "Running Errands",
    term == "leisure" ~ "Leisure Activities",
    term == "sports" ~ "Sports/Exercise",
    term == "travel" ~ "Travelling/Commuting",
    term == "selfCare" ~ "Self-care/eating/drinking",
    #term == "alone" ~ "Social context: Alone",
    term == "with_others" ~ "Social context: With other people",
    TRUE ~ term  # Keep other terms unchanged
  ))


# Create plot
ocd_context<-ggplot(df_est, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity",  alpha = 0.7) +  # Bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars
  geom_point(size = 3, color = "black") +  # Coefficient points
  geom_hline(yintercept = 0, linetype = "dashed") +  # Vertical reference line
  scale_fill_viridis_d(option = "plasma") +  
  labs(title = "Contexts Predicting OCD Severity Ratings",
       x = "", y = "Model Estimate") +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position="none")+
  font("ylab", size = 15)+
  font("title", size = 15)+
  theme(axis.text.x = element_text(angle = 45, size = 15, hjust = 1))+ 
  ylim(-0.4, 0.4)


##### symptom log #####

#First need to process symptom log to make it leading (future) variable

combined<-dplyr::bind_rows(EMA_data_filtered, symptom_log_filtered)
combined<-combined[order(combined$dataID, combined$DateTime),]


#make symptom log leading var
df_symplog_lead <- combined %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime,SymptomLog), ~lead(.), .names = "{.col}lead")) %>% # make the var lead
  ungroup()

#replace NAs in symptom log with 0
df_symplog_lead<-df_symplog_lead %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

#get only participants who logged symptoms

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_lead <- subset(df_symplog_lead, dataID %in% highSymp_id)


#model-fitting
model <- glmer(Log_within4Hours ~  work + resting + errands + leisure + sports 
               + travel + selfCare + with_others +  (1 | dataID), data = df_symplog_lead, 
               family = binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)

vif(model) # alone and with others very high vif ()

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))


#with slopes (not converging)
summary(glmer(Log_within4Hours ~ work + resting + errands + leisure + sports + travel + selfCare + 
                alone + with_others + (work  | dataID) + (resting | dataID) + (errands | dataID) +
                (leisure | dataID) + (sports | dataID) +(travel | dataID) + (selfCare | dataID) + 
                (alone | dataID) + (with_others | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"))) 

#with age and gender
summary(glmer(Log_within4Hours ~  work + resting + errands + leisure + sports 
              + travel + selfCare + with_others + zAge + Gender_2 +  (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


  # Extract model estimates with confidence intervals
  df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
    filter(term != "(Intercept)") # Remove the intercept
  
  df_est <- df_est %>%
    mutate(term = case_when(
      term == "work" ~ "Working/Studying",
      term == "resting" ~ "Resting",
      term == "errands" ~ "Running Errands",
      term == "leisure" ~ "Leisure Activities",
      term == "sports" ~ "Sports/Exercise",
      term == "travel" ~ "Travelling/Commuting",
      term == "selfCare" ~ "Self-care/eating/drinking",
      # term == "alone" ~ "Social context: Alone",
      term == "with_others" ~ "Social context: With other people",
      TRUE ~ term  # Keep other terms unchanged
    ))
  
  
  # Create a ggplot with bars overlaid on error bars
  sympLog_context<-ggplot(df_est, aes(x = term, y = estimate, fill = term)) +
    geom_bar(stat = "identity",  alpha = 0.7) +  # Bars
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars
    geom_point(size = 3, color = "black") +  # Coefficient points
    geom_hline(yintercept = 0, linetype = "dashed") +  # Vertical reference line
    scale_fill_viridis_d(option = "plasma") +  
    labs(title = "",
         x = "", y = "Model Estimate") +
    theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
          axis.text.y = element_text(size = 20, hjust = 1))+
    theme(legend.position="none")+
    font("ylab", size = 20)+
    font("title", size = 20)
  


##### self-confidence #####
model<-lmerTest::lmer(zConfidence ~ work + resting + errands + 
                        leisure + sports + travel + selfCare +  
                        with_others + (1 | dataID), data = EMA_data_filtered)

vif(model) 

#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#with slopes (model not converging)
summary(lmerTest::lmer(zConfidence ~ work + resting + errands + leisure + sports + travel + selfCare + 
                         alone + with_others + (work  | dataID) + (resting | dataID) + (errands | dataID) +
                         (leisure | dataID) + (sports | dataID) +(travel | dataID) + (selfCare | dataID) + 
                         (alone | dataID) + (with_others | dataID), data = EMA_data_filtered))

#with age and gender
summary(lmerTest::lmer(zConfidence ~ work + resting + errands + 
                         leisure + sports + travel + selfCare +  
                         with_others + zAge + Gender_2 +  (1 | dataID), 
                       data = EMA_data_filtered))


# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "work" ~ "Working/Studying",
    term == "resting" ~ "Resting",
    term == "errands" ~ "Running Errands",
    term == "leisure" ~ "Leisure Activities",
    term == "sports" ~ "Sports/Exercise",
    term == "travel" ~ "Travelling/Commuting",
    term == "selfCare" ~ "Self-care/eating/drinking",
    term == "alone" ~ "Social context: Alone",
    term == "with_others" ~ "Social context: With other people",
    TRUE ~ term  # Keep other terms unchanged
  ))


# Create a ggplot with bars overlaid on error bars
conf_context<-ggplot(df_est, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity",  alpha = 0.7) +  # Bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars
  geom_point(size = 3, color = "black") +  # Coefficient points
  geom_hline(yintercept = 0, linetype = "dashed") +  # Vertical reference line
  scale_fill_viridis_d(option = "plasma") +  
  labs(title = "Contexts Predicting Self-Reported EMA Confidence Ratings",
       x = "", y = "Model Estimate") +
  #theme(axis.text.x = element_blank()) +
  #theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none")+
  font("ylab", size = 20)+
  font("title", size = 20)+
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1))+
  ylim (-0.15, 0.7)




##### metacognitive bias #####
model<-lmerTest::lmer(zmeanConf ~ work + resting + errands + leisure + sports + travel + selfCare + 
                        alone + with_others + (1 | dataID), data = EMA_task)

# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "work" ~ "Working/Studying",
    term == "resting" ~ "Resting",
    term == "errands" ~ "Running Errands",
    term == "leisure" ~ "Leisure Activities",
    term == "sports" ~ "Sports/Exercise",
    term == "travel" ~ "Travelling/Commuting",
    term == "selfCare" ~ "Self-care/eating/drinking",
    term == "alone" ~ "Social context: Alone",
    term == "with_others" ~ "Social context: With other people",
    TRUE ~ term  # Keep other terms unchanged
  ))


# Create a ggplot with bars overlaid on error bars
ggplot(df_est, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity",  alpha = 0.7) +  # Bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars
  geom_point(size = 3, color = "black") +  # Coefficient points
  geom_hline(yintercept = 0, linetype = "dashed") +  # Vertical reference line
  scale_fill_viridis_d(option = "plasma") +  
  labs(title = "Contexts Predicting Mean Task Confidence",
       x = "", y = "Model Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none")


#### State variables predicting each other ####

##### predicting OCD severity #####

#without intercept because all are within-person centered
model<-lm(zOCD ~ zThink_clear + zConfidence 
          + zAnxiety + zHappy, data = EMA_data_filtered)
vif(model)
confint(model)
#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#with slopes, no intercept
summary(lmerTest::lmer(zOCD ~ zThink_clear + zConfidence 
                       + zAnxiety + zHappy +  (0+zThink_clear | dataID) +
                         (0+zConfidence | dataID) + (0+zAnxiety | dataID) + 
                         (0+zHappy | dataID), data = EMA_data_filtered))

#with age and gender
summary(lm(zOCD ~ zThink_clear + zConfidence 
           + zAnxiety + zHappy + zAge + Gender_2, data = EMA_data_filtered))


#controlling for current activities and social context

summary(lmerTest::lmer(zOCD ~ work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + zThink_clear + zConfidence 
                       + zAnxiety + zHappy + (1 | dataID), data = EMA_data_filtered))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zOCD ~ Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + zThink_clear + zConfidence 
                       + zAnxiety + zHappy +Time_of_day +
                         (1 | dataID), data = new))


#sleep model separately 
sleep_model<-lmerTest::lmer(zOCD ~ zSleep + (1 | dataID), data = EMA_data_filtered)

summary(sleep_model)
vif(sleep_model)
confint(sleep_model)
#inspecting normality of residuals
qqnorm(resid(sleep_model))
qqline(resid(sleep_model))

#sleep model with slopes (failed to converge)
summary(lmerTest::lmer(zOCD ~ zSleep + (0+zSleep | dataID), data = EMA_data_filtered))

#with age and gender
summary(lm(zOCD ~ zSleep + zAge + Gender_2, data = EMA_data_filtered))

#controlling for current activities and social context

summary(lmerTest::lmer(zOCD ~ work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + zSleep + (1 | dataID), data = EMA_data_filtered))


#controlling for day of week
new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zOCD ~ Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + zSleep +
                         (1 | dataID), data = new))

#controlling for time of day
summary(lmerTest::lmer(zOCD ~ zSleep + Time_of_day + (1 | dataID), data = EMA_data_filtered))


##### predicting self-confidence #####

#without intercept because all are within-person centered
model<-lm(zConfidence ~ zThink_clear + zOCD
          + zAnxiety + zHappy, data = EMA_data_filtered)
vif(model)
confint(model)
#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#with slopes, no intercept
summary(lmerTest::lmer(zConfidence ~ zThink_clear + zOCD 
                       + zAnxiety + zHappy +  (0+zThink_clear | dataID) +
                         (0+zOCD | dataID) + (0+zAnxiety | dataID) + 
                         (0+zHappy | dataID), data = EMA_data_filtered))

#with age and gender
summary(lm(zConfidence ~ zThink_clear + zOCD 
           + zAnxiety + zHappy + zAge + Gender_2, data = EMA_data_filtered))


#controlling for current activities and social context

summary(lmerTest::lmer(zConfidence ~ work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + zThink_clear + zOCD 
                       + zAnxiety + zHappy + (1 | dataID), data = EMA_data_filtered))


#controlling for day of week
new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zConfidence ~ Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + zThink_clear + zOCD 
                       + zAnxiety + zHappy +
                         (1 | dataID), data = new))

#controlling for time of day
summary(lmerTest::lmer(zConfidence ~ zThink_clear + zOCD 
                       + zAnxiety + zHappy + Time_of_day + (1 | dataID), data = EMA_data_filtered))


#sleep model separately 
sleep_model<-lm(zConfidence ~ zSleep, data = EMA_data_filtered)

summary(sleep_model)
vif(sleep_model)
confint(sleep_model)
#inspecting normality of residuals
qqnorm(resid(sleep_model))
qqline(resid(sleep_model))

#sleep model with slopes (failed to converge)
summary(lmerTest::lmer(zConfidence ~ zSleep + (0+zSleep | dataID), data = EMA_data_filtered))

#with age and gender
summary(lm(zConfidence ~ zSleep + zAge + Gender_2, data = EMA_data_filtered))

#controlling for current activities and social context

summary(lmerTest::lmer(zConfidence ~ work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + zSleep + (1 | dataID), data = EMA_data_filtered))


#controlling for day of week
new<-fastDummies::dummy_cols(EMA_data_filtered, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zConfidence ~ Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + zSleep +
                         (1 | dataID), data = new))

#controlling for time of day
summary(lmerTest::lmer(zConfidence ~ zSleep + Time_of_day + (1 | dataID), data = EMA_data_filtered))



#### Raincloud plots of within-subject correlations (for visualisation purposes) ####

##### OCD severity #####
cor_per_subject<-EMA_data_filtered%>%
  group_by(dataID)%>%
  summarise(conf_corr = cor(zConfidence,zOCD, method = "spearman"),
            anx_corr = cor(zAnxiety,zOCD, method = "spearman"),
            happy_corr = cor(zHappy,zOCD, method = "spearman"),
            diff_think_corr = cor(zThink_clear,zOCD, method = "spearman"))

#for sleep
df_sleep<-EMA_data_filtered %>%
  group_by(dataID, Day) %>%
  summarise(meanzOCD = mean(zOCD, na.rm = TRUE),
            sleep_by_day = mean(zSleep, na.rm = TRUE))

#remove participants with no sleep data
df_sleep<-df_sleep[df_sleep$dataID != 17682,]
df_sleep<-df_sleep[df_sleep$dataID != 19318,]
df_sleep<-df_sleep[df_sleep$dataID != 19336,]

cor_sleep<-df_sleep%>%
  group_by(dataID)%>%
  summarise(sleep_corr = cor(sleep_by_day,meanzOCD, method = "spearman", use = "complete.obs"))

cor_per_subject<-merge(cor_per_subject, cor_sleep, by = 'dataID', all.x = TRUE) 

#cor_per_subject$dataID<-summary_ema_OCI_traitQ$dataID

cor_per_subject<-gather(cor_per_subject, Corr_type, Score, conf_corr:sleep_corr, factor_key=TRUE,  na.rm = T)

# Calculate the mean and standard error for each corr
corr_summary <- cor_per_subject %>%
  group_by(Corr_type) %>%
  summarise(
    mean_Score = mean(Score, na.rm = TRUE),
    se_Score = sd(Score, na.rm = TRUE) / sqrt(n())
  )

#create plot
library(RColorBrewer)

# Get Set2 colors based on number of Corr_type categories
set2_colors <- brewer.pal(n = length(unique(cor_per_subject$Corr_type)), name = "Set2")

ggplot(cor_per_subject, aes(x=Corr_type, y=Score, fill=Corr_type)) +
  geom_flat_violin(aes(fill = Corr_type), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  geom_point(aes(x = Corr_type, y = Score, colour = Corr_type), position = position_jitter(width = .01), size = 5, alpha = 0.5) +
  geom_boxplot(aes(x = Corr_type, y = Score, colour = Corr_type), position = position_dodgenudge(width = 0.3, x = -.2), outlier.shape = NA, alpha = .5, width = .2, colour = "black") +
  geom_line(data = corr_summary, aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type), linetype = 3, position = position_nudge(x = 0.1)) +
  geom_point(data = corr_summary, aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type), shape = 18, position = position_nudge(x = 0.1)) +
  geom_errorbar(data = corr_summary, 
                aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type, 
                    ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), 
                width = .07, position = position_nudge(x = 0.1)) +
  #scale_y_continuous(limits = c(40, 100)) +  
  labs(y = expression(paste ("Spearman's Rho"))) + 
  scale_x_discrete(labels = c('Self-Confidence', 'Anxiety', 'Happiness', 'Brain Fog', 'Sleep Quality')) +
  scale_fill_manual(values = set2_colors) +
  scale_colour_manual(values = set2_colors) +
  theme_bw() +
  font("ylab", size = 15) +
  font("title", size = 15) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(legend.position="none")


# do one sample t-tests to check difference of spearman rhos from 0

#self-confidence
only_meanConf<-cor_per_subject %>% filter(Corr_type== 'conf_corr')

t.test(only_meanConf$Score, mu = 0)

#brain fog
only_meanConf<-cor_per_subject %>% filter(Corr_type== 'diff_think_corr')

t.test(only_meanConf$Score, mu = 0)

#anxiety
only_meanConf<-cor_per_subject %>% filter(Corr_type== 'anx_corr')

t.test(only_meanConf$Score, mu = 0)

#happy
only_meanConf<-cor_per_subject %>% filter(Corr_type== 'happy_corr')

t.test(only_meanConf$Score, mu = 0)

#sleep quality
only_meanConf<-cor_per_subject %>% filter(Corr_type== 'sleep_corr')

t.test(only_meanConf$Score, mu = 0)


##### Self-confidence #####
cor_per_subject<-EMA_data_filtered%>%
  group_by(dataID)%>%
  summarise(ocd_corr = cor(zConfidence,zOCD, method = "spearman"),
            anx_corr = cor(zAnxiety,zConfidence, method = "spearman"),
            happy_corr = cor(zHappy,zConfidence, method = "spearman"),
            diff_think_corr = cor(zThink_clear,zConfidence, method = "spearman"))

#for sleep
df_sleep<-EMA_data_filtered %>%
  group_by(dataID, Day) %>%
  summarise(meanzOCD = mean(zConfidence, na.rm = TRUE),
            sleep_by_day = mean(zSleep, na.rm = TRUE))

#remove participants with no sleep correlation data (because lack of variation in scores)

df_sleep<-df_sleep[df_sleep$dataID != 17682,]
df_sleep<-df_sleep[df_sleep$dataID != 19318,]
df_sleep<-df_sleep[df_sleep$dataID != 19336,]
df_sleep<-df_sleep[df_sleep$dataID != 19362,]

cor_sleep<-df_sleep%>%
  group_by(dataID)%>%
  summarise(sleep_corr = cor(sleep_by_day,meanzOCD, method = "spearman", use = "complete.obs"))

cor_per_subject<-merge(cor_per_subject, cor_sleep, by = 'dataID', all.x = TRUE) 

#cor_per_subject$dataID<-summary_ema_OCI_traitQ$dataID

cor_per_subject<-gather(cor_per_subject, Corr_type, Score, ocd_corr:sleep_corr, factor_key=TRUE,  na.rm = T)

# Calculate the mean and standard error for each corr
corr_summary <- cor_per_subject %>%
  group_by(Corr_type) %>%
  summarise(
    mean_Score = mean(Score, na.rm = TRUE),
    se_Score = sd(Score, na.rm = TRUE) / sqrt(n())
  )

#make plot

library(RColorBrewer)

# Get Set2 colors based on number of Corr_type categories
set2_colors <- brewer.pal(n = length(unique(cor_per_subject$Corr_type)), name = "Set2")
# Replace the first color with red
custom_colors <- set2_colors
custom_colors[1] <- "indianred4"

ggplot(cor_per_subject, aes(x=Corr_type, y=Score, fill=Corr_type)) +
  geom_flat_violin(aes(fill = Corr_type), position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  geom_point(aes(x = Corr_type, y = Score, colour = Corr_type), position = position_jitter(width = .01), size = 5, alpha = 0.5) +
  geom_boxplot(aes(x = Corr_type, y = Score, colour = Corr_type), position = position_dodgenudge(width = 0.3, x = -.2), outlier.shape = NA, alpha = .5, width = .2, colour = "black") +
  geom_line(data = corr_summary, aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type), linetype = 3, position = position_nudge(x = 0.1)) +
  geom_point(data = corr_summary, aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type), shape = 18, position = position_nudge(x = 0.1)) +
  geom_errorbar(data = corr_summary, 
                aes(x = Corr_type, y = mean_Score, group = Corr_type, colour = Corr_type, 
                    ymin=mean_Score - se_Score, ymax=mean_Score + se_Score), 
                width = .07, position = position_nudge(x = 0.1)) +
  #scale_y_continuous(limits = c(40, 100)) +  
  labs(y = expression(paste ("Spearman's Rho"))) + 
  scale_x_discrete(labels = c('OCD Severity', 'Anxiety', 'Happiness', 'Brain Fog', 'Sleep Quality')) +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) +
  theme_bw() +
  font("ylab", size = 15) +
  font("title", size = 15) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(legend.position="none")

#### Directional effects of self-confidence over OCD severity ####

#make lagged and leading OCD variables

#unconstrained to days
df_lag_lead <- EMA_data_filtered%>%
  group_by(dataID) %>%
  group_by(dataID) %>% #constrained to be within a day
  mutate(across(c(zOCD, zAnxiety, zConfidence, zHappy, 
                  zThink_clear), 
                .fns = list(
                  lag = ~ lag(., 1),
                  lead = ~ lead(., 1)
                ), 
                .names = "{.col}{.fn}")) %>%
  ungroup()

#just to check
compare = subset(df_lag_lead, 
                 select = c(zOCD, zOCDlag, zOCDlead))


#without intercepts
model<-lm(zConfidence ~  zOCDlag + zOCD + zOCDlead, data = df_lag_lead)

vif(model)
confint(model)
#inspecting normality of residuals
qqnorm(resid(model))
qqline(resid(model))

#with slopes
summary(lmerTest::lmer(zConfidence ~  zOCDlag + zOCD + zOCDlead + (0 + zOCDlag | dataID) +
                         (0 + zOCD | dataID) + (0 + zOCDlead | dataID), data = df_lag_lead))

#with age and gender
summary(lm(zConfidence ~  zOCDlag + zOCD + zOCDlead +zAge + Gender_2, data = df_lag_lead))


#controlling for current activities and social context

summary(lmerTest::lmer(zConfidence ~  zOCDlag + zOCD + zOCDlead + work + resting + errands + leisure + sports + travel + selfCare 
                       + with_others + (1 | dataID), data = df_lag_lead))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(df_lag_lead, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zConfidence ~  zOCDlag + zOCD + zOCDlead + Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))



# Define custom labels for predictor variables
custom_labels <- c("OCD (t+1)" = "zOCDlag",
                   "OCD (t)" = "zOCD",
                   "OCD (t-1)" = "zOCDlead")

# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != c("(Intercept)")) #remove intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "zOCDlag" ~ "Past OCD",
    term == "zOCD" ~ "Current OCD",
    term == "zOCDlead" ~ "Future OCD",
    TRUE ~ term  # Keep other terms unchanged
  ),
  term = factor(term, levels = c("Future OCD", "Current OCD", "Past OCD")))


# Create a ggplot with bars overlaid on error bars
ggplot(df_est, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "indianred4", alpha = 0.5) +  # Bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars
  geom_point(size = 3, color = "black") +  # Coefficient points
  geom_hline(yintercept = 0, linetype = "dashed") +  # Vertical reference line
  labs(title = "Self-confidence Predicting Past, Present, and Future OCD Severity",
       x = "", y = "Model Estimate") +
  font("xlab", size = 15)+
  font("title", size = 15)+
  theme(axis.text.x = element_text(angle = 45, size = 15, hjust = 1),
        axis.text.y = element_text(size = 15, hjust = 1))+
  coord_flip()


#### Do task measures correspond to OCD severity? No ####

#metacognitive bias
model <-lm(zmeanConf ~ zOCD, data = EMA_task)
summary(model)

#meta-d'/d'
model <-lm(zm_ratio ~ zOCD, data = EMA_task)
summary(model)

#### Do task measures correspond to any other states (self-confidence) ####

##### Metacognitive bias ####

#metacognitive bias predicted by other states (including sleep)
summary(lm(zmeanConf ~ zAnxiety + zThink_clear + zConfidence + zHappy + zSleep, data = EMA_task))

#excluding sleep to retain data
model <-lm(zmeanConf ~ zAnxiety + zThink_clear + zConfidence + zHappy, data = EMA_task)

summary(model)
confint(model)

#checking with just confidence in model
summary(lm(zmeanConf ~ zConfidence, data = EMA_task))

#checking with slopes
summary(lmerTest::lmer(zmeanConf ~ zAnxiety + zThink_clear + zConfidence + zHappy + (0 + zAnxiety | dataID) +
                         (0 + zThink_clear | dataID) + (0 + zConfidence | dataID) + (0 + zHappy | dataID)
                       , data = EMA_task))

summary(lmerTest::lmer(zmeanConf ~ zConfidence +(0 + zConfidence | dataID), data = EMA_task))

#controlling for age and gender
summary(lm(zmeanConf ~ zAnxiety + zThink_clear + zConfidence + zHappy + zAge + Gender_2, data = EMA_task))
summary(lm(zmeanConf ~ zConfidence + zAge + Gender_2, data = EMA_task))

#controlling for other task measures
summary(lm(zConfidence ~ zabs_evdiff + zmeanConf + zmeanChoiceRT +  zAccuracy, data = EMA_task))


#controlling for current activities and social context

summary(lmerTest::lmer( zmeanConf ~ zConfidence + work + resting + errands + leisure + sports + travel + selfCare 
                        + with_others + (1 | dataID), data = EMA_task))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(EMA_task, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zmeanConf ~zConfidence + Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))


##### Meta-d'/d' ##### 

#meta-d'/d' predicted by other states (nothing significant)
summary(lm(zm_ratio ~ zAnxiety + zThink_clear + zConfidence + zHappy, data = EMA_task))


#### Within-subject Correlation plot between metacognitive bias and self-confidence ####

#scatterplot
EMA_task$dataID<- as.factor(EMA_task$dataID)

plot <- ggscatter(EMA_task, x = "zConfidence", y = "zmeanConf",
                  color = "black", fill = "dataID", shape = 21, stroke = 1, size = 3,
                  add = "reg.line",                                 # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "black",
                                    fill = "lightgray"), 
                  ylab = "Meta-Cognitive Bias", 
                  xlab = "EMA Confidence", 
                  title = "") +
  font("xlab", size = 15) +
  font("ylab", size = 15) +
  font("title", size = 15)+
  coord_fixed(ratio = 1)+
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none"
  )

plot + stat_cor(method = "pearson", size = 5)


# within-subject correlations

#get the within subject correlation values
cor_per_subject<-EMA_task%>%
  group_by(dataID)%>%
  summarise(conf_corr = cor(zConfidence,zmeanConf, method = "spearman"))

# Calculate the mean and standard error for each corr
corr_summary <- cor_per_subject %>%
  summarise(
    mean_Score = mean(conf_corr, na.rm = TRUE),
    se_Score = sd(conf_corr, na.rm = TRUE) / sqrt(n())
  )

ggplot(cor_per_subject, aes(x = 1, y = conf_corr)) +
  stat_halfeye(
    adjust = 0.2,
    scale = 0.5,
    justification = -0.2, #centres the plot
    .width = 0,
    point_colour = NA,
    fill = "darkorchid3"
  ) +
  geom_errorbar(
    data = corr_summary,
    aes(x = 1, ymin = mean_Score - se_Score, ymax = mean_Score + se_Score),
    width = 0.1,
    size = 1,
    color = "black",
    inherit.aes = FALSE  # to not use aes from ggplot layer above
  ) +
  geom_point(
    data = corr_summary,
    aes(x = 1, y = mean_Score),
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_jitter(
    data = cor_per_subject,
    aes(x = 1, y = conf_corr),
    width = 0.05,
    alpha = 0.3,
    size = 3,         
    color = "darkorchid3"   
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(0.85, 1.6))  +  
  labs(
    title = "Within-Subject Correlations Between EMA Confidence and Meta-Cognitive Bias",
    x = "",
    y = "Spearman's Rho"
  ) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  geom_hline(aes(yintercept = 0), linetype = "dotdash", size = 1)+
  theme(
    axis.text.y = element_text(size = 13)
  )+
  font("ylab", size = 15)+ 
  font("title", size = 15)

#check that spearman rho significantly different from 0 for within subject correlation
shapiro.test(cor_per_subject$conf_corr) 
wilcox.test(cor_per_subject$conf_corr, mu = 0)


#check within-subjects correlation with metacognitive efficiency
cor_per_subject<-EMA_task%>%
  group_by(dataID)%>%
  summarise(conf_corr = cor(zConfidence,zm_ratio, method = "spearman"))

shapiro.test(cor_per_subject$conf_corr) 

wilcox.test(cor_per_subject$conf_corr, mu = 0)

#### Symptom log Analysis ####

##### EMA #####

# get only symptom logs 4 hours max before EMA

combined<-dplyr::bind_rows(EMA_data_filtered, symptom_log_filtered)
combined<-combined[order(combined$dataID, combined$DateTime),]

#make symptom log leading var
df_symplog_lead <- combined %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime, Time_of_day, SymptomLog), ~lead(.), .names = "{.col}lead")) %>% # make the var lead
  ungroup()

#replace NAs in symptom log with 0
df_symplog_lead<-df_symplog_lead %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

# if we want to check only participants who have completed symptom logs

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_lead <- subset(df_symplog_lead, dataID %in% highSymp_id)

require(lme4)

#check with all state variables predicting future symptom log
model <- glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy + zSleep + zConfidence + zThink_clear + (1 | dataID), data = df_symplog_lead, 
               family = binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)

#check overdispersion
library(blmeco)
blmeco::dispersion_glmer(model) #value indicates no overdispersion

vif(model) #variance inflation


#checking only self-confidence
summary(glmer(Log_within4Hours ~ zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#control for activities and social contexts
summary(glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy + zThink_clear + zConfidence +zSleep +
                work + resting + errands + leisure + sports + travel + selfCare + 
                alone + with_others + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(df_symplog_lead, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy + zThink_clear + zConfidence + zSleep + Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))



# check with age and gender 
summary(glmer(Log_within4Hours ~ zAge + Gender_2 + zOCD + zAnxiety + zHappy + zSleep + zThink_clear + zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#check with slopes (not converging)
summary(glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy + zSleep 
              + zConfidence + zThink_clear + (zOCD | dataID) + (zAnxiety | dataID) +
                (zHappy | dataID) + (zConfidence | dataID) + (zThink_clear | dataID) +
                (zSleep | dataID), data = df_symplog_lead, 
              family = binomial))


# check without sleep to retain all data (not just morning data)
summary(glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy  + zThink_clear + zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

m<-glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy  + zThink_clear + zConfidence + (1 | dataID), data = df_symplog_lead, 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),
         nAGQ = 10)

# since sleep is only tested in the morning and confidence is low in the morning 
# check if there's a morning effect over confidence

new<-fastDummies::dummy_cols(df_symplog_lead, select_columns = "Time_of_daylead") # using the symptom log time
m<-glmer(Log_within4Hours ~  zConfidence*Time_of_daylead_Morning + zOCD + zAnxiety +  (1 | dataID), data = new, 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),
         nAGQ = 10)
#there is a significant negative interaction between morning and confidence

#plot state variables predicting future symptom log
m<-glmer(Log_within4Hours ~ zOCD + zAnxiety + zHappy  + zThink_clear + zConfidence + zSleep + (1 | dataID), data = df_symplog_lead, 
         family = binomial, control = glmerControl(optimizer = "bobyqa"),
         nAGQ = 10) #without slopes

set_theme(base = theme_bw()) # for plots
# Extract model estimates with confidence intervals
df_est <- tidy(m, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "zOCD" ~ "OCD Severity Rating",
    term == "zAnxiety" ~ "Anxiety",
    term == "zHappy" ~ "Happiness",
    term == "zSleep" ~ "Sleep Quality",
    term == "zThink_clear" ~ "Brain Fog",
    term == "zConfidence" ~ "Self-Confidence",
    term == "zSleep" ~ "Sleep Quality",
    TRUE ~ term  # Keep other terms unchanged
  ))


df_est <- df_est %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term),  # Preserve order
         fill_id = row_number())  # Numeric index for color gradient

# Create a gradient of green colors
green_gradient <- colorRampPalette(c("#d0f0c0", "#006400"))(nrow(df_est))  # light to dark green

# Plot
ggplot(df_est, aes(x = term, y = estimate, fill = fill_id)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradientn(colors = green_gradient, guide = "none") +  # Gradient fill
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Model Estimate") +
  font("ylab", size = 20) +
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
        axis.text.y = element_text(size = 20, hjust = 1))+
  ylim(-0.8, 0.8)

##### checking symptom log effects in smaller time windows #####

# 4 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#3 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 3) & SymptomLoglead == 1, 1, 0))

summary(glmer(Log_within4Hours ~  zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#2 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 2) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


# 1 hour
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 1) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


##### Task #####

task_with_symplog<-dplyr::bind_rows(task_data, symptom_log_filtered)
task_with_symplog<-task_with_symplog[order(task_with_symplog$dataID, task_with_symplog$DateTime),]

#make symptom log leading var
df_symplog_lead <- task_with_symplog %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime,SymptomLog), ~lead(.), .names = "{.col}lead")) %>% # make the var lead
  ungroup()


#remove any rows not associated with tasks
df_symplog_lead<-df_symplog_lead %>% drop_na(accuracy)


#replace NAs in symptom log with 0
df_symplog_lead<-df_symplog_lead %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

# get only subjects who have completed symptom logs
high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_lead <- subset(df_symplog_lead, dataID %in% highSymp_id)


#metacognitive bias (significant)
model<-glmer(Log_within4Hours ~  zmeanConf + (1 | dataID), data = df_symplog_lead, 
             family = binomial, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)


#check overdispersion
library(blmeco)
blmeco::dispersion_glmer(model) #value indicates no overdispersion


#with slopes
summary(glmer(Log_within4Hours ~  zmeanConf + (0+zmeanConf | dataID), data = df_symplog_lead, 
              family = binomial))

#with age and gender
summary(glmer(Log_within4Hours ~  zAge + Gender_2 + zmeanConf + (1 | dataID), data = df_symplog_lead, 
              family = binomial))

#controlling for other task measures
m <- glmer(Log_within4Hours ~ zabs_evdiff + zmeanConf + zmeanChoiceRT +  zAccuracy + z_d + zmeta_d + zm_ratio + (1 | dataID), data = df_symplog_lead, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)


#with age and gender
m <- glmer(Log_within4Hours ~ zAge + Gender_2 + zabs_evdiff + zmeanConf + zmeanChoiceRT +  zAccuracy + (1 | dataID), data = df_symplog_lead, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

#controlling for compliance (because compliance correlates with overall mean metacognitive bias)
df_symplog_lead <- df_symplog_lead%>%
  left_join(summary_ema_OCI_traitQ %>% select(dataID,Number_notifs_answered), by = "dataID")

summary(glmer(Log_within4Hours ~ zmeanConf + Number_notifs_answered + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#controlling for meta-d'/d'
summary(glmer(Log_within4Hours ~  zmeanConf + zm_ratio + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10)) # metacognitive bias becomes marginally significant. Less data points?


#meta-d'/d' (not significant)
summary(glmer(Log_within4Hours ~  zm_ratio + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#plotting metacognitive bias predicting symptom log for paper

m <- glmer(Log_within4Hours ~ zabs_evdiff + zmeanConf + zmeanChoiceRT +  zAccuracy + (1 | dataID), data = df_symplog_lead, 
           family = binomial, control = glmerControl(optimizer = "bobyqa"), #without slopes
           nAGQ = 10)

set_theme(base = theme_bw()) # for plots
# Extract model estimates with confidence intervals
df_est <- tidy(m, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "zabs_evdiff" ~ "Signal Strength",
    term == "zmeanConf" ~ "Metacognitive Bias",
    term == "zmeanChoiceRT" ~ "Choice RT",
    term == "zAccuracy" ~ "Staircased Accuracy",
    TRUE ~ term  # Keep other terms unchanged
  ))


df_est <- df_est %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term),  # Preserve order
         fill_id = row_number())  # Numeric index for color gradient

# Create a gradient of red colours
green_gradient <- colorRampPalette(c("#ffd6d6", "#ff0000"))(nrow(df_est))  # light to dark green

# Plot
ggplot(df_est, aes(x = term, y = estimate, fill = fill_id)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradientn(colors = green_gradient, guide = "none") +  # Gradient fill
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Model Estimate") +
  font("ylab", size = 20) +
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
        axis.text.y = element_text(size = 20, hjust = 1))


##### checking symptom log effects in smaller time windows #####

# 4 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zmeanConf + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#3 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 3) & SymptomLoglead == 1, 1, 0))

summary(glmer(Log_within4Hours ~  zmeanConf + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#2 hours
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 2) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zmeanConf + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))



# 1 hour
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 1) & SymptomLoglead == 1, 1, 0))
summary(glmer(Log_within4Hours ~  zmeanConf + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


##### Controlling for state variables in metacognitive bias -> symptom log analysis #####

# get only symptom logs 4 hours max before EMA

combined<-dplyr::bind_rows(EMA_task, symptom_log_filtered)
combined<-combined[order(combined$dataID, combined$DateTime),]

#make symptom log leading var
df_symplog_lead <- combined %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime,SymptomLog), ~lead(.), .names = "{.col}lead")) %>% # make the var lead
  ungroup()

#remove any rows not associated with tasks
df_symplog_lead<-df_symplog_lead %>% drop_na(accuracy)


#replace NAs in symptom log with 0
df_symplog_lead<-df_symplog_lead %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_lead<-df_symplog_lead %>% mutate(Log_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

# if we want to check only participants who have completed symptom logs

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_lead <- subset(df_symplog_lead, dataID %in% highSymp_id)

#without ema confidence

summary(glmer(Log_within4Hours ~  zmeanConf + zOCD + zAnxiety + zHappy + zThink_clear + zSleep + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))

#with ema confidence (ema confidence dominates over other variables)

summary(glmer(Log_within4Hours ~  zmeanConf + zOCD + zAnxiety + zHappy + zThink_clear + zConfidence + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


#control for activities and social contexts
summary(glmer(Log_within4Hours ~  zmeanConf + 
                work + resting + errands + leisure + sports + travel + selfCare + 
                alone + with_others + (1 | dataID), data = df_symplog_lead, 
              family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 10))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(df_symplog_lead, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(Log_within4Hours ~  zmeanConf + Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))



#### Directional effects over symptom logs ####

##### For tasks ####

#combine symptom logs with prior created task_ema
task_with_symplog<-dplyr::bind_rows(task_data, symptom_log_filtered)
task_with_symplog<-task_with_symplog[order(task_with_symplog$dataID, task_with_symplog$DateTime),]

# get only symptom logs 4 hours max before EMA

#lag and lead symptom log
df_symplog_task <- task_with_symplog %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime,SymptomLog),
                .fns = list(
                  lag = ~ lag(., 1),
                  lead = ~ lead(., 1)
                ), 
                .names = "{.col}{.fn}")) %>% # Lag the variables
  ungroup()

#remove any rows not associated with EMA
df_symplog_task<-df_symplog_task %>% drop_na(accuracy)


#replace NAs in symptom log with 0
df_symplog_task<-df_symplog_task %>% mutate(SymptomLoglag = ifelse(is.na(SymptomLoglag), 0, SymptomLoglag))

df_symplog_task<-df_symplog_task %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_task<-df_symplog_task %>% mutate(Lag_within4Hours = ifelse((difftime(DateTime, DateTimelag, units = "hours") <= 4) & SymptomLoglag == 1, 1, 0))

df_symplog_task<-df_symplog_task %>% mutate(Lead_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

# if we want to check only participants who have completed symptom logs

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_task <- subset(df_symplog_task, dataID %in% highSymp_id)

# do lmer and plotting

# directionality on metacognitive bias
model <-lmerTest::lmer(zmeanConf ~ Lag_within4Hours + Lead_within4Hours + (1 | dataID), data = df_symplog_task)
library(blmeco)
blmeco::dispersion_glmer(model) #value indicates no overdispersion

#with slopes
summary(lmerTest::lmer(zmeanConf ~ Lag_within4Hours + Lead_within4Hours + (Lag_within4Hours | dataID) + (Lead_within4Hours | dataID), data = df_symplog_task)) 

#with gender and age
summary(lmerTest::lmer(zmeanConf ~ Lag_within4Hours + Lead_within4Hours + zAge + Gender_2 + (1 | dataID), data = df_symplog_task))


# Define custom labels for predictor variables
custom_labels <- c("Future Symptom Log" = "Lead_within4Hours", "Past Symptom Log" = "Lag_within4Hours")

# Plot with updated labels
plot_model(model, type = "est", title = "Effects of Past and Future Symptom Log on Meta-Cognitive Bias",
           show.values = TRUE, show.p = TRUE, value.offset = .3, vline.color = "black",
           axis.labels = names(custom_labels))


# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "Lead_within4Hours" ~ "Future Symptom Log",
    term == "Lag_within4Hours" ~ "Past Symptom Log",
    TRUE ~ term  # Keep other terms unchanged
  ))

df_est <- df_est %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term),  # Preserve order
         fill_id = row_number())  # Numeric index for color gradient

# Create a gradient of green colors
green_gradient <- colorRampPalette(c("#ffd6d6", "#ff0000"))(nrow(df_est))  # light to dark green

# Plot
ggplot(df_est, aes(x = term, y = estimate, fill = fill_id)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradientn(colors = green_gradient, guide = "none") +  # Gradient fill
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Model Estimate") +
  font("xlab", size = 20)+
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
        axis.text.y = element_text(size = 20, hjust = 1))+
  coord_flip()


##### For EMA #####
combined<-dplyr::bind_rows(EMA_data_filtered, symptom_log_filtered)
combined<-combined[order(combined$dataID, combined$DateTime),]

# get only symptom logs 4 hours max before EMA

#lag and lead symptom log
df_symplog_task <- combined %>%
  group_by(dataID) %>%
  mutate(across(c(DateTime,SymptomLog),
                .fns = list(
                  lag = ~ lag(., 1),
                  lead = ~ lead(., 1)
                ), 
                .names = "{.col}{.fn}")) %>% # Lag the variables
  ungroup()


#remove any rows not associated with EMA
df_symplog_task<-df_symplog_task %>% drop_na(OCD)

#replace NAs in symptom log with 0
df_symplog_task<-df_symplog_task %>% mutate(SymptomLoglag = ifelse(is.na(SymptomLoglag), 0, SymptomLoglag))

df_symplog_task<-df_symplog_task %>% mutate(SymptomLoglead = ifelse(is.na(SymptomLoglead), 0, SymptomLoglead))


# find out which symptom logs are within 4 hours of an EMA
df_symplog_task<-df_symplog_task %>% mutate(Lag_within4Hours = ifelse((difftime(DateTime, DateTimelag, units = "hours") <= 4) & SymptomLoglag == 1, 1, 0))

df_symplog_task<-df_symplog_task %>% mutate(Lead_within4Hours = ifelse((difftime(DateTimelead, DateTime, units = "hours") <= 4) & SymptomLoglead == 1, 1, 0))

# if we want to check only participants who have completed symptom logs

high_symptom_count<- summary_ema_OCI_traitQ[summary_ema_OCI_traitQ$Symptom_count >= 1, ]
highSymp_id<-unique(high_symptom_count$dataID)

df_symplog_task <- subset(df_symplog_task, dataID %in% highSymp_id)

# for confidence

# do lmer and plotting
model <-lmerTest::lmer(zConfidence ~ Lag_within4Hours + Lead_within4Hours + (1 | dataID), data = df_symplog_task)

blmeco::dispersion_glmer(model) #value indicates no overdispersion

#checking with slopes (failed to converge)
summary(lmerTest::lmer(zConfidence ~ Lag_within4Hours + Lead_within4Hours + (Lag_within4Hours | dataID) + (Lead_within4Hours | dataID), data = df_symplog_task)) 


#checking with age and gender
summary(lmerTest::lmer(zConfidence ~ Lag_within4Hours + Lead_within4Hours + zAge + Gender_2 + (1| dataID), data = df_symplog_task)) 

#control for activities and social contexts
summary(lmerTest::lmer(zConfidence ~  Lag_within4Hours + Lead_within4Hours + 
                         work + resting + errands + leisure + sports + travel + selfCare + 
                         alone + with_others + (1 | dataID), data = df_symplog_task))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(df_symplog_task, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zConfidence ~   Lag_within4Hours + Lead_within4Hours +  Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))


# Define custom labels for predictor variables
custom_labels <- c("Future Symptom Log" = "Lead_within4Hours", "Past Symptom Log" = "Lag_within4Hours")

# Plot with updated labels
plot_model(model, type = "est", title = "Effects of Past and Future Symptom Log on EMA Confidence",
           show.values = TRUE, show.p = TRUE, value.offset = .3, vline.color = "black",
           axis.labels = names(custom_labels))


# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "Lead_within4Hours" ~ "Future Symptom Log",
    term == "Lag_within4Hours" ~ "Past Symptom Log",
    TRUE ~ term  # Keep other terms unchanged
  ))


df_est <- df_est %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term),  # Preserve order
         fill_id = row_number())  # Numeric index for color gradient

# Create a gradient of green colors
green_gradient <- colorRampPalette(c("#d0f0c0", "#006400"))(nrow(df_est))  # light to dark green

# Plot
ggplot(df_est, aes(x = term, y = estimate, fill = fill_id)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradientn(colors = green_gradient, guide = "none") +  # Gradient fill
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Model Estimate") +
  font("xlab", size = 20)+
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
        axis.text.y = element_text(size = 20, hjust = 1))+
  coord_flip()


#for OCD severity

# do lmer and plotting
model <-lmerTest::lmer(zOCD ~ Lag_within4Hours + Lead_within4Hours + (1 | dataID), data = df_symplog_task)

blmeco::dispersion_glmer(model) #value indicates no overdispersion

#checking with slopes
summary(lmerTest::lmer(zOCD ~ Lag_within4Hours + Lead_within4Hours + (Lag_within4Hours | dataID) + (Lead_within4Hours | dataID), data = df_symplog_task)) 


#checking with age and gender
summary(lmerTest::lmer(zOCD ~ Lag_within4Hours + Lead_within4Hours + zAge + Gender_2 + (1| dataID), data = df_symplog_task)) 

#control for activities and social contexts
summary(lmerTest::lmer(zOCD ~  Lag_within4Hours + Lead_within4Hours + 
                work + resting + errands + leisure + sports + travel + selfCare + 
                alone + with_others + (1 | dataID), data = df_symplog_task))


#controlling for day of week and time of day
new<-fastDummies::dummy_cols(df_symplog_task, select_columns = c("Day_of_week","dayType"))


summary(lmerTest::lmer(zOCD ~   Lag_within4Hours + Lead_within4Hours +  Day_of_week_Monday + Day_of_week_Tuesday + 
                         Day_of_week_Wednesday + Day_of_week_Thursday +  
                         Day_of_week_Friday + Day_of_week_Saturday + Time_of_day +
                         (1 | dataID), data = new))


#checking with context

# Define custom labels for predictor variables
custom_labels <- c("Future Symptom Log" = "Lead_within4Hours", "Past Symptom Log" = "Lag_within4Hours")

# Plot with updated labels
plot_model(model, type = "est", title = "Effects of Past and Future Symptom Log on EMA Confidence",
           show.values = TRUE, show.p = TRUE, value.offset = .3, vline.color = "black",
           axis.labels = names(custom_labels))


# Extract model estimates with confidence intervals
df_est <- tidy(model, effects = "fixed", conf.int = TRUE)%>%
  filter(term != "(Intercept)") # Remove the intercept

df_est <- df_est %>%
  mutate(term = case_when(
    term == "Lead_within4Hours" ~ "Future Symptom Log",
    term == "Lag_within4Hours" ~ "Past Symptom Log",
    TRUE ~ term  # Keep other terms unchanged
  ))


df_est <- df_est %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term),  # Preserve order
         fill_id = row_number())  # Numeric index for color gradient

# Create a gradient of green colors
green_gradient <- colorRampPalette(c("pink", "pink3"))(nrow(df_est))  # light to dark green

# Plot
ggplot(df_est, aes(x = term, y = estimate, fill = fill_id)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradientn(colors = green_gradient, guide = "none") +  # Gradient fill
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Model Estimate") +
  font("xlab", size = 20)+
  theme(axis.text.x = element_text(angle = 45, size = 20, hjust = 1),
        axis.text.y = element_text(size = 20, hjust = 1))+
  coord_flip()


#### Reliability measures ####

#Taken from Sam Hewitt's code PNAS: https://www.pnas.org/doi/abs/10.1073/pnas.2417964122 

library(nlme)

summary=task_data %>%
  group_by(dataID, game) %>%
  summarise(
    meanConf= mean(meanConf, na.rm=T),
    abs_evdiff= mean(abs_evdiff, na.rm=T),
    m_ratio= mean(m_ratio, na.rm=T),
    meanChoiceRT= mean(meanChoiceRT, na.rm=T)) %>%
  ungroup()

#mean icc within each subject
get_icc <- function(data, vars, group_var="dataID") {
  res <- list(); icc<-list()
  if(names(data)[1]=="subID"){names(data)[1]=group_var}
  for (v in seq(vars)){
    # build formula
    fm <- as.formula(paste(vars[v], "~ game"))
    # run lme 
    res[[v]] <- lme(fm, random = ~1|dataID , data=data, na.action=na.omit)
    #save ICC using varcorr
    icc[[v]]<-round(as.numeric(VarCorr(res[[v]]))[1] / (as.numeric(VarCorr(res[[v]]))[1] + 
                                                          as.numeric(VarCorr(res[[v]]))[2]), digits=2)
  }
  icc<-as.data.frame(icc)
  colnames(icc)<-c(vars)
  return(icc)
}

# ICC mean level
vars = c('meanConf', 'abs_evdiff', 'm_ratio', 'meanChoiceRT')
icc_means<-get_icc(summary, vars) %>%
  dplyr::rename("metacognitive_bias" = meanConf)
icc = cbind(icc_means)

# calculate on just game 1-2 (~48 hours)
icc_2metacog_bias = list(); icc_2abs_evdiff = list(); icc_2choice_rt = list(); icc_2m_ratio = list()

# Generate all possible combinations sessions (e.g., 4-8; 1-2; 3-7)
all_combinations <- combn(1:8, 2)

for (comb in seq(all_combinations[1,])){
  
  # metacognitive bias:
  icc_2metacog_bias[[comb]]<-get_icc(filter(summary, game %in% c(all_combinations[1,comb],
                                                                 all_combinations[2,comb])),
                                     c("meanConf")) 
  
  # abs ev diff:
  icc_2abs_evdiff[[comb]]<-get_icc(filter(summary, game %in% c(all_combinations[1,comb],
                                                               all_combinations[2,comb])),
                                   c("abs_evdiff")) 
  #rt
  
  icc_2choice_rt[[comb]]<-get_icc(filter(summary, game %in% c(all_combinations[1,comb],
                                                              all_combinations[2,comb])),
                                  c("meanChoiceRT")) 
  #m-ratio
  icc_2m_ratio [[comb]]<-get_icc(filter(summary, game %in% c(all_combinations[1,comb],
                                                             all_combinations[2,comb])),
                                 c("m_ratio")) 
  
}


# Combine the ICC values into a data frame
icc_df = as.data.frame(unlist(icc_2metacog_bias)) %>%
  mutate(game1 = all_combinations[1,],
         game2 = all_combinations[2,])  %>% dplyr::rename("metacog_bias"=1) %>%
  # add abs ev diff estimates
  inner_join(as.data.frame(unlist(icc_2abs_evdiff)) %>%
               mutate(game1 = all_combinations[1,],
                      game2 = all_combinations[2,]) %>% dplyr::rename("absEvDiff"=1),
             by=c("game1", "game2")) %>%
  # add rt
  inner_join(as.data.frame(unlist(icc_2choice_rt)) %>%
               mutate(game1 = all_combinations[1,],
                      game2 = all_combinations[2,]) %>% dplyr::rename("choiceRT"=1),
             by=c("game1", "game2")) %>%
  #add m-ratio
  inner_join(as.data.frame(unlist(icc_2m_ratio)) %>%
               mutate(game1 = all_combinations[1,],
                      game2 = all_combinations[2,]) %>% dplyr::rename("m_ratio"=1),
             by=c("game1", "game2")) %>%
  
  pivot_longer(cols=c(1,4:6), names_to="behaviour", values_to="icc") %>% 
  mutate(behaviour = as.factor(behaviour)) %>% ungroup()

# Group by combinations and calculate mean and confidence interval
icc_summary <- icc_df %>%
  group_by(behaviour) %>%
  summarise(n= n(),
            mean_icc = round(mean(icc),2),
            sem_icc = round(sd(icc)/sqrt(n),2),
            ci_lower = round(mean_icc - 1.96 * sd(icc) / sqrt(n),2),
            ci_upper = round(mean_icc + 1.96 * sd(icc) / sqrt(n),2)) %>% ungroup()

# Print or use icc_summary as needed
print(icc_summary) 


icc_df<-filter(icc_df, behaviour == "metacog_bias")

# plot only meanConf
ggplot(icc_df, aes(x = as.factor(behaviour), fill=as.factor(behaviour), 
                   y = icc)) +
  geom_boxplot(outlier.shape = NA, alpha = .5, width = .1) +
  geom_jitter(aes(color=as.factor(behaviour)), width=0.2, alpha=0.5, shape=20, size=4)+
  theme_classic(base_size=24) +
  labs(y="ICC", x="") + 
  scale_x_discrete(labels=c("Metacognitive Bias")) + 
  theme(legend.position="none") 


