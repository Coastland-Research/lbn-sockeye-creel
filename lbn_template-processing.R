library(tidyverse) # first make sure you have tidyverse package installed - run
# install.packages("tidyverse") in console if not already installed.

# 1. Read in the data as 3 data frames, one from each sheet of the excel 
# spreadsheet "LBN_creel-template.xlsx", each sheet saved as a csv:
interview_data <- read_csv("data/interview recording sheet.csv")
daily_data <- read_csv("data/daily angler profile.csv")
boatcount_data <- read_csv("data/boat counts.csv")

# 2. Calculate effort, catch, harvest, CPUE and HPUE by daytype-location strata 
creel_stratified <- interview_data %>%
  subset(!is.na(`Time of Interview`)) %>%
  filter(!is.na(`Interview location`)) %>%
  mutate(day_type = case_when(week_day == "Monday" | week_day == "Tuesday" | week_day == "Wednesday" | week_day == "Thursday" | week_day == "Friday" ~ "weekday",
                              week_day == "Saturday" | week_day == "Sunday" ~ "weekend")) %>%
  mutate(`SK killed` = ifelse(is.na(`SK killed`), 0, `SK killed`),
         `Total sk catch` = ifelse(is.na(`Total sk catch`), 0, `Total sk catch`)) %>%
  mutate(`SK killed` = as.numeric(`SK killed`), total_eff = as.numeric(`Total fishing effort`), 
         `Total sk catch` = as.numeric(`Total sk catch`), Date = mdy(Date)) %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park")) %>%
  group_by(`Interview location`, day_type) %>%
  summarise("Number of interviews" = n(),
            "Total Effort" = sum(`Total fishing effort`),
            "Total catch" = sum(`Total sk catch`),
            "Total harvest" = sum(`SK killed`),
            "CPUE" = sum(`Total sk catch`)/sum(`Total fishing effort`),
            "Variance (CPUE)" = (sum(`Total sk catch` - `Total fishing effort`*CPUE)^2)/
              (((sum(`Total fishing effort`))/`Number of interviews`)^2)*(`Number of interviews`*(`Number of interviews`-1)),
            "HPUE" = sum(`SK killed`)/sum(`Total fishing effort`),
            "Variance (HPUE)" = (sum(`SK killed` - `Total fishing effort`*HPUE)^2)/ # change this so it adds CPUE/HPUE/variances by stratum instead of by date.
              (((sum(`Total fishing effort`))/`Number of interviews`)^2)*(`Number of interviews`*(`Number of interviews`-1)))

# Create a table showing stats by stratum
summary_table <- gt(creel_stratified) %>%
  fmt_number(columns = c("CPUE", "HPUE"), decimals = 3, drop_trailing_zeros = TRUE) %>%
  fmt_scientific(columns = c("Variance (CPUE)", "Variance (HPUE)"), decimals = 2, exp_style = "e1") %>%
  tab_header(title = "Lake Babine Creel statistics by interview location and day type") |>
  cols_width(starts_with("Total Effort")~px(200)) |>
  cols_width(starts_with("day_type")~px(200)) |>
  cols_label(starts_with("day_type")~"")


# 2. Format interview data by day
# Add day_type
daily_data <- daily_data %>%
  mutate(day_type = case_when(week_day == "Monday" | week_day == "Tuesday" | week_day == "Wednesday" | week_day == "Thursday" | week_day == "Friday" ~ "weekday",
                              week_day == "Saturday" | week_day == "Sunday" ~ "weekend"))

# 3.  Plot stats by date to look at trends through the season
cpue_pt <- daily_data %>%
  ggplot(aes(x = Date, y = CPUE)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("CPUE (sockeye caught per hour of fishing)")

hpue_pt <- daily_data %>%
  ggplot(aes(x = Date, y = HPUE)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal()+
  ylab("HPUE (sockeye harvested per hour of fishing)")

grid.arrange(cpue_pt, hpue_pt, nrow = 1, top=textGrob("Babine Lake creel survey CPUE and HPUE vs Date"))

# Catch by date and harvest by date
catchplot <- daily_data %>%
  ggplot(aes(x = Date, y = `Total sk catches`))+
  geom_point() +
  theme_minimal() +
  ylab("Daily total sockeye catches")+ xlab("")+
  ggtitle("Babine Lake sockeye catches (harvests and releases) by Date")
harvestplot <- daily_data %>%
  ggplot(aes(x = Date, y = `Total sk harvests`))+
  geom_point() +
  theme_minimal() +
  ylab("Daily total sockeye harvests")+ xlab("Date")+
  ggtitle("Babine Lake sockeye harvests by Date")

grid.arrange(catchplot, harvestplot, nrow = 2)

# total effort by date: 
effortplot <- daily_data %>%
  ggplot(aes(x = Date, y = `Total fishing effort`)) +
  geom_point()+
  theme_minimal() +
  ylab("Daily effort (angler hours) from interviews")+
  ggtitle("Babine Lake Creel Survey Effort by Date")

# CPUE and HPUE by day type
cpueplot <- daily_data %>%
  ggplot(aes(x = day_type, y = CPUE))+
  geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("CPUE")+
  ggtitle("CPUE on weekdays vs weekends")

hpueplot <- daily_data %>%
  ggplot(aes(x = day_type, y = HPUE))+
  geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("HPUE")+
  ggtitle("HPUE on weekdays vs weekends")

grid.arrange(cpueplot, hpueplot, nrow = 1)

# Barplot showing total catch by both date (x) and location (colour):
daily_data %>%
  filter(!is.na(`Interview location`)) %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park")) %>%
  ggplot(aes(x = Date, y = `Total sk catches`, fill = `Interview location`))+
  geom_col() +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  ggtitle("Babine Lake creel survey: sockeye catch by date and location")+
  ylab("Total sockeye catch")

# 4. Analysis of Variance
aov_effort_daytype <- aov(`Total fishing effort` ~ day_type, data = daily_data)
summary(aov_effort_daytype)

aov_effort_daytype <- aov(`Total fishing effort` ~ day_type, data = daily_data)
summary(aov_effort_daytype)

# Repeat for other statistics of interest i.e. catch, harvest, CPUE and HPUE by day type and by 
# interview location.

# 5. Boat count data and estimation of harvest and catch

# put the data in long format and add percent effort by block:
hourly_boats <- boatcount_data %>%
  pivot_longer(cols = `8:00`:`18:00`, names_to = "hour block", values_to = "number of boats") %>%
  mutate(per_effort = `number of boats`/ `Daily boat count`,
         `number of boats` = ifelse(is.na(`number of boats`), 0, `number of boats`)) %>%
  filter(!is.na(`Daily boat count`))
hourly_boats$Date <- mdy(hourly_boats$Date)


# Plot percent of daily effort in each hourly time block
hourly_boats %>%
  mutate(hour_block = factor(`hour block`, levels = c("8:00", "12:00", "14:00", "16:00", "18:00"))) %>%
  ggplot(aes(x = hour_block, y = per_effort))+
  geom_boxplot() +
  xlab("Hourly time block") + ylab("Percent of daily boat effort") +
  theme_minimal() +
  ggtitle("Percent of daily boat effort by hour block during boat count days")

# Effort by area (colour) and date surveyed:
hourly_boats %>% 
  filter(!is.na(`Area Surveyed`)) %>%
  ggplot(aes(x = Date, y = Effort, fill = `Area Surveyed`))+
  geom_col() +
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal() +
  ylab("Boat effort") +
  ggtitle("Babine Lake creel survey: boat effort by date and area surveyed")

# Plot boat counts by date
eff_date <- hourly_boats %>%
  ggplot(aes(x = Date, y = `Daily boat effort`)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("Effort (average of hourly boat counts x 12h/boat)")

count_date <- hourly_boats %>%
  ggplot(aes(x = Date, y = `Daily boat count`)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("Boat Count (total boats counted on day)")

grid.arrange(count_date, eff_date, nrow = 1, top = textGrob("Daily boat counts and daily boat effort by Date"))

# plot percent effort by hour block (colour) and date (x), and y = total effort:
hourly_boats %>%
  rename("Hour Block" = `hour block`) %>%
  ggplot(aes(x = Date, y = per_effort, fill = `Hour Block`))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  theme_minimal() +
  ylab("Percent Effort") + xlab("Date") +
  ggtitle("Percent of daily boat effort by date and hour block of counting")

# merge daily effort data with boat count data:
merged.df <- merge(daily_data, hourly_boats, by = "Date") %>%
  mutate(boat_periods = n_distinct(`hour block`[`number of boats` > 0]),
         mean_boats = (sum(`number of boats`))/boat_periods*12)

# Add catch and harvest estimates
merged.df <- merged.df %>%
  mutate(catch = mean_boats*`mean anglers per boat`*CPUE,
         harvest = mean_boats*`mean anglers per boat`*HPUE) 

  
  