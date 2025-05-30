library(tidyverse)
library(grid)
library(gridExtra)
library(lubridate)
library(gt)
library(plotrix)
library(goeveg)

# 1. read data ------------------------------------------------------------

creel_2023 <- read_csv("data/2023 creel data.csv")
boatcounts <- read_csv("data/boatcounts.csv")
interviews_bydate <- read_csv("data/interviews_bydate.csv")
boat_eff <- read_csv("data/boat_effort.csv")

# 2. process and format data for analysis ---------------------------------
 
# interviews grouped by day
creeldata <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(`Time of Interview`)) %>%
  select(-c(CPUE, HPUE)) %>%
  rename(sockeye_killed = `Catch Killed`, rainbow_killed = `...14`, laketrout_killed = `...15`,
         whitefish_killed = `...16`, burbot_killed = `...17`, coho_killed = `...18`,
         sockeye_rel = `Catch Released`, rainbow_rel = `...20`, laketrout_rel = `...21`,
         whitefish_rel = `...22`, burbot_rel = `...23`, coho_rel = `...24`, total_sk_catch = `Total Sk Catch + Rel`,
         total_eff = `No Anglers X Hrs fished`) %>%
  mutate(day_type = case_when(week_day == "Monday" | week_day == "Tuesday" | week_day == "Wednesday" | week_day == "Thursday" | week_day == "Friday" ~ "weekday",
                              week_day == "Saturday" | week_day == "Sunday" ~ "weekend")) %>%
  group_by(Date) %>%
  mutate(sockeye_killed = as.numeric(sockeye_killed), total_eff = as.numeric(total_eff), 
         total_sk_catch = as.numeric(total_sk_catch),
         Date = mdy(Date), 
         cpue = sum(total_sk_catch)/sum(total_eff),
         hpue = sum(sockeye_killed)/sum(total_eff), 
         daily_eff = sum(total_eff),
         daily_catch = sum(total_sk_catch),
         daily_harv = sum(sockeye_killed))
  
# interviews grouped by STRATUM
creel_stratified <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(`Time of Interview`)) %>%
  filter(!is.na(`Interview location`)) %>%
  rename(sockeye_killed = `Catch Killed`, rainbow_killed = `...14`, laketrout_killed = `...15`,
         whitefish_killed = `...16`, burbot_killed = `...17`, coho_killed = `...18`,
         sockeye_rel = `Catch Released`, rainbow_rel = `...20`, laketrout_rel = `...21`,
         whitefish_rel = `...22`, burbot_rel = `...23`, coho_rel = `...24`, total_sk_catch = `Total Sk Catch + Rel`,
         total_eff = `No Anglers X Hrs fished`) %>%
  mutate(day_type = case_when(week_day == "Monday" | week_day == "Tuesday" | week_day == "Wednesday" | week_day == "Thursday" | week_day == "Friday" ~ "weekday",
                              week_day == "Saturday" | week_day == "Sunday" ~ "weekend")) %>%
  mutate(sockeye_killed = ifelse(is.na(sockeye_killed), 0, sockeye_killed),
         total_sk_catch = ifelse(is.na(total_sk_catch), 0, total_sk_catch)) %>%
  mutate(sockeye_killed = as.numeric(sockeye_killed), total_eff = as.numeric(total_eff), 
                 total_sk_catch = as.numeric(total_sk_catch), Date = mdy(Date),
         CPUE = (total_sk_catch/total_eff), HPUE = (sockeye_killed/total_eff)) %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park"))

creel_grouped <- creel_stratified %>%
  group_by(`Interview location`, day_type) %>%
  summarise("Number of interviews" = n(),
            "Total Effort" = sum(total_eff),
            "Total catch" = sum(total_sk_catch),
            "Total harvest" = sum(sockeye_killed),
            "Mean CPUE" = mean(CPUE),
          #  "variance (CPUE)" = var(CPUE),
          #  "SD (CPUE)" = sd(CPUE),
            "SE (CPUE)" = std.error(CPUE),
            "CV (CPUE)" = cv(CPUE),
            "Mean HPUE" = mean(HPUE),
          #  "SD (HPUE)" = sd(HPUE),
            "SE (HPUE)" = std.error(HPUE),
            "CV (HPUE)" = cv(HPUE))
  
creel_total <- creel_stratified %>%
  summarise("Number of interviews" = n(),
            "Total Effort" = sum(total_eff),
            "Total catch" = sum(total_sk_catch),
            "Total harvest" = sum(sockeye_killed),
            "Mean CPUE" = mean(CPUE),
            "Mean HPUE" = mean(HPUE)
            )
  
# Write csv for reformatted data file
write.csv(creeldata, "~/coastland/lbn-sockeye-creel/data/creel2023_reformatted.csv", row.names = FALSE) 

# TABLE FOR STRATIFIED RESULTS:
summary_table <- gt(creel_grouped) %>%
  fmt_number(columns = c("Mean CPUE", "Mean HPUE"), n_sigfig = 3, drop_trailing_zeros = TRUE) %>%
  fmt_number(columns = c("SE (CPUE)", "CV (CPUE)", "SE (HPUE)", "CV (HPUE)"), decimals = 3) %>%
  tab_header(title = "Lake Babine Creel statistics by interview location and day type") |>
  cols_width(starts_with("Total Effort")~px(200)) |>
  cols_width(starts_with("day_type")~px(200)) |>
  cols_label(starts_with("day_type")~"")

# TABLE FOR TOTAL STATS
  
# Calculate overall totals (across all locations and day types)
overall_totals <- creel_stratified %>%
  summarize(
    `Interview location` = "Total",
    day_type = "",
    `Number of interviews` = n(),
    `Total Effort` = sum(total_eff),
    `Total catch` = sum(total_sk_catch),
    `Total harvest` = sum(sockeye_killed),
    "Mean CPUE" = mean(CPUE),
    # "SD (CPUE)" = sd(CPUE),
    "SE (CPUE)" = std.error(CPUE),
    "CV (CPUE)" = cv(CPUE),
    "Mean HPUE" = mean(HPUE),
    # "SD (HPUE)" = sd(HPUE),
    "SE (HPUE)" = std.error(HPUE),
    "CV (HPUE)" = cv(HPUE))

# Combine the grouped data with the overall totals
final_data <- bind_rows(creel_grouped, overall_totals)

# Create the gt table
summary_table <- gt(final_data) %>%
  fmt_number(columns = c("Mean CPUE", "Mean HPUE"), decimals = 3, drop_trailing_zeros = TRUE) %>%
  fmt_number(columns = c("SE (CPUE)", "CV (CPUE)", "SE (HPUE)","CV (HPUE)"), decimals = 3) %>%
  tab_header(title = "Lake Babine Creel statistics by interview location and day type") %>%
  cols_width(starts_with("Total Effort") ~ px(200)) %>%
  cols_width(starts_with("day_type") ~ px(200)) %>%
  cols_label(starts_with("day_type") ~ "") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = `Interview location` == "Total")
  )

# Print the table
summary_table

###
creel_grouped2 <- creel_stratified %>%
  group_by(day_type) %>%
  summarise("Number of interviews" = n(),
            "Total Effort" = sum(total_eff),
            "Total catch" = sum(total_sk_catch),
            "Total harvest" = sum(sockeye_killed),
           # "SE (Harvest)" = std.error(sockeye_killed),
            `Harvest lower 95%` = sum(sockeye_killed) - 1.96 * sd(sockeye_killed) * sqrt(n()),
            `Harvest upper 95%` = sum(sockeye_killed) + 1.96 * sd(sockeye_killed) * sqrt(n()),
            "Mean CPUE" = mean(CPUE),
           # "SE (CPUE)" = std.error(CPUE),
            "CV (CPUE)" = cv(CPUE),
            "Mean HPUE" = mean(HPUE),
            #"SE (HPUE)" = std.error(HPUE),
            "CV (HPUE)" = cv(HPUE))
overall_totals2 <- creel_stratified %>%
  summarize(
    day_type = "All Days",
    `Number of interviews` = n(),
    `Total Effort` = sum(total_eff),
    `Total catch` = sum(total_sk_catch),
    `Total harvest` = sum(sockeye_killed),
   # "SE (Harvest)" = std.error(sockeye_killed),
    `Harvest lower 95%` = sum(sockeye_killed) - 1.96 * sd(sockeye_killed) * sqrt(n()),
    `Harvest upper 95%` = sum(sockeye_killed) + 1.96 * sd(sockeye_killed) * sqrt(n()),
    "Mean CPUE" = mean(CPUE),
#    "SE (CPUE)" = std.error(CPUE),
    "CV (CPUE)" = cv(CPUE),
    "Mean HPUE" = mean(HPUE),
   # "SE (HPUE)" = std.error(HPUE),
    "CV (HPUE)" = cv(HPUE)
  )

final_data2 <- bind_rows(creel_grouped2, overall_totals2)

summary_table2 <- gt(final_data2) %>%
  fmt_number(columns = c("Mean CPUE", "Mean HPUE"), decimals = 3, drop_trailing_zeros = TRUE) %>%
  fmt_number(columns = c("CV (CPUE)","CV (HPUE)"), decimals = 3) %>%
  fmt_number(columns = c(`Harvest lower 95%`, `Harvest upper 95%`, `Total Effort`), decimals = 0, sep_mark = "") %>%
  tab_header(title = "Lake Babine Creel statistics") %>%
  cols_width(starts_with("Total Effort") ~ px(100)) %>%
  cols_width(starts_with("day_type") ~ px(100)) %>%
  cols_label(starts_with("day_type") ~ "Day Type") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = day_type == "All Days"))

# Print the table
summary_table2

# 3. Plot CPUE and HPUE trends by date ------------------------------------

cpue_pt <- creeldata %>%
  ggplot(aes(x = Date, y = cpue)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("CPUE (sockeye caught per hour of fishing)")

hpue_pt <- creeldata %>%
  ggplot(aes(x = Date, y = hpue)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal()+
  ylab("HPUE (sockeye harvested per hour of fishing)")

grid.arrange(cpue_pt, hpue_pt, nrow = 1, top=textGrob("Babine Lake creel survey CPUE and HPUE vs Date"))

# 4. Plot daily total catch and daily total harvest by date ---------------

catchplot <- creeldata %>%
  ggplot(aes(x = Date, y = daily_catch))+
  geom_col(fill = "#66C2A5") +
  theme_minimal() +
  ylab("Daily total sockeye catches")+ xlab("")+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  ggtitle("Babine Lake sockeye catches (harvests and releases) by Date")

harvestplot <- creeldata %>%
  ggplot(aes(x = Date, y = daily_harv))+
  geom_col(fill = "#3288BD") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  ylab("Daily total sockeye harvests")+ xlab("Date")+
  ggtitle("Babine Lake sockeye harvests by Date")

grid.arrange(catchplot, harvestplot, nrow = 2)

# total effort by date: 
creeldata %>%
  ggplot(aes(x = Date, y = daily_eff)) +
  geom_col(fill = "#66C2A5")+
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  ylab("Daily effort (angler hours) from interviews")+
  ggtitle("Babine Lake Creel Survey Effort by Date")

# 5. Analysis of effort by day of the week and location -------------
# significant difference in EFFORT by daytype
# significant difference in CPUE and HPUE by daytype
# significant difference in EFFORT by location
# NO significant difference in CPUE by location.

### weekend vs weekday ANOVA
aov_day_eff <- aov(total_eff~day_type, data = creeldata)
summary(aov_day_eff) # F = 11.29 on 1,750 Df, p = 0.0008

# Tested across all days of week using ANOVA: statistical difference (p = 0.033) 
# but maybe a result of weekday/weekend difference. So, try filtering out weekends and test:
weekends <- creeldata %>%
  filter(day_type == "weekend")
aov_weekday_eff <- aov(total_eff ~ week_day, data = weekdays)
summary(aov_weekday_eff) # outcome: no significant difference BETWEEN WEEKDAYS
# use *weekday vs weekend* as strata (2 categories), rather than every day of week (7 categories). 

# Difference in CPUE or HPUE?
aov_daytype <- aov(cpue~day_type, data = creeldata)
summary(aov_daytype) # p = 2.49e-7

aov_daytype2 <- aov(hpue~day_type, data = creeldata)
summary(aov_daytype2)

aov_catch <- aov(total_sk_catch~day_type, data = creeldata)
summary(aov_catch)
aov_h <- aov(sockeye_killed~day_type, data = creeldata)
summary(aov_h)

#effort by location
aov_loc <- aov(total_eff ~ `Interview location`, data = creeldata)
summary(aov_loc) # F = 2.99 on 5,743 Df, p = 0.011
# CPUE and HPUE by location:
aov_loc_cpue <- aov(cpue ~ `Interview location`, data = creeldata)
summary(aov_loc_cpue) 

aov_loc_hpue <- aov(hpue ~ `Interview location`, data = creeldata)
summary(aov_loc_hpue) 


# 6. Boxplots -------------------------------------------------------------
# CPUE and HPUE by day type
cpueplot <- creeldata %>%
  ggplot(aes(x = day_type, y = cpue))+geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("CPUE")+
  ggtitle("CPUE on weekdays vs weekends")

hpueplot <- creeldata %>%
  ggplot(aes(x = day_type, y = hpue))+geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("HPUE")+
  ggtitle("HPUE on weekdays vs weekends")

grid.arrange(cpueplot, hpueplot, nrow = 1)

# CPUE and HPUE by location
cp_loc <- creeldata %>%
  filter(!is.na(`Interview location`)) %>%
  ggplot(aes(x = `Interview location`, y = cpue))+
  geom_boxplot() +
  theme_minimal() +
  ylab("CPUE") + xlab("") +
  ggtitle("CPUE by interview location")

hp_loc <- creeldata %>%
  filter(!is.na(`Interview location`)) %>%
  ggplot(aes(x = `Interview location`, y = hpue))+
  geom_boxplot() +
  theme_minimal() +
  ylab("HPUE") +
  ggtitle("HPUE by interview location")

grid.arrange(cp_loc, hp_loc, nrow = 2)

# Barplot showing total catch by both date (x) and location (colour):
creeldata %>%
  filter(!is.na(`Interview location`)) %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park")) %>%
  ggplot(aes(x = Date, y = total_sk_catch, fill = `Interview location`))+
  geom_col() +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  ggtitle("Babine Lake creel survey: sockeye catch by date and location")+
  ylab("Total sockeye catch")

# 8. Boat count data ---------------------------------------------------------

# For each hour block, calculate % effort x boat effort -------------------
hourly_boats <- boatcounts %>%
  pivot_longer(cols = `8:00`:`18:00`, names_to = "hour block", values_to = "number of boats") %>%
  filter(!is.na(`Lake Cond`)) %>% # remove the days where boat counts were not done
  mutate(per_effort = `number of boats`/ Count)
         # `number of boats` = ifelse(is.na(`number of boats`), 0, `number of boats`)) %>%
  # filter(!is.na(Count))
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
  ggplot(aes(x = Date, y = Effort)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  ylab("Effort (average of hourly boat counts x 12h/boat)")

count_date <- hourly_boats %>%
  ggplot(aes(x = Date, y = Count)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  ylab("Boat Count (total boats counted on day)")

grid.arrange(count_date, eff_date, nrow = 1, top = textGrob("Babine Lake daily boat counts and daily boat effort"))

# plot percent effort by hour block (colour) and date (x), and y = total effort:
hourly_boats %>%
  rename("Hour Block" = `hour block`) %>%
  ggplot(aes(x = Date, y = per_effort, fill = `Hour Block`))+
  geom_col()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  theme_minimal() +
  ylab("Percent Effort") + xlab("Date") +
  ggtitle("Percent of daily boat effort by date and hour block of counting")
  

# 9. Merge interview & boat data to estimate TOTAL CATCH and HARVEST ---------
# use estimated CPUE and HPUE rates to take boat effort and estimate daily harvest 

# Prep creel data by keeping only the columns we want: CPUE, HPUE. Date. week_day. day_type. 
merge_creel <- creeldata %>%
  select(Date, week_day, day_type, cpue, hpue) %>%
  distinct(.keep_all = T)

#prep boat effort sheet:
merge_boat_eff <- boat_eff %>%
  select(Date, `Boat Effort`, `Mean Anglers/Boat`) %>%
  mutate(Date = mdy(Date))


# merge hourly_boats and creel_2023 by date, remove unnecessary columns:
merged.df <- merge(merge_creel, hourly_boats, by = "Date") %>%
  select(-c(Weather, `Lake Cond`, `...12`, ))

merged.df2 <- merge(merged.df, merge_boat_eff, by = "Date")

boat_periods_per_date <- merged.df2 %>%
  group_by(Date) %>%
  summarise(boat_periods = n_distinct(`hour block`[`number of boats` > 0]),
            mean_boats = (sum(`number of boats`))/boat_periods*12)

# Join the result back to the original data frame
merged.df2 <- merged.df2 %>%
  left_join(boat_periods_per_date, by = "Date")

# Add column for estimate of HARVEST and CATCH per hour period by multiplying HPUE (from interview info) with daily total effort
# estimate (the product of number of boats in period * avg anglers per boat)

merged.df2 <- merged.df2 %>%
  mutate(catch = mean_boats*`Mean Anglers/Boat`*cpue,
         harvest = mean_boats*`Mean Anglers/Boat`*hpue) 