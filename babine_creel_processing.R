library(tidyverse)
library(grid)
library(gridExtra)
library(lubridate)
library(gt)

# 1. read data ------------------------------------------------------------

creel_2023 <- read_csv("data/2023 creel data.csv")
boatcounts <- read_csv("data/boatcounts.csv")
interviews_bydate <- read_csv("data/interviews_bydate.csv")
boat_eff <- read_csv("data/boat_effort.csv")

# 2. process and format data for analysis ---------------------------------

creel_bydate <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(`Time of Interview`)) %>%
  select(-c(CPUE, HPUE)) %>%
  rename(sockeye_killed = `Catch Killed`, rainbow_killed = `...14`, laketrout_killed = `...15`,
         whitefish_killed = `...16`, burbot_killed = `...17`, coho_killed = `...18`,
         sockeye_rel = `Catch Released`, rainbow_rel = `...20`, laketrout_rel = `...21`,
         whitefish_rel = `...22`, burbot_rel = `...23`, coho_rel = `...24`, total_sk_catch = `Total Sk Catch + Rel`,
         total_eff = `No Anglers X Hrs fished`) %>%
  group_by(Date) %>%
  add_count(Date, name = "n_interviews_on_date") %>%
  mutate(sockeye_killed = as.numeric(sockeye_killed), total_eff = as.numeric(total_eff), 
         total_sk_catch = as.numeric(total_sk_catch),
         daily_catch = sum(total_sk_catch),
         daily_harv = sum(sockeye_killed),
         Date = mdy(Date)) %>% 
  mutate(cpue = sum(total_sk_catch)/sum(total_eff),
         hpue = sum(sockeye_killed)/sum(total_eff),
         var_hpue = (sum(sockeye_killed - total_eff*hpue)^2)/ # change this so it adds CPUE/HPUE/variances by stratum instead of by date.
           (((sum(total_eff))/n_interviews_on_date)^2)*(n_interviews_on_date*(n_interviews_on_date-1)),
         var_cpue = (sum(total_sk_catch - total_eff*cpue)^2)/
           (((sum(total_eff))/n_interviews_on_date)^2)*(n_interviews_on_date*(n_interviews_on_date-1)))
  
creel_stratified <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(`Time of Interview`)) %>%
  select(-c(CPUE, HPUE)) %>%
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
                 total_sk_catch = as.numeric(total_sk_catch), Date = mdy(Date)) %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park")) %>%
  group_by(`Interview location`, day_type) %>%
  summarise("Number of interviews" = n(),
            "Total Effort" = mean(total_eff),
            "Total catch" = sum(total_sk_catch),
            "Total harvest" = sum(sockeye_killed),
            "CPUE" = sum(total_sk_catch)/sum(total_eff),
            "Variance (CPUE)" = (sum(total_sk_catch - total_eff*CPUE)^2)/
              (((sum(total_eff))/`Number of interviews`)^2)*(`Number of interviews`*(`Number of interviews`-1)),
            "HPUE" = sum(sockeye_killed)/sum(total_eff),
            "Variance (HPUE)" = (sum(sockeye_killed - total_eff*HPUE)^2)/ # change this so it adds CPUE/HPUE/variances by stratum instead of by date.
               (((sum(total_eff))/`Number of interviews`)^2)*(`Number of interviews`*(`Number of interviews`-1)))
  
  
# Write csv for reformatted data file
write.csv(creel_stratified, "~/coastland/lbn-sockeye-creel/data/creel2023_reformatted.csv", row.names = FALSE) 

# TABLE FOR STRATIFIED RESULTS:
summary_table <- gt(creel_stratified) %>%
  fmt_number(columns = c("CPUE", "HPUE"), decimals = 3, drop_trailing_zeros = TRUE) %>%
  fmt_scientific(columns = c("Variance (CPUE)", "Variance (HPUE)"), decimals = 2, exp_style = "e1") %>%
  tab_header(title = "Lake Babine Creel statistics by interview location and day type") |>
  cols_width(starts_with("Total Effort")~px(200)) |>
  cols_width(starts_with("day_type")~px(200)) |>
  cols_label(starts_with("day_type")~"")

# 3. Plot CPUE and HPUE trends by date ------------------------------------

cpue_pt <- creel_2023 %>%
  ggplot(aes(x = Date, y = cpue)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("CPUE (sockeye caught per hour of fishing)")


hpue_pt <- creel_2023 %>%
  ggplot(aes(x = Date, y = hpue)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal()+
  ylab("HPUE (sockeye harvested per hour of fishing)")

grid.arrange(cpue_pt, hpue_pt, nrow = 1, top=textGrob("Babine Lake creel survey CPUE and HPUE vs Date"))


# 4. Plot daily total catch and daily total harvest by date ---------------

harvestplot <- creel_2023 %>%
  ggplot(aes(x = Date, y = daily_harv))+
  geom_point() +
  theme_minimal() +
  #geom_smooth(colour = "grey40")+
  ylab("Daily total sockeye harvests")+
  ggtitle("Babine Lake sockeye catches (harvests and releases) by Date")
catchplot <- creel_2023 %>%
  ggplot(aes(x = Date, y = daily_catch))+
  geom_point() +
  theme_minimal() +
  #geom_smooth(colour = "grey40")+
  ylab("Daily total sockeye catches")+ xlab("")+
  ggtitle("Babine Lake sockeye harvests by Date")
grid.arrange(catchplot, harvestplot, nrow = 2)


# 5. Analysis of effort by day of the week and location -------------

### weekend vs weekday ANOVA
aov_day_eff <- aov(total_eff~day_type, data = creel_2023)
summary(aov_day_eff) # F = 11.29 on 1,750 Df, p = 0.0008
# Tested across all days of week using ANOVA: statistical difference (p = 0.033) 
# but maybe a result of weekday/weekend difference. So, try filtering out weekends and test:
weekdays <- creel_2023 %>%
  filter(day_type == "weekday")
aov_weekday_eff <- aov(total_eff ~ week_day, data = weekdays)
summary(aov_weekday_eff) # outcome: no significant difference BETWEEN WEEKDAYS
# use *weekday vs weekend* as strata (2 categories), rather than every day of week (7 categories). 
aov_daytype <- aov(cpue ~ day_type, data = creel_2023)
summary(aov_daytype) # p = 2.49e-07
#Effect of day type on number of interviews conducted per day:
aov_n_day <- aov(n_interviews_on_date ~ day_type, data = creel_2023)
summary(aov_n_day)

#effort by location
aov_loc <- aov(total_eff ~ `Interview location`, data = creel_2023)
summary(aov_loc) # F = 2.99 on 5,743 Df, p = 0.011

aov_loc_cpue <- aov(cpue ~ `Interview location`, data = creel_2023)
summary(aov_loc_cpue) # not statistically significant - same for HPUE

aov_n_loc <- aov(n_interviews_on_date ~ `Interview location`, data = creel_2023)
summary(aov_n_loc) # not significant

# 6. Boxplots -------------------------------------------------------------

# CPUE and HPUE by day type
cpueplot <- creel_2023 %>%
  ggplot(aes(x = day_type, y = cpue))+geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("CPUE")+
  ggtitle("CPUE on weekdays vs weekends")

hpueplot <- creel_2023 %>%
  ggplot(aes(x = day_type, y = hpue))+geom_boxplot() +
  theme_minimal()+
  xlab("") + ylab("HPUE")+
  ggtitle("HPUE on weekdays vs weekends")

grid.arrange(cpueplot, hpueplot, nrow = 1)
# CPUE and HPUE by location
cp_loc <- creel_2023 %>%
  filter(!is.na(`Interview location`)) %>%
  ggplot(aes(x = `Interview location`, y = cpue))+
  geom_boxplot() +
  theme_minimal() +
  ylab("CPUE") + xlab("") +
  ggtitle("CPUE by interview location")

hp_loc <- creel_2023 %>%
  filter(!is.na(`Interview location`)) %>%
  ggplot(aes(x = `Interview location`, y = hpue))+
  geom_boxplot() +
  theme_minimal() +
  ylab("HPUE") +
  ggtitle("HPUE by interview location")

grid.arrange(cp_loc, hp_loc, nrow = 2)


# 7. Estimate statistics by strata (day type and location) ----------------------
# calculate totals (effort, catch, harvest), then calculate CPUE/HPUE and its variance
strata_grouped <- creel_2023 %>%
  mutate(`Interview location` = recode(`Interview location`,
                                       BL = "Babine Lodge",
                                       CD = "Coop Dogs Resort",
                                       GM = "Granisle Marina",
                                       LB = "Lions Beach Park",
                                       RB = "Red Bluff Provincial Park")) %>%
  filter(!is.na(`Interview location`)) %>%
  group_by(`Interview location`, day_type) %>%
  summarise("Total Effort" = sum(total_eff),
            "Catch" = sum(total_sk_catch),
            "Harvest" = sum(sockeye_killed),
            "Mean CPUE" = mean(cpue),
            "Variance (CPUE)" = mean(var_cpue),
            "Mean HPUE" = mean(hpue),
            "Variance (HPUE)" = mean(var_hpue))

(summary_table <- gt(strata_grouped) %>%
  fmt_number(columns = c("Mean CPUE", "Mean HPUE"), decimals = 3, drop_trailing_zeros = TRUE) %>%
  fmt_scientific(columns = c("Variance (CPUE)", "Variance (HPUE)"), decimals = 2, exp_style = "e1") %>%
    tab_header(title = "Lake Babine Creel statistics by interview location and day type") |>
    cols_label(starts_with("day_type")~"") |>
    cols_width(starts_with("Total Effort")~px(200)))


creel_2023 %>%
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
  mutate(per_effort = `number of boats`/ Count,
         `number of boats` = ifelse(is.na(`number of boats`), 0, `number of boats`)) %>%
  filter(!is.na(Count))
hourly_boats$Date <- mdy(hourly_boats$Date)

# Plot percent of daily effort in each hourly time block
hourly_boats %>%
  mutate(hour_block = factor(`hour block`, levels = c("8:00", "12:00", "14:00", "16:00", "18:00"))) %>%
  ggplot(aes(x = hour_block, y = per_effort))+
  geom_boxplot() +
  xlab("Hourly time block") + ylab("Percent of daily boat effort") +
  theme_minimal() +
  ggtitle("Percent of daily boat effort by hour block during boat count days")

# Effort by area surveyed
hourly_boats %>%
  ggplot(aes(x = `Area Surveyed`, y = Effort)) +
  geom_boxplot()
# And by both area and date:
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
  ylab("Effort (average of hourly boat counts x 12h/boat)")

count_date <- hourly_boats %>%
  ggplot(aes(x = Date, y = Count)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "grey40")+
  theme_minimal() +
  ylab("Boat Count (total boats counted on day)")

grid.arrange(count_date, eff_date, nrow = 1, top = textGrob("Daily boat counts and daily boat effort by Date"))


# Merge interview & boat data to estimate TOTAL CATCH and HARVEST ---------
# use estimated CPUE and HPUE rates to take boat effort and estimate daily harvest 

# Prep creel data by keeping only the columns we want: CPUE, HPUE. Date. week_day. day_type. 
merge_creel <- creel_2023 %>%
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