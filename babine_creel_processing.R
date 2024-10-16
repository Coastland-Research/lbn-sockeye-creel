library(tidyverse)
library(readxl)

creel_2023 <- read_csv("data/2023 creel data.csv")
boatcounts <- read_csv("data/boatcounts.csv")
interviews_bydate <- read_csv("data/interviews_bydate.csv")
boat_eff <- read_csv("data/boat_effort.csv")

creel_2023 <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(Date)) %>%
  select(-c(CPUE, HPUE)) %>%
  rename(sockeye_killed = `Catch Killed`, rainbow_killed = `...14`, laketrout_killed = `...15`,
         whitefish_killed = `...16`, burbot_killed = `...17`, coho_killed = `...18`,
         sockeye_rel = `Catch Released`, rainbow_rel = `...20`, laketrout_rel = `...21`,
         whitefish_rel = `...22`, burbot_rel = `...23`, coho_rel = `...24`, total_sk_catch = `Total Sk Catch + Rel`,
         total_eff = `No Anglers X Hrs fished`) %>%
  mutate(cpue = as.numeric(total_sk_catch)/as.numeric(total_eff),
         hpue = as.numeric(sockeye_killed)/as.numeric(total_eff))


# fixed CPUE and HPUE calculations
# separate CPUE and HPUE estimates by day_type:
weekdays <- creel_2023 %>%
  filter(day_type == "weekday")
mean(weekdays$cpue, na.rm = T)
mean(weekdays$hpue, na.rm = T)

weekends <- creel_2023 %>%
  filter(day_type == "weekend")
mean(weekends$cpue, na.rm = T)
mean(weekends$hpue, na.rm = T)

# Test whether CPUE and HPUE differ statistically between weekend and weekday
# creel surveys: t-test

t.test(cpue ~ day_type, data = creel_2023) # no significant difference

creel_2023 %>%
  ggplot(aes(x = day_type, y = cpue))+
  geom_boxplot()

t.test(hpue ~ day_type, data = creel_2023)

creel_2023 %>%
  ggplot(aes(x = day_type, y = hpue))+
  geom_boxplot()


# For each hour block, calculate % effort x boat effort -------------------
# use boatcounts data. 
hourly_boats <- boatcounts %>%
  pivot_longer(cols = `8:00`:`18:00`, names_to = "hour block", values_to = "number of boats")

hourly_boats %>%
  ggplot(aes(x = `hour block`, y = `number of boats`))+
  geom_boxplot()
