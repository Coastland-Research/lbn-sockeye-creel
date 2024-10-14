library(tidyverse)
library(readxl)

creel_2023 <- read_csv("data/2023 creel data.csv")

creel_2023 <- creel_2023 %>%
  subset(Date!= "Totals") %>%
  subset(!is.na(Date)) %>%
  #mutate(Date = as.Date(as.numeric(Date, "%m/%d/%y"))) %>%
  rename(sockeye_killed = `Catch Killed`, rainbow_killed = `...13`, laketrout_killed = `...14`,
         whitefish_killed = `...15`, burbot_killed = `...16`, coho_killed = `...17`,
         sockeye_rel = `Catch Released`, rainbow_rel = `...19`, laketrout_rel = `...20`,
         whitefish_rel = `...21`, burbot_rel = `...22`, coho_rel = `...23`)