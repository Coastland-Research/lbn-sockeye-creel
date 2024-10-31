library(tidyverse) # first make sure you have tidyverse package installed - run
# install.packages("tidyverse") in console if not already installed.

# Read in the data as 3 data frames, one from each sheet of the excel 
# spreadsheet "LBN_creel-template.xlsx", each sheet saved as a csv:
interview_data <- read_csv("data/interview recording sheet.csv")
daily_data <- read_csv("data/daily angler profile.csv")
boatcount_data <- read_csv("data/boat counts.csv")

