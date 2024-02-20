## Merge_Datasheets.R
## Objective: Merge Data Sheets into One Cohesive Sheet for Comparison

library(dplyr)


HND <- read.csv("RAW/HAND_4_MERGE.csv")
CNT <- read.csv("RAW/COUNTER_4_MERGE.csv")
MVN <- read.csv("RAW/MARVIN_4_MERGE.csv")


merged1 <- full_join(HND,CNT, by = "ID")
merged2 <- full_join(merged1, MVN, by = "ID")




write.csv(merged2, "RAW/MERGED_DATA.csv")


