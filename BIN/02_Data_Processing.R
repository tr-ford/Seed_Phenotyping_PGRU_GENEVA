## Data_Processing.R 

## Date January 25, 2024

## Author: Tori Ford

# Objeective: Compare methods using stastitical methods to confirm agreement between old and new method of quantatative measurements.

library(plyr)
library(dplyr)
library(MethodCompare)
library(ggplot2)
library(irr)


merge_data <- read.csv("RAW/MERGED_DATA.csv")


# ## Compare LoA for Seed Count -------------------------------------------


## Use the measure_compare function to generate LoA statistics and plots
CNT_compare <- measure_compare(merge_data,new="MVN_CNT_NUM",Ref="HND_CNT_NUM",ID="ID")

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_BAlt_Seed_Num.svg",width = 6,height = 6)
bland_altman_plot(merge_data,new="MVN_CNT_NUM",Ref="HND_CNT_NUM",ID="ID",fill=TRUE)
dev.off()

## generate bias plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_BIAS_Seed_Num.svg",width = 6,height = 6)
bias_plot(CNT_compare)
dev.off()

## generate precision plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_PREC_Seed_Num.svg",width = 6,height = 6)
precision_plot(CNT_compare)
dev.off()

## generate bias plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_COMP_Seed_Num.svg",width = 6,height = 6)
compare_plot(CNT_compare)
dev.off()

# CNT_select <- merge_data %>% select(MVN_CNT_NUM,HND_CNT_NUM, SDCNT_CNT_NUM)
# CNT_icc <- icc(
#   CNT_select, model = "twoway", 
#   type = "agreement", unit = "average"
# )



# ## Compare LoA for TGW --------------------------------------------------

TGW_compare <- measure_compare(merge_data,new="MVN_TGW",Ref="HND_TGW",ID="ID")

## generate bland-altman plot for TGW, save as svg

svg("RAW/GRAPHs/B4_edit_BAlt_TGW.svg",width = 6,height = 6)
BA_TGW_Model <- bland_altman_plot(merge_data,new="MVN_TGW",Ref="HND_TGW",ID="ID",fill=TRUE)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_BIAS_TGW.svg",width = 6,height = 6)
bias_plot(TGW_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_PREC_TGW.svg",width = 6,height = 6)
precision_plot(TGW_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_COMP_TGW.svg",width = 6,height = 6)
compare_plot(TGW_compare)
dev.off()


# TGW_select <- merge_data %>% select(MVN_TGW,HND_TGW,SDCNT_TGW)
# TGW_icc <- icc(
#   TGW_select, model = "twoway", 
#   type = "agreement", unit = "average"
# )

# ## Compare LoA for Seed Width --------------------------------------------------

WIDTH_compare <- measure_compare(merge_data,new="MVN_WIDTH",Ref="HND_WIDTH",ID="ID")

## generate bland-altman plot for TGW, save as svg

svg("RAW/GRAPHs/B4_edit_BAlt_WIDTH.svg",width = 6,height = 6)
BA_WIDTH_Model <- bland_altman_plot(merge_data,new="MVN_WIDTH",Ref="HND_WIDTH",ID="ID",fill=TRUE)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_BIAS_WIDTH.svg",width = 6,height = 6)
bias_plot(WIDTH_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_PREC_WIDTH.svg",width = 6,height = 6)
precision_plot(WIDTH_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_COMP_WIDTH.svg",width = 6,height = 6)
compare_plot(WIDTH_compare)
dev.off()


# ## Compare LoA for Seed LENGTH --------------------------------------------------

LENGTH_compare <- measure_compare(merge_data,new="MVN_LENGTH",Ref="HND_LENGTH",ID="ID")

## generate bland-altman plot for TGW, save as svg

svg("RAW/GRAPHs/B4_edit_BAlt_LENGTH.svg",width = 6,height = 6)
BA_LENGTH_Model <- bland_altman_plot(merge_data,new="MVN_LENGTH",Ref="HND_LENGTH",ID="ID",fill=TRUE)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_BIAS_LENGTH.svg",width = 6,height = 6)
bias_plot(LENGTH_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_PREC_LENGTH.svg",width = 6,height = 6)
precision_plot(LENGTH_compare)
dev.off()

## generate bland-altman plot for seed count, save as svg

svg("RAW/GRAPHs/B4_edit_COMP_LENGTH.svg",width = 6,height = 6)
compare_plot(LENGTH_compare)
dev.off()


# Time Averages -----------------------------------------------------------


HND_AVG_TIME = round(sum(merge_data$HND_TIME_SEC)/nrow(merge_data), digits = 2)

MVN_AVG_TIME = round(sum(merge_data$MVN_TIME_SEC)/nrow(merge_data), digits = 2)

SDCNT_AVG_TIME = round(sum(merge_data$SDCNT_TIME_SEC)/nrow(merge_data), digits = 2)




