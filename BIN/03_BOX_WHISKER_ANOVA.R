## BOX_WHISKER_ANOVA.R
## Tori Ford
## Feb 5, 2024

library(plyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(multcompView)
library(reshape)
library(reshape2)
library(rstatix)
library(RColorBrewer)



# Prepare Data for Comparison ---------------------------------------------


## Read in Datasheet Containing ALL Methods
merge_data <- read.csv("RAW/MERGED_DATA.csv")


## Select Out Time Variables
time_merge <- merge_data[c("ID","HND_TIME_SEC","MVN_TIME_SEC","SDCNT_TIME_SEC")]

## Change Names
colnames(time_merge) <- c('id','Hand', "MARViN","SeedCounter")


## Select Out Seed Count Variables
count_merge <- merge_data[c("ID","HND_CNT_NUM","MVN_CNT_NUM","SDCNT_CNT_NUM")]

## Change Names
colnames(count_merge) <- c('id','Hand', "MARViN","SeedCounter")


## Select Out Thousand Grain Weight Variables
tgw_merge <- merge_data[c("ID","HND_TGW","MVN_TGW","SDCNT_TGW")]

## Change Names
colnames(tgw_merge) <- c('id','Hand', "MARViN","SeedCounter")


## Select Out Seed Width Variables
width_merge <- merge_data[c("ID","HND_WIDTH","MVN_WIDTH")]

## Change Names
colnames(width_merge) <- c('id','Hand', "MARViN")


## Select Out Seed Width Variables
length_merge <- merge_data[c("ID","HND_LENGTH","MVN_LENGTH")]

## Change Names
colnames(length_merge) <- c('id','Hand', "MARViN")

# Melt Frames
melt_time <- melt(time_merge)

melt_seednum <- melt(count_merge)

melt_tgw <- melt(tgw_merge)

melt_width <- melt(width_merge)

melt_length <- melt(length_merge)


# Whisker Measurement Method Comparisons ----------------------------------


## Time Whisker Plots

t <- ggplot(melt_time, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free") + ylab("Time(sec)") +xlab("Method") +stat_boxplot(geom = 'errorbar')
ggsave("RST/Time_Whisker_Plot.svg", plot = t, width = 8, height = 8)
ggsave("RST/Time_Whisker_Plot.png", plot = t, width = 8, height = 8)


## Seed Count Whisker Plots

sn <- ggplot(melt_seednum, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free") + ylab("Seeds(n)") +xlab("Method") +stat_boxplot(geom = 'errorbar')
ggsave("RST/SeedNum_Whisker_Plot.svg", plot = sn, width = 8, height = 8)
ggsave("RST/SeedNum_Whisker_Plot.png", plot = sn, width = 8, height = 8)


## Thousand Grain Weight Whisker Plots

tgw <- ggplot(melt_tgw, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free") + ylab("Thousand Grain Weight(g)") +xlab("Method") +stat_boxplot(geom = 'errorbar')
ggsave("RST/TGW_Whisker_Plot.svg", plot = tgw, width = 8, height = 8)
ggsave("RST/TGW_Whisker_Plot.png", plot = tgw, width = 8, height = 8)


## Seed Width Whisker Plots

widths <- ggplot(melt_width, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free_x") + ylab("Seed Width(mm)") +xlab("Method") +stat_boxplot(geom = 'errorbar')
ggsave("RST/width_Whisker_Plot.svg", plot = widths, width = 8, height = 8)
ggsave("RST/width_Whisker_Plot.png", plot = widths, width = 8, height = 8)


## Seed Width Whisker Plots

slength <- ggplot(melt_length, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free") + ylab("Seed Length(mm)") +xlab("Method") +stat_boxplot(geom = 'errorbar')
ggsave("RST/length_Whisker_Plot.svg", plot = slength, width = 8, height = 8)
ggsave("RST/length_Whisker_Plot.png", plot = slength, width = 8, height = 8)




# ANOVAs ------------------------------------------------------------------
## Section Modified Directly From: https://statdoe.com/one-way-anova-and-box-plot-in-r/

## Anova on Time Diff
# Select 2 Melted Columns, One with Identifyers for Method, One For Individual Measuments
time_aov <- melt_time[c("variable","value")]
colnames(time_aov) <- c('MeasurementMethod','Time')

# Run Anova (Time by~ Measurement Method)
anova_time <- aov(Time ~ MeasurementMethod, data = time_aov)
summary(anova_time)

# Run Tukey Post-Hoc
tukey_time <- TukeyHSD(anova_time)

# Compact Letter Display to Identify Significance Groups
cld_time <- multcompLetters4(anova_time, tukey_time)
print(cld_time)

# Create Table with 3rd Quantile Paired With CLD Groups for Graphing Later
tk_time <- group_by(time_aov, MeasurementMethod) %>%
  summarise(mean=mean(Time), quant = quantile(Time, probs = 0.75)) %>%
  arrange(desc(mean))
cld_time <- as.data.frame.list(cld_time$MeasurementMethod)
tk_time$cld <- cld_time$Letters

# Plot
time_plot <- ggplot(time_aov, aes(MeasurementMethod, Time)) + 
  geom_boxplot(aes(fill=MeasurementMethod)) + theme_bw() +
  labs(x="Measurement Method", y="Time (sec)",title = "Comparing Time Per Measurement from 3 Measurement Methods") +
  geom_text(data = tk_time, aes(x=MeasurementMethod,y=quant,label=cld), vjust=-2,hjust=-2) +
  scale_fill_brewer(palette = "Blues")
svg("RST/Anova_Time.svg", width = 10, height = 8)
time_plot
dev.off()
ggsave("RST/Anova_Time.png", plot = time_plot, width = 10, height = 8)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #
  
## Anova on Seed Count
# Select 2 Melted Columns, One with Identifyers for Method, One For Individual Measuments
count_aov <- melt_seednum[c("variable","value")]
colnames(count_aov) <- c('MeasurementMethod','Count')

# Run Anova (Time by~ Measurement Method)
anova_count <- aov(Count ~ MeasurementMethod, data = count_aov)
summary(anova_count)

# Run Tukey Post-Hoc
tukey_count <- TukeyHSD(anova_count)

# Compact Letter Display to Identify Significance Groups
cld_count <- multcompLetters4(anova_count, tukey_count)
print(cld_count)

# Create Table with 3rd Quantile Paired With CLD Groups for Graphing Later
tk_count <- group_by(count_aov, MeasurementMethod) %>%
  summarise(mean=mean(Count), quant = quantile(Count, probs = 0.75)) %>%
  arrange(desc(mean))
cld_count <- as.data.frame.list(cld_count$MeasurementMethod)
tk_count$cld <- cld_count$Letters

# Plot Counnt
count_plot <- ggplot(count_aov, aes(MeasurementMethod, Count)) + 
  geom_boxplot(aes(fill=MeasurementMethod)) + theme_bw() +
  labs(x="Measurement Method", y="Seed Count",title = "Comparing Seed Count from 3 Measurement Methods") +
  geom_text(data = tk_count, aes(x=MeasurementMethod,y=quant,label=cld), vjust=-2,hjust=-2) +
  scale_fill_brewer(palette = "Blues")
svg("RST/Anova_Count.svg", width = 10, height = 8)
count_plot
dev.off()
ggsave("RST/Anova_Count.png", plot = count_plot, width = 10, height = 8)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #

## Anova on Thousand Grain Weight
# Select 2 Melted Columns, One with Identifyers for Method, One For Individual Measuments
tgw_aov <- melt_tgw[c("variable","value")]
colnames(tgw_aov) <- c('MeasurementMethod','TGW')

# Run Anova (TGW by~ Measurement Method)
anova_tgw <- aov(TGW ~ MeasurementMethod, data = tgw_aov)
summary(anova_tgw)

# Run Tukey Post-Hoc
tukey_tgw <- TukeyHSD(anova_tgw)

# Compact Letter Display to Identify Significance Groups
cld_tgw <- multcompLetters4(anova_tgw, tukey_tgw)
print(cld_tgw)

# Create Table with 3rd Quantile Paired With CLD Groups for Graphing Later
tk_tgw <- group_by(tgw_aov, MeasurementMethod) %>%
  summarise(mean=mean(TGW), quant = quantile(TGW, probs = 0.75)) %>%
  arrange(desc(mean))
cld_tgw <- as.data.frame.list(cld_tgw$MeasurementMethod)
tk_tgw$cld <- cld_tgw$Letters

# Plot Counnt
tgw_plot <- ggplot(tgw_aov, aes(MeasurementMethod, TGW)) + 
  geom_boxplot(aes(fill=MeasurementMethod)) + theme_bw() +
  labs(x="Measurement Method", y="Thousand Grain Weight (g)",title = "Comparing Thousand Grain Weights from 3 Measurement Methods") +
  geom_text(data = tk_tgw, aes(x=MeasurementMethod,y=quant,label=cld), vjust=-2,hjust=-2) +
  scale_fill_brewer(palette = "Blues")
svg("RST/Anova_TGW.svg", width = 10, height = 8)
tgw_plot
dev.off()
ggsave("RST/Anova_TGW.png", plot = tgw_plot, width = 10, height = 8)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #

## Anova on Seed Width
# Select 2 Melted Columns, One with Identifyers for Method, One For Individual Measuments
width_aov <- melt_width[c("variable","value")]
colnames(width_aov) <- c('MeasurementMethod','Width')

# Run Anova (TGW by~ Measurement Method)
anova_width <- aov(Width ~ MeasurementMethod, data = width_aov)
summary(anova_width)

# Run Tukey Post-Hoc
tukey_width <- TukeyHSD(anova_width)

# Compact Letter Display to Identify Significance Groups
cld_width <- multcompLetters4(anova_width, tukey_width)
print(cld_width)

# Create Table with 3rd Quantile Paired With CLD Groups for Graphing Later
tk_width <- group_by(width_aov, MeasurementMethod) %>%
  summarise(mean=mean(Width), quant = quantile(Width, probs = 0.75)) %>%
  arrange(desc(mean))
cld_width <- as.data.frame.list(cld_width$MeasurementMethod)
tk_width$cld <- cld_width$Letters

# Plot Counnt
width_plot <- ggplot(width_aov, aes(MeasurementMethod, Width)) + 
  geom_boxplot(aes(fill=MeasurementMethod)) + theme_bw() +
  labs(x="Measurement Method", y="Seed Width (mm)",title = "Comparing Seed Width from 2 Measurement Methods") +
  geom_text(data = tk_width, aes(x=MeasurementMethod,y=quant,label=cld), vjust=-2,hjust=-2) +
  scale_fill_brewer(palette = "Blues")
svg("RST/Anova_Width.svg", width = 10, height = 8)
width_plot
dev.off()
ggsave("RST/Anova_Width.png", plot = width_plot, width = 10, height = 8)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ #

## Anova on Seed Length
# Select 2 Melted Columns, One with Identifyers for Method, One For Individual Measuments
length_aov <- melt_length[c("variable","value")]
colnames(length_aov) <- c('MeasurementMethod','Length')

# Run Anova (Length by~ Measurement Method)
anova_length <- aov(Length ~ MeasurementMethod, data = length_aov)
summary(anova_length)

# Run Tukey Post-Hoc
tukey_length <- TukeyHSD(anova_length)

# Compact Letter Display to Identify Significance Groups
cld_length <- multcompLetters4(anova_length, tukey_length)
print(cld_tgw)

# Create Table with 3rd Quantile Paired With CLD Groups for Graphing Later
tk_length <- group_by(length_aov, MeasurementMethod) %>%
  summarise(mean=mean(Length), quant = quantile(Length, probs = 0.75)) %>%
  arrange(desc(mean))
cld_length <- as.data.frame.list(cld_length$MeasurementMethod)
tk_length$cld <- cld_length$Letters

# Plot Counnt
length_plot <- ggplot(length_aov, aes(MeasurementMethod, Length)) + 
  geom_boxplot(aes(fill=MeasurementMethod)) + theme_bw() +
  labs(x="Measurement Method", y="Seed Length (mm)",title = "Comparing Seed Length from 2 Measurement Methods") +
  geom_text(data = tk_length, aes(x=MeasurementMethod,y=quant,label=cld), vjust=-2,hjust=-2) +
  scale_fill_brewer(palette = "Blues")
svg("RST/Anova_Length.svg", width = 10, height = 8)
length_plot
dev.off()
ggsave("RST/Anova_Length.png", plot = length_plot, width = 10, height = 8)


# Linear Regression (Seed Count by Time) ----------------------------------
## MODIFIED FROM: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
## Equation -- time = Intercept + (m*Seed Count)


# Subset Data, Time and Seedcount, Change Names
lm_all <- merge_data[c("MVN_TIME_SEC", "MVN_CNT_NUM","HND_TIME_SEC","HND_CNT_NUM","SDCNT_TIME_SEC", "SDCNT_CNT_NUM")]
colnames(lm_all) <- c("MARViN_Time", "MARViN_Seed_Count", "Hand_Time","Hand_Seed_Count", "SeedCounter_Time", "SeedCounter_Seed_Count")


# Linear Models (Intercepts + Slopes)
linear_mvn <- lm(MVN_TIME_SEC~MVN_CNT_NUM, data = merge_data)

linear_hnd <- lm(HND_TIME_SEC~HND_CNT_NUM, data = merge_data)

linear_sdcnt <- lm(SDCNT_TIME_SEC~SDCNT_CNT_NUM, data = merge_data)

# Calculate Linear Intercepts
# MARViN+Hand INtercept
mvhdint <- rbind(coef(linear_mvn),coef(linear_hnd)) # Coefficient matrix
mvhdint<- c(-solve(cbind(mvhdint[,2],-1)) %*% mvhdint[,1]) #Output to Ln. 206
# SeedCounter+Hand INtercept
schdint <- rbind(coef(linear_sdcnt),coef(linear_hnd)) # Coefficient matrix
schdint<- c(-solve(cbind(schdint[,2],-1)) %*% schdint[,1]) # Output to Ln. 208


## Melt Dataframe to Suit Plotting
lm_mvn <- merge_data[c("MVN_TIME_SEC","MVN_CNT_NUM")]
lm_mvn$method <- "MARViN"
colnames(lm_mvn) <- c("time","seedcount","method")

lm_hand <- merge_data[c("HND_TIME_SEC","HND_CNT_NUM")]
lm_hand$method <- "Hand"
colnames(lm_hand) <- c("time","seedcount","method")

lm_sdcnt <- merge_data[c("SDCNT_TIME_SEC", "SDCNT_CNT_NUM")]
lm_sdcnt$method <- "SeedCounter"
colnames(lm_sdcnt) <- c("time","seedcount","method")

# Combine All Data
lm_all <- rbind(lm_mvn, lm_hand, lm_sdcnt)
lm_all_melt <- melt(lm_all)

# Subset Back into Groups
mvn <- subset(lm_all, method == "MARViN")
hnd <- subset(lm_all, method == "Hand")
sdcnt <- subset(lm_all, method == "SeedCounter")


svg("RST/regression.svg", width = 10, height = 6)
# jpeg("RST/regression.jpg", units = "px", width = 750, height = 600)
plot(x=lm_all$seedcount, y=lm_all$time, 
     main = "Time by Seedcount",
     xlab = "Seed Count",
     ylab = "Time per Measurement (sec)",
     col = ifelse(lm_all$method=="MARViN","lightblue",ifelse(lm_all$method=="Hand","lightgreen","#fff687"))
     )#c("lightblue","lightgreen","#fff687")
legend("topleft", 
       pch = c(1, 1), 
       c("MARViN", "Hand","SeedCounter"), 
       col = c("lightblue","lightgreen","#fff687")) 
abline(lm(mvn$time ~ mvn$seedcount), col = "lightblue", lwd=3)
abline(lm(hnd$time ~ hnd$seedcount), col = "lightgreen", lwd=3)
abline(lm(sdcnt$time ~ sdcnt$seedcount), col = "#fff687", lwd=3)
## MARVIN Breakpoint
segments(x0 = mvhdint[1],y0 = -82,x1=mvhdint[1],y1=mvhdint[2], lty=2)
## SeedCounter Breakpoint
segments(x0 = schdint[1],y0 = -82,x1=schdint[1],y1=schdint[2], lty=2)
dev.off()
# dev.off()

# Heatmap Measure Method Comparisons ----------------------------------------------



## Create Correlation Matrix for Seed Count Comparison
# 
# Remove id column
cm <- count_merge[c("Hand","MARViN","SeedCounter")]
# Calculate Pearsons Correlation
count_corr <- cor_mat(cm, method = "pearson")
# Plot the Pearson's Correlation
cor_plot(count_corr, type = "lower", label = TRUE, method = "pie",) + title("Pearson's Correlation Between Seed Count Measurements")



## Create Correlation Matrix for TGW Comparison
# 
# Remove id column
tgwm <- tgw_merge[c("Hand","MARViN","SeedCounter")]
# Calculate Pearsons Correlation
tgw_corr <- cor_mat(tgwm, method = "pearson")
# Plot the Pearson's Correlation
cor_plot(tgw_corr, type = "lower", label = TRUE, method = "pie",) + title("Pearson's Correlation Between TGW Measurements")



## Create Correlation Matrix for Seed Width Comparison
# 
# Remove id column
widthm <- width_merge[c("Hand","MARViN")]
# Calculate Pearsons Correlation
width_corr <- cor_test(widthm, method = "pearson") 
width_corr %>% as_cor_mat()

# Plot the Pearson's Correlation
cor_plot(width_corr, type = "lower", label = TRUE, method = "pie",) + title("Pearson's Correlation Between TGW Measurements")

