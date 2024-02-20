## Data_Crunch.R
## Tori Ford
## Goal: Make a series of correlative tests and statistical tests to determine what 
## treatment provides the most accurate measures compared to hand measurements, also consider time
## This is to determine the cost-benifit of said treatments
## Proposed Task: Use Inter-Class Correlation to identify the reliability of the three raters(treatments).
# https://medium.com/@SalahAssana/a-beginners-guide-to-the-intraclass-correlation-coefficient-icc-288f7fe7bcfc - "Interrater Reliability: 
# Measures the degree of agreement between different raters assessing the same data. We measure interrater reliability when a dataset is being
# assessed by one or more raters. It is important to have good interrater reliability because people are subjective so different raters’ evaluations 
# naturally differ. Researchers should aim to minimize subjectivity in their study to ensure others can replicate the same results."

## ICC: Two-Way Mixed Effect Model Type, Single Rater, Absolute Agreement 

## https://cran.r-project.org/web/packages/irr/irr.pdf

## https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/

## https://doi.org/10.1371%2Fjournal.pone.0219854 

# Anova 
# T-Test
# ICC (?)

library(plyr)
library(dplyr)
library(ggplot2)
library(irr)



prelim <- read.csv("RAW/Prelim_Data.csv")


abvr <- prelim %>% select(X.Seeds_MARVIN,X.Seed_Hand, X.Seed_Counter)


iccy <- icc(
  abvr, model = "twoway", 
  type = "agreement", unit = "average"
)
