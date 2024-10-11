# R script created by Jacob J. Coutts (Copyright 2024)
# Conditional Process Analysis Lecture (PSYC489)

# load required packages 
library(ggplot2)
library(jtools)
source("/Users/jjcoutts/Library/CloudStorage/GoogleDrive-jjcoutts@umd.edu/My Drive/FlashDrive/Macros/PROCESS/PROCESS v4.3 for R/process.R")
# Make sure the source command above routes to where PROCESS is located in YOUR computer

# read in data and summarize - be sure to change file path to appropriate location on your computer
selfc <- read.table(file="/Users/jjcoutts/Library/CloudStorage/GoogleDrive-jjcoutts@umd.edu/My Drive/FlashDrive/Data/CSVs/self_compassion.csv", sep=",", header = TRUE)
summary(self)

# fit the model 
m_mod <- lm(scc_TOT ~ self_TOT*cesd_TOT, data =selfc); summary(m_mod)
y_mod <- lm(swl_TOT ~ self_TOT*cesd_TOT + scc_TOT*cesd_TOT, data = selfc); summary(y_mod)

# both effects sig

### end of script 