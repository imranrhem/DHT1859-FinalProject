# data cleaning and variables

library(dplyr)
library(tidyverse)

# import dataset as dataframe
raw_liver_data <- read.csv(file = "project_data.csv", header = T)
names(raw_liver_data)
str(raw_liver_data)

# create new dataframe with only the variables that we need to use
liver_data <-  dplyr::select(raw_liver_data, c("Subject", "Gender", "Age", "BMI", "Time.from.transplant",
                                                "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                                "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Epworth.Sleepiness.Scale",
                                                "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                                                "SF36.PCS", "SF36.MCS"))
sapply(liver_data, class)

# convert categorical variables to factors
categorical <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                 "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid")

liver_data[categorical] <- lapply(liver_data[categorical], factor)
sapply(liver_data, class)

# inspect to see if there are any strange values that may not have been encoded by NA
summary(liver_data)

# Create binary variables for sleep disturbance scales (1 = yes, 0 = no)
liver_data2 <- liver_data %>%
  mutate(ESS.binary = ifelse(Epworth.Sleepiness.Scale > 10, "1", "0")) %>%
  mutate(PSQI.binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, "1", "0")) %>%
  mutate(AIS.binary = ifelse(Athens.Insomnia.Scale > 5, "1", "0"))

liver_data2[c("ESS.binary", "PSQI.binary", "AIS.binary")] <- lapply(liver_data2[c("ESS.binary", "PSQI.binary", "AIS.binary")], factor)

nrow(liver_data2) / 15

table(liver_data2$ESS.binary) / 15
table(liver_data2$PSQI.binary) / 15
table(liver_data2$AIS.binary) / 15
table(liver_data2$Berlin.Sleepiness.Scale)/15

