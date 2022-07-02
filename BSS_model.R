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

liver_data[categorical] <- lapply(liver_data[categorical], factor) #convert to factor
sapply(liver_data, class)

# inspect to see if there are any strange values that may not have been encoded by NA
summary(liver_data)

# Create binary variables for sleep disturbance scales (1 = yes, 0 = no)
liver_data2 <- liver_data %>%
  mutate(ESS.binary = ifelse(Epworth.Sleepiness.Scale > 10, "1", "0")) %>%
  mutate(PSQI.binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, "1", "0")) %>%
  mutate(AIS.binary = ifelse(Athens.Insomnia.Scale > 5, "1", "0"))

liver_data2[c("ESS.binary", "PSQI.binary", "AIS.binary")] <- lapply(liver_data2[c("ESS.binary", "PSQI.binary", "AIS.binary")], factor)

liver_data3 <- na.omit(liver_data2)
library(car)

summary(liver_data2$Berlin.Sleepiness.Scale)

Berlin_model1 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                 Renal.Failure + Depression + Corticoid, data= liver_data2, family = "binomial")
summary(Berlin_model1)
vif(Berlin_model1) #shows df of each of the predictor

#Removing renal.failure because p-value is high which is 0.9833
Berlin_model2 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis + Depression + Corticoid, 
                 data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model2)
vif(Berlin_model2)
anova(Berlin_model1, Berlin_model2, test = "Chisq") #model2 is better, shorter
#df is 6 for mine, #predictors

#Removing depression because p-value = 0.9763 
Berlin_model3 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                       Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis + Corticoid, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model3)
vif(Berlin_model3)
anova(Berlin_model2, Berlin_model3, test = "Chisq") #model 3 is better, p-value = 0.9763, 12df

#removing Corticoid because p-value for Corticoid = 0.6927
Berlin_model4 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                       Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model4)
vif(Berlin_model4)
anova(Berlin_model3, Berlin_model4, test = "Chisq")

#removing liver diagnosis because p-value for liver diagnosis2 = 0.6726
Berlin_model5 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                       Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model5)
sum(vif(Berlin_model5))
anova(Berlin_model4, Berlin_model5, test = "Chisq") #p-value 0.6579, go with model 5

#remove gender because p = 0.9191 
Berlin_model6 <- glm(Berlin.Sleepiness.Scale ~ Age + BMI + Time.from.transplant + 
                       Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model6)
vif(Berlin_model6)
anova(Berlin_model5, Berlin_model6, test = "Chisq") #p = 0.919

#removing age because p = 0.4874
Berlin_model7 <- glm(Berlin.Sleepiness.Scale ~ BMI + Time.from.transplant + 
                       Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model7)
vif(Berlin_model7) #count degrees of freedom
anova(Berlin_model6, Berlin_model7, test = "Chisq") #p = 0.4855

#removing Any.fibrosis because p = 0.4538
Berlin_model8 <- glm(Berlin.Sleepiness.Scale ~ BMI + Time.from.transplant + 
                       Recurrence.of.disease + Rejection.graft.dysfunction, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model8)
vif(Berlin_model8) #**model 8 is good, df = 4
anova(Berlin_model7, Berlin_model8, test = "Chisq") #0.4514

#removing Time.from.transplant because p-value is 0.1906
Berlin_model9 <- glm(Berlin.Sleepiness.Scale ~ BMI +
                       Recurrence.of.disease + Rejection.graft.dysfunction, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model9)
vif(Berlin_model9)
anova(Berlin_model8, Berlin_model9, test = "Chisq") 

#removing Rejection.graft.dysfunction because p = 0.1961
Berlin_model10 <- glm(Berlin.Sleepiness.Scale ~ BMI +
                       Recurrence.of.disease, 
                     data=na.omit(liver_data2[,all.vars(formula(Berlin_model1))]), family = "binomial")
summary(Berlin_model10)
vif(Berlin_model10)
anova(Berlin_model9, Berlin_model10, test = "Chisq")  #p = 0.1899

#If you cannot commit, do git stash then git stash apply in terminal


# MODEL 8 #
#ORs and CIs
anova(Berlin_model8, Berlin_model1, test = "Chisq") #p = 0.7681
summary(Berlin_model8)
exp(Berlin_model8$coefficients)
# Rejection.graft.dysfunction1 = 0.608255745
# Recurrence.of.disease1 = 2.110853702
# Time.from.transplant = 1.038912256
# BMI = 1.180362045
# Intercept = 0.004828451 

# Calculate 95% CIs
exp(confint(Berlin_model8))

# (Intercept)                  0.000779685 0.02534669
# BMI                          1.115592923 1.25585640
# Time.from.transplant         0.981203461 1.10072234
# Recurrence.of.disease1       1.047650271 4.30172982
# Rejection.graft.dysfunction1 0.287825620 1.24092360










