library(MASS)
library(car)

# Set of predictors to omit NAs from
predictors = c("Gender", "Age", "BMI", "Time.from.transplant", "Liver.Diagnosis", 
               "Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis",
               "Renal.Failure", "Depression", "Corticoid")

# Determine maximum number of predictors for each model
table(liver_data2$PSQI.binary) / 15 # 5 predictors
table(liver_data2$AIS.binary) / 15 # 7 predictors
table(liver_data2$ESS.binary) / 15 # 4 predicotrs
table(liver_data2$Berlin.Sleepiness.Scale)/15 # 6 predictors

#Backwards model selection for PSQI
psqi_full <- glm(PSQI.binary~ 
                   Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, 
                   data = na.omit(liver_data2[,c("PSQI.binary", predictors)]), family = "binomial")


psqi_backwards <- stepAIC(psqi_full, direction = "backward", trace = F)
summary(psqi_backwards) # does not follow p < m/15 = 5, remove Rejection.graft.dysfunction due to highest p-value

psqi_final <- glm(PSQI.binary~ 
                    Gender + Age + Recurrence.of.disease + Depression + Corticoid, 
                  data = na.omit(liver_data2[,c("PSQI.binary", predictors)]), family = "binomial")

anova(psqi_final, psqi_backwards, test = "Chisq") # p > 0.05, accept smaller model

summary(psqi_final)
exp(psqi_final$coefficients)
exp(confint(psqi_final))

#Backwards model selection for AIS
athens_full <- glm(AIS.binary~ 
                     Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, 
                   data = na.omit(liver_data2[,c("AIS.binary", predictors)]), family = "binomial")

athens_backwards <- stepAIC(athens_full, direction = "backward", trace = F) # follows p < m/15 = 7
summary(athens_backwards)
exp(athens_backwards$coefficients)
exp(confint(athens_backwards))

#Backwards model selection for ESS
ess_full <- glm(ESS.binary~ 
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, 
                  data = na.omit(liver_data2[,c("ESS.binary", predictors)]), family = "binomial")

vif(ess_full)
ess_backwards <- stepAIC(ess_full, direction = "backward", trace = F)
summary(ess_backwards) # does not follow, p < m/15 = 4, remove Renal.failure due to highest p-value

ess_final <- glm(ESS.binary~ 
                   Gender + Recurrence.of.disease + Depression + Corticoid, 
                   data = na.omit(liver_data2[,c("ESS.binary", predictors)]), family = "binomial")

anova(ess_final, ess_full, test = "Chisq")# p > 0.05, accept smaller model

summary(ess_final)
exp(ess_final$coefficients)
exp(confint(ess_final))

#Backwards model selection for BSS
bss_full <- glm(Berlin.Sleepiness.Scale~ 
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, 
                  data = na.omit(liver_data2[,c("Berlin.Sleepiness.Scale", predictors)]), family = "binomial")

vif(bss_full)

bss_backwards <- stepAIC(bss_full, direction = "backward", trace = F) # follows p < m/15 = 6
summary(bss_backwards)
exp(bss_backwards$coefficients)
exp(confint(bss_backwards))
