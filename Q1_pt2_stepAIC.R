library(MASS)
library(car)
#Backwards model selection for AIS
athens_full <- glm(AIS.binary~ 
                     Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data = na.omit(liver_data2), family = "binomial")

vif(athens_full)

athens_backwards <- stepAIC(athens_full)
summary(athens_backwards)

#Backwards model selection for PSQI
psqi_full <- glm(PSQI.binary~ 
                   Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, data = na.omit(liver_data2), family = "binomial")

vif(psqi_full)

psqi_backwards <- stepAIC(psqi_full)
summary(psqi_backwards)

#Backwards model selection for ESS
ess_full <- glm(ESS.binary~ 
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_data2), family = "binomial")

vif(ess_full)

ess_backwards <- stepAIC(ess_full)
summary(ess_backwards)

#Backwards model selection for BSS
bss_full <- glm(Berlin.Sleepiness.Scale~ 
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_data2), family = "binomial")

vif(bss_full)

bss_backwards <- stepAIC(bss_full)
summary(bss_backwards)
