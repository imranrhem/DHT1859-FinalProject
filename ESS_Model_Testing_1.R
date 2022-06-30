# Using backwards selection model 
# numerical is sample size, cat = number of __ in the smallest group

## use liver_data3 for comparing liver_data2 for the inital summary investigation
## use that equation for your second model for all future models 
## use formula when creating the models 

library(car)

modESS <- glm(ESS.binary~Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                Renal.Failure + Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS)

# remove Renal.Failure1 - p-value =  0.9833 

modESS2 <- glm(ESS.binary~Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS2)

vif(modESS2) # df = 13
anova(modESS,modESS2, test = "Chisq")

# p value = 0.06802

# remove Time.from.transplant - p-value = 0.9507

modESS3 <- glm(ESS.binary~Gender + Age + BMI + Liver.Diagnosis + 
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS3)

vif(modESS3) # df = 12
anova(modESS2,modESS3, test = "Chisq")
# p value = 0.9507

# remove Recurrence.of.disease1 p=0.8403 

modESS4 <- glm(ESS.binary~Gender + Age + BMI + Liver.Diagnosis + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS4)
vif(modESS4) # df = 11
anova(modESS3, modESS4, test = "Chisq")

# remove Age p=0.8077
modESS5 <- glm(ESS.binary~Gender + BMI + Liver.Diagnosis + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

