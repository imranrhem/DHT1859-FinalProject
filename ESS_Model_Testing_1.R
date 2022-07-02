# Using backwards selection model 


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

# p value = 0.06802, suggesting that we adopt the smaller model 

# remove Time.from.transplant - p-value = 0.9507

modESS3 <- glm(ESS.binary~Gender + Age + BMI + Liver.Diagnosis + 
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS3)

vif(modESS3) # df = 12
anova(modESS2,modESS3, test = "Chisq")
# p value = 0.9507, suggesting that we adopt the smaller model 

# remove Recurrence.of.disease1 p=0.8403 

modESS4 <- glm(ESS.binary~Gender + Age + BMI + Liver.Diagnosis + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS4)
vif(modESS4) # df = 11
anova(modESS3, modESS4, test = "Chisq")

# remove Age p-value=0.8077
modESS5 <- glm(ESS.binary~Gender + BMI + Liver.Diagnosis + Rejection.graft.dysfunction + Any.fibrosis +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS5)
vif(modESS5) # df = 10 

anova(modESS4, modESS5, test = "Chisq")
# p-value = 0.8078, adopt smaller model

# remove Any.Fibrosis, p-value = 0.7569 

modESS6 <- glm(ESS.binary~Gender + BMI + Liver.Diagnosis + Rejection.graft.dysfunction +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS6)
vif(modESS6) # df = 9

anova(modESS5, modESS6, test = "Chisq")
# p-value = 0.7566, accept smaller model 

# Remove BMI, p-value = 0.5202

modESS7 <- glm(ESS.binary~Gender +  Liver.Diagnosis + Rejection.graft.dysfunction +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS7)
vif(modESS7) # df = 8

anova(modESS6, modESS7, test = "Chisq")
# p-value = 0.5213, p-value still no significant, adopt smaller model 

# Remove Gender, p-value = 0.33353
modESS8 <- glm(ESS.binary~Liver.Diagnosis + Rejection.graft.dysfunction +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS8)
vif(modESS8) # df = 7

anova(modESS7, modESS8, test = "Chisq")
# p-value = 0.3302, adopt smaller model 

# Remove Liver.Diagnosis, as Liver.Diagnosis4 p value= 0.22342 and Liver.Diagnosis2 p value = 0.26457 

modESS9 <- glm(ESS.binary~ Rejection.graft.dysfunction +
                 Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS9)
vif(modESS9) #df =3

anova(modESS8, modESS9, test = "Chisq")
# p-value = 0.05017
