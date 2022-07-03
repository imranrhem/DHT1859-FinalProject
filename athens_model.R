# ATHENS LOGISTIC MODEL

attach(liver_data2)
library(car)

# total dof = 7
table(liver_data2$AIS.binary) / 15

# MODEL 1 #
athens_mod1 <- glm(AIS.binary~ 
        Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
        Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
        Renal.Failure + Depression + Corticoid, data = liver_data2, family = "binomial")

athens_mod1
vif(athens_mod1)

# Gender2, p = 0.8608
summary(athens_mod1)

# MODEL 2 #

# Removal of Gender as a predictor from model 
athens_mod2 <- glm(AIS.binary~ 
                     Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# comparison of with/without gender, p = 0.8608
anova(athens_mod2, athens_mod1, test = "Chisq")

# BMI, p = 0.735
summary(athens_mod2)
sum(vif(athens_mod2))

# MODEL 3 #

# Removal of BMI as a predictor
athens_mod3 <- glm(AIS.binary~ 
                     Age + Time.from.transplant + Liver.Diagnosis + 
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# comparsion of with/without BMI - p = 0.7348
anova(athens_mod3, athens_mod2, test = "Chisq")

#Liver.Diagnosis2 = p = 0.5539
summary(athens_mod3)
vif(athens_mod3)

# MODEL 4 #

# Removal of Liver.Diganosis as a predictor
athens_mod4 <- glm(AIS.binary~ 
                     Age + Time.from.transplant + Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# comparison of with/without Liver Diagnosis = 0.309
anova(athens_mod4, athens_mod3, test = "Chisq")

#Renal.Failure1, p = 0.5281
summary(athens_mod4)
vif(athens_mod4)

# MODEL 5 #

#Removal of Renal.Failure as a predictor
athens_mod5 <- glm(AIS.binary~ 
                     Age + Time.from.transplant + Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# Wtith/Without renal fialure, p = 0.3
anova(athens_mod5, athens_mod4, test = "Chisq")

#Time.from.transplant, p = 0.46791
summary(athens_mod5)
vif(athens_mod5)

# MODEL 6 #

#Removal of Time.from.transplant as a predictor
athens_mod6 <- glm(AIS.binary~ 
                     Age + Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# With/Without Time from transplant, p = 0.474
anova(athens_mod6, athens_mod5, test = "Chisq")

# Any.fobrosis, p = 0.39
summary(athens_mod6)

# MODEL 7 #

athens_mod7 <- glm(AIS.binary~ 
                     Age + Recurrence.of.disease + Rejection.graft.dysfunction + 
                     Depression + Corticoid, data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# With/without any.fibrosis, p = 0.3865
anova(athens_mod7, athens_mod6, test = "Chisq")

#Rejection graft dysfunction, p = 0.2857
summary(athens_mod7)

# MODEL 8 #

athens_mod8 <- glm(AIS.binary ~ Age + Recurrence.of.disease + Depression + Corticoid, 
                   data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

anova(athens_mod8, athens_mod7, test = "Chisq")

#Coritcoid, p = 0.13050
summary(athens_mod8)

# MODEL 9 # -> BEST MODEL, DO SOME ADDITIONAL TESTS

athens_mod9 <- glm(AIS.binary ~ Age + Recurrence.of.disease + Depression, 
                            data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

anova(athens_mod9, athens_mod8, test = "Chisq")
summary(athens_mod9)

athens_mod10 <- glm(AIS.binary ~ Age + Depression, 
                  data=na.omit(liver_data2[,all.vars(formula(athens_mod1))]), family = "binomial")

# MODEL 10 #

anova(athens_mod10, athens_mod9, test = "Chisq")
summary(athens_mod10)
exp(athens_mod10$coefficients)
