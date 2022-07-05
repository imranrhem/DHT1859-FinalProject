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

modESS9 <- glm(ESS.binary~ Rejection.graft.dysfunction +
                 Depression + Corticoid, data=liver_data2, family = "binomial")

summary(modESS9)
# Rejection.graft.dysfunction appears to be the most significant predictor
# Rejection.graft.dysfunction1   est = 0.6579, 0.0464 *  
# Depression1                    est = 0.5148, 0.1328    
# Corticoid1                     est = 0.3382, 0.2983  
# (Intercept)                    est = -1.4027 3.17e-10 ***


vif(modESS9) #df =3

anova(modESS8, modESS9, test = "Chisq")
# p-value = 0.05017


modESS9 <- glm(ESS.binary~ Rejection.graft.dysfunction +
                 Depression + Corticoid, data=liver_data2, family = "binomial")
summary(modESS9)
### ORs & CIs ####

# ORs for model 9 - Rejection.graft.dysfunction + Depression + Corticoid
exp(modESS9$coefficients)

# Rejection.graft.dysfunction1 = 1.9307769
# Depression1 = 1.6733334 
# Corticoid1 = 1.4023509
# Intercept = 0.2459276  

# Calculate 95% CIs
exp(confint(modESS9))

# (Intercept)                  0.1560900 0.3751905
# Rejection.graft.dysfunction1 1.0041912 3.6831691
# Depression1                  0.8455672 3.2563450
# Corticoid1                   0.7348915 2.6400258


# H0: OR = 1 - that there is no association between predictors and response variable (ESS)
# All predictor CIs contain 1, suggesting that we do not have strong evidence against the null hypothesis 

## Make additional models with dif combos of predictors to see if can get to significance...

# Adding Recurrence.of.disease back in
modESS10 <- glm(ESS.binary~ Rejection.graft.dysfunction +
                            Depression + Corticoid + Recurrence.of.disease, data=na.omit(liver_data2[,all.vars(formula(modESS))]), family = "binomial")

summary(modESS10)
# (Intercept)                   -1.5238     0.2437  -6.253 4.04e-10 ***
# Rejection.graft.dysfunction1   est = 0.4730, p-value = 0.182    
# Depression1                    est = 0.5162, p-value = 0.133    
# Corticoid1                     est = 0.4354, p-value = 0.191    
# Recurrence.of.disease1         est = 0.5045, p-value = 0.157


anova(modESS10, modESS9, test = "Chisq")
# P-value = 0.1604
# Adding recurrence of disease increased the p-value in the anova but improved the p-value of Corticoid from 0.2983 in modESS9
# to 0.191 in modESS10 but increased the p-value of Rejection.graft.dysfunction from 0.0464 to 0.182 

#### Additional Predictions ####

## Do additional testing on model - attempt to predict if positive on ESS based on setting certain predictors positive

ESS_pred_df1 <- data.frame(Depression="1", Rejection.graft.dysfunction="1",Corticoid="1")

predict(modESS9, newdata = ESS_pred_df1, type = "response")
# My modESS9 predicts the probability of testing pos on ESS for someone who is pos on all 3 predictors is 0.5270167

ESS_pred_df2 <- data.frame(Depression="1", Rejection.graft.dysfunction="1",Corticoid="1", Recurrence.of.disease = "1")

predict(modESS10, newdata = ESS_pred_df2, type = "response")
# My modESS10 predicts the probability of testing pos on ESS for someone pos in all 4 predictors is 0.599969
