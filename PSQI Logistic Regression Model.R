# BTC1859 Group Project
# Logistic Regression Models for PSQI
summary(liver_data2)
library(car)

# Model 1: all potential predictors included.
PSQI_mod1 <- glm(PSQI.binary ~ Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, data = liver_data2, family = "binomial")

summary(PSQI_mod1)

# Model 2: BMI removed (largest p = 0.6544).
PSQI_mod2 <- glm(PSQI.binary ~ Gender + Age + Time.from.transplant + Liver.Diagnosis + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod2)

# Comparing the "goodness of fit" between Model 1 & 2.
anova(PSQI_mod1, PSQI_mod2, test = "Chisq")

#' H0: the smaller model (Model 2) is the correct model.
#' p = 0.6529, suggesting that the larger model (Model 1) does not fit data 
#' significantly better than the smaller model (Model 2). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 2).

vif(PSQI_mod1) # df = 14
vif(PSQI_mod2) # df = 13

# Model 3: Renal.Failure removed (largest p = 0.60171).
PSQI_mod3 <- glm(PSQI.binary ~ Gender + Age + Time.from.transplant + Liver.Diagnosis + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod3)

# Comparing the "goodness of fit" between Model 2 & 3.
anova(PSQI_mod2, PSQI_mod3, test = "Chisq")

#' H0: the smaller model (Model 3) is the correct model.
#' p = 0.5935, suggesting that the larger model (Model 2) does not fit data 
#' significantly better than the smaller model (Model 3). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 3).

vif(PSQI_mod2) # df = 13
vif(PSQI_mod3) # df = 12

# Model 4: Liver.Diagnosis removed (largest p = 0.58169).
PSQI_mod4 <- glm(PSQI.binary ~ Gender + Age + Time.from.transplant + 
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod4) 

# Comparing the "goodness of fit" between Model 3 & 4.
anova(PSQI_mod3, PSQI_mod4, test = "Chisq")

#' H0: the smaller model (Model 4) is the correct model.
#' p = 0.6702, suggesting that the larger model (Model 3) does not fit data 
#' significantly better than the smaller model (Model 4). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 4).

vif(PSQI_mod3) # df = 12
vif(PSQI_mod4) # df = 8

# Model 5: Any.fibrosis removed (largest p = 0.52454).
PSQI_mod5 <- glm(PSQI.binary ~ Gender + Age + Time.from.transplant + 
                   Recurrence.of.disease + Rejection.graft.dysfunction +
                   Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod5)

# Comparing the "goodness of fit" between Model 4 & 5.
anova(PSQI_mod4, PSQI_mod5, test = "Chisq")

#' H0: the smaller model (Model 5) is the correct model.
#' p = 0.5227, suggesting that the larger model (Model 4) does not fit data 
#' significantly better than the smaller model (Model 5). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 5).

vif(PSQI_mod4) # df = 8
vif(PSQI_mod5) # df = 7

# Model 6: Time.from.transplant removed (largest p = 0.29748).
PSQI_mod6 <- glm(PSQI.binary ~ Gender + Age +  
                   Recurrence.of.disease + Rejection.graft.dysfunction +
                   Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod6)

# Comparing the "goodness of fit" between Model 5 & 6.
anova(PSQI_mod5, PSQI_mod6, test = "Chisq")

#' H0: the smaller model (Model 6) is the correct model.
#' p = 0.2949, suggesting that the larger model (Model 5) does not fit data 
#' significantly better than the smaller model (Model 6). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 6).

vif(PSQI_mod5) # df = 7
vif(PSQI_mod6) # df = 6

# Model 7: Age removed (p = 0.07423).
PSQI_mod7 <- glm(PSQI.binary ~ Gender + Recurrence.of.disease + Depression + Corticoid, 
                 data = na.omit(liver_data2[,all.vars(formula(PSQI_mod1))]), 
                 family = "binomial")

summary(PSQI_mod7)

# Comparing the "goodness of fit" between Model 6 & 7.
anova(PSQI_mod6, PSQI_mod7, test = "Chisq")

#' H0: the smaller model (Model 7) is the correct model.
#' p = 0.0537, suggesting that the larger model (Model 6) does not fit data 
#' significantly better than the smaller model (Model 7). Therefore, we fail to
#' reject H0, and we adopt the smaller model (Model 7).
exp(PSQI_mod7$coefficients)
exp(confint(PSQI_mod7))
vif(PSQI_mod6) # df = 6
vif(PSQI_mod7) # df = 5



