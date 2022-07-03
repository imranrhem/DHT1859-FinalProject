# Q1: Part 1

library(dplyr)
#### Calculate prevalence of sleep disturbance based on ESS ####

# Prevalence = # of ppl w sleep disturbance/ n (total number of values)

attach(liver_data2)

prop.table(table(ESS.binary))

# prevalence of sleep disturbance when using ESS = 0.2669323
prev_ESS <- 0.2669323

#### Calculate Sensitivity of ESS in relation to each test ####

#### ESS Sensitivity in relation to  PSQI ####

# Probability of having a positive result on ESS, given a positive result on PSQI
# P(ESS+|PSQI+) = P(PSQI+ & ESS+)/P(PSQI+)

# Remove NAs rows 

ESS_PSQI <- liver_data2 %>%
  drop_na(c(PSQI.binary,ESS.binary))

nrow(ESS_PSQI)
# Total of 179 rows
attach(ESS_PSQI)

# Get number of rows where PSQI and ESS both = 1 

length(which(ESS.binary == "1" & PSQI.binary == "1"))

# 34 rows where pt who are positive for both the PSQI and the ESS 
# Find proportion of those who tested positive for both PSQI and ESS

ESSp_PSQIp <- 34/179
# P(PSQI+ & ESS+) = 0.1899441
ESSp_PSQIp <- 0.1899441 

# Find the proportion of positive cases for PSQI

prop.table(table(liver_data2$PSQI.binary))
# P(PSQI+) = 0.5464481

prev_PSQI <- 0.5464481

# Final calculation 
# P(ESS+|PSQI+) = P(PSQI+ & ESS+)/P(PSQI+)

sens_ESS_PSQI <- ESSp_PSQIp/prev_PSQI
sens_ESS_PSQI
# The sensitivity of ESS in relation to the PSQI test is 0.3475977

#### ESS sensitivity in relation to BSS ####

# Probability of having a positive result on ESS, given a positive result on BSS
# P(ESS+|BSS+) = P(BSS+ & ESS+)/P(BSS+)

# Remove NAs from BSS and ESS rows 

ESS_BSS <- liver_data2 %>%
  drop_na(c(Berlin.Sleepiness.Scale, ESS.binary))

nrow(ESS_BSS)
# 249 rows, NAs removed

# Find proportion of positive tests for BSS 

prop.table(table(liver_data2$Berlin.Sleepiness.Scale))
# P(BSS+) = 0.389313
prev_BSS <- 0.389313

# Find the number of pt who tested pos on both ESS and BSS

attach(ESS_BSS)
length(which(Berlin.Sleepiness.Scale == 1 & ESS.binary == 1))

# 37 pt tested positive for both 

# Find proportion of those who tested positive for both ESS and BSS

ESSp_BSSp <-37/249 

# P(BSS+ & ESS+) = 0.1485944
ESSp_BSSp <- 0.1485944

# P(ESS+|BSS+) = P(BSS+ & ESS+)/P(BSS+)

sens_ESS_BSS <- ESSp_BSSp/prev_BSS
sens_ESS_BSS
# P(ESS+|BSS+) =0.3816836
# The sensitivity for ESS in relation to the BSS test is 0.3816836

#### Calculate sensitivity of ESS in relation to AIS ####

# Probability of having a positive result on ESS, given a positive result on BSS
# P(ESS+|AIS+) = P(AIS+ & ESS+)|P(AIS+)


ESS_AIS <- liver_data2 %>%
  drop_na(c(AIS.binary, ESS.binary))

nrow(ESS_AIS)
# 250 rows, NAs removed

# Find proportion of positive tests for AIS 

prop.table(table(liver_data2$AIS.binary))
# P(AIS+) = 0.5534351
prev_AIS <- 0.5534351

# Find the number of pt who tested positive on both ESS and AIS

attach(ESS_AIS)
length(which(AIS.binary == 1 & ESS.binary == 1))

# 46 pt tested positive for both test

# Find proportion of those who tested positive for both ESS and BSS
# # P(AIS+ & ESS+) 
ESSp_AISp <- 46/250

# P(AIS+ & ESS+) = 0.184
# P(ESS+|AIS+) = P(BSS+ & AIS+)|P(AIS+)

sens_ESS_AIS <- ESSp_AISp/prev_AIS
sens_ESS_AIS

# P(ESS+|AIS+) = 0.332469
# The sensitivity for ESS in relation to the AIS test is 0.332469

#### Calculate specificity of ESS in relation to all other tests ####

#### Specificity of of ESS in relation to PSQI ####

#P(ESS-|PSQI-) = P(PSQI- & ESS-)/P(1-P(PSQI))

attach(ESS_PSQI)

# Get number of rows where PSQI and ESS both = 0 

length(which(ESS.binary == "0" & PSQI.binary == "0"))
# P(PSQI- & ESS-) = 68

# Recall a total of 179 rows with NAs removed

# Find proportion who tested negative on both tests

ESSn_PSQIn <- 68/179
# P(PSQI- & ESS-) = 0.3798883

spec_ESS_PSQI <- (ESSn_PSQIn)/ (1-prev_PSQI)
spec_ESS_PSQI
# P(ESS-|PSQI-) = 0.837585
# The specificity of ESS in relation to PSQI is 0.837585

#### Specificity of of ESS in relation to BSS ####

#P(ESS-|BSS-) = P(BSS- & ESS-)/P(1-P(BSS))

attach(ESS_BSS)

# Get number of rows where BSS and ESS both = 0 

length(which(ESS.binary == "0" & Berlin.Sleepiness.Scale == "0"))
# P(BSS- & ESS-) = 123

# Recall a total of 249 rows with NAs removed

# Find proportion who tested negative on both tests

ESSn_BSSn <- 123/249
# P(BSS- & ESS-) =  0.4939759

spec_ESS_BSS <- (ESSn_BSSn)/ (1-prev_BSS)
spec_ESS_BSS
# P(ESS-|BSS-) = 0.8088856
# The specificity of ESS in relation to BSS is 0.8088856

#### Specificity of of ESS in relation to AIS ####

#P(ESS-|AIS-) = P(AIS- & ESS-)/P(1-P(AIS))

attach(ESS_AIS)

# Get number of rows where AIS and ESS both = 0 

length(which(ESS.binary == "0" & AIS.binary == "0"))
# P(AIS- & ESS-) = 90

# Recall a total of 250 rows with NAs removed

# Find proportion who tested negative on both tests

ESSn_AISn <- 90/250
# P(AIS- & ESS-) =  0.36


spec_ESS_AIS <- (ESSn_AISn)/(1-prev_AIS)
spec_ESS_AIS

# P(ESS-|AIS-) = 0.8061538
# The specificity of ESS in relation to AIS is 0.8061538

#### Calculate NPV & PPV ####

#### NPV of ESS in relation to PSQI ####

#P(PSQI-|ESS-) = P(ESS-|PSQI-)/ P(ESS-)
ESS_NPV_PSQI <- (ESSn_PSQIn) / (1-prev_ESS) 
# ESS_NPV_PSQI = 0.5182172

#### PPV of ESS in relation to PSQI ####

#P(PSQI+|ESS+) = P(ESS+|PSQI+)/ P(ESS+)
ESS_PPV_PSQI <- (ESSp_PSQIp)/(prev_ESS)
# ESS_PPV_PSQI = 0.7115816

#### NPV of ESS in relation to BSS ####

#P(BSS-|ESS-) = P(ESS-|BSS-)/ P(ESS-)
ESS_NPV_BSS <- (ESSn_BSSn)/ (1-prev_ESS)
# ESS_NPV_BSS = 0.6738476

#### PPV of ESS in relation to BSS ####

#P(BSS+|ESS+) = P(ESS+|BSS+)/ P(ESS+)
ESS_PPV_BSS <- (ESSp_BSSp)/(prev_ESS)
# ESS_PPV_BSS = 0.5566745

#### NPV of ESS in relation to AIS ####

#P(AIS-|ESS-) = P(ESS-|AIS-)/ P(ESS-)
ESS_NPV_AIS <- (ESSn_AISn)/ (1-prev_ESS)

# ESS_NPV_AIS = 0.491087

#### PPV of ESS in relation to AIS ####

#P(AIS+|ESS+) = P(ESS+|AIS+)/ P(ESS+)
ESS_PPV_AIS <- (ESSp_AISp)/(prev_ESS)

# ESS_PPV_AIS = 0.6893134


