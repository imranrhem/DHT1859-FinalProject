# Q1: Part 1

library(dplyr)
#### Calculate prevalence of sleep disturbance based on ESS ####

# Prevalence = # of ppl w sleep disturbance/ n (total number of values)

attach(liver_data2)

prop.table(table(ESS.binary))

# prevalence of sleep disturbance when using ESS = 0.2669323

#### Calculate PPV of ESS in relation to each test ####

#### ESS PPV in relation to  PSQI ####

# Probability of having a positive result on ESS, given a positive result on PSQI
# P(ESS+|PSQI+) = P(PSQI+ & ESS+)|P(PSQI+)

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

34/179
# P(PSQI+ & ESS+) = 0.1899441

# Find the proportion of positive cases for PSQI

prop.table(table(liver_data2$PSQI.binary))
# P(PSQI+) = 0.5464481

# Final calculation 
# P(ESS+|PSQI+) = P(PSQI+ & ESS+)/P(PSQI+)

ESSp_PSQIp <- 0.1899441/0.5464481
ESSp_PSQIp
# The PPV of ESS in relation to the PSQI test is 0.3475977

#### ESS PPV in relation to BSS ####

# Probability of having a positive result on ESS, given a positive result on BSS
# P(ESS+|BSS+) = P(BSS+ & ESS+)|P(BSS+)

# Remove NAs from BSS and ESS rows 

ESS_BSS <- liver_data2 %>%
  drop_na(c(Berlin.Sleepiness.Scale, ESS.binary))

nrow(ESS_BSS)
# 249 rows, NAs removed

# Find proportion of positive tests for BSS 

prop.table(table(liver_data2$Berlin.Sleepiness.Scale))
# P(BSS+) = 0.389313 

# Find the number of pt who tested pos on both ESS and BSS

attach(ESS_BSS)
length(which(Berlin.Sleepiness.Scale == 1 & ESS.binary == 1))

# 37 pt tested positive for both 

# Find proportion of those who tested positive for both ESS and BSS

37/249

# P(BSS+ & ESS+) = 0.1485944

# P(ESS+|BSS+) = P(BSS+ & ESS+)|P(BSS+)

ESSp_BSSp <- 0.1485944/0.389313 

# P(ESS+|BSS+) = 0.3816836
# The PPV for ESS in relation to the BSS test is 0.3816836

#### Calculate PPV of ESS in relation to AIS ####

# Probability of having a positive result on ESS, given a positive result on BSS
# P(ESS+|AIS+) = P(AIS+ & ESS+)|P(AIS+)


ESS_AIS <- liver_data2 %>%
  drop_na(c(AIS.binary, ESS.binary))

nrow(ESS_AIS)
# 250 rows, NAs removed

# Find proportion of positive tests for AIS 

prop.table(table(liver_data2$AIS.binary))
# P(AIS+) = 0.5534351

# Find the number of pt who tested positive on both ESS and AIS

attach(ESS_AIS)
length(which(AIS.binary == 1 & ESS.binary == 1))

# 46 pt tested positive for both test

# Find proportion of those who tested positive for both ESS and BSS
# # P(AIS+ & ESS+)
46/250

# P(AIS+ & ESS+) = 0.184
# P(ESS+|AIS+) = P(BSS+ & AIS+)|P(AIS+)

ESSp_AISp <- 0.184/0.5534351
ESSp_AISp

# P(ESS+|AIS+) = 0.332469
# The PPV for ESS in relation to the AIS test is 0.332469

#### Calculate NPV of ESS in relation to all other tests ####

#### NPV of of ESS in relation to PSQI ####

#P(ESS-|PSQI-) = P(PSQI- & ESS-)|P(PSQI-)

attach(ESS_PSQI)

# Get number of rows where PSQI and ESS both = 0 

length(which(ESS.binary == "0" & PSQI.binary == "0"))
# P(PSQI- & ESS-) = 68

# Recall a total of 179 rows with NAs removed

# Find proportion who tested negative on both tests

68/179
# P(PSQI- & ESS-) = 0.3798883

# Find proportion of negative tests on PSQI

prop.table(table(PSQI.binary))
# P(PSQI-) = 0.452514

ESSn_PSQIn <- 0.3798883 / 0.452514

# P(ESS-|PSQI-) = 0.8395062
# The NPV of ESS in relation to PSQI is 0.8395062




# Calculate sensitivity and specificity of test





