attach(liver_data2)

prop.table(table(liver_data2$Berlin.Sleepiness.Scale)) #prevalence = 0.389313
Berlin.Sleepiness.Scale <- data.frame(Berlin.Sleepiness.Scale)

#### BSS sensitivity in relation to  PSQI ####

# Probability of having a positive result on BSS, given a positive result on PSQI
# P(BSS+|PSQI+) = P(PSQI+ & BSS+)|P(PSQI+)

BSS_PSQI <- liver_data2 %>%
  drop_na(c(PSQI.binary,Berlin.Sleepiness.Scale))

nrow(BSS_PSQI)
# Total of 182 rows

# Number of cases when positive for both BSS and PSQI = 46
length(which(BSS_PSQI$Berlin.Sleepiness.Scale == "1" & BSS_PSQI$PSQI.binary == "1"))

# Find proportion of those who tested positive for both PSQI and BSS

46/182
# P(PSQI+ & BSS+) = 0.2527473

# Find the proportion of positive cases for PSQI
prop.table(table(liver_data2$PSQI.binary))
# P(PSQI+) = 0.5464481

# Final calculation 
# P(BSS+|PSQI+) = P(PSQI+ & BSS+)/P(PSQI+)

BSSp_PSQIp <- 0.2527473/0.5464481
BSSp_PSQIp
# The sensitivity of BSS in relation to the PSQI test is 0.4625275

#### BSS sensitivity in relation to ESS ####

# Probability of having a positive result on BSS, given a positive result on ESS
# P(BSS+|ESS+) = P(BSS+ & ESS+)|P(ESS+)

# Remove NAs from BSS and ESS rows 

BSS_ESS <- liver_data2 %>%
  drop_na(c(Berlin.Sleepiness.Scale, ESS.binary))

nrow(BSS_ESS)
# 249 rows, NAs removed

# Find proportion of positive tests for ESS

prop.table(table(ESS.binary))
# P(ESS+) = 0.2690763 

# Find the number of pt who tested positive on both ESS and BSS

length(which(BSS_ESS$Berlin.Sleepiness.Scale == 1 & BSS_ESS$ESS.binary == 1))

# 37 pt tested positive for both 

# Find proportion of those who tested positive for both ESS and BSS

37/249

# P(BSS+ & ESS+) = 0.1485944

# P(ESS+|BSS+) = P(BSS+ & ESS+)|P(BSS+)

BSSp_ESSp <- 0.1485944/0.269076
BSSp_ESSp
# P(BSS+|ESS+) = 0.5522395
# The sensitivity for ESS in relation to the BSS test is 0.5522395

#### Calculate sensitivity of BSS in relation to AIS ####

# Probability of having a positive result on BSS, given a positive result on AIS
# P(BSS+|AIS+) = P(AIS+ & BSS+)|P(AIS+)


BSS_AIS <- liver_data2 %>%
  drop_na(c(AIS.binary, Berlin.Sleepiness.Scale))

nrow(BSS_AIS)
# 261 rows, NAs removed

# Find proportion of positive tests for AIS 

prop.table(table(liver_data2$AIS.binary))
# P(AIS+) = 0.5534351

# Find the number of pt who tested positive on both BSS and AIS

length(which(BSS_AIS$AIS.binary == 1 & BSS_AIS$Berlin.Sleepiness.Scale == 1))

# 72 pt tested positive for both test

# Find proportion of those who tested positive for both BSS and AIS
# # P(AIS+ & BSS+)
72/261

# P(AIS+ & BSS+) = 0.2758621
# P(BSS+|AIS+) = P(BSS+ & AIS+)|P(AIS+)

BSSp_AISp <- 0.2758621/0.5534351
BSSp_AISp

# P(BSS+|AIS+) = 0.4984543
# The sensitivity for BSS in relation to the AIS test is 0.4984543

#### Calculate Specificity of BSS in relation to all other tests ####

#### Specificity of of BSS in relation to PSQI ####

#P(BSS-|PSQI-) = P(PSQI- & BSS-)|P(PSQI-)


# Get number of rows where PSQI and BSS both = 0 

length(which(BSS_PSQI$Berlin.Sleepiness.Scale == 0 & BSS_PSQI$PSQI.binary == 0))
# P(PSQI- & BSS-) ROW = 53

# Recall a total of 182 rows with NAs removed

# Find proportion who tested negative on both tests

53/182
# P(PSQI- & BSS-) = 0.2912088

# Find proportion of negative tests on PSQI

prop.table(table(PSQI.binary))
# P(PSQI-) = 0.4505495

BSSn_PSQIn <- 0.2912088 / 0.4505495
BSSn_PSQIn 

# P(BSS-|PSQI-) = 0.6463414
# The Specificity of BSS in relation to PSQI is 0.6463414

#### Specificity of of BSS in relation to AIS ####

#P(BSS-|AIS-) = P(AIS- & BSS-)|P(AIS-)


# Get number of rows where AIS and BSS both = 0 

length(which(BSS_AIS$Berlin.Sleepiness.Scale == 0 & BSS_AIS$AIS.binary == 0))
# P(AIS- & BSS-) ROW = 86

# Recall a total of 261 rows with NAs removed

# Find proportion who tested negative on both tests

86/261
# P(AIS- & BSS-) = 0.3295019

# Find proportion of negative tests on AIS

prop.table(table(AIS.binary))
# P(AIS-) = 0.4444444

BSSn_AISn <- 0.3295019 / 0.4444444
BSSn_AISn 

# P(BSS-|AIS-) =  0.7413793
# The Specificity of BSS in relation to AIS is  0.7413793

#### Specificity of of BSS in relation to ESS ####

#P(BSS-|ESS-) = P(ESS- & BSS-)|P(ESS-)


# Get number of rows where ESS and BSS both = 0 

length(which(BSS_ESS$Berlin.Sleepiness.Scale == 0 & BSS_ESS$ESS.binary == 0))
# P(ESS- & BSS-) ROW = 123

# Recall a total of 249 rows with NAs removed

# Find proportion who tested negative on both tests

123/249
# P(ESS- & BSS-) = 0.4939759

# Find proportion of negative tests on ESS

prop.table(table(ESS.binary))
# P(ESS-) = 0.7309237

BSSn_ESSn <- 0.4939759 / 0.7309237
BSSn_ESSn 

# P(BSS-|ESS-) = 0.6758242
# The Specificity of BSS in relation to ESS is 0.6758242

#### Calculate PPV of BSS in relation to PSQI ####
# P(PSQI+|BSS+) = P(PSQI+ & BSS+)|P(BSS+)

nrow(BSS_PSQI)
# Total of 182 rows

# Number of cases when positive for both BSS and PSQI = 46
length(which(BSS_PSQI$Berlin.Sleepiness.Scale == "1" & BSS_PSQI$PSQI.binary == "1"))

# Find proportion of those who tested positive for both PSQI and BSS

46/182
# P(PSQI+ & BSS+) =  0.2527473

# Find the proportion of positive cases for BSS
prop.table(table(liver_data2$Berlin.Sleepiness.Scale))
# P(BSS+) = 0.389313 

# Final calculation 
# P(PSQI+|BSS+) = P(PSQI+ & BSS+)|P(BSS+)

PSQIp_BSSp <-  0.2527473/0.389313
PSQIp_BSSp
# The PPV of BSS in relation to the PSQI test is 0.6492136

#### Calculate PPV of BSS in relation to ESS ####

# P(ESS+|BSS+) = P(BSS+ & ESS+)|P(BSS+)
nrow(BSS_ESS)
# 249 rows, NAs removed

# Find proportion of positive tests for BSS

prop.table(table(Berlin.Sleepiness.Scale))
# P(BSS+) = 0.389313

# Find the number of pt who tested positive on both ESS and BSS

length(which(BSS_ESS$Berlin.Sleepiness.Scale == 1 & BSS_ESS$ESS.binary == 1))

# 37 pt tested positive for both 

# Find proportion of those who tested positive for both ESS and BSS

37/249

# P(ESS+ & BSS+) = 0.1485944

# P(ESS+|BSS+) = P(BSS+ & ESS+)|P(BSS+)

ESSp_BSSp <- 0.1485944/0.389313
ESSp_BSSp
# P(ESS+|BSS+) = 0.3816836
# The PPV for ESS in relation to the BSS test is 0.3816836

#### Calculate PPV of AIS in relation to BSS ####
# P(AIS+|BSS+) = P(AIS+ & BSS+)|P(BSS+)

nrow(BSS_AIS)
# 261 rows, NAs removed

# Find proportion of positive tests for BSS

prop.table(table(liver_data2$Berlin.Sleepiness.Scale))
# P(BSS+) = 0.389313 

# Find the number of pt who tested positive on both BSS and AIS

length(which(BSS_AIS$AIS.binary == 1 & BSS_AIS$Berlin.Sleepiness.Scale == 1))

# 72 pt tested positive for both test

# Find proportion of those who tested positive for both BSS and AIS
# # P(AIS+ & BSS+)
72/261

# P(AIS+ & BSS+) = 0.2490421
# P(AIS+|BSS+) = P(BSS+ & AIS+)|P(BSS+)

AISp_BSSp <- 0.2758621/0.389313 
AISp_BSSp

# P(AIS+|BSS+) = 0.7085869
# The PVV for BSS in relation to the AIS test is 0.7085869

#### Calculate NPV of BSS in relation to all other tests ####

#### NPV of of BSS in relation to PSQI ####

#P(PSQI-|BSS-) = P(PSQI- & BSS-)|P(BSS-)


# Get number of rows where PSQI and BSS both = 0 

length(which(BSS_PSQI$Berlin.Sleepiness.Scale == 0 & BSS_PSQI$PSQI.binary == 0))
# P(PSQI- & BSS-) ROW = 53

# Recall a total of 182 rows with NAs removed

# Find proportion who tested negative on both tests

53/182
# P(PSQI- & BSS-) = 0.2912088

# Find proportion of negative tests on BSS

prop.table(table(Berlin.Sleepiness.Scale))
# P(BSS-) = 0.610687

PSQIn_BSSn <- 0.2912088 / 0.610687
PSQIn_BSSn 

# P(PSQI-|BSS-) = 0.4768544
# The NPV of BSS in relation to PSQI is 0.4768544

#### NPV of of BSS in relation to AIS ####

#P(AIS-|BSS-) = P(AIS- & BSS-)|P(BSS-)


# Get number of rows where AIS and BSS both = 0 

length(which(BSS_AIS$Berlin.Sleepiness.Scale == 0 & BSS_AIS$AIS.binary == 0))
# P(AIS- & BSS-) ROW = 86

# Recall a total of 261 rows with NAs removed

# Find proportion who tested negative on both tests

86/261
# P(AIS- & BSS-) = 0.3295019

# Find proportion of negative tests on AIS

prop.table(table(Berlin.Sleepiness.Scale))
# P(BSS-) = 0.610687

AISn_BSSn <- 0.3295019 / 0.610687
AISn_BSSn 

# P(AIS-|BSS-) = 0.5395594
# The NPV of BSS in relation to AIS is 0.5395594

#### NPV of of BSS in relation to ESS ####

#P(ESS-|BSS-) = P(ESS- & BSS-)|P(BSS-)


# Get number of rows where ESS and BSS both = 0 

length(which(BSS_ESS$Berlin.Sleepiness.Scale == 0 & BSS_ESS$ESS.binary == 0))
# P(ESS- & BSS-) ROW = 123

# Recall a total of 249 rows with NAs removed

# Find proportion who tested negative on both tests

123/249
# P(ESS- & BSS-) =  0.4939759

# Find proportion of negative tests on BSS

prop.table(table(Berlin.Sleepiness.Scale))
# P(BSS-) = 0.610687

ESSn_BSSn <-  0.4939759 / 0.610687
ESSn_BSSn 

# P(ESS-|BSS-) = 0.8088856
# The NPV of BSS in relation to ESS is 0.8088856



