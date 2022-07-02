# BTC1859 Group Project
# Prevalence Calculation for PSQI

### Prevalence: probability of a positive test for PSQI ###
prop.table(table(liver_data2$PSQI.binary)) # 100/(83+100) = 0.5464481 


### Probability of a positive PSQI test, given the positive result from ESS ###
# P(A|B) = P(A^B)/P(B)
# P(A^B): probability of positive test for both PSQI & ESS

# Drop rows with NA in the PSQI & ESS columns 
PSQI_ESS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "ESS.binary"))

# Total = 179 (NA excluded)
nrow(PSQI_ESS)

# Number of cases when positive for both tests = 34
length(which(PSQI_ESS$PSQI.binary == "1" & PSQI_ESS$ESS.binary == "1"))

# P(A^B) = 0.1899441
34/179

# P(B): probability of positive test for ESS
prop.table(table(liver_data2$ESS.binary)) # P(B) = 67/(67+184) = 0.2669323 

# P(A|B) = P(A^B)/P(B) = 0.7115816
0.1899441/0.2669323


### Probability of a positive PSQI test, given the positive result from AIS ###
# P(A|B) = P(A^B)/P(B)
# P(A^B): probability of positive test for both PSQI & AIS

# Drop rows with NA in the PSQI & AIS columns 
PSQI_AIS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "AIS.binary"))

# Total = 183 (NA excluded)
nrow(PSQI_AIS)

# Number of cases when positive for both tests = 81
length(which(PSQI_AIS$PSQI.binary == "1" & PSQI_AIS$AIS.binary == "1"))

# P(A^B) = 0.442623
81/183

# P(B): probability of positive test for AIS
prop.table(table(liver_data2$AIS.binary)) # P(B) = 145/(117+145) = 0.5534351 

# P(A|B) = P(A^B)/P(B) = 0.799774
0.442623/0.5534351 


### Probability of a positive PSQI test, given the positive result from BSS ###
# P(A|B) = P(A^B)/P(B)
# P(A^B): probability of positive test for both PSQI & BSS

# Drop rows with NA in the PSQI & BSS columns 
PSQI_BSS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "Berlin.Sleepiness.Scale"))

# Total = 182 (NA excluded)
nrow(PSQI_BSS)

# Number of cases when positive for both tests = 46
length(which(PSQI_BSS$PSQI.binary == "1" & PSQI_BSS$Berlin.Sleepiness.Scale == "1"))

# P(A^B) = 0.2527473
46/182

# P(B): probability of positive test for BSS
prop.table(table(liver_data2$Berlin.Sleepiness.Scale)) # P(B) = 102/(160+102) = 0.389313 

# P(A|B) = P(A^B)/P(B) = 0.6492136
0.2527473/0.389313 



