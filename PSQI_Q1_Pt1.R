# BTC1859 Group Project
# Prevalence Calculation for PSQI

# Equation used for conditional probability: P(A|B) = P(A^B)/P(B)

### Prevalence: probability of a positive test for PSQI ###
prop.table(table(liver_data2$PSQI.binary)) # 100/(83+100) = 0.5464481 


### P(PSQI+|ESS+) ###
# P(PSQI+|ESS+) = P(PSQI+ and ESS+)/P(ESS+) = 0.7115816

# Drop rows with NA in the PSQI & ESS columns 
PSQI_ESS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "ESS.binary"))

# P(PSQI+ and ESS+) = 0.1899441
length(which(PSQI_ESS$PSQI.binary == "1" & PSQI_ESS$ESS.binary == "1")) / nrow(PSQI_ESS)

# P(ESS+) = 0.2669323
prop.table(table(liver_data2$ESS.binary)) 

# P(PSQI+|ESS+) = P(PSQI+ and ESS+)/P(ESS+) = 0.7115816
0.1899441/0.2669323


### P(PSQI+|AIS+) ###
# P(PSQI+|AIS+) = P(PSQI+ and AIS+)/P(AIS+) = 0.799774

# Drop rows with NA in the PSQI & AIS columns 
PSQI_AIS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "AIS.binary"))

# P(PSQI+ and AIS+) = 0.442623
length(which(PSQI_AIS$PSQI.binary == "1" & PSQI_AIS$AIS.binary == "1")) / nrow(PSQI_AIS)

# P(AIS+) = 0.5534351
prop.table(table(liver_data2$AIS.binary)) 

# P(PSQI+|AIS+) = P(PSQI+ and AIS+)/P(AIS+) = 0.799774
0.442623/0.5534351 


### P(PSQI+|BSS+) ###
# P(PSQI+|BSS+) = P(PSQI+ and BSS+)/P(BSS+) = 0.799774

# Drop rows with NA in the PSQI & BSS columns 
PSQI_BSS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "Berlin.Sleepiness.Scale"))

# P(PSQI+ and BSS+) = 0.2527473
length(which(PSQI_BSS$PSQI.binary == "1" & PSQI_BSS$Berlin.Sleepiness.Scale == "1")) / nrow(PSQI_BSS)

# P(BSS+) = 0.389313
prop.table(table(liver_data2$Berlin.Sleepiness.Scale)) 

# P(PSQI+|BSS+) = P(PSQI+ and BSS+)/P(BSS+) = 0.6492136
0.2527473/0.389313 

################################################################################

### P(PSQI-|ESS-) ###
# P(PSQI-|ESS-) = P(PSQI- and ESS-)/P(ESS-) = 0.5182172

# P(PSQI- and ESS-) = 0.3798883
length(which(PSQI_ESS$PSQI.binary == "0" & PSQI_ESS$ESS.binary == "0")) / nrow(PSQI_ESS)

# P(ESS-) = 0.7330677
prop.table(table(liver_data2$ESS.binary)) 

# P(PSQI-|ESS-) = P(PSQI- and ESS-)/P(ESS-) = 0.5182172
0.3798883/0.7330677


### P(PSQI-|AIS-) ###
# P(PSQI-|AIS-) = P(PSQI- and AIS-)/P(AIS-) = 0.8076221

# P(PSQI- and AIS-) = 0.3606557
length(which(PSQI_AIS$PSQI.binary == "0" & PSQI_AIS$AIS.binary == "0")) / nrow(PSQI_AIS)

# P(AIS-) = 0.4465649
prop.table(table(liver_data2$AIS.binary)) 

# P(PSQI-|AIS-) = P(PSQI- and AIS-)/P(AIS-) = 0.8076221
0.3606557/0.4465649


### P(PSQI-|BSS-) ###
# P(PSQI-|BSS-) = P(PSQI- and BSS-)/P(BSS-) = 0.4768544

# P(PSQI- and BSS-) = 0.2912088
length(which(PSQI_BSS$PSQI.binary == "0" & PSQI_BSS$Berlin.Sleepiness.Scale == "0")) / nrow(PSQI_BSS)

# P(BSS-) = 0.610687
prop.table(table(liver_data2$Berlin.Sleepiness.Scale)) 

# P(PSQI-|BSS-) = P(PSQI- and BSS-)/P(BSS-) = 0.4768544
0.2912088/0.610687

################################################################################

### P(ESS+|PSQI+) ###
# P(ESS+|PSQI+) = P(ESS+ and PSQI+)/P(PSQI+) = 0.3475977

# P(ESS+ and PSQI+) = 0.1899441
# P(PSQI+) = 0.5464481 
0.1899441/0.5464481 


### P(AIS+|PSQI+) ###
# P(AIS+|PSQI+) = P(AIS+ and PSQI+)/P(PSQI+) = 0.8100001

# P(AIS+ and PSQI+) = 0.442623 
# P(PSQI+) = 0.5464481 
0.442623/0.5464481 


### P(BSS+|PSQI+) ###
# P(BSS+|PSQI+) = P(BSS+ and PSQI+)/P(PSQI+) = 0.4625275

# P(BSS+ and PSQI+) = 0.2527473
# P(PSQI+) = 0.5464481 
0.2527473/0.5464481 

################################################################################

### P(ESS-|PSQI-) ###
# P(ESS-|PSQI-) = P(ESS- and PSQI-)/P(PSQI-) = 0.8375851 

# P(ESS- and PSQI-) = 0.3798883
# P(PSQI-) = 0.4535519
prop.table(table(liver_data2$PSQI.binary))
0.3798883/0.4535519


### P(AIS-|PSQI-) ###
# P(AIS-|PSQI-) = P(AIS- and PSQI-)/P(PSQI-) = 0.7951807

# P(AIS- and PSQI-) = 0.3606557
# P(PSQI-) = 0.4535519
0.3606557/0.4535519


### P(BSS-|PSQI-) ###
# P(BSS-|PSQI-) = P(BSS- and PSQI-)/P(PSQI-) = 0.6420628

# P(BSS- and PSQI-) = 0.2912088
# P(PSQI-) = 0.4535519
0.2912088/0.4535519




