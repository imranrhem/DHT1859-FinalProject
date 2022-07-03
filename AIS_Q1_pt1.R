#ATHENS prevalence

library(dplyr)

# Total prevalence of sleep disorder in the entire dataset

total_sleepdis <- liver_data2$AIS.binary == "1" | liver_data2$ESS.binary == "1" | 
              liver_data2$PSQI.binary == "1" | liver_data2$Berlin.Sleepiness.Scale == "1"

sleepdis_count <- sum(total_sleepdis, na.rm = TRUE)
missing_sleepdis <- sum(is.na(total_sleepdis))
total_prev <- sleepdis_count / (nrow(liver_data2) - missing_sleepdis)

# Determine total counts and prevalence for each scale

#AIS 
AIS <- liver_data2$AIS.binary == "1"
AIS_total <- sum(AIS, na.rm = TRUE)
AIS_prev <- AIS_total / (nrow(liver_data2) - sum(is.na(AIS))) #remove amount of NAs prse in AIS from denominator

#ESS
ESS <- liver_data2$ESS.binary == "1"
ESS_total <- sum(ESS, na.rm = TRUE)
ESS_prev <- ESS_total / (nrow(liver_data2) - sum(is.na(ESS)))

#PSQI
PSQI <- liver_data2$PSQI.binary == "1"
PSQI_total <- sum(PSQI, na.rm = TRUE)
PSQI_prev <- PSQI_total / (nrow(liver_data2) - sum(is.na(PSQI)))

#BSS
BSS <- liver_data2$Berlin.Sleepiness.Scale == "1"
BSS_total <- sum(BSS, na.rm = TRUE)
BSS_prev <- BSS_total / (nrow(liver_data2) - sum(is.na(BSS)))

### Sensitivity, specificity, PPV, and NPV of AIS in relation to PSQI

# Drop NAs
PSQI_AIS <- liver_data2 %>%
  drop_na(c("PSQI.binary", "AIS.binary"))

AandP_prev = sum(PSQI_AIS$AIS.binary == "1" & PSQI_AIS$PSQI.binary == "1") / nrow(PSQI_AIS) #P(AIS+ ^ PSQI+)
AandP_neg_prev = sum(PSQI_AIS$AIS.binary == "0" & PSQI_AIS$PSQI.binary == "0") / nrow(PSQI_AIS) #P(AIS_- ^ PSQI-)

AP_sensitivity = AandP_prev / PSQI_prev # P(AIS|PSQI) = P(AIS+ ^ PSQI+) / P(PSQI+)
AP_specificity = (AandP_neg_prev) / (1-PSQI_prev) # P(AIS-|PSQI-) = P(AIS- ^ PSQI-) / P(PSQI-)
AP_PPV = AandP_prev / AIS_prev  #P(PSQI+|AIS+) = P(AIS+ ^ PSQI+) / P(AIS+)
AP_NPV = (AandP_neg_prev) / (1-AIS_prev) #P(PSQI-|AIS-) = P(AIS+ ^ PSQI+) / P(AIS-)

table(c(PSQI_AIS$AIS.binary, PSQI_AIS$PSQI.binary))

### Sensitivity, specificity, PPV, and NPV of AIS in relation to ESS

#Drop NAs
ESS_AIS <- liver_data2 %>%
  drop_na(c("ESS.binary", "AIS.binary"))

AandE_prev = sum(ESS_AIS$AIS.binary == "1" & ESS_AIS$ESS.binary == "1") / nrow(ESS_AIS) #P(AIS+ ^ ESS+)
AandE_neg_prev = sum(ESS_AIS$AIS.binary == "0" & ESS_AIS$ESS.binary == "0") / nrow(ESS_AIS) #P(AIS_- ^ ESS-)

AE_sensitivity = AandE_prev / ESS_prev # P(AIS|ESS) = P(AIS+ ^ ESS+) / P(ESS+)
AE_specificity = (AandE_neg_prev) / (1-ESS_prev) # P(AIS-|ESS-) = P(AIS- ^ ESS-) / P(ESS-)
AE_PPV = AandE_prev / AIS_prev  #P(ESS+|AIS+) = P(AIS+ ^ ESS+) / P(AIS+)
AE_NPV = (AandE_neg_prev) / (1-AIS_prev) #P(ESS-|AIS-) = P(AIS+ ^ ESS+) / P(AIS-)

# Sensitivity, specificity, PPV, and NPV of AIS in relation to BSS
BSS_AIS <-
  liver_data2 %>%
  drop_na(c("Berlin.Sleepiness.Scale", "AIS.binary"))

AandB_prev = sum(BSS_AIS$AIS.binary == "1" & BSS_AIS$Berlin.Sleepiness.Scale == "1") / nrow(BSS_AIS) #P(AIS+ ^ BSS+)
AandB_neg_prev = sum(BSS_AIS$AIS.binary == "0" & BSS_AIS$Berlin.Sleepiness.Scale == "0") / nrow(BSS_AIS) #P(AIS_- ^ BSS-)

AB_sensitivity = AandB_prev / BSS_prev # P(AIS|BSS) = P(AIS+ ^ BSS+) / P(BSS+)
AB_specificity = (AandB_neg_prev) / (1-BSS_prev) # P(AIS-|BSS-) = P(AIS- ^ BSS-) / P(BSS-)
AB_PPV = AandB_prev / AIS_prev  #P(BSS+|AIS+) = P(AIS+ ^ BSS+) / P(AIS+)
AB_NPV = (AandB_neg_prev) / (1-AIS_prev) #P(BSS-|AIS-) = P(AIS+ ^ BSS+) / P(AIS-)

