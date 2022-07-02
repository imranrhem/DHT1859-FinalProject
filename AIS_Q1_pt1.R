#ATHENS prevalence

# Total prevalence of sleep disorder in the entire dataset

total_sleepdis <- liver_data2$AIS.binary == "1" | liver_data2$ESS.binary == "1" | 
              liver_data2$PSQI.binary == "1" | liver_data2$Berlin.Sleepiness.Scale == "1"

test <- total_sleepdis <- liver_data2$AIS.binary == "1" & liver_data2$ESS.binary == "1" & 
  liver_data2$PSQI.binary == "1" & liver_data2$Berlin.Sleepiness.Scale == "1"

sleepdis_count <- sum(total_sleepdis, na.rm = TRUE)
missing_sleepdis <- sum(is.na(total_sleepdis))
total_prev <- sleepdis_count / (nrow(liver_data2) - missing_sleepdis)

# Determine total counts and prevalence for each scale

#AIS 
AIS <- liver_data2$AIS.binary == "1"
AIS_total <- sum(AIS, na.rm = TRUE)
AIS_prev <- AIS_total / (nrow(liver_data2) - sum(is.na(AIS))) #remove amount of NAs in AIS from denominator

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

# Sensitivity, specificity, PPV, and NPV of AIS in relation to PSQI
AandP = liver_data2$AIS.binary == "1" & liver_data2$PSQI.binary == "1"
AandP_total = sum(AandP, na.rm = TRUE)
AandP_prev = AandP_total / (nrow(liver_data2) - sum(is.na(AandP)))

AP_sensitivity = (AandP_prev*AIS_prev) / PSQI_prev # P(AIS|PSQI) = P(PSQI+|AIS+)*P(AIS) / P(PSQI)
AP_specificity = ((1-AandP_prev)*(1-AIS_prev)) / (1-PSQI_prev) #P(AIS-|PSQI-) = P(PSQI-|AIS-)*P(AIS-) / P(PSQI-)
AP_PPV = (AP_sensitivity*PSQI_prev) / AIS_prev  #P(PSQI+|AIS+) = P(AIS+|PSQI+)*P(PSQI) / P(AIS)
AP_NPV = (AP_specificity*(1-PSQI_prev)) / (1-AIS_prev) #P(PSQI-|AIS-) = P(AIS-|PSQI-)*P(PSQI-) / P(AIS-)

# Sensitivity, specificity, PPV, and NPV of AIS in relation to ESS
AandE = liver_data2$AIS.binary == "1" & liver_data2$ESS.binary == "1"
AandE_total = sum(AndE, na.rm = TRUE)
AandE_prev = AndE_total / (nrow(liver_data2) - sum(is.na(AndE)))

AE_sensitivity = (AandE_prev*AIS_prev) / ESS_prev # P(AIS|ESS) = P(ESS+|AIS+)*P(AIS) / P(ESS)
AE_specificity = ((1-AandE_prev)*(1-AIS_prev)) / (1-ESS_prev) #P(AIS-|ESS-) = 
AE_PPV = (AE_sensitivity*ESS_prev) / AIS_prev  #P(ESS+|AIS+) = P(AIS+|ESS+)*P(ESS) / P(AIS)
AE_NPV = (AE_specificity*(1-ESS_prev)) / (1-AIS_prev) #P(ESS-|AIS-) = P(AIS-|ESS-)*P(ESS-) / P(AIS-)

# Sensitivity, specificity, PPV, and NPV of AIS in relation to BSS
AandB = liver_data2$AIS.binary == "1" & liver_data2$Berlin.Sleepiness.Scale == "1"
AandB_total = sum(AandB, na.rm = TRUE)
AandB_prev = AandB_total / (nrow(liver_data2) - sum(is.na(AandB)))

AB_sensitivity = (AandB_prev*AIS_prev) / BSS_prev # P(AIS|BSS) = P(BSS+|AIS+)*P(AIS) / P(BSS)
AB_specificity = ((1-AandB_prev)*(1-AIS_prev)) / (1-BSS_prev) #P(AIS-|BSS-) = 
AB_PPV = (AB_sensitivity*BSS_prev) / AIS_prev  #P(BSS+|AIS+) = P(AIS+|BSS+)*P(BSS) / P(AIS)
AB_NPV = (AB_specificity*(1-BSS_prev)) / (1-AIS_prev) #P(BSS-|AIS-) = P(AIS-|BSS-)*P(BSS-) / P(AIS-)

