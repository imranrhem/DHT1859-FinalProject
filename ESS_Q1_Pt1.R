# Q1: Part 1
# Calculate prevalence of sleep disturbance based on ESS

# Prevalence = # of ppl w sleep disturbance/ n (total number of values)

attach(liver_data2)

dfESS <- as.data.frame(ESS.binary)

table(dfESS$ESS.binary)

# Number of people with sleep disturbance according to ESS is 67
# Total 251 (excluding NAs)

prevalance_ESS <- 67/251

# Calculate sensitivity and specificity of test
# Calculate NPV
