# test file

# total dof = 17.866 for everything except for BSS
nrow(liver_data2) / 15

# total dof = 4
table(liver_data2$ESS.binary) / 15

# total dof = 5
table(liver_data2$PSQI.binary) / 15

# total dof = 7
table(liver_data2$AIS.binary) / 15

# total dof = 6
table(liver_data2$Berlin.Sleepiness.Scale)/15
