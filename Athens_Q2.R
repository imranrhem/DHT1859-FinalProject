# TESTING AIS MODEL

library(dplyr)
library(tidyverse)
library(ggplot2)


# Scatter plots of AIS scores and SF36.PCS/SF36.MCS scores
plot.new()
par(mfrow=c(2,1))
plot(liver_data2$Athens.Insomnia.Scale, liver_data2$SF36.PCS)
plot(liver_data2$Athens.Insomnia.Scale, liver_data2$SF36.MCS)
dev.off()

#Correlation tests
cor.test(liver_data2$Athens.Insomnia.Scale, liver_data2$SF36.PCS, method = "pearson")
cor.test(liver_data2$Athens.Insomnia.Scale, liver_data2$SF36.MCS, method = "pearson")


# linear model testing - PCS
names(liver_data2)

linear_PCS <- lm(SF36.PCS~Athens.Insomnia.Scale+Epworth.Sleepiness.Scale+
                   Berlin.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score,
                 data = liver_data2)

summary(linear_PCS)

# linear model testing - MCS

linear_MCS <- lm(SF36.MCS~Athens.Insomnia.Scale+Epworth.Sleepiness.Scale+
                   Berlin.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score,
                 data = liver_data2)

summary(linear_MCS)

### EXCESS / RANDOM TESTS ###

# AIS as categorical
plot.new()
par(mfrow=c(2,1))
plot(liver_data2$SF36.PCS~liver_data2$AIS.binary)
plot(liver_data2$SF36.MCS~liver_data2$AIS.binary)
dev.off()

# Setup data for t-tests
test_data <- liver_data2 %>%
  drop_na("AIS.binary") %>%
  drop_na("SF36.PCS") %>%
  drop_na("SF36.MCS") 
AIS_0 <- test_data[test_data$AIS.binary == "0",]
AIS_1 <- test_data[test_data$AIS.binary == "1",]

ggplot(test_data, aes(x=AIS.binary, y = SF36.PCS))

# t-tests
t.test(AIS_0$SF36.PCS, AIS_1$SF36.PCS)
t.test(AIS_0$SF36.MCS, AIS_1$SF36.MCS)
