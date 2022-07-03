# Q2 - ESS
# Association between SF36.PCF and ESS Score 

# Create a linear regression model with test scores as the predictor and SF36.PCS/MCS as the response

PCS_mod <- lm(SF36.PCS~Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale + 
                   Pittsburgh.Sleep.Quality.Index.Score, data = liver_data2)

summary(PCS_mod)

MCS_mod <- lm(SF36.MCS~Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale + 
                   Pittsburgh.Sleep.Quality.Index.Score, data = liver_data2)

summary(MCS_ESSmod)

summary(PCS_ESS_mod)

#### Correlation tests ####

cor.test(liver_data2$Epworth.Sleepiness.Scale, liver_data2$SF36.PCS, method = "pearson")
# cor = -0.2748211, p-value = 1.928e-05

cor.test(liver_data2$Epworth.Sleepiness.Scale, liver_data2$SF36.MCS, method = "pearson")
# cor = -0.2991111, p-value = 3.039e-06

#### Scatter plots ####
plot.new()
library(ggplot2)
par(mfrow=c(2,1))
plot(liver_data2$Epworth.Sleepiness.Scale, liver_data2$SF36.PCS)
ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.PCS)) + geom_point() +  geom_smooth(method=lm)


plot(liver_data2$Epworth.Sleepiness.Scale, liver_data2$SF36.MCS)
ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.MCS)) + geom_point() +  geom_smooth(method=lm)


#### Box plot ####
par(mfrow=c(2,1))
boxplot(liver_data2$SF36.PCS ~ liver_data2$ESS.binary)
boxplot(liver_data2$SF36.MCS ~ liver_data2$ESS.binary)

#### T-tests

library(dplyr)

ESS_test_data <- liver_data2 %>%
  drop_na("SF36.PCS") %>%
  drop_na("SF36.MCS") %>%
  drop_na("ESS.binary") 

ESS_0 <- ESS_test_data[ESS_test_data$ESS.binary == "0",]
ESS_1 <- ESS_test_data[ESS_test_data$ESS.binary == "1",]

# Performing t-tests to examine the mean 

t.test(ESS_0$SF36.PCS, ESS_1$SF36.PCS)
# p-value = 0.0008976
t.test(ESS_0$SF36.MCS, ESS_1$SF36.MCS)
# p-value = 1.315e-05
