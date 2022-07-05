# BTC1859 Group Project
# Q2 PSQI

library(dplyr)
library(tidyverse)
library(ggplot2)

PSQI_mod <- glm(PSQI.binary ~ Gender + Recurrence.of.disease + Depression + Corticoid, 
                 data = liver_data2, family = "binomial")

### Scatter plots of PSQI scores & SF36.PCS/SF36.MCS scores ###
par(mfrow=c(2,1))
plot(liver_data2$Pittsburgh.Sleep.Quality.Index.Score, liver_data2$SF36.PCS)
ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.PCS)) + geom_point() + geom_smooth(method = lm, se = F)
dev.off()

plot(liver_data2$Pittsburgh.Sleep.Quality.Index.Score, liver_data2$SF36.MCS)
ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.MCS)) + geom_point() + geom_smooth(method = lm, se = F)
dev.off()

### Box plots of PSQI scores & SF36.PCS/SF36.MCS scores ###
plot.new()
par(mfrow=c(2,1))
plot(liver_data2$SF36.PCS~liver_data2$PSQI.binary)
plot(liver_data2$SF36.MCS~liver_data2$PSQI.binary)
dev.off()


### Testing Relationships ###

## Correlation test (PSQI as continuous variable) ##
cor.test(liver_data2$Pittsburgh.Sleep.Quality.Index.Score, liver_data2$SF36.PCS, method = "pearson") # r = -0.3455852; p = 2.813e-06
cor.test(liver_data2$Pittsburgh.Sleep.Quality.Index.Score, liver_data2$SF36.MCS, method = "pearson") # r = -0.5481288; p = 4.12e-15


## t-test (PSQI as categorical variable) ##
test_data <- liver_data2 %>%
  drop_na(c("PSQI.binary", "SF36.PCS", "SF36.MCS"))

PSQI_neg <- test_data[test_data$PSQI.binary == "0",] 
PSQI_pos <- test_data[test_data$PSQI.binary == "1",] 

t.test(PSQI_neg$SF36.PCS, PSQI_pos$SF36.PCS) # p=3.442e-05
t.test(PSQI_neg$SF36.MCS, PSQI_pos$SF36.MCS) # p=1.578e-11




