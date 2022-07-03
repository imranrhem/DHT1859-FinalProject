#BSS categorical
boxplot(liver_data2$SF36.PCS ~ liver_data2$Berlin.Sleepiness.Scale)
boxplot(liver_data2$SF36.MCS ~ liver_data2$Berlin.Sleepiness.Scale)

#correlation for relationship
cor.test(liver_data2$Berlin.Sleepiness.Scale, liver_data2$SF36.PCS, method = "pearson")
cor.test(liver_data2$Berlin.Sleepiness.Scale, liver_data2$SF36.MCS, method = "pearson")

test_data <- liver_data2 %>% 
  drop_na("Berlin.Sleepiness.Scale") %>%
  drop_na("SF36.PCS") %>%
  drop_na("SF36.MCS")

BSS_1 <- test_data[test_data$Berlin.Sleepiness.Scale == "1", ]
BSS_0 <- test_data[test_data$Berlin.Sleepiness.Scale == "0", ]

t.test(BSS_1$SF36.PCS, BSS_0$SF36.PCS)
t.test(BSS_1$SF36.MCS, BSS_0$SF36.MCS)

names(liver_data2)
linear_PCS <- lm(SF36.PCS~Berlin.Sleepiness.Scale+Athens.Insomnia.Scale+Epworth.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score,
                 data = liver_data2)

summary(linear_PCS)
plot(fitted(linear_PCS), resid(linear_PCS))
qqnorm(resid(linear_PCS))
qqline(resid(linear_PCS), col = 2)

linear_MCS <- lm(SF36.MCS~Berlin.Sleepiness.Scale+Athens.Insomnia.Scale+Epworth.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score,
                 data = liver_data2)

summary(linear_MCS)
plot(fitted(linear_MCS), resid(linear_MCS))
qqnorm(resid(linear_MCS))
qqline(resid(linear_MCS), col = 2)
