# plots

library(ggplot2)
library(gridExtra)
library(grid)

#### PSQI Q2 plots ####
grid.arrange(splot_1, splot_2, nrow=2)

# SF36.PCS
splot_1 <- ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.PCS)) + geom_point() + geom_smooth(method = lm, se=F)

# SF36.MCS
splot_2 <- ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.MCS)) + geom_point() + geom_smooth(method = lm, se=F)


#### ESS Q2 plots ####

grid.arrange(splot_3, splot_4, nrow=2)
# SF36.PCS
splot_3 <- ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.PCS)) + geom_point() +  geom_smooth(method=lm, se=F)

# SF36.MCS
splot_4 <- ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.MCS)) + geom_point() +  geom_smooth(method=lm, se=F)

#### AIS Q2 plots ####

grid.arrange(splot_5, splot_6, nrow=2)
# SF36.PCS
splot_5 <- ggplot(liver_data2, aes(x = Athens.Insomnia.Scale, y = SF36.PCS)) + geom_point() + geom_smooth(method=lm, se = F)

# SF36.MCS
splot_6 <- ggplot(liver_data2, aes(x = Athens.Insomnia.Scale, y = SF36.MCS)) + geom_point() + geom_smooth(method=lm, se = F)

#### BSS Q2 plots ####

par(mfrow=c(2,1))
# SF36.PCS
boxplot(liver_data2$SF36.PCS ~ liver_data2$Berlin.Sleepiness.Scale)

# SF36.MCS
boxplot(liver_data2$SF36.MCS ~ liver_data2$Berlin.Sleepiness.Scale)
