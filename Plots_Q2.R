### Q2 plots ###

library(ggplot2)
library(gridExtra)
library(ggthemes)
library(grid)

#### PSQI Q2 plots ####

# SF36.PCS
splot_1 <- ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.PCS)) + geom_point() + geom_smooth(method = lm, se=F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Pittsburgh Sleep Quality Index Scores") + ylab("SF36.PCS Score")
# SF36.MCS
splot_2 <- ggplot(liver_data2, aes(x = Pittsburgh.Sleep.Quality.Index.Score, y = SF36.MCS)) + geom_point() + geom_smooth(method = lm, se=F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Pittsburgh Sleep Quality Index Scores") + ylab("SF36.MCS Score")

grid.arrange(splot_1, splot_2, nrow=2)
                              
#### ESS Q2 plots ####

# SF36.PCS
splot_3 <- ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.PCS)) + geom_point() +  geom_smooth(method=lm, se=F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Epworth Sleepiness Scale Score") + ylab("SF36.PCS Score")
                              
# SF36.MCS
splot_4 <- ggplot(liver_data2, aes(x = Epworth.Sleepiness.Scale, y = SF36.MCS)) + geom_point() +  geom_smooth(method=lm, se=F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Epworth Sleepiness Scale Score") + ylab("SF36.MCS Score")

grid.arrange(splot_3, splot_4, nrow=2)

#### AIS Q2 plots ####

# SF36.PCS
splot_5 <- ggplot(liver_data2, aes(x = Athens.Insomnia.Scale, y = SF36.PCS)) + geom_point() + geom_smooth(method=lm, se = F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Athens Insomnia Scale Score") + ylab("SF36.PCS Score")

# SF36.MCS
splot_6 <- ggplot(liver_data2, aes(x = Athens.Insomnia.Scale, y = SF36.MCS)) + geom_point() + geom_smooth(method=lm, se = F) + theme(panel.background = element_rect(fill ="#FFFFFF", colour = "gray")) + xlab("Athens Insomnia Scale Score") + ylab("SF36.MCS Score")

grid.arrange(splot_5, splot_6, nrow=2)

#### BSS Q2 plots ####

par(mfrow=c(2,1))
# SF36.PCS
boxplot(liver_data2$SF36.PCS ~ liver_data2$Berlin.Sleepiness.Scale, xlab = "Berlin Sleepiness Scale", ylab = "SF36.PCS Score")

# SF36.MCS
boxplot(liver_data2$SF36.MCS ~ liver_data2$Berlin.Sleepiness.Scale, xlab = "Berlin Sleepiness Scale", ylab = "SF36.MCS Score")
