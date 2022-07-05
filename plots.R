# plots

### Barplot: Prevalence of sleep disturbance for different scales ###
prev <- c(0.5464481, 0.5534351, 0.2669323, 0.389313)
barplot(prev, main="Prevalence of Sleep Disturbance for Different Measurement Scales",
        cex.main = 1,
        xlab = "Scales of measurement",
        ylab = "Prevalence of sleep disturbance",
        ylim = c(0.0, 1.0),
        names.arg = c("PSQI", "AIS", "ESS", "BSS"))


### Barplots: PPVs comparing different scales ###
plot.new()

ppv_PSQI <- c(0.7998, 0.7116, 0.6492)
barplot(ppv_PSQI, main="PPV of AIS, ESS, and BSS relative to PSQI",
     cex.main = 1,
     xlab = "Scales of measurement",
     ylab = "PPV",
     ylim = c(0.0, 1.0),
     names.arg = c("AIS", "ESS", "BSS"))

ppv_AIS <- c(0.8100, 0.6893, 0.7086)
barplot(ppv_AIS, main="PPV of PSQI, ESS, and BSS relative to AIS",
        cex.main = 1,
        xlab = "Scales of measurement",
        ylab = "PPV",
        ylim = c(0.0, 1.0),
        names.arg = c("PSQI", "ESS", "BSS"))

ppv_ESS <- c(0.3476, 0.3325, 0.3817)
barplot(ppv_ESS, main="PPV of PSQI, AIS, and BSS relative to ESS",
        cex.main = 1,
        xlab = "Scales of measurement",
        ylab = "PPV",
        ylim = c(0.0, 1.0),
        names.arg = c("PSQI", "AIS", "BSS"))

ppv_BSS <- c(0.4625, 0.4985, 0.5567)
barplot(ppv_ESS, main="PPV of PSQI, AIS, and ESS relative to BSS",
        cex.main = 1,
        xlab = "Scales of measurement",
        ylab = "PPV",
        ylim = c(0.0, 1.0),
        names.arg = c("PSQI", "AIS", "ESS"))

dev.off()

