# plots

# Barplot: Prevalence of sleep disturbance for different scales
prev <- c(0.5464481, 0.5534351, 0.2669323, 0.389313)
barplot(prev, main="Prevalence of Sleep Disturbance for Different Measurement Scales",
        cex.main = 1,
        xlab = "Scales of measurement",
        ylab = "Prevalence of sleep disturbance",
        ylim = c(0.0, 0.6),
        names.arg = c("PSQI", "AIS", "ESS", "BSS"))




