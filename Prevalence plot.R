#### Barplot of prevalence of pos tests ####

df_prev<- as.data.frame(x=c(prev_ESS,prev_BSS, prev_PSQI, prev_AIS), row.names = c("ESS","BSS", "PSQI"
                    , "AIS"))

barplot(t(as.matrix(df_prev)),beside=TRUE, main = "Prevalences of Sleep Disorder as Measured by Different Tests", 
        xlab = "Test", ylab = "Prevalence of sleep disorder", ylim = c(0, 1))


