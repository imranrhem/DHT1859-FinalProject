#data analysis

<<<<<<< HEAD
=======
fpath <- "/Users/dannima/Desktop/MBiotech/BTC1859/Group_Project/"
df <- read.csv(paste0(fpath, "project_data.csv"))

mod1 <- lm(Pittsburgh.Sleep.Quality.Index.Score ~ 
             Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
             Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
             Renal.Failure + Depression + Corticoid, data= df)
summary(mod1)

mod2 <- lm(Athens.Insomnia.Scale ~ 
             Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
             Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
             Renal.Failure + Depression + Corticoid, data= df)
summary(mod2)
 

mod3 <- lm(Epworth.Sleepiness.Scale ~ 
             Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis + 
             Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
             Renal.Failure + Depression + Corticoid, data= df)
summary(mod3)
>>>>>>> d5c5837eb1a570fcc64e9385d2322a74ed65173b
