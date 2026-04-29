library(nhanesA)
library(survival)
library(survminer)

demo <- nhanes("DEMO_J")
bmx  <- nhanes("BMX_J")
smq  <- nhanes("SMQ_J")
data <- merge(demo, bmx, by = "SEQN")
data_clean <- data[, c("SEQN", "RIDAGEYR", 
                        "RIAGENDR", "BMXBMI",
                        "WTMEC2YR", "SDMVPSU", 
                        "SDMVSTRA")]

data_clean <- data_clean[data_clean$RIDAGEYR >= 18, ]
data_clean$smoking <- ifelse(
  smq$SMQ020[match(data_clean$SEQN, smq$SEQN)] == "Yes", 1,
  ifelse(smq$SMQ020[match(data_clean$SEQN, smq$SEQN)] == "No", 0, NA))

dim(data_clean)
table(data_clean$smoking)
mort_raw <- read.fwf(
  "C:/Users/mahed/Downloads/NHANES_2017_2018_MORT_2019_PUBLIC.dat",
  widths = c(14, 1, 1, 3, 1, 1, 1, 4, 18, 3),
  col.names = c("SEQN", "ELIGSTAT", "MORTSTAT",
                "UCOD_LEADING", "DIABETES",
                "HYPERTEN", "DODQTR", "DODYEAR",
                "PERMTH_INT", "PERMTH_EXM"),
  stringsAsFactors = FALSE
)

mort_raw$PERMTH_EXM <- as.numeric(trimws(mort_raw$PERMTH_EXM))
mort_raw$MORTSTAT <- as.numeric(trimws(mort_raw$MORTSTAT))
table(mort_raw$MORTSTAT)
surv_data <- merge(data_clean, mort_raw, by = "SEQN")
surv_data <- surv_data[surv_data$ELIGSTAT == 1, ]

surv_data$time_years <- surv_data$PERMTH_EXM / 12

surv_data$time_years[is.na(surv_data$time_years)] <- 
  max(surv_data$time_years, na.rm = TRUE)
surv_data$MORTSTAT[is.na(surv_data$MORTSTAT)] <- 0

surv_final <- na.omit(surv_data[, c("SEQN",
                                     "time_years",
                                     "MORTSTAT",
                                     "smoking",
                                     "RIDAGEYR",
                                     "RIAGENDR",
                                     "BMXBMI")])
dim(surv_final)
table(surv_final$MORTSTAT)
surv_obj <- Surv(time = surv_final$time_years,
                 event = surv_final$MORTSTAT)
km_fit <- survfit(surv_obj ~ smoking,
                  data = surv_final)


ggsurvplot(km_fit,
           data = surv_final,
           pval = TRUE,
           conf.int = TRUE,
           legend.labs = c("non-smoker", "smoker"),
           title = "Survival — smoking vs death",
           xlab = "time (year)",
           ylab = "Survival Probability")

cox_model <- coxph(surv_obj ~ smoking + RIDAGEYR + RIAGENDR + BMXBMI,
                   data = surv_final)

summary(cox_model)

