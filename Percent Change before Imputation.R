# Set working directory and import
setwd("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Data PreProcessing")
set.seed(2626)

# Import with outliers removed
df <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Data PreProcessing/PLS_NewLibID after Outlier Review.txt")
PLS_Vars <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Data/PLS_Vars.csv")
vars <- colnames(PLS_Vars)
df <- df[vars]
df$KIDCIRCL <- NULL
df$PHYSCIR <- NULL
summary(df$REAPLOCALE)

# View NAs
colSums(is.na(df))

# CALCULATE PERCENT CHANGE

# Melt data
library(reshape2)
df_melt <- melt(df, na.rm=TRUE, id = c("NewLibID", "YEAR"))
setwd("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data")
write.csv(df_melt, "SUPP.csv")
df_melt_PY <- df_melt
df_melt_PY$YEAR <- df_melt_PY$YEAR+1
colnames(df_melt_PY) <- c("NewLibID", "YEAR", "Variable", "PYValue")
colnames(df_melt) <- c("NewLibID", "YEAR", "Variable", "CYValue")
df_perc_change <- merge(df_melt, df_melt_PY, by=c("NewLibID", "YEAR", "Variable"))
df_perc_change$PYValue <- as.numeric(df_perc_change$PYValue)
df_perc_change$CYValue <- as.numeric(df_perc_change$CYValue)
df_perc_change$PercCh <- ((df_perc_change$CYValue-df_perc_change$PYValue)/df_perc_change$PYValue)
# export
setwd("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data")
write.csv(df_perc_change, "df_perc_change.csv")
write.csv(df, "df.csv")

# import imputed with mean
df_vals_pcChange <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_vals_pcChange.txt")
pc_vars <- colnames(df_vals_pcChange)
pc_vars_select <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/pc_vars_select.csv")
pc_vars_selected <- colnames(pc_vars_select)
df_vals_pcChange <- df_vals_pcChange[pc_vars_selected]
pc_vars_select_translate <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/pc_vars_select_translate.csv")
pc_vars_transl <- colnames(pc_vars_select_translate)
colnames(df_vals_pcChange) <- colnames(pc_vars_select_translate)
df_vals_pcChange_melt <- melt(df_vals_pcChange, na.rm=FALSE, id = c("NewLibID", "YEAR"))
colnames(df_vals_pcChange_melt) <- c("NewLibID", "YEAR", "Variable", "Calc_PC")
perc_change <- merge(df_perc_change, df_vals_pcChange_melt, by=c("NewLibID", "YEAR", "Variable"))

# NAs in percent change
colSums(is.na(df_vals_pcChange))


# Import as time series
df_timeseries <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_timeseries.txt")
df_timeseries_CY <- subset(df_timeseries, df_timeseries$Var_type == "CurrentYear")
df_timeseries_PY <- subset(df_timeseries, df_timeseries$Var_type == "PreviousYear")
df_timeseries_PC <- subset(df_timeseries, df_timeseries$Var_type == "PercentChange")

# NAs in percent change
colSums(is.na(df_timeseries_CY))
colSums(is.na(df_timeseries_PC))

# Imputation for time series - spline
library(imputeTS)
df_timeseries_CY_imp <- na_interpolation(df_timeseries_CY, option="spline", na.identifier="NA")
df_timeseries_PC_imp <- na_interpolation(df_timeseries_PC, option="spline", na.identifier="NA")

# NAs in percent change
colSums(is.na(df_timeseries_CY_imp))
colSums(is.na(df_timeseries_PC_imp))

# Reformat and reshape, add in 2012
df_timeseries_CY_imp_melt <- melt(df_timeseries_CY_imp, id = c("NewLibID", "Variable"))
write.csv(df_timeseries_CY_imp_melt, "df_timeseries_CY_imp_melt.csv")
df_cy <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_CY_with2012.txt")
# NAs in percent change
colSums(is.na(df_cy))

# Imputation for remaining values
df_cy_melt <- melt(df_cy, na.rm=FALSE, id = c("NewLibID", "Year"))
df_cy_melt_impute <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_cy_melt_Crosstab.txt")
# Outliers

colSums(is.na(df_cy_melt_impute))
df_cy_melt_impute$Total.Of.value <- NULL
colnames(df_cy_melt_impute) <- c("NewLibID", "Variable", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
df_CY_imp <- na_interpolation(df_no_out_1, option="spline", na.identifier="NA")
colSums(is.na(df_CY_imp))

#Import and melt full with outliers removed
df_no_out <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_cy_outliers37_rem.csv")
df_no_out_1 <- melt(df_no_out, na.rm=FALSE, id = c("NewLibID", "Year"))
df_no_out_1$Total.Of.value <- NULL
write.csv(df_no_out_1, "df_no_out1.csv")

df_no_out1_byYear <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Working Files/Data/df_no_out1_byYear.txt")
colnames(df_no_out1_byYear) <- c("NewLibID", "Variable", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

dfEBOOK <- df_no_out1_byYear[ which(df_no_out1_byYear$Variable=='EBOOK'), ]
dfEBOOK[dfEBOOK < 0] <- NA
dfEBOOK <- na_interpolation(dfEBOOK, option="spline", na.identifier="NA")
dfEBOOK[dfEBOOK < 0] <- 0
colSums(is.na(dfEBOOK))

dfELMATCIR <- df_no_out1_byYear[ which(df_no_out1_byYear$Variable=='ELMATCIR'), ]
dfELMATCIR[dfELMATCIR < 0] <- NA
dfELMATCIR <- na_interpolation(dfELMATCIR, option="spline", na.identifier="NA")
dfELMATCIR[dfELMATCIR < 0] <- 0
colSums(is.na(dfELMATCIR))

dfELMATEXP <- df_no_out1_byYear[ which(df_no_out1_byYear$Variable=='ELMATEXP'), ]
dfELMATEXP[dfELMATEXP < 0] <- NA
dfELMATEXP <- na_interpolation(dfELMATEXP, option="spline", na.identifier="NA")
dfELMATEXP[dfELMATEXP < 0] <- 0
colSums(is.na(dfELMATEXP))

dfTOTCIR <- df_no_out1_byYear[ which(df_no_out1_byYear$Variable=='TOTCIR'), ]
dfTOTCIR[dfTOTCIR < 0] <- NA
dfTOTCIR <- na_interpolation(dfTOTCIR, option="spline", na.identifier="NA")
dfTOTCIR[dfTOTCIR < 0] <- 0
colSums(is.na(dfELMATCIR))

dfAUDIO_DL <- df_no_out1_byYear[ which(df_no_out1_byYear$Variable=='AUDIO_DL'), ]
dfAUDIO_DL[dfAUDIO_DL < 0] <- NA
dfAUDIO_DL <- na_interpolation(dfAUDIO_DL, option="spline", na.identifier="NA")
dfAUDIO_DL[dfAUDIO_DL < 0] <- 0
colSums(is.na(dfAUDIO_DL))


df_no_out_2 <- na_interpolation(df_no_out1_byYear, option="spline", na.identifier="NA")
colSums(is.na(df_no_out_2))
df_by_year <- df_no_out_2

df_no_out_3 <- melt(df_no_out_2, na.rm=FALSE, id = c("NewLibID", "Year", "variable"))
write.csv(df_no_out_3, "df_no_out3.csv")


df_by_var <- subset




# IMPUTE
library(mice)
# View pattern
md.pattern(df_cy)


# Impute all
imputed <- mice(df_cy, m=5, maxit=10, method='cart', seed=2626)
df_imputed <- mice::complete(imputed)







write.csv(imputed, "imputed.csv")
df_imputed <- mice::complete(imputed)
write.csv(df_imputed, "df_imputed.csv")
df_imputed <- read.csv("C:/Users/Laura/OneDrive/UMGC/Capstone Project/Data PreProcessing/df_imputed.csv")
colSums(is.na(df_imputed))