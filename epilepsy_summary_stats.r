# Install packages if necessary
#install.packages("googlesheets4")
#install.packages("dplyr")
#install.packages("finalfit")
library(googlesheets4)
library(dplyr)
library(finalfit)

epilepsy_df <- read_sheet("https://docs.google.com/spreadsheets/d/1UTFaBQiaD4vyVr1mPJwzCm-patal6hx6ORAGal_RAsQ/edit?usp=sharing", sheet = "NSQIP_Epilepsy_CPT_ICD_2010_2020")

# Set numeric variables
numeric_variables <- c(
  "CASEID", "WORKRVU", "AGE", "ADMYR", "ADMSYR", "OPERYR", "HEIGHT", "WEIGHT",
  "PACKS", "DPRNA", "DPRBUN", "DPRCREAT", "DPRALBUM", "DPRBILI", "DPRSGOT",
  "DPRALKPH", "DPRWBC", "DPRHCT", "DPRPLATE", "DPRPTT", "DPRPT", "DPRINR",
  "PRSODM", "PRBUN", "PRCREAT", "PRALBUM", "PRBILI", "PRSGOT", "PRALKPH",
  "PRWBC", "PRHCT", "PRPLATE", "PRPTT", "PRINR", "PRPT", "PGY", "MALLAMP",
  "MORTPROB", "MORBPROB", "RBC", "ANESURG", "SURGANE", "DPATRM", "ANETIME",
  "OPTIME", "SDISDT", "HDISDT", "YRDEATH", "TOTHLOS", "ADMQTR", "HTOODAY",
  "STOODAY", "TOTSLOS", "NSUPINFEC", "DSUPINFEC", "NWNDINFD", "DWNDINFD",
  "NORGSPCSSI", "NDEHIS", "DDEHIS", "NOUPNEUMO", "DOUPNEUMO", "NREINTUB",
  "DREINTUB", "NPULEMBOL", "DPULEMBOL", "NFAILWEAN", "DFAILWEAN", "NRENAINSF",
  "DRENAINSF", "NOPRENAFL", "DOPRENAFL", "NURNINFEC", "DURNINFEC", "NCNSCVA",
  "DCNSCVA", "NCNSCOMA", "DCNSCOMA", "NNEURODEF", "DNEURODEF", "NCDARREST",
  "DCDARREST", "NCDMI", "DCDMI", "NOTHBLEED", "DOTHBLEED", "NOTHGRAFL",
  "DOTHGRAFL", "NOTHDVT", "DOTHDVT", "NOTHSYSEP", "DOTHSYSEP", "NOTHSESHOCK",
  "DOTHSESHOCK", "DSDTOHD", "DOPERTOD", "DOPTODIS", "RETORPODAYS",
  "RETOR2PODAYS", "READMPODAYS1", "READMPODAYS2", "READMPODAYS3",
  "READMPODAYS4", "READMPODAYS5", "NOTHCDIFF", "DOTHCDIFF", "BLEED_UNITS_TOT",
  "OTHERWRVU", "CONWRVU"
)

# Convert -99 to NA for all numeric_variables
epilepsy_df[numeric_variables][epilepsy_df[numeric_variables] == -99] <- NA

# Update numeric data class type
epilepsy_df <- epilepsy_df %>%
  mutate(across(all_of(numeric_variables), ~as.numeric(as.character(.))))

# Calculate BMI and insert it as the 19th column
epilepsy_df <- cbind(epilepsy_df[, 1:18], BMI = with(epilepsy_df, 703 * WEIGHT / ((HEIGHT)^2)), epilepsy_df[, 19:ncol(epilepsy_df)])

numeric_variables <- c(
  numeric_variables[1:8],
  "BMI",
  numeric_variables[9:length(numeric_variables)]
)

# ----------------------------------------------------------------------
# Construct summary tables for race and sex using finalfit
# ----------------------------------------------------------------------

# Summary table for race 

# Set the dependent variable
dependent = "RACE_NEW"

# Set explanatory variables
explanatory <- c(
  "SEX", "ETHNICITY_HISPANIC", "PRNCPTX", "CPT", "WORKRVU", "INOUT",
  "TRANST", "AGE", "ADMYR", "ADMSYR", "OPERYR", "ANESTHES", "ATTEND", "SURGSPEC", "HEIGHT",
  "WEIGHT", "BMI", "DIABETES", "SMOKE", "PACKS", "ETOH", "DYSPNEA", "DNR", "FNSTATUS1", "FNSTATUS2",
  "VENTILAT", "HXCOPD", "CPNEUMON", "ASCITES", "ESOVAR", "HXCHF", "HXMI", "PRVPCI", "PRVPCS",
  "HXANGINA", "HYPERMED", "HXPVD", "RESTPAIN", "RENAFAIL", "DIALYSIS", "IMPSENS", "COMA",
  "HEMI", "HXTIA", "CVA", "CVANO", "TUMORCNS", "PARA", "QUAD", "DISCANCR", "WNDINF", "STEROID",
  "WTLOSS", "BLEEDDIS", "TRANSFUS", "CHEMO", "RADIO", "PRSEPIS", "PREGNANCY", "PROPER30", "DPRNA",
  "DPRBUN", "DPRCREAT", "DPRALBUM", "DPRBILI", "DPRSGOT", "DPRALKPH", "DPRWBC", "DPRHCT", "DPRPLATE",
  "DPRPTT", "DPRPT", "DPRINR", "PRSODM", "PRBUN", "PRCREAT", "PRALBUM", "PRBILI", "PRSGOT", "PRALKPH",
  "PRWBC", "PRHCT", "PRPLATE", "PRPTT", "PRINR", "PRPT", "OPNOTE", "PGY", "EMERGNCY", "WNDCLAS", "ASACLAS",
  "AIRTRA", "MALLAMP", "MORTPROB", "MORBPROB", "RBC", "ANESURG", "SURGANE", "DPATRM", "ANETIME", "OPTIME",
  "TYPEINTOC", "SDISDT", "HDISDT", "YRDEATH", "TOTHLOS", "ADMQTR", "HTOODAY", "STOODAY", "TOTSLOS",
  "NSUPINFEC", "SUPINFEC", "DSUPINFEC", "NWNDINFD", "WNDINFD", "DWNDINFD", "NORGSPCSSI", "ORGSPCSSI",
  "DORGSPCSSI", "NDEHIS", "DEHIS", "DDEHIS", "NOUPNEUMO", "OUPNEUMO", "DOUPNEUMO", "NREINTUB", "REINTUB",
  "DREINTUB", "NPULEMBOL", "PULEMBOL", "DPULEMBOL", "NFAILWEAN", "FAILWEAN", "DFAILWEAN", "NRENAINSF",
  "RENAINSF", "DRENAINSF", "NOPRENAFL", "OPRENAFL", "DOPRENAFL", "NURNINFEC", "URNINFEC", "DURNINFEC",
  "NCNSCVA", "CNSCVA", "DCNSCVA", "NCNSCOMA", "CNSCOMA", "DCNSCOMA", "NNEURODEF", "NEURODEF", "DNEURODEF",
  "NCDARREST", "CDARREST", "DCDARREST", "NCDMI", "CDMI", "DCDMI", "NOTHBLEED", "OTHBLEED", "DOTHBLEED",
  "NOTHGRAFL", "OTHGRAFL", "DOTHGRAFL", "NOTHDVT", "OTHDVT", "DOTHDVT", "NOTHSYSEP", "OTHSYSEP", "DOTHSYSEP",
  "NOTHSESHOCK", "OTHSESHOCK", "DOTHSESHOCK", "PODIAG", "PODIAGTX", "RETURNOR", "DSDTOHD", "DOPERTOD", "DOPTODIS",
  "OTHERCPT", "OTHERPROC", "CONCPT", "CONCURR", "ELECTSURG", "DISCHDEST", "READMISSION", "UNPLANREADMISSION",
  "REOPERATION", "STILLINHOSP", "SSSIPATOS", "DSSIPATOS", "OSSIPATOS", "PNAPATOS", "UTIPATOS", "VENTPATOS",
  "SEPSISPATOS", "SEPSHOCKPATOS", "RETORPODAYS", "RETORRELATED", "RETOR2PODAYS", "RETOR2RELATED", "READMPODAYS1",
  "READMPODAYS2", "READMPODAYS3", "READMPODAYS4", "READMPODAYS5", "PODIAG10", "PODIAGTX10", "WOUND_CLOSURE",
  "PODIAG_OTHER", "PODIAG_OTHER10", "ANESTHES_OTHER", "PUFYEAR", "OTHCDIFF", "NOTHCDIFF", "DOTHCDIFF", "EOL_WDCARE",
  "BLEED_UNITS_TOT", "OTHERWRVU", "CONWRVU", "UNPLANNEDREADMISSION", "REOPORCPT", "REOPORICD9", "REOPORICD10",
  "READMSUSPREASON", "READMRELATED", "READMRELICD9", "READMRELICD10", "READMUNRELSUSP", "READMUNRELICD9", "READMUNRELICD10"
)

# Generate summary table for race
epilepsy_summary_race <- epilepsy_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include = FALSE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p = FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Summary table for sex

# Set dependent variable
dependent = "SEX"

# Set explanatory variables
explanatory <- c(
  "RACE_NEW", "ETHNICITY_HISPANIC", "PRNCPTX", "CPT", "WORKRVU", "INOUT",
  "TRANST", "AGE", "ADMYR", "ADMSYR", "OPERYR", "ANESTHES", "ATTEND", "SURGSPEC", "HEIGHT",
  "WEIGHT", "BMI", "DIABETES", "SMOKE", "PACKS", "ETOH", "DYSPNEA", "DNR", "FNSTATUS1", "FNSTATUS2",
  "VENTILAT", "HXCOPD", "CPNEUMON", "ASCITES", "ESOVAR", "HXCHF", "HXMI", "PRVPCI", "PRVPCS",
  "HXANGINA", "HYPERMED", "HXPVD", "RESTPAIN", "RENAFAIL", "DIALYSIS", "IMPSENS", "COMA",
  "HEMI", "HXTIA", "CVA", "CVANO", "TUMORCNS", "PARA", "QUAD", "DISCANCR", "WNDINF", "STEROID",
  "WTLOSS", "BLEEDDIS", "TRANSFUS", "CHEMO", "RADIO", "PRSEPIS", "PREGNANCY", "PROPER30", "DPRNA",
  "DPRBUN", "DPRCREAT", "DPRALBUM", "DPRBILI", "DPRSGOT", "DPRALKPH", "DPRWBC", "DPRHCT", "DPRPLATE",
  "DPRPTT", "DPRPT", "DPRINR", "PRSODM", "PRBUN", "PRCREAT", "PRALBUM", "PRBILI", "PRSGOT", "PRALKPH",
  "PRWBC", "PRHCT", "PRPLATE", "PRPTT", "PRINR", "PRPT", "OPNOTE", "PGY", "EMERGNCY", "WNDCLAS", "ASACLAS",
  "AIRTRA", "MALLAMP", "MORTPROB", "MORBPROB", "RBC", "ANESURG", "SURGANE", "DPATRM", "ANETIME", "OPTIME",
  "TYPEINTOC", "SDISDT", "HDISDT", "YRDEATH", "TOTHLOS", "ADMQTR", "HTOODAY", "STOODAY", "TOTSLOS",
  "NSUPINFEC", "SUPINFEC", "DSUPINFEC", "NWNDINFD", "WNDINFD", "DWNDINFD", "NORGSPCSSI", "ORGSPCSSI",
  "DORGSPCSSI", "NDEHIS", "DEHIS", "DDEHIS", "NOUPNEUMO", "OUPNEUMO", "DOUPNEUMO", "NREINTUB", "REINTUB",
  "DREINTUB", "NPULEMBOL", "PULEMBOL", "DPULEMBOL", "NFAILWEAN", "FAILWEAN", "DFAILWEAN", "NRENAINSF",
  "RENAINSF", "DRENAINSF", "NOPRENAFL", "OPRENAFL", "DOPRENAFL", "NURNINFEC", "URNINFEC", "DURNINFEC",
  "NCNSCVA", "CNSCVA", "DCNSCVA", "NCNSCOMA", "CNSCOMA", "DCNSCOMA", "NNEURODEF", "NEURODEF", "DNEURODEF",
  "NCDARREST", "CDARREST", "DCDARREST", "NCDMI", "CDMI", "DCDMI", "NOTHBLEED", "OTHBLEED", "DOTHBLEED",
  "NOTHGRAFL", "OTHGRAFL", "DOTHGRAFL", "NOTHDVT", "OTHDVT", "DOTHDVT", "NOTHSYSEP", "OTHSYSEP", "DOTHSYSEP",
  "NOTHSESHOCK", "OTHSESHOCK", "DOTHSESHOCK", "PODIAG", "PODIAGTX", "RETURNOR", "DSDTOHD", "DOPERTOD", "DOPTODIS",
  "OTHERCPT", "OTHERPROC", "CONCPT", "CONCURR", "ELECTSURG", "DISCHDEST", "READMISSION", "UNPLANREADMISSION",
  "REOPERATION", "STILLINHOSP", "SSSIPATOS", "DSSIPATOS", "OSSIPATOS", "PNAPATOS", "UTIPATOS", "VENTPATOS",
  "SEPSISPATOS", "SEPSHOCKPATOS", "RETORPODAYS", "RETORRELATED", "RETOR2PODAYS", "RETOR2RELATED", "READMPODAYS1",
  "READMPODAYS2", "READMPODAYS3", "READMPODAYS4", "READMPODAYS5", "PODIAG10", "PODIAGTX10", "WOUND_CLOSURE",
  "PODIAG_OTHER", "PODIAG_OTHER10", "ANESTHES_OTHER", "PUFYEAR", "OTHCDIFF", "NOTHCDIFF", "DOTHCDIFF", "EOL_WDCARE",
  "BLEED_UNITS_TOT", "OTHERWRVU", "CONWRVU", "UNPLANNEDREADMISSION", "REOPORCPT", "REOPORICD9", "REOPORICD10",
  "READMSUSPREASON", "READMRELATED", "READMRELICD9", "READMRELICD10", "READMUNRELSUSP", "READMUNRELICD9", "READMUNRELICD10"
)

# Generate summary table for sex
epilepsy_summary_sex <- epilepsy_df %>%
  summary_factorlist(dependent, explanatory,
                     na_include = FALSE,
                     na_include_dependent = FALSE,
                     total_col = TRUE,
                     add_col_totals = TRUE,
                     col_totals_prefix = "n = ",
                     p = FALSE,
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = FALSE,
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frames to the shared Google Drive
write_sheet(epilepsy_summary_race, ss = "https://docs.google.com/spreadsheets/d/1UTFaBQiaD4vyVr1mPJwzCm-patal6hx6ORAGal_RAsQ/edit?usp=sharing", sheet = "Epilepsy Race Summary")
write_sheet(epilepsy_summary_sex, ss = "https://docs.google.com/spreadsheets/d/1UTFaBQiaD4vyVr1mPJwzCm-patal6hx6ORAGal_RAsQ/edit?usp=sharing", sheet = "Epilepsy Sex Summary")

# ----------------------------------------------------------------------
# Hot-encoding ANOVA and chi-square analysis for race (>2 subgroups)
# ----------------------------------------------------------------------

# Create  empty list to store results
epilepsy_stats_race_list <- list()

# Loop through each explanatory variable
for (variable in names(epilepsy_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("RACE_NEW")) {
    next
  }

  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      # Perform ANOVA
      anova_result <- aov(epilepsy_df[[variable]] ~ RACE_NEW, data = epilepsy_df)
      
      # Extract the p-value from ANOVA summary
      p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
      
      # Append results to the list
      epilepsy_stats_race_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(epilepsy_df[[variable]])
      for (level in levels) {
        contingency_table <- table(epilepsy_df[[variable]] == level, epilepsy_df$RACE_NEW)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        epilepsy_stats_race_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    # If an error occurs, print NA in the p-value column
    epilepsy_stats_race_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
epilepsy_stats_race <- do.call(rbind, epilepsy_stats_race_list)

# ----------------------------------------------------------------------
# Hot-encoding unpaired t-test and chi-square analysis for sex (2 subgroups)
# ----------------------------------------------------------------------

# Create an empty list to store results
epilepsy_stats_sex_list <- list()

# Loop through each explanatory variable
for (variable in names(epilepsy_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("SEX")) {
    next
  }

  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      # Perform unpaired t-test
      t_test_result <- t.test(epilepsy_df[[variable]] ~ SEX, data = epilepsy_df)
      p_value <- t_test_result$p.value
      
      # Append results to the list
      epilepsy_stats_sex_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(epilepsy_df[[variable]])
      for (level in levels) {
        contingency_table <- table(epilepsy_df[[variable]] == level, epilepsy_df$SEX)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        epilepsy_stats_sex_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    # If an error occurs, print NA in the p-value column
    epilepsy_stats_sex_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
epilepsy_stats_sex <- do.call(rbind, epilepsy_stats_sex_list)

# Write to the Google drive
write_sheet(epilepsy_stats_race, ss = "https://docs.google.com/spreadsheets/d/1UTFaBQiaD4vyVr1mPJwzCm-patal6hx6ORAGal_RAsQ/edit?usp=sharing", sheet = "Epilepsy Race Stats")
write_sheet(epilepsy_stats_sex, ss = "https://docs.google.com/spreadsheets/d/1UTFaBQiaD4vyVr1mPJwzCm-patal6hx6ORAGal_RAsQ/edit?usp=sharing", sheet = "Epilepsy Sex Stats")
