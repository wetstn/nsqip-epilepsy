# ----------------------------------------------------------------------
# ANOVA and chi-square analysis for race (>2 subgroups)
# ----------------------------------------------------------------------

# Install packages if necessary
#install.packages("googlesheets4")
#install.packages("dplyr")
#install.packages("finalfit")
library(googlesheets4)
library(dplyr)
library(finalfit)

epilepsy_df <- read_sheet("https://docs.google.com/spreadsheets/d/1v4m3ONAJY-QX06uH49JpdSuSuA7hWcYDBPBTBDRlD3Y/edit?usp=sharing", sheet = "NSQIP_Epilepsy_CPT_ICD_2010_202")

# Set numeric variables 
numeric_variables <- c("AGE", "height", "weight", "packs", "MORTPROB", "MORBPROB",
                        "anesthesia_time", "op_time", "hlos", "READMPODAYS1")

# Update numeric data class type
epilepsy_df <- epilepsy_df %>%
  mutate(across(all_of(numeric_variables), ~as.numeric(as.character(.))))

# ----------------------------------------------------------------------
# Construct summary tables for race and sex using finalfit
# ----------------------------------------------------------------------

# Summary table for race 

# Set the dependent variable
dependent = "race"

# Set explanatory variables
explanatory <- c("sex", "inpatient_outpatient", "AGE", "height", "weight", "diabetes", "smoking", "packs",
                 "EtOH", "dyspnea", "functional_status", "HXCOPD", "CPNEUMON", "HXCHF", "HXMI", "HYPERMED",
                 "HXPVD", "HEMI", "CVA", "tumor_cns", "steroid", "bleeding_disorder", "wound_class", "asa_class",
                 "MORTPROB", "MORBPROB", "anesthesia_time", "op_time", "hlos", "superficial_ssi", "wound_infection",
                 "organ_space_ssi", "dehiscience", "pneumonia", "reintubation", "PE", "failed_wean", "uti", "cva",
                 "coma", "deficit", "MI", "dvt", "septic_shock", "return_to_OR", "ELECTSURG", "discharge",
                 "unplanned_readmission", "reoperation", "READMPODAYS1", "READMPODAYS2", "READMPODAYS3",
                 "READMPODAYS4", "cdiff", "UNPLANNEDREADMISSION")

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
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Summary table for sex

# Set dependent variable
dependent = "sex"

# Set explanatory variables
explanatory <- c("race", "inpatient_outpatient", "AGE", "height", "weight", "diabetes", "smoking", "packs",
                 "EtOH", "dyspnea", "functional_status", "HXCOPD", "CPNEUMON", "HXCHF", "HXMI", "HYPERMED",
                 "HXPVD", "HEMI", "CVA", "tumor_cns", "steroid", "bleeding_disorder", "wound_class", "asa_class",
                 "MORTPROB", "MORBPROB", "anesthesia_time", "op_time", "hlos", "superficial_ssi", "wound_infection",
                 "organ_space_ssi", "dehiscience", "pneumonia", "reintubation", "PE", "failed_wean", "uti", "cva",
                 "coma", "deficit", "MI", "dvt", "septic_shock", "return_to_OR", "ELECTSURG", "discharge",
                 "unplanned_readmission", "reoperation", "READMPODAYS1", "READMPODAYS2", "READMPODAYS3",
                 "READMPODAYS4", "cdiff", "UNPLANNEDREADMISSION")

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
                     orderbytotal = TRUE,
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frames to the shared Google Drive
#write_sheet(epilepsy_summary_race, ss = "https://docs.google.com/spreadsheets/d/1REKLjXEztNweS6VUXgqRwKENtQx5Ju17U6NmNOpdtg8/edit?usp=sharing", sheet = "epilepsy_summary_race")
#write_sheet(epilepsy_summary_sex, ss = "https://docs.google.com/spreadsheets/d/1REKLjXEztNweS6VUXgqRwKENtQx5Ju17U6NmNOpdtg8/edit?usp=sharing", sheet = "epilepsy_summary_sex")

# ----------------------------------------------------------------------
# Hot-encoding ANOVA and chi-square analysis for race (>2 subgroups)
# ----------------------------------------------------------------------
# 
# Create  empty list to store results
epilepsy_stats_race_list <- list()

# Loop through each explanatory variable
for (variable in names(epilepsy_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("race")) {
    next
  }

  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      # Perform ANOVA
      anova_result <- aov(epilepsy_df[[variable]] ~ race, data = epilepsy_df)
      
      # Extract the p-value from ANOVA summary
      p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
      
      # Append results to the list
      epilepsy_stats_race_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(epilepsy_df[[variable]])
      for (level in levels) {
        contingency_table <- table(epilepsy_df[[variable]] == level, epilepsy_df$race)
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
  if (variable %in% c("sex")) {
    next
  }

  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      # Perform unpaired t-test
      t_test_result <- t.test(epilepsy_df[[variable]] ~ sex, data = epilepsy_df)
      p_value <- t_test_result$p.value
      
      # Append results to the list
      epilepsy_stats_sex_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(epilepsy_df[[variable]])
      for (level in levels) {
        contingency_table <- table(epilepsy_df[[variable]] == level, epilepsy_df$sex)
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
#write_sheet(epilepsy_stats_race, ss = "https://docs.google.com/spreadsheets/d/1REKLjXEztNweS6VUXgqRwKENtQx5Ju17U6NmNOpdtg8/edit?usp=sharing", sheet = "epilepsy_stats_race")
#write_sheet(epilepsy_stats_sex, ss = "https://docs.google.com/spreadsheets/d/1REKLjXEztNweS6VUXgqRwKENtQx5Ju17U6NmNOpdtg8/edit?usp=sharing", sheet = "epilepsy_stats_sex")