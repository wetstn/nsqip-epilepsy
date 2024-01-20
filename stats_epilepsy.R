library(googlesheets4)

final_Epilepsy <- read_sheet("https://docs.google.com/spreadsheets/d/1L3-UIFNSQ8TGLZBRRQTrmsZFTyG9gqIBDA-rZf33BJs/edit?usp=sharing", sheet = "final_Epilepsy")

# --------------------------------------------------
# WOUND COMPLICATIONS
# --------------------------------------------------
# Multivariate logistic regression
logistic_model <- glm(DISCHDESTBIN ~ AGE + SEX + BMI + RACE_NEW + ETHNICITY_HISPANIC +
                        MED_COMORBID + INF_RISK + VASC_RISK + ASACLAS +
                        FNSTATUS2 + EPILEPSY + TRANST, 
                      data = final_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# Univariate logistic regression
logistic_model <- glm(DISCHDESTBIN ~ TRANST, 
                      data = final_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# Display summary of the model
summary_output <- summary(logistic_model)
# Extract coefficients and their standard errors directly
coef_inf_risk_yes <- summary_output$coefficients["ETHNICITY_HISPANICYes", c("Estimate", "Std. Error")]

# Check if the predictor is found
if (is.null(coef_inf_risk_yes)) {
  cat("Predictor 'ETHNICITY_HISPANICYes' not found in the model.\n")
} else {
  # Calculate odds ratio and confidence interval
  odds_ratio_inf_risk_yes <- exp(coef_inf_risk_yes["Estimate"])
  lower_ci_inf_risk_yes <- exp(coef_inf_risk_yes["Estimate"] - 1.96 * coef_inf_risk_yes["Std. Error"])
  upper_ci_inf_risk_yes <- exp(coef_inf_risk_yes["Estimate"] + 1.96 * coef_inf_risk_yes["Std. Error"])
  
  # Display results
  cat("Odds Ratio for ETHNICITY_HISPANICYes:", odds_ratio_inf_risk_yes, "\n")
  cat("95% Confidence Interval for ETHNICITY_HISPANICYes:", lower_ci_inf_risk_yes, "-", upper_ci_inf_risk_yes, "\n")
}



# --------------------------------------------------
# PULMONARY COMPLICATIONS
# --------------------------------------------------
logistic_model <- glm(PULMCOMP ~ AGE + SEX + BMI + RACE_NEW + ETHNICITY_HISPANIC +
                        MED_COMORBID + INF_RISK + VASC_RISK + ASACLAS +
                        FNSTATUS2 + EPILEPSY + TRANST, 
                      data = full_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# --------------------------------------------------
# MEDICAL COMPLICATIONS
# --------------------------------------------------
logistic_model <- glm(MEDCOMP ~ AGE + SEX + BMI + RACE_NEW + ETHNICITY_HISPANIC +
                        MED_COMORBID + INF_RISK + VASC_RISK + ASACLAS +
                        FNSTATUS2 + EPILEPSY + TRANST, 
                      data = full_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# --------------------------------------------------
# NEURO COMPLICATIONS
# --------------------------------------------------
logistic_model <- glm(NEUROCOMP ~ AGE + SEX + BMI + RACE_NEW + ETHNICITY_HISPANIC +
                        MED_COMORBID + INF_RISK + VASC_RISK + ASACLAS +
                        FNSTATUS2 + EPILEPSY + TRANST, 
                      data = full_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)



logistic_model <- glm(NEUROCOMP ~ AGE + SEX + BMI + RACE_NEW + ETHNICITY_HISPANIC +
                        MED_COMORBID + INF_RISK + VASC_RISK + ASACLAS +
                        FNSTATUS2 + EPILEPSY + TRANST, 
                      data = full_Epilepsy, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

