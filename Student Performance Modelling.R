# SECTION 1: Load Libraries and Dataset
library(corrplot)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(forcats)

df <- read_csv("/Users/seohyori/Desktop/StudentPerformanceFactors.csv")

# SECTION 2: Initial Data Preparation
# 2.1 Standardize column names (lowercase + underscores)
names(df) <- tolower(gsub(" ", "_", names(df)))

# 2.2 Identify categorical variables
categorical_vars <- c(
  "parental_involvement", "access_to_resources", "extracurricular_activities",
  "motivation_level", "internet_access", "family_income", "teacher_quality",
  "school_type", "peer_influence", "learning_disabilities",
  "parental_education_level", "distance_from_home", "gender"
)

# 2.3 Convert categorical columns to factors
df <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))

# SECTION 3: Baseline Linear Regression (Raw Data, No Cleaning)
model <- lm(exam_score ~ ., data = as.data.frame(df))
cat("\n=== Linear Regression on Raw Data (No Cleaning) ===\n")
summary(model)

# SECTION 4: Data Cleaning
# 4.1 Remove rows with missing values
df <- df %>% drop_na()

# 4.2 Remove outliers in numeric columns using z-score filtering
numeric_vars <- c("hours_studied", "attendance", "sleep_hours",
                  "previous_scores", "tutoring_sessions", "physical_activity", "exam_score")

df_clean <- df %>%
  filter(if_all(all_of(numeric_vars), ~ abs(scale(.)) <= 3))

# SECTION 5: Correlation Check
prepare_correlation_heatmap <- function(df) {
  
  # Ordinal Encoding for Low-Medium-High style variables
  df <- df %>%
    mutate(
      family_income = fct_relevel(family_income, "Low", "Medium", "High"),
      access_to_resources = fct_relevel(access_to_resources, "Low", "Medium", "High"),
      parental_involvement = fct_relevel(parental_involvement, "Low", "Medium", "High"),
      motivation_level = fct_relevel(motivation_level, "Low", "Medium", "High"),
      teacher_quality = fct_relevel(teacher_quality, "Low", "Medium", "High")
    ) %>%
    mutate(
      family_income_numeric = as.numeric(family_income) - 1,
      access_to_resources_numeric = as.numeric(access_to_resources) - 1,
      parental_involvement_numeric = as.numeric(parental_involvement) - 1,
      motivation_level_numeric = as.numeric(motivation_level) - 1,
      teacher_quality_numeric = as.numeric(teacher_quality) - 1
    )
  
  # Ensure missing values & outliers are removed
  numeric_vars <- c("hours_studied", "attendance", "sleep_hours", "previous_scores", "tutoring_sessions", "physical_activity", "exam_score")
  
  df_clean <- df %>%
    drop_na() %>%
    filter(if_all(all_of(numeric_vars), ~ abs(scale(.)) <= 3))
  
  # Prepare variables for Correlation Heatmap
  cor_vars <- c("exam_score", "hours_studied", "attendance", "previous_scores",
                "family_income_numeric", "access_to_resources_numeric", "parental_involvement_numeric", 
                "motivation_level_numeric", "teacher_quality_numeric")
  
  valid_vars <- cor_vars[cor_vars %in% colnames(df_clean)]
  cor_data <- df_clean[, valid_vars]
  
  # Compute the correlation matrix
  cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
  
  # Visualization of Correlation Heatmap
  corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black", 
           addCoef.col = "black", number.cex = 0.7)
}

# To run the function on your dataframe
prepare_correlation_heatmap(df)

# SECTION 6: Dummy Encoding for Cleaned Data
# 6.1 Create dummy variables (excluding exam_score)
dummy_vars <- model.matrix(~ . - exam_score, data = df_clean)[, -1]

# 6.2 Combine dummy variables with target variable
final_df <- cbind(exam_score = df_clean$exam_score, dummy_vars)

# 6.3 Average Exam Scores of Important Dummy Variables
final_df <- as.data.frame(final_df)
print(final_df)

average_scores <- final_df %>%
  summarise(
    avg_hours_studied = mean(exam_score, na.rm = TRUE),
    avg_family_incomeLow = mean(exam_score[family_incomeLow == 1], na.rm = TRUE),
    avg_family_incomeMedium = mean(exam_score[family_incomeMedium == 1], na.rm = TRUE),
    avg_tutoring_sessions = mean(exam_score[tutoring_sessions == 1], na.rm = TRUE),
    avg_extracurricular_activitiesYes = mean(exam_score[extracurricular_activitiesYes == 1], na.rm = TRUE)
  )

average_scores_long <- average_scores %>%
  pivot_longer(cols = everything(),
               names_to = "dummy_variable",
               values_to = "average_exam_score")

average_scores_long$dummy_variable <- gsub("avg_", "", average_scores_long$dummy_variable)

# 6.4 Visualization of average exam score by important dummy variables
ggplot(average_scores_long, aes(x = dummy_variable, y = average_exam_score, fill = dummy_variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "extracurricular_activitiesYes" = "lightpink",
    "family_incomeLow" = "lightskyblue",  
    "family_incomeMedium" = "lightcyan",  
    "hours_studied" = "lightgoldenrodyellow",
    "tutoring_sessions" = "lavender"
  )) +
  scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +  
  geom_text(aes(label = round(average_exam_score, 4)), vjust = -0.5, size = 4) +  
  theme_minimal() +
  labs(x = "Dummy Variable", y = "Average Exam Score", title = "Average Exam Score by Dummy Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# SECTION 7: Linear Regression on Cleaned & Encoded Data
model <- lm(exam_score ~ ., data = as.data.frame(final_df))
cat("\n=== Linear Regression on Cleaned & Dummy-Encoded Data ===\n")
summary(model)

# SECTION 8: Visualization of Exam score vs Each Predictor
# Scatter Plots (Exam Score vs Each Numerical Predictor, excluding exam_score vs exam_score)
cat("\n=== Scatter Plots: Exam Score vs Each Numerical Predictor ===\n")
# Loop through each numerical variable, excluding exam_score
for (var in numeric_vars) {
  if (var != "exam_score") {  
    p <- ggplot(final_df, aes(x = .data[[var]], y = exam_score)) +
      geom_point(color = "dodgerblue", alpha = 0.5) +  
      geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) + 
      ggtitle(paste("Exam Score vs.", var)) +  
      theme_minimal() +  
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)
      )
    print(p)
  }
}

# Bar Plots (Exam Score vs Each Categorical Predictor)
# Categorical Variables
categorical_vars_bar <- c("parental_involvementLow", "parental_involvementMedium", 
                      "access_to_resourcesLow", "access_to_resourcesMedium", 
                      "extracurricular_activitiesYes", "motivation_levelLow", 
                      "motivation_levelMedium", "internet_accessYes", 
                      "family_incomeLow", "family_incomeMedium", 
                      "teacher_qualityLow", "teacher_qualityMedium", 
                      "school_typePublic", "peer_influenceNeutral", 
                      "peer_influencePositive", "learning_disabilitiesYes", 
                      "parental_education_levelHigh School", 
                      "parental_education_levelPostgraduate", 
                      "distance_from_homeModerate", "distance_from_homeNear", 
                      "genderMale")

cat("\n=== Bar Plots: Exam Score vs Each Categorical Predictor ===\n")
# Loop through each categorical variable
for (var in categorical_vars_bar) {
  p <- ggplot(final_df, aes(x = .data[[var]], y = exam_score)) +
    geom_bar(stat = "summary", fun = "mean", fill = "dodgerblue", color = "black", alpha = 0.7) +
    ggtitle(paste("Exam Score vs.", var)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1) 
    ) +
    geom_text(
      stat = "summary", 
      fun = "mean", 
      aes(label = round(..y.., 4)), 
      vjust = -0.5,
      size = 4      
    ) +
    scale_y_continuous(limits = c(0, 100))  
  print(p)
}

# SECTION 9: Heteroskedasticity Detection and Analysis
# 9.1 Residuals vs. Each Independent Variable
# Purpose: Visually detect non-constant variance across individual predictors (fan shapes, increasing spread, etc.)
cat("\n=== 9.1 Residuals vs. Individual Predictors ===\n")
final_df <- as.data.frame(final_df)
final_df$residuals <- residuals(model)
independent_vars <- setdiff(names(final_df), c("exam_score", "residuals"))

for (var in independent_vars) {
  p <- ggplot(final_df, aes(x = .data[[var]], y = residuals)) +
    geom_point(color = "dodgerblue", alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
    ggtitle(paste("Residuals vs.", var)) +
    xlab(var) + ylab("Residuals") +
    coord_cartesian(ylim = c(-1, 1)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11)
    )
  print(p)
}

# 9.2 Standard Deviation of Residuals by Low-Cardinality Predictors
# Purpose: Check if residual variance differs across categorical or low-cardinality variables
cat("\n=== 9.2 Standard Deviation of Residuals by Categorical or Discrete Variables ===\n")
final_df$residuals <- residuals(model)
for (var in independent_vars) {
  n_unique <- length(unique(final_df[[var]]))
  
  if (n_unique <= 40) {
    temp_df <- final_df %>%
      group_by(across(all_of(var))) %>%
      summarise(SD_Residual = sd(residuals), .groups = "drop") %>%
      arrange(across(all_of(var)))
    
    total_sd <- sd(final_df$residuals)
    
    cat("\n--------------------------------------------------\n")
    cat(paste0("Standard Deviation of Residuals by '", var, "' (", n_unique, " groups)\n"))
    cat("--------------------------------------------------\n")
    print(temp_df, n = Inf)
    cat(paste0("Total SD across all values of ", var, ": ", round(total_sd, 6), "\n"))
  }
}

# 9.3 Comparison: OLS vs Robust Standard Errors
# Purpose: Check how standard errors and significance change under heteroskedasticity
cat("\n=== 9.3 Comparison of OLS vs. Robust Standard Errors ===\n")
ols_result <- coeftest(model)
robust_result <- coeftest(model, vcov. = vcovHC(model, type = "HC1"))

comparison_df <- data.frame(
  Coefficient = coef(model),
  `OLS Std. Err.` = ols_result[, "Std. Error"],
  `OLS t` = ols_result[, "t value"],
  `OLS P>|t|` = ols_result[, "Pr(>|t|)"],
  `Robust Std. Err.` = robust_result[, "Std. Error"],
  `Robust t` = robust_result[, "t value"],
  `Robust P>|t|` = robust_result[, "Pr(>|t|)"]
)

comparison_df <- round(comparison_df, 6)
print(comparison_df)

# 9.4 Breusch-Pagan Test (Formal Statistical Test)
# H0: Constant variance (no heteroskedasticity)
cat("\n=== 9.4 Breusch-Pagan Test for Heteroskedasticity ===\n")
bp_result <- bptest(model)
cat("H0: Constant variance (homoskedasticity)\n\n")
cat(paste0("Chi-squared(", bp_result$parameter, ") = ", round(bp_result$statistic, 2), "\n"))
cat(paste0("Prob > chi2 = ", format.pval(bp_result$p.value, digits = 4), "\n"))

# 9.5 Interpretation of BP Test
if (bp_result$p.value < 0.05) {
  cat("\nResult: Reject H0 → Evidence of heteroskedasticity detected.\n")
} else {
  cat("\nResult: Fail to reject H0 → No evidence of heteroskedasticity.\n")
}

# SECTION 10: Multicollinearity Detection
# 10.1 VIF (Variance Inflation Factor)
cat("\n=== 10.1 Variance Inflation Factor (VIF) ===\n")
vif_values <- vif(model)
vif_values <- sort(vif_values, decreasing = TRUE)
print(round(vif_values, 3))

high_vif <- vif_values[vif_values > 5]
if (length(high_vif) > 0) {
  cat("\nVariables with VIF > 5 (potential multicollinearity):\n")
  print(round(high_vif, 3))
} else {
  cat("\nNo significant multicollinearity detected (all VIFs ≤ 5).\n")
}

# 10.2 Simple Correlation Matrix (Numerical Variables)
cat("\n=== 10.2 Correlation Matrix (Numerical Variables) ===\n")
cor_matrix <- cor(df_clean %>% select(all_of(numeric_vars)))
print(round(cor_matrix, 3))
high_cor <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("\nHighly correlated variable pairs (|correlation| > 0.8):\n")
  for (i in 1:nrow(high_cor)) {
    cat(paste0("- ", rownames(cor_matrix)[high_cor[i,1]], " & ",
               colnames(cor_matrix)[high_cor[i,2]], ": ",
               round(cor_matrix[high_cor[i,1], high_cor[i,2]], 3), "\n"))
  }
} else {
  cat("\nNo variable pairs with high correlation (|correlation| > 0.8).\n")
}

# 10.3 Auxiliary Regressions (Corrected - Excluding Dependent Variable)
cat("\n=== 10.3 Auxiliary Regressions (Corrected) ===\n")

# Identify numeric variables excluding the dependent variable
available_numeric_vars <- setdiff(intersect(numeric_vars, names(df_clean)), "exam_score")

for (var in available_numeric_vars) {
  predictors <- setdiff(available_numeric_vars, var)  # other independent variables
  df_subset <- df_clean %>% select(all_of(c(var, predictors)))
  
  if (length(predictors) >= 1) {
    aux_model <- lm(df_subset[[var]] ~ ., data = df_subset %>% select(-all_of(var)))
    
    r2 <- summary(aux_model)$r.squared
    fstat <- summary(aux_model)$fstatistic
    
    cat(paste0("- ", var, ": R² = ", round(r2, 3)))
    
    if (!is.null(fstat)) {
      cat(paste0(", F(", fstat["numdf"], ", ", fstat["dendf"], ") = ", round(fstat["value"], 2)))
      if (fstat["value"] > 10) {
        cat(" - High F-statistic suggests potential multicollinearity\n")
      } else {
        cat(" - Acceptable\n")
      }
    } else {
      cat(" (No F-statistic available)\n")
    }
  } else {
    cat(paste0("- ", var, ": Skipped (not enough predictors)\n"))
  }
}

# 10.4 Change in Sign of Coefficients
cat("\n=== 10.4 Change in Sign of Coefficients ===\n")
# Create a reduced model excluding attendance and sleep_hours
reduced_model <- lm(exam_score ~ . - attendance - sleep_hours, data = as.data.frame(final_df))
full_coefs <- coef(model)
reduced_coefs <- coef(reduced_model)
common_vars <- intersect(names(full_coefs), names(reduced_coefs))

for (var in common_vars) {
  sign_change <- sign(full_coefs[[var]]) != sign(reduced_coefs[[var]])
  if (sign_change) {
    cat(paste0("- ", var, ": Sign changed (Full = ", round(full_coefs[[var]], 3),
               ", Reduced = ", round(reduced_coefs[[var]], 3), ")\n"))
  }
}

# SECTION 11: Endogeneity Detection (Updated, No IV Analysis)
# 11.1 Correlation Check: Residuals vs Predictors
cat("\n=== 11.1 Correlation Matrix: Residuals vs Predictors ===\n")
model_ols <- lm(exam_score ~ ., data = final_df)
final_df$resid <- residuals(model_ols)
resid_corr_matrix <- cor(final_df[, c("resid", setdiff(names(final_df), c("exam_score", "resid")))])
print(round(resid_corr_matrix["resid", , drop = FALSE], 3))

# 11.2 Durbin-Wu-Hausman Test (Manual implementation)
cat("\n=== 11.2 Durbin-Wu-Hausman Test for Endogeneity (Manual) ===\n")

# First stage: regress potentially endogenous variable (e.g., family_incomeLow) on other predictors
first_stage_dwh <- lm(family_incomeLow ~ 
                        hours_studied + attendance + sleep_hours + previous_scores + tutoring_sessions +
                        parental_involvementLow + parental_involvementMedium +
                        access_to_resourcesLow + access_to_resourcesMedium +
                        extracurricular_activitiesYes + motivation_levelLow + motivation_levelMedium +
                        internet_accessYes + teacher_qualityLow + teacher_qualityMedium + 
                        school_typePublic + peer_influenceNeutral + peer_influencePositive + 
                        learning_disabilitiesYes + `parental_education_levelHigh School` +
                        parental_education_levelPostgraduate + distance_from_homeModerate + 
                        distance_from_homeNear + genderMale, 
                      data = final_df)

# Save residuals from first stage
final_df$resid_first_stage <- residuals(first_stage_dwh)

# Second stage: original model + residuals as additional regressor (Control Function Approach)
correct_cf_model <- lm(exam_score ~ hours_studied + attendance + sleep_hours + previous_scores + 
                         tutoring_sessions + parental_involvementLow + parental_involvementMedium +
                         access_to_resourcesLow + access_to_resourcesMedium + extracurricular_activitiesYes +
                         motivation_levelLow + motivation_levelMedium + internet_accessYes +
                         teacher_qualityLow + teacher_qualityMedium + school_typePublic +
                         peer_influenceNeutral + peer_influencePositive + learning_disabilitiesYes +
                         `parental_education_levelHigh School` + parental_education_levelPostgraduate +
                         distance_from_homeModerate + distance_from_homeNear + genderMale + 
                         resid_first_stage,
                       data = final_df)

# If the coefficient for resid_first_stage is significant → endogeneity is detected
summary(correct_cf_model)

# 12. Hypothesis Testing
# 12.1 First-Level: Full model with all predictors
model_level_1 <- lm(exam_score ~ parental_involvement + access_to_resources + extracurricular_activities +
                      motivation_level + internet_access + family_income + teacher_quality + school_type +
                      peer_influence + learning_disabilities + parental_education_level +
                      distance_from_home + gender + hours_studied + attendance + sleep_hours +
                      previous_scores + tutoring_sessions + physical_activity, data = df_clean)
cat("\n=== 12.1 First-Level OLS Regression (Full Model) ===\n")
summary(model_level_1)

# 12.2 Second-Level: Refactored Hypothesis Testing
# Center numeric predictors
df_clean <- df_clean %>%
  mutate(
    c_hours_studied = hours_studied - mean(hours_studied),
    c_tutoring_sessions = tutoring_sessions - mean(tutoring_sessions)
  )

# Run second-level OLS regression
model_updated <- lm(
  exam_score ~ c_hours_studied + c_tutoring_sessions + family_income + extracurricular_activities +
    gender + learning_disabilities + distance_from_home + parental_education_level + 
    school_type + teacher_quality + peer_influence +
    access_to_resources + internet_access + parental_involvement,
  data = df_clean
)

# Regression output
cat("\n=== Updated OLS Regression (Key Regressors: Study + Tutoring + Income + Extracurriculars) ===\n")
summary(model_updated)

# With interaction effect considered
interaction_candidates <- c("parental_education_level", "access_to_resources", "parental_involvement", 
                            "motivation_level", "learning_disabilities", "extracurricular_activities")

for (var in interaction_candidates) {
  interaction_formula <- lm(exam_score ~ family_income * parental_education_level + 
                              hours_studied + tutoring_sessions + gender + distance_from_home + 
                              school_type + teacher_quality + peer_influence + internet_access, 
                            data = df_clean)
  
  
  cat("\n=== Model: family_income *", var, "===\n")
  model <- lm(interaction_formula, data = df_clean)
  print(summary(model))
}

# Compute average exam scores for each combination
df_summary <- df_clean %>%
  group_by(family_income, access_to_resources) %>%
  summarise(mean_exam_score = mean(exam_score, na.rm = TRUE), .groups = "drop")

# Visualization
for (var in interaction_candidates) {
  p <- ggplot(df_clean, aes_string(x = "family_income", y = "exam_score", color = var)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", aes_string(group = var), se = FALSE) +
    labs(title = paste("Interaction Effect: Family Income ×", var),
         x = "Family Income",
         y = "Exam Score",
         color = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

