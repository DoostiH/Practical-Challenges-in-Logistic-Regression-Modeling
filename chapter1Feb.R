################################################################################
#                                                                              #
#  Chapter 1: Review of Logistic Regression Fundamentals                       #
#  Supplementary R Code for Figures and Analysis Output                        #
#                                                                              #
#  Book: Practical Challenges in Logistic Regression Modeling                  #
#  Author: Hassan Doosti                                                       #
#                                                                              #
#  This script generates all figures and analysis output used in Chapter 1.    #
#  Figures are saved as JPEG files (300 DPI, B&W) for inclusion in LaTeX.     #
#  Analysis output is saved to text files for reference.                       #
#                                                                              #
#  Data: Heart.csv from "An Introduction to Statistical Learning"              #
#        by James, Witten, Hastie, and Tibshirani                              #
#                                                                              #
################################################################################
# =============================================================================
# Setup and Required Packages
# =============================================================================
# Install required packages if not already installed
required_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "pROC",
  "scales",
  "gridExtra",
  "RColorBrewer",
  "car",
  "pscl",
  "ResourceSelection"
)
# Check and install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(pROC)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(car)
library(pscl)
library(ResourceSelection)
# Set seed for reproducibility
set.seed(123)
# Create output directory for figures if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}
# Create output directory for analysis results
if (!dir.exists("output")) {
  dir.create("output")
}
# Define a consistent B&W theme for all figures
theme_chapter1 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size, color = "grey40"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      panel.grid.minor = element_blank()
    )
}
# =============================================================================
# PART 1: SIMULATED DATA ANALYSIS
# =============================================================================
cat("\n")
cat("================================================================\n")
cat("PART 1: SIMULATED DATA ANALYSIS\n")
cat("================================================================\n\n")
# -----------------------------------------------------------------------------
# Generate Simulated Data
# -----------------------------------------------------------------------------
cat("Generating simulated data...\n\n")
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rbinom(n, 1, 0.5)  # Binary predictor
# True coefficients
beta0 <- -1
beta1 <- 0.8
beta2 <- -0.5
beta3 <- 1.5
# Create linear predictor and generate response
linear_pred <- beta0 + beta1*x1 + beta2*x2 + beta3*x3
p <- plogis(linear_pred)
y <- rbinom(n, 1, p)
# Create data frame
sim_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
# -----------------------------------------------------------------------------
# Fit Logistic Regression Model
# -----------------------------------------------------------------------------
model <- glm(y ~ x1 + x2 + x3, family = binomial(link = "logit"), data = sim_data)
# Save and display model summary
sink("output/simulated_model_summary.txt")
cat("LOGISTIC REGRESSION MODEL - SIMULATED DATA\n")
cat("==========================================\n\n")
cat("True coefficients used in data generation:\n")
cat("  Intercept (beta0):", beta0, "\n")
cat("  x1 (beta1):       ", beta1, "\n")
cat("  x2 (beta2):       ", beta2, "\n")
cat("  x3 (beta3):       ", beta3, "\n\n")
cat("Model Summary:\n")
cat("--------------\n")
print(summary(model))
sink()
cat("Model fitted. Summary saved to output/simulated_model_summary.txt\n")
# Display key results
cat("\nEstimated Coefficients:\n")
print(round(coef(model), 4))
cat("\nTrue vs Estimated Coefficients:\n")
comparison <- data.frame(
  Parameter = c("Intercept", "x1", "x2", "x3"),
  True = c(beta0, beta1, beta2, beta3),
  Estimated = round(coef(model), 4)
)
print(comparison, row.names = FALSE)
# -----------------------------------------------------------------------------
# Odds Ratios with Confidence Intervals
# -----------------------------------------------------------------------------
cat("\n\nOdds Ratios with 95% Confidence Intervals:\n")
or_table <- exp(cbind(
  OR = coef(model),
  confint.default(model)
))
colnames(or_table) <- c("OR", "2.5%", "97.5%")
print(round(or_table, 4))
# Save OR table
sink("output/simulated_odds_ratios.txt")
cat("ODDS RATIOS WITH 95% CONFIDENCE INTERVALS\n")
cat("==========================================\n\n")
print(round(or_table, 4))
cat("\nInterpretation:\n")
cat("- x1: For each 1-unit increase in x1, the odds of Y=1 increase by a factor of",
    round(exp(coef(model)["x1"]), 2), "\n")
cat("- x2: For each 1-unit increase in x2, the odds of Y=1 decrease by a factor of",
    round(exp(coef(model)["x2"]), 2), "\n")
cat("- x3: The odds of Y=1 are", round(exp(coef(model)["x3"]), 2),
    "times higher when x3=1 compared to x3=0\n")
sink()
# -----------------------------------------------------------------------------
# Model Fit Statistics
# -----------------------------------------------------------------------------
cat("\n\nModel Fit Statistics:\n")
cat("Null deviance:    ", round(model$null.deviance, 2), "on", model$df.null, "df\n")
cat("Residual deviance:", round(model$deviance, 2), "on", model$df.residual, "df\n")
cat("AIC:              ", round(AIC(model), 2), "\n")
cat("BIC:              ", round(BIC(model), 2), "\n")
# McFadden's R-squared
mcfadden_r2 <- 1 - (model$deviance / model$null.deviance)
cat("McFadden's R²:    ", round(mcfadden_r2, 4), "\n")
# Likelihood ratio test
lr_stat <- model$null.deviance - model$deviance
lr_df <- model$df.null - model$df.residual
lr_pval <- pchisq(lr_stat, lr_df, lower.tail = FALSE)
cat("\nLikelihood Ratio Test:\n")
cat("Chi-square:", round(lr_stat, 2), ", df =", lr_df, ", p-value =",
    format.pval(lr_pval, digits = 3), "\n")
# -----------------------------------------------------------------------------
# Classification Performance
# -----------------------------------------------------------------------------
pred_prob <- predict(model, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
# Confusion matrix
conf_matrix <- table(Actual = sim_data$y, Predicted = pred_class)
cat("\n\nConfusion Matrix (threshold = 0.5):\n")
print(conf_matrix)
# Performance metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[2,])
specificity <- conf_matrix[1,1] / sum(conf_matrix[1,])
precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
cat("\nClassification Metrics:\n")
cat("Accuracy:   ", round(accuracy, 4), "\n")
cat("Sensitivity:", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:  ", round(precision, 4), "\n")
cat("F1 Score:   ", round(f1_score, 4), "\n")
# ROC and AUC
roc_obj <- roc(sim_data$y, pred_prob, quiet = TRUE)
auc_value <- auc(roc_obj)
cat("AUC:        ", round(auc_value, 4), "\n")
# Optimal threshold
optimal <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
cat("\nOptimal Threshold (Youden's J):\n")
cat("Threshold:  ", round(optimal$threshold, 4), "\n")
cat("Sensitivity:", round(optimal$sensitivity, 4), "\n")
cat("Specificity:", round(optimal$specificity, 4), "\n")
# Save classification results
sink("output/simulated_classification.txt")
cat("CLASSIFICATION PERFORMANCE - SIMULATED DATA\n")
cat("============================================\n\n")
cat("Confusion Matrix (threshold = 0.5):\n")
print(conf_matrix)
cat("\nMetrics:\n")
cat("Accuracy:   ", round(accuracy, 4), "\n")
cat("Sensitivity:", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:  ", round(precision, 4), "\n")
cat("F1 Score:   ", round(f1_score, 4), "\n")
cat("AUC:        ", round(auc_value, 4), "\n")
cat("\nOptimal Threshold (Youden's J):\n")
print(optimal)
sink()
# =============================================================================
# PART 2: HEART DISEASE DATA ANALYSIS
# =============================================================================
cat("\n\n")
cat("================================================================\n")
cat("PART 2: HEART DISEASE DATA ANALYSIS\n")
cat("================================================================\n\n")
# -----------------------------------------------------------------------------
# Load and Prepare Heart Data
# -----------------------------------------------------------------------------
# Load data from CSV (place Heart.csv in working directory or specify path)
# Data source: "An Introduction to Statistical Learning" by James et al.
Heart <- read.csv("Heart.csv", row.names = 1)
cat("Heart Disease Dataset\n")
cat("Source: An Introduction to Statistical Learning (James et al.)\n\n")
# Data structure
cat("Dataset Structure:\n")
cat("Observations:", nrow(Heart), "\n")
cat("Variables:   ", ncol(Heart), "\n\n")
# Variable descriptions
cat("Variable Descriptions:\n")
cat("  Age      - Age in years\n")
cat("  Sex      - Sex (1 = male, 0 = female)\n")
cat("  ChestPain - Chest pain type (typical, asymptomatic, nonanginal, nontypical)\n")
cat("  RestBP   - Resting blood pressure (mm Hg)\n")
cat("  Chol     - Serum cholesterol (mg/dl)\n")
cat("  Fbs      - Fasting blood sugar > 120 mg/dl (1 = true, 0 = false)\n")
cat("  RestECG  - Resting ECG results (0, 1, 2)\n")
cat("  MaxHR    - Maximum heart rate achieved\n")
cat("  ExAng    - Exercise induced angina (1 = yes, 0 = no)\n")
cat("  Oldpeak  - ST depression induced by exercise\n")
cat("  Slope    - Slope of peak exercise ST segment\n")
cat("  Ca       - Number of major vessels colored by fluoroscopy (0-3)\n")
cat("  Thal     - Thalium stress test result (fixed, normal, reversable)\n")
cat("  AHD      - Diagnosis of heart disease (Yes/No) - OUTCOME\n\n")
# Check for missing values
missing_count <- colSums(is.na(Heart))
cat("Missing Values:\n")
print(missing_count[missing_count > 0])
cat("Total complete cases:", sum(complete.cases(Heart)), "of", nrow(Heart), "\n\n")
# Remove missing values
Heart_complete <- na.omit(Heart)
# Convert Sex to factor with labels for better interpretation
Heart_complete$Sex <- factor(Heart_complete$Sex, levels = c(0, 1),
                             labels = c("Female", "Male"))
# Convert AHD to factor (required for glm with binomial family)
Heart_complete$AHD <- factor(Heart_complete$AHD, levels = c("No", "Yes"))
# Convert ChestPain to factor
Heart_complete$ChestPain <- factor(Heart_complete$ChestPain)
# Summary statistics
cat("Summary Statistics (Complete Cases):\n")
print(summary(Heart_complete[, c("Age", "Sex", "ChestPain", "MaxHR", "Oldpeak", "AHD")]))
# Save data summary
sink("output/heart_data_summary.txt")
cat("HEART DISEASE DATASET SUMMARY\n")
cat("=============================\n\n")
cat("Source: An Introduction to Statistical Learning (James et al.)\n\n")
cat("Dimensions:", nrow(Heart), "observations,", ncol(Heart), "variables\n")
cat("Complete cases:", nrow(Heart_complete), "\n\n")
cat("Missing Values:\n")
print(missing_count[missing_count > 0])
cat("\nSummary Statistics:\n")
print(summary(Heart_complete))
sink()
# -----------------------------------------------------------------------------
# Fit Heart Disease Model
# -----------------------------------------------------------------------------
cat("\nFitting Logistic Regression Model...\n\n")
heart_model <- glm(AHD ~ Age + Sex + ChestPain + MaxHR + Oldpeak,
                   family = binomial(link = "logit"),
                   data = Heart_complete)
# Model summary
cat("MODEL SUMMARY\n")
cat("=============\n\n")
print(summary(heart_model))
# Save detailed model output
sink("output/heart_model_summary.txt")
cat("LOGISTIC REGRESSION MODEL - HEART DISEASE DATA\n")
cat("===============================================\n\n")
cat("Model Formula: AHD ~ Age + Sex + ChestPain + MaxHR + Oldpeak\n\n")
print(summary(heart_model))
sink()
# -----------------------------------------------------------------------------
# Variance Inflation Factors (Multicollinearity Check)
# -----------------------------------------------------------------------------
cat("\n\nVariance Inflation Factors (VIF):\n")
vif_values <- vif(heart_model)
print(round(vif_values, 3))
cat("\nInterpretation: VIF < 5 indicates no serious multicollinearity issues.\n")
# -----------------------------------------------------------------------------
# Odds Ratios for Heart Model
# -----------------------------------------------------------------------------
cat("\n\nOdds Ratios with 95% Confidence Intervals:\n")
heart_or <- exp(cbind(OR = coef(heart_model), confint(heart_model)))
print(round(heart_or, 4))
# Create formatted OR table for the book
sink("output/heart_odds_ratios.txt")
cat("ODDS RATIOS WITH 95% CONFIDENCE INTERVALS\n")
cat("==========================================\n\n")
cat("Model: AHD ~ Age + Sex + ChestPain + MaxHR + Oldpeak\n\n")
print(round(heart_or, 4))
cat("\n\nCLINICAL INTERPRETATION:\n")
cat("========================\n\n")
cat("Age (per 1-year increase):\n")
cat("  OR =", round(heart_or["Age", "OR"], 3),
    "(95% CI:", round(heart_or["Age", "2.5 %"], 3), "-",
    round(heart_or["Age", "97.5 %"], 3), ")\n")
cat("  Each additional year of age increases the odds of heart disease by",
    round((heart_or["Age", "OR"] - 1) * 100, 1), "%\n\n")
cat("Sex (Male vs Female):\n")
cat("  OR =", round(heart_or["SexMale", "OR"], 3),
    "(95% CI:", round(heart_or["SexMale", "2.5 %"], 3), "-",
    round(heart_or["SexMale", "97.5 %"], 3), ")\n")
cat("  Males have", round(heart_or["SexMale", "OR"], 1),
    "times higher odds of heart disease compared to females\n\n")
cat("MaxHR (per 1-unit increase in maximum heart rate):\n")
cat("  OR =", round(heart_or["MaxHR", "OR"], 3),
    "(95% CI:", round(heart_or["MaxHR", "2.5 %"], 3), "-",
    round(heart_or["MaxHR", "97.5 %"], 3), ")\n")
cat("  Higher exercise capacity is protective; each 10-bpm increase in MaxHR\n")
cat("  reduces the odds by approximately",
    round((1 - heart_or["MaxHR", "OR"]^10) * 100, 1), "%\n\n")
cat("Oldpeak (per 1-unit increase in ST depression):\n")
cat("  OR =", round(heart_or["Oldpeak", "OR"], 3),
    "(95% CI:", round(heart_or["Oldpeak", "2.5 %"], 3), "-",
    round(heart_or["Oldpeak", "97.5 %"], 3), ")\n")
cat("  Each 1-unit increase in ST depression increases the odds by",
    round((heart_or["Oldpeak", "OR"] - 1) * 100, 1), "%\n\n")
cat("Chest Pain Type (Reference: asymptomatic):\n")
cat("  Patients with asymptomatic presentation have the HIGHEST risk\n")
cat("  All other chest pain types have lower odds relative to asymptomatic:\n")
cat("    - nonanginal:  OR =", round(heart_or["ChestPainnonanginal", "OR"], 3), "\n")
cat("    - nontypical:  OR =", round(heart_or["ChestPainnontypical", "OR"], 3), "\n")
cat("    - typical:     OR =", round(heart_or["ChestPaintypical", "OR"], 3), "\n")
sink()
cat("\nDetailed interpretation saved to output/heart_odds_ratios.txt\n")
# -----------------------------------------------------------------------------
# Model Fit Statistics for Heart Model
# -----------------------------------------------------------------------------
cat("\n\nModel Fit Statistics:\n")
cat("Null deviance:    ", round(heart_model$null.deviance, 2),
    "on", heart_model$df.null, "df\n")
cat("Residual deviance:", round(heart_model$deviance, 2),
    "on", heart_model$df.residual, "df\n")
cat("AIC:              ", round(AIC(heart_model), 2), "\n")
# McFadden's R-squared
heart_mcfadden <- 1 - (heart_model$deviance / heart_model$null.deviance)
cat("McFadden's R²:    ", round(heart_mcfadden, 4), "\n")
# Hosmer-Lemeshow Test
hl_test <- hoslem.test(as.numeric(Heart_complete$AHD == "Yes"),
                       fitted(heart_model), g = 10)
cat("\nHosmer-Lemeshow Test:\n")
cat("Chi-square:", round(hl_test$statistic, 3), ", df =", hl_test$parameter,
    ", p-value =", round(hl_test$p.value, 4), "\n")
# -----------------------------------------------------------------------------
# Classification Performance for Heart Model
# -----------------------------------------------------------------------------
heart_pred_prob <- predict(heart_model, type = "response")
heart_pred_class <- ifelse(heart_pred_prob > 0.5, "Yes", "No")
# Confusion matrix
heart_conf <- table(Actual = Heart_complete$AHD, Predicted = heart_pred_class)
cat("\n\nConfusion Matrix (threshold = 0.5):\n")
print(heart_conf)
# Metrics
heart_accuracy <- sum(diag(heart_conf)) / sum(heart_conf)
heart_sensitivity <- heart_conf["Yes", "Yes"] / sum(heart_conf["Yes", ])
heart_specificity <- heart_conf["No", "No"] / sum(heart_conf["No", ])
cat("\nClassification Metrics:\n")
cat("Accuracy:   ", round(heart_accuracy, 4), "\n")
cat("Sensitivity:", round(heart_sensitivity, 4), "\n")
cat("Specificity:", round(heart_specificity, 4), "\n")
# ROC and AUC
heart_roc <- roc(Heart_complete$AHD, heart_pred_prob, quiet = TRUE)
heart_auc <- auc(heart_roc)
cat("AUC:        ", round(heart_auc, 4), "\n")
# Save Heart classification results
sink("output/heart_classification.txt")
cat("CLASSIFICATION PERFORMANCE - HEART DISEASE MODEL\n")
cat("=================================================\n\n")
cat("Confusion Matrix (threshold = 0.5):\n")
print(heart_conf)
cat("\nMetrics:\n")
cat("Accuracy:   ", round(heart_accuracy, 4), "\n")
cat("Sensitivity:", round(heart_sensitivity, 4), "\n")
cat("Specificity:", round(heart_specificity, 4), "\n")
cat("AUC:        ", round(heart_auc, 4), "\n")
cat("\nHosmer-Lemeshow Test:\n")
cat("Chi-square:", round(hl_test$statistic, 3), ", df =", hl_test$parameter,
    ", p-value =", round(hl_test$p.value, 4), "\n")
cat("\nInterpretation: A non-significant p-value suggests adequate model fit.\n")
sink()
# =============================================================================
# PART 3: GENERATE ALL FIGURES
# =============================================================================
cat("\n\n")
cat("================================================================\n")
cat("PART 3: GENERATING FIGURES\n")
cat("================================================================\n\n")
# -----------------------------------------------------------------------------
# Figure 1.1: The Logistic (Sigmoid) Function
# -----------------------------------------------------------------------------
cat("Generating Figure 1.1: The Logistic Function...\n")
lp_seq <- seq(-6, 6, length.out = 300)
prob_seq <- plogis(lp_seq)
df_logistic <- data.frame(
  linear_predictor = lp_seq,
  probability = prob_seq
)
annotations_df <- data.frame(
  x = c(0, -2.5, 2.5),
  y = c(0.5, plogis(-2.5), plogis(2.5)),
  label = c("P = 0.5",
            paste0("P = ", round(plogis(-2.5), 2)),
            paste0("P = ", round(plogis(2.5), 2)))
)
fig_1_1 <- ggplot(df_logistic, aes(x = linear_predictor, y = probability)) +
  geom_line(linewidth = 1.3, colour = "black") +
  geom_hline(yintercept = c(0, 0.5, 1), linetype = "dashed",
             alpha = 0.4, colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4, colour = "grey50") +
  geom_point(data = annotations_df, aes(x = x, y = y),
             size = 3, colour = "black", shape = 17) +
  geom_text(data = annotations_df, aes(x = x, y = y, label = label),
            vjust = -1, hjust = 0.5, size = 3.5, colour = "black") +
  labs(
    x = expression("Linear Predictor (" * eta * " = " * beta[0] + beta[1]*x[1] + ldots + beta[p]*x[p] * ")"),
    y = expression("Probability " * pi),
    title = "The Logistic (Sigmoid) Function",
    subtitle = expression(pi * " = " * frac(e^eta, 1 + e^eta) * " maps any real value to (0, 1)")
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  theme_chapter1()
ggsave("figures/fig_1_1_logistic_curve.jpeg", fig_1_1, width = 8, height = 5.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.2: Non-linear Effect on Probability Scale
# -----------------------------------------------------------------------------
cat("Generating Figure 1.2: Non-linear Effect on Probability Scale...\n")
beta_demo <- 0.8
baselines <- data.frame(
  scenario = c("Low baseline", "Medium baseline", "High baseline"),
  baseline_lp = c(-2.5, 0, 2.5)
) %>%
  mutate(
    baseline_prob = plogis(baseline_lp),
    new_lp = baseline_lp + beta_demo,
    new_prob = plogis(new_lp),
    prob_change = new_prob - baseline_prob
  )
curve_df <- data.frame(
  lp = seq(-5, 5, length.out = 200),
  prob = plogis(seq(-5, 5, length.out = 200))
)
fig_1_2 <- ggplot() +
  geom_line(data = curve_df, aes(x = lp, y = prob),
            linewidth = 1, colour = "grey60") +
  geom_segment(data = baselines,
               aes(x = baseline_lp, xend = new_lp,
                   y = baseline_prob, yend = baseline_prob),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "black", linewidth = 0.8) +
  geom_segment(data = baselines,
               aes(x = new_lp, xend = new_lp,
                   y = baseline_prob, yend = new_prob),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               colour = "black", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = baselines, aes(x = baseline_lp, y = baseline_prob),
             size = 3, colour = "black", shape = 16) +
  geom_point(data = baselines, aes(x = new_lp, y = new_prob),
             size = 3, colour = "black", shape = 17) +
  geom_label(data = baselines,
             aes(x = new_lp + 0.3,
                 y = (baseline_prob + new_prob)/2,
                 label = paste0("\u0394P = ", sprintf("%.3f", prob_change))),
             hjust = 0, size = 3, fill = "white", label.size = 0.3) +
  geom_text(data = baselines,
            aes(x = baseline_lp, y = baseline_prob - 0.08,
                label = paste0("P = ", sprintf("%.2f", baseline_prob))),
            size = 3, colour = "black") +
  labs(
    x = "Linear Predictor",
    y = "Probability",
    title = "Same Coefficient, Different Probability Changes",
    subtitle = expression("Effect of one-unit increase (" * beta * " = 0.8) at different baselines")
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-4, 4, 2)) +
  theme_chapter1() +
  annotate("segment", x = -4.5, xend = -3.5, y = 0.95, yend = 0.95,
           colour = "black", linewidth = 0.8,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text", x = -3.3, y = 0.95, label = "Change in LP",
           hjust = 0, size = 3, colour = "black") +
  annotate("segment", x = -4.5, xend = -3.5, y = 0.88, yend = 0.88,
           colour = "black", linewidth = 0.8, linetype = "dashed",
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text", x = -3.3, y = 0.88, label = "Change in Probability",
           hjust = 0, size = 3, colour = "black")
ggsave("figures/fig_1_2_nonlinear_effect.jpeg", fig_1_2, width = 9, height = 6,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.3: Odds Ratio Interpretation
# -----------------------------------------------------------------------------
cat("Generating Figure 1.3: Odds Ratio Interpretation...\n")
or_values <- c(0.25, 0.5, 1, 2, 4)
baseline_intercept <- 0
df_or <- expand.grid(
  OR = or_values,
  x = seq(0, 1, length.out = 50)
) %>%
  mutate(
    beta = log(OR),
    lp = baseline_intercept + beta * x,
    prob = plogis(lp),
    OR_label = factor(paste0("OR = ", OR),
                      levels = paste0("OR = ", or_values))
  )
df_endpoints <- df_or %>%
  filter(x == 1) %>%
  mutate(prob_label = sprintf("%.2f", prob))
fig_1_3 <- ggplot(df_or, aes(x = x, y = prob, linetype = OR_label)) +
  geom_line(linewidth = 1.0, colour = "black") +
  geom_point(data = df_or %>% filter(x %in% c(0, 1)),
             aes(shape = OR_label), size = 3, colour = "black") +
  geom_text(data = df_endpoints,
            aes(x = 1.05, y = prob, label = prob_label),
            hjust = 0, size = 3, show.legend = FALSE) +
  scale_linetype_manual(name = "Odds Ratio",
                        values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_shape_manual(name = "Odds Ratio",
                     values = c(16, 17, 15, 3, 4)) +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("0 (Reference)", "1 (Exposed)"),
                     limits = c(0, 1.15)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x = "Binary Predictor (e.g., Treatment Group)",
    y = "Probability of Outcome",
    title = "How Odds Ratios Affect Predicted Probabilities",
    subtitle = "Starting from P = 0.50 in reference group (x = 0)"
  ) +
  theme_chapter1() +
  theme(legend.position = "right") +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey50")
ggsave("figures/fig_1_3_odds_ratios.jpeg", fig_1_3, width = 9, height = 5.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.4: ROC Curve with Annotations
# -----------------------------------------------------------------------------
cat("Generating Figure 1.4: ROC Curve...\n")
roc_df <- data.frame(
  sensitivity = roc_obj$sensitivities,
  specificity = roc_obj$specificities,
  threshold = roc_obj$thresholds
) %>%
  mutate(fpr = 1 - specificity)
fig_1_4 <- ggplot() +
  geom_line(data = roc_df, aes(x = fpr, y = sensitivity),
            linewidth = 1.3, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "grey50", linewidth = 0.8) +
  geom_point(aes(x = 1 - optimal$specificity, y = optimal$sensitivity),
             colour = "black", size = 4, shape = 18) +
  annotate("text", x = 0.7, y = 0.25,
           label = paste0("AUC = ", sprintf("%.3f", auc_value)),
           size = 5, fontface = "bold", colour = "black") +
  annotate("label", x = 1 - optimal$specificity + 0.02,
           y = optimal$sensitivity - 0.08,
           label = paste0("Optimal (Youden's J)\nThreshold = ",
                          sprintf("%.2f", optimal$threshold),
                          "\nSens = ", sprintf("%.2f", optimal$sensitivity),
                          "\nSpec = ", sprintf("%.2f", optimal$specificity)),
           hjust = 0, size = 2.8, fill = "white") +
  annotate("text", x = 0.55, y = 0.45, label = "Random classifier\n(AUC = 0.5)",
           size = 3, colour = "grey50", angle = 45) +
  labs(
    x = "False Positive Rate (1 \u2212 Specificity)",
    y = "True Positive Rate (Sensitivity)",
    title = "ROC Curve for Logistic Regression Model",
    subtitle = "Simulated data example"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_equal() +
  theme_chapter1()
ggsave("figures/fig_1_4_roc_curve.jpeg", fig_1_4, width = 7, height = 6.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.5: Calibration Plot
# -----------------------------------------------------------------------------
cat("Generating Figure 1.5: Calibration Plot...\n")
cal_data <- data.frame(
  pred = pred_prob,
  obs = sim_data$y
) %>%
  mutate(decile = ntile(pred, 10)) %>%
  group_by(decile) %>%
  summarise(
    mean_pred = mean(pred),
    mean_obs = mean(obs),
    n = n(),
    se = sqrt(mean_obs * (1 - mean_obs) / n),
    lower = pmax(0, mean_obs - 1.96 * se),
    upper = pmin(1, mean_obs + 1.96 * se),
    .groups = "drop"
  )
loess_fit <- loess(mean_obs ~ mean_pred, data = cal_data, span = 1)
loess_pred <- data.frame(
  mean_pred = seq(min(cal_data$mean_pred), max(cal_data$mean_pred), length.out = 100)
)
loess_pred$mean_obs <- predict(loess_fit, loess_pred)
fig_1_5 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "grey50", linewidth = 0.8) +
  geom_errorbar(data = cal_data,
                aes(x = mean_pred, ymin = lower, ymax = upper),
                width = 0.02, colour = "grey40", alpha = 0.7) +
  geom_point(data = cal_data, aes(x = mean_pred, y = mean_obs),
             size = 4, colour = "black", shape = 16) +
  geom_line(data = loess_pred, aes(x = mean_pred, y = mean_obs),
            colour = "black", linewidth = 1, linetype = "longdash") +
  annotate("text", x = 0.15, y = 0.85,
           label = "Perfect calibration",
           angle = 45, colour = "grey50", size = 3) +
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Proportion of Events",
    title = "Calibration Plot",
    subtitle = "Comparing predicted probabilities to observed frequencies by decile"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_equal() +
  theme_chapter1()
ggsave("figures/fig_1_5_calibration.jpeg", fig_1_5, width = 7, height = 6.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.6: Predicted Probabilities with Confidence Bands
# -----------------------------------------------------------------------------
cat("Generating Figure 1.6: Predicted Probabilities with CI...\n")
x1_range <- seq(-3, 3, length.out = 100)
newdata <- expand.grid(
  x1 = x1_range,
  x2 = 0,
  x3 = c(0, 1)
)
preds <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
newdata$prob <- plogis(preds$fit)
newdata$lower <- plogis(preds$fit - 1.96 * preds$se.fit)
newdata$upper <- plogis(preds$fit + 1.96 * preds$se.fit)
newdata$x3_label <- factor(newdata$x3, levels = c(0, 1),
                           labels = c("x3 = 0", "x3 = 1"))
fig_1_6 <- ggplot(newdata, aes(x = x1, y = prob, linetype = x3_label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = x3_label), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_rug(data = sim_data, aes(x = x1, y = NULL, linetype = NULL),
           alpha = 0.1, sides = "b") +
  scale_linetype_manual(name = "Group", values = c("solid", "dashed")) +
  scale_fill_manual(name = "Group", values = c("grey70", "grey40")) +
  labs(
    x = expression("Predictor " * x[1]),
    y = "Predicted Probability of Y = 1",
    title = expression("Effect of " * x[1] * " on Predicted Probability"),
    subtitle = expression("Stratified by " * x[3] * ", with " * x[2] * " = 0; shaded = 95% CI")
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  theme_chapter1() +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", colour = "grey80"))
ggsave("figures/fig_1_6_predicted_probs.jpeg", fig_1_6, width = 8, height = 6,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.7: Marginal Effects Visualization
# -----------------------------------------------------------------------------
cat("Generating Figure 1.7: Marginal Effects...\n")
x1_vals <- seq(-3, 3, length.out = 100)
me_data <- data.frame(x1 = x1_vals) %>%
  rowwise() %>%
  mutate(
    prob_x3_0 = plogis(coef(model)[1] + coef(model)[2]*x1 +
                         coef(model)[3]*mean(sim_data$x2) + coef(model)[4]*0),
    prob_x3_1 = plogis(coef(model)[1] + coef(model)[2]*x1 +
                         coef(model)[3]*mean(sim_data$x2) + coef(model)[4]*1),
    me_x1_x3_0 = coef(model)[2] * prob_x3_0 * (1 - prob_x3_0),
    me_x1_x3_1 = coef(model)[2] * prob_x3_1 * (1 - prob_x3_1)
  ) %>%
  ungroup()
me_long <- me_data %>%
  select(x1, me_x1_x3_0, me_x1_x3_1) %>%
  pivot_longer(cols = c(me_x1_x3_0, me_x1_x3_1),
               names_to = "group", values_to = "marginal_effect") %>%
  mutate(group = ifelse(group == "me_x1_x3_0", "x3 = 0", "x3 = 1"))
ame_x1 <- mean(coef(model)[2] * pred_prob * (1 - pred_prob))
fig_1_7 <- ggplot(me_long, aes(x = x1, y = marginal_effect, linetype = group)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_hline(yintercept = ame_x1, linetype = "dotted", colour = "grey40") +
  annotate("text", x = 2.5, y = ame_x1 + 0.01,
           label = paste0("AME = ", sprintf("%.3f", ame_x1)),
           size = 3.5, colour = "grey40") +
  scale_linetype_manual(name = "Group", values = c("solid", "dashed")) +
  labs(
    x = expression("Value of " * x[1]),
    y = expression("Marginal Effect of " * x[1] * " on P(Y=1)"),
    title = expression("Marginal Effect of " * x[1] * " Across Its Range"),
    subtitle = "Dotted line shows average marginal effect (AME)"
  ) +
  theme_chapter1() +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", colour = "grey80"))
ggsave("figures/fig_1_7_marginal_effects.jpeg", fig_1_7, width = 8, height = 5.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.8: Confusion Matrix Heatmap
# -----------------------------------------------------------------------------
cat("Generating Figure 1.8: Confusion Matrix Heatmap...\n")
thresholds <- c(0.3, 0.5, 0.7)
create_cm_data <- function(threshold, actual, predicted_prob) {
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  cm <- table(Actual = actual, Predicted = predicted_class)
  if (ncol(cm) == 1) {
    if (colnames(cm) == "0") {
      cm <- cbind(cm, "1" = c(0, 0))
    } else {
      cm <- cbind("0" = c(0, 0), cm)
    }
  }
  data.frame(
    actual = rep(c("0 (Negative)", "1 (Positive)"), each = 2),
    predicted = rep(c("0 (Negative)", "1 (Positive)"), 2),
    count = as.vector(cm),
    threshold = paste0("Threshold = ", threshold)
  )
}
cm_data <- bind_rows(lapply(thresholds, create_cm_data,
                            actual = sim_data$y,
                            predicted_prob = pred_prob))
cm_data <- cm_data %>%
  mutate(
    label_type = case_when(
      actual == "1 (Positive)" & predicted == "1 (Positive)" ~ "TP",
      actual == "0 (Negative)" & predicted == "0 (Negative)" ~ "TN",
      actual == "1 (Positive)" & predicted == "0 (Negative)" ~ "FN",
      actual == "0 (Negative)" & predicted == "1 (Positive)" ~ "FP"
    ),
    cell_label = paste0(label_type, "\n", count)
  )
fig_1_8 <- ggplot(cm_data, aes(x = predicted, y = actual, fill = count)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = cell_label), size = 4, fontface = "bold") +
  facet_wrap(~threshold, ncol = 3) +
  scale_fill_gradient(low = "white", high = "grey30", name = "Count") +
  scale_y_discrete(limits = rev(c("0 (Negative)", "1 (Positive)"))) +
  labs(
    x = "Predicted Class",
    y = "Actual Class",
    title = "Confusion Matrices at Different Classification Thresholds",
    subtitle = "TP = True Positive, TN = True Negative, FP = False Positive, FN = False Negative"
  ) +
  theme_chapter1() +
  theme(panel.grid = element_blank()) +
  coord_equal()
ggsave("figures/fig_1_8_confusion_matrix.jpeg", fig_1_8, width = 11, height = 4.5,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.9: Heart Disease Example - Effect Plots
# -----------------------------------------------------------------------------
cat("Generating Figure 1.9: Heart Disease Effect Plots...\n")
# Age effect
age_range <- seq(min(Heart_complete$Age), max(Heart_complete$Age), length.out = 100)
age_newdata <- data.frame(
  Age = age_range,
  Sex = factor("Male", levels = levels(Heart_complete$Sex)),
  ChestPain = factor("asymptomatic", levels = levels(Heart_complete$ChestPain)),
  MaxHR = mean(Heart_complete$MaxHR),
  Oldpeak = mean(Heart_complete$Oldpeak)
)
age_preds <- predict(heart_model, newdata = age_newdata, type = "link", se.fit = TRUE)
age_newdata$prob <- plogis(age_preds$fit)
age_newdata$lower <- plogis(age_preds$fit - 1.96 * age_preds$se.fit)
age_newdata$upper <- plogis(age_preds$fit + 1.96 * age_preds$se.fit)
p_age <- ggplot(age_newdata, aes(x = Age, y = prob)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1.2) +
  geom_rug(data = Heart_complete, aes(x = Age, y = NULL), alpha = 0.1, sides = "b") +
  labs(x = "Age (years)", y = "P(Heart Disease)", title = "Effect of Age") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_chapter1(base_size = 10)
# MaxHR effect
maxhr_range <- seq(min(Heart_complete$MaxHR), max(Heart_complete$MaxHR), length.out = 100)
maxhr_newdata <- data.frame(
  Age = mean(Heart_complete$Age),
  Sex = factor("Male", levels = levels(Heart_complete$Sex)),
  ChestPain = factor("asymptomatic", levels = levels(Heart_complete$ChestPain)),
  MaxHR = maxhr_range,
  Oldpeak = mean(Heart_complete$Oldpeak)
)
maxhr_preds <- predict(heart_model, newdata = maxhr_newdata, type = "link", se.fit = TRUE)
maxhr_newdata$prob <- plogis(maxhr_preds$fit)
maxhr_newdata$lower <- plogis(maxhr_preds$fit - 1.96 * maxhr_preds$se.fit)
maxhr_newdata$upper <- plogis(maxhr_preds$fit + 1.96 * maxhr_preds$se.fit)
p_maxhr <- ggplot(maxhr_newdata, aes(x = MaxHR, y = prob)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1.2, linetype = "dashed") +
  geom_rug(data = Heart_complete, aes(x = MaxHR, y = NULL), alpha = 0.1, sides = "b") +
  labs(x = "Maximum Heart Rate (bpm)", y = "P(Heart Disease)",
       title = "Effect of Max Heart Rate") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_chapter1(base_size = 10)
# Sex effect
sex_newdata <- data.frame(
  Age = mean(Heart_complete$Age),
  Sex = factor(c("Female", "Male"), levels = levels(Heart_complete$Sex)),
  ChestPain = factor("asymptomatic", levels = levels(Heart_complete$ChestPain)),
  MaxHR = mean(Heart_complete$MaxHR),
  Oldpeak = mean(Heart_complete$Oldpeak)
)
sex_preds <- predict(heart_model, newdata = sex_newdata, type = "link", se.fit = TRUE)
sex_newdata$prob <- plogis(sex_preds$fit)
sex_newdata$lower <- plogis(sex_preds$fit - 1.96 * sex_preds$se.fit)
sex_newdata$upper <- plogis(sex_preds$fit + 1.96 * sex_preds$se.fit)
p_sex <- ggplot(sex_newdata, aes(x = Sex, y = prob)) +
  geom_col(width = 0.6, alpha = 0.8, fill = "grey60", colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Sex", y = "P(Heart Disease)", title = "Effect of Sex") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_chapter1(base_size = 10)
# Chest Pain type effect
cp_newdata <- data.frame(
  Age = mean(Heart_complete$Age),
  Sex = factor("Male", levels = levels(Heart_complete$Sex)),
  ChestPain = levels(Heart_complete$ChestPain),
  MaxHR = mean(Heart_complete$MaxHR),
  Oldpeak = mean(Heart_complete$Oldpeak)
)
cp_preds <- predict(heart_model, newdata = cp_newdata, type = "link", se.fit = TRUE)
cp_newdata$prob <- plogis(cp_preds$fit)
cp_newdata$lower <- plogis(cp_preds$fit - 1.96 * cp_preds$se.fit)
cp_newdata$upper <- plogis(cp_preds$fit + 1.96 * cp_preds$se.fit)
# Assign distinct grey fills for each chest pain type
cp_newdata$cp_ordered <- reorder(cp_newdata$ChestPain, cp_newdata$prob)
cp_fills <- c("grey85", "grey65", "grey45", "grey25")
p_cp <- ggplot(cp_newdata, aes(x = cp_ordered, y = prob, fill = cp_ordered)) +
  geom_col(width = 0.6, alpha = 0.9, colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_fill_manual(values = cp_fills) +
  labs(x = "Chest Pain Type", y = "P(Heart Disease)",
       title = "Effect of Chest Pain Type") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_chapter1(base_size = 10) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
fig_1_9 <- gridExtra::grid.arrange(
  p_age, p_maxhr, p_sex, p_cp,
  ncol = 2,
  top = grid::textGrob("Heart Disease Prediction: Effect Plots",
                       gp = grid::gpar(fontsize = 14, fontface = "bold"))
)
ggsave("figures/fig_1_9_heart_effects.jpeg", fig_1_9, width = 10, height = 8,
       dpi = 300, device = "jpeg")
# -----------------------------------------------------------------------------
# Figure 1.10: Heart Disease ROC Curve
# -----------------------------------------------------------------------------
cat("Generating Figure 1.10: Heart Disease ROC Curve...\n")
heart_roc_df <- data.frame(
  sensitivity = heart_roc$sensitivities,
  specificity = heart_roc$specificities
) %>%
  mutate(fpr = 1 - specificity)
heart_optimal <- coords(heart_roc, "best", ret = c("threshold", "sensitivity", "specificity"))
fig_1_10 <- ggplot() +
  geom_line(data = heart_roc_df, aes(x = fpr, y = sensitivity),
            linewidth = 1.3, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "grey50", linewidth = 0.8) +
  geom_point(aes(x = 1 - heart_optimal$specificity, y = heart_optimal$sensitivity),
             colour = "black", size = 4, shape = 18) +
  annotate("text", x = 0.7, y = 0.25,
           label = paste0("AUC = ", sprintf("%.3f", heart_auc)),
           size = 5, fontface = "bold", colour = "black") +
  labs(
    x = "False Positive Rate (1 \u2212 Specificity)",
    y = "True Positive Rate (Sensitivity)",
    title = "ROC Curve for Heart Disease Prediction Model",
    subtitle = "Data: Heart disease dataset (James et al.)"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_equal() +
  theme_chapter1()
ggsave("figures/fig_1_10_heart_roc.jpeg", fig_1_10, width = 7, height = 6.5,
       dpi = 300, device = "jpeg")
# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n")
cat("================================================================\n")
cat("SCRIPT COMPLETED SUCCESSFULLY\n")
cat("================================================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  fig_1_1_logistic_curve.jpeg\n")
cat("  fig_1_2_nonlinear_effect.jpeg\n")
cat("  fig_1_3_odds_ratios.jpeg\n")
cat("  fig_1_4_roc_curve.jpeg\n")
cat("  fig_1_5_calibration.jpeg\n")
cat("  fig_1_6_predicted_probs.jpeg\n")
cat("  fig_1_7_marginal_effects.jpeg\n")
cat("  fig_1_8_confusion_matrix.jpeg\n")
cat("  fig_1_9_heart_effects.jpeg\n")
cat("  fig_1_10_heart_roc.jpeg\n")
cat("\nAnalysis output saved to 'output/' directory:\n")
cat("  simulated_model_summary.txt\n")
cat("  simulated_odds_ratios.txt\n")
cat("  simulated_classification.txt\n")
cat("  heart_data_summary.txt\n")
cat("  heart_model_summary.txt\n")
cat("  heart_odds_ratios.txt\n")
cat("  heart_classification.txt\n")
cat("\nData files:\n")
cat("  Heart.csv (required input - place in working directory)\n")