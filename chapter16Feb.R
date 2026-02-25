# ============================================================================
# Chapter 16: Reporting Standards and Domain Applications
# R Code for Figures and Analysis - Complete Worked Example
#
# This script generates all figures and outputs for Chapter 16
# Research Question: What factors predict 30-day readmission after
#                    heart failure hospitalization?
# Required packages: tidyverse, gtsummary, pROC, rms, broom
# ============================================================================
# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(broom)
library(pROC)
# Set seed for reproducibility
set.seed(123)
# Create output directories if they don't exist
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("output")) dir.create("output")
# Set ggplot B&W theme for consistency
theme_set(theme_bw(base_size = 11) +
            theme(
              panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              plot.subtitle = element_text(size = 10, color = "grey40"),
              legend.position = "right",
              strip.background = element_rect(fill = "grey95"),
              strip.text = element_text(face = "bold")
            ))
cat("\n=======================================================\n")
cat("CHAPTER 16: REPORTING STANDARDS AND DOMAIN APPLICATIONS\n")
cat("Complete Worked Example: Heart Failure Readmission\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 16.1: Simulate Heart Failure Readmission Data
# ============================================================================
cat("Simulating heart failure readmission dataset...\n")
n_patients <- 8432
# Patient demographics
age <- round(rnorm(n_patients, mean = 72, sd = 12))
age <- pmax(30, pmin(100, age))
sex <- factor(rbinom(n_patients, 1, 0.45), levels = c(0, 1),
              labels = c("Female", "Male"))
race <- factor(sample(1:4, n_patients, replace = TRUE,
                      prob = c(0.65, 0.20, 0.10, 0.05)),
               levels = 1:4,
               labels = c("White", "Black", "Hispanic", "Other"))
insurance <- factor(sample(1:3, n_patients, replace = TRUE,
                           prob = c(0.70, 0.20, 0.10)),
                    levels = 1:3,
                    labels = c("Medicare", "Private", "Medicaid/Uninsured"))
# Clinical characteristics
ejection_fraction <- round(rnorm(n_patients, mean = 38, sd = 12))
ejection_fraction <- pmax(10, pmin(70, ejection_fraction))
comorbidity_count <- rpois(n_patients, lambda = 3)
comorbidity_count <- pmin(comorbidity_count, 10)
prior_hf_admission <- rbinom(n_patients, 1, 0.35)
length_of_stay <- round(rexp(n_patients, rate = 0.2) + 1)
length_of_stay <- pmin(length_of_stay, 30)
# Sodium level
sodium <- round(rnorm(n_patients, mean = 138, sd = 4))
sodium <- pmax(125, pmin(150, sodium))
# BNP level (log-normal)
bnp <- round(exp(rnorm(n_patients, mean = 6.5, sd = 0.8)))
bnp <- pmin(bnp, 5000)
# Generate outcome: 30-day readmission
lp <- -3.0 +
  0.015 * (age - 70) +
  0.2 * (sex == "Male") +
  0.3 * (race == "Black") +
  0.25 * (insurance == "Medicaid/Uninsured") +
  -0.02 * (ejection_fraction - 40) +
  0.12 * comorbidity_count +
  0.8 * prior_hf_admission +
  0.08 * (length_of_stay - 5) +
  -0.05 * (sodium - 138) +
  0.0003 * (bnp - 1000)
prob_readmit <- plogis(lp)
readmit_30 <- rbinom(n_patients, 1, prob_readmit)
# Create dataset
hf_data <- data.frame(
  patient_id = 1:n_patients,
  age = age,
  sex = sex,
  race = race,
  insurance = insurance,
  ejection_fraction = ejection_fraction,
  comorbidity_count = comorbidity_count,
  prior_hf_admission = factor(prior_hf_admission, levels = c(0, 1),
                              labels = c("No", "Yes")),
  length_of_stay = length_of_stay,
  sodium = sodium,
  bnp = bnp,
  readmit_30 = readmit_30
)
# Create categorical versions
hf_data$age_group <- cut(hf_data$age,
                         breaks = c(0, 65, 75, 85, Inf),
                         labels = c("18-64", "65-74", "75-84", "85+"))
hf_data$los_cat <- cut(hf_data$length_of_stay,
                       breaks = c(0, 3, 7, Inf),
                       labels = c("1-3 days", "4-7 days", ">7 days"))
hf_data$ef_cat <- cut(hf_data$ejection_fraction,
                      breaks = c(0, 40, 50, Inf),
                      labels = c("Reduced (<40%)", "Mid-range (40-50%)", "Preserved (>50%)"))
# Add some missing data (realistic scenario)
set.seed(456)
missing_ef <- sample(1:n_patients, round(0.08 * n_patients))
missing_bnp <- sample(1:n_patients, round(0.15 * n_patients))
missing_sodium <- sample(1:n_patients, round(0.03 * n_patients))
hf_data$ejection_fraction[missing_ef] <- NA
hf_data$ef_cat[missing_ef] <- NA
hf_data$bnp[missing_bnp] <- NA
hf_data$sodium[missing_sodium] <- NA
# ============================================================================
# Section 16.2: Study Flow and Sample Description
# ============================================================================
cat("Creating study flow and sample description...\n")
n_total <- nrow(hf_data)
n_readmit <- sum(hf_data$readmit_30)
n_no_readmit <- n_total - n_readmit
sink("output/study_flow.txt")
cat("=== Study Flow and Sample Description ===\n\n")
cat("Study: 30-Day Readmission After Heart Failure Hospitalization\n")
cat("Design: Retrospective cohort study\n")
cat("Data Source: Hospital administrative records, 2018-2022\n")
cat("Population: Adults discharged with primary diagnosis of heart failure\n\n")
cat("--- Sample Selection ---\n")
cat("Initial HF discharges: 10,234\n")
cat("Excluded:\n")
cat("  - In-hospital death: 423\n")
cat("  - Transfer to other facility: 876\n")
cat("  - Left against medical advice: 312\n")
cat("  - Age < 18 years: 191\n")
cat("Final analytic sample:", n_total, "\n\n")
cat("--- Outcome Distribution ---\n")
cat("30-day readmission:\n")
cat("  Yes:", n_readmit, "(", round(100 * n_readmit/n_total, 1), "%)\n")
cat("  No:", n_no_readmit, "(", round(100 * n_no_readmit/n_total, 1), "%)\n\n")
cat("--- Missing Data ---\n")
missing_summary <- data.frame(
  Variable = c("Age", "Sex", "Race", "Insurance", "Ejection Fraction",
               "Comorbidity Count", "Prior HF Admission", "Length of Stay",
               "Sodium", "BNP"),
  N_Missing = c(sum(is.na(hf_data$age)), sum(is.na(hf_data$sex)),
                sum(is.na(hf_data$race)), sum(is.na(hf_data$insurance)),
                sum(is.na(hf_data$ejection_fraction)), sum(is.na(hf_data$comorbidity_count)),
                sum(is.na(hf_data$prior_hf_admission)), sum(is.na(hf_data$length_of_stay)),
                sum(is.na(hf_data$sodium)), sum(is.na(hf_data$bnp))),
  Pct_Missing = NA
)
missing_summary$Pct_Missing <- round(100 * missing_summary$N_Missing / n_total, 1)
print(missing_summary, row.names = FALSE)
sink()
# ============================================================================
# Section 16.3: Table 1 - Baseline Characteristics
# ============================================================================
cat("Creating Table 1: Baseline Characteristics...\n")
create_table1 <- function(data) {
  cont_vars <- c("age", "ejection_fraction", "comorbidity_count",
                 "length_of_stay", "sodium", "bnp")
  cont_stats <- data.frame()
  for (var in cont_vars) {
    overall_mean <- mean(data[[var]], na.rm = TRUE)
    overall_sd <- sd(data[[var]], na.rm = TRUE)
    readmit_mean <- mean(data[[var]][data$readmit_30 == 1], na.rm = TRUE)
    readmit_sd <- sd(data[[var]][data$readmit_30 == 1], na.rm = TRUE)
    no_readmit_mean <- mean(data[[var]][data$readmit_30 == 0], na.rm = TRUE)
    no_readmit_sd <- sd(data[[var]][data$readmit_30 == 0], na.rm = TRUE)
    cont_stats <- rbind(cont_stats, data.frame(
      Variable = var,
      Overall = sprintf("%.1f (%.1f)", overall_mean, overall_sd),
      No_Readmit = sprintf("%.1f (%.1f)", no_readmit_mean, no_readmit_sd),
      Readmit = sprintf("%.1f (%.1f)", readmit_mean, readmit_sd)
    ))
  }
  return(cont_stats)
}
table1_cont <- create_table1(hf_data)
cat_summary <- function(data, var) {
  tab_overall <- table(data[[var]])
  tab_readmit <- table(data[[var]][data$readmit_30 == 1])
  tab_no_readmit <- table(data[[var]][data$readmit_30 == 0])
  n_overall <- sum(tab_overall)
  n_readmit <- sum(tab_readmit)
  n_no_readmit <- sum(tab_no_readmit)
  result <- data.frame(
    Variable = paste0("  ", names(tab_overall)),
    Overall = sprintf("%d (%.1f%%)", tab_overall, 100 * tab_overall/n_overall),
    No_Readmit = sprintf("%d (%.1f%%)", tab_no_readmit, 100 * tab_no_readmit/n_no_readmit),
    Readmit = sprintf("%d (%.1f%%)", tab_readmit, 100 * tab_readmit/n_readmit)
  )
  return(result)
}
sink("output/table1_characteristics.txt")
cat("=== Table 1: Baseline Characteristics by 30-Day Readmission Status ===\n\n")
cat("                                    Overall        No Readmit      Readmit\n")
cat("                                   (N=", n_total, ")      (N=", n_no_readmit, ")      (N=", n_readmit, ")\n", sep="")
cat("-----------------------------------------------------------------------------\n")
cat("\nContinuous Variables, mean (SD):\n")
cat("Age, years                        ", table1_cont$Overall[1], "    ",
    table1_cont$No_Readmit[1], "    ", table1_cont$Readmit[1], "\n")
cat("Ejection fraction, %              ", table1_cont$Overall[2], "    ",
    table1_cont$No_Readmit[2], "    ", table1_cont$Readmit[2], "\n")
cat("Comorbidity count                 ", table1_cont$Overall[3], "     ",
    table1_cont$No_Readmit[3], "     ", table1_cont$Readmit[3], "\n")
cat("Length of stay, days              ", table1_cont$Overall[4], "     ",
    table1_cont$No_Readmit[4], "     ", table1_cont$Readmit[4], "\n")
cat("Sodium, mEq/L                     ", table1_cont$Overall[5], "   ",
    table1_cont$No_Readmit[5], "   ", table1_cont$Readmit[5], "\n")
cat("BNP, pg/mL                        ", table1_cont$Overall[6], " ",
    table1_cont$No_Readmit[6], " ", table1_cont$Readmit[6], "\n")
cat("\nCategorical Variables, n (%):\n")
cat("\nSex:\n")
sex_tab <- cat_summary(hf_data, "sex")
for (i in 1:nrow(sex_tab)) {
  cat(sprintf("%-36s %-14s %-14s %s\n", sex_tab$Variable[i],
              sex_tab$Overall[i], sex_tab$No_Readmit[i], sex_tab$Readmit[i]))
}
cat("\nRace:\n")
race_tab <- cat_summary(hf_data, "race")
for (i in 1:nrow(race_tab)) {
  cat(sprintf("%-36s %-14s %-14s %s\n", race_tab$Variable[i],
              race_tab$Overall[i], race_tab$No_Readmit[i], race_tab$Readmit[i]))
}
cat("\nInsurance:\n")
ins_tab <- cat_summary(hf_data, "insurance")
for (i in 1:nrow(ins_tab)) {
  cat(sprintf("%-36s %-14s %-14s %s\n", ins_tab$Variable[i],
              ins_tab$Overall[i], ins_tab$No_Readmit[i], ins_tab$Readmit[i]))
}
cat("\nPrior HF Admission:\n")
prior_tab <- cat_summary(hf_data, "prior_hf_admission")
for (i in 1:nrow(prior_tab)) {
  cat(sprintf("%-36s %-14s %-14s %s\n", prior_tab$Variable[i],
              prior_tab$Overall[i], prior_tab$No_Readmit[i], prior_tab$Readmit[i]))
}
sink()
# ============================================================================
# Section 16.4: Model Development
# ============================================================================
cat("Developing prediction model...\n")
hf_complete <- hf_data %>%
  select(readmit_30, age, sex, race, insurance, ejection_fraction,
         comorbidity_count, prior_hf_admission, length_of_stay, sodium, bnp) %>%
  na.omit()
n_complete <- nrow(hf_complete)
n_events_complete <- sum(hf_complete$readmit_30)
# Fit unadjusted (crude) models
crude_results <- data.frame()
vars_to_test <- c("age", "sex", "race", "insurance", "ejection_fraction",
                  "comorbidity_count", "prior_hf_admission", "length_of_stay",
                  "sodium", "bnp")
for (var in vars_to_test) {
  formula <- as.formula(paste("readmit_30 ~", var))
  model <- glm(formula, family = binomial, data = hf_complete)
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_model <- tidy_model[tidy_model$term != "(Intercept)", ]
  tidy_model$variable <- var
  crude_results <- rbind(crude_results, tidy_model)
}
# Fit multivariable model
full_model <- glm(readmit_30 ~ age + sex + race + insurance + ejection_fraction +
                    comorbidity_count + prior_hf_admission + length_of_stay +
                    sodium + bnp,
                  family = binomial, data = hf_complete)
full_summary <- tidy(full_model, conf.int = TRUE, exponentiate = TRUE)
sink("output/model_results.txt")
cat("=== Logistic Regression Model Results ===\n\n")
cat("--- Sample for Analysis ---\n")
cat("Complete cases:", n_complete, "\n")
cat("Events (readmissions):", n_events_complete, "\n")
cat("Event rate:", round(100 * n_events_complete/n_complete, 1), "%\n")
cat("Events per variable:", round(n_events_complete/10, 1), "\n\n")
cat("--- Crude (Unadjusted) Odds Ratios ---\n")
crude_table <- crude_results %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    OR = round(estimate, 3),
    CI = paste0("(", round(conf.low, 3), "-", round(conf.high, 3), ")"),
    p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  select(term, OR, CI, p)
print(crude_table, row.names = FALSE)
cat("\n--- Adjusted Odds Ratios (Multivariable Model) ---\n")
adj_table <- full_summary %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    OR = round(estimate, 3),
    CI = paste0("(", round(conf.low, 3), "-", round(conf.high, 3), ")"),
    p = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  select(term, OR, CI, p)
print(adj_table, row.names = FALSE)
cat("\n--- Full Model Equation ---\n")
coefs <- coef(full_model)
cat("logit(p) = ", round(coefs[1], 4), "\n")
for (i in 2:length(coefs)) {
  sign <- ifelse(coefs[i] >= 0, "+", "-")
  cat("         ", sign, round(abs(coefs[i]), 4), "*", names(coefs)[i], "\n")
}
sink()
# ============================================================================
# Section 16.5: Model Performance
# ============================================================================
cat("Evaluating model performance...\n")
hf_complete$pred_prob <- predict(full_model, type = "response")
roc_obj <- roc(hf_complete$readmit_30, hf_complete$pred_prob)
auc_val <- auc(roc_obj)
auc_ci <- ci.auc(roc_obj)
# Hosmer-Lemeshow test (manual implementation)
hl_test <- function(observed, predicted, g = 10) {
  data <- data.frame(obs = observed, pred = predicted)
  data$decile <- cut(data$pred, breaks = quantile(data$pred, probs = seq(0, 1, length.out = g + 1)),
                     include.lowest = TRUE, labels = 1:g)
  hl_table <- data %>%
    group_by(decile) %>%
    summarise(
      n = n(),
      obs_events = sum(obs),
      exp_events = sum(pred),
      obs_non = n - obs_events,
      exp_non = n - exp_events
    )
  chi_sq <- sum((hl_table$obs_events - hl_table$exp_events)^2 / hl_table$exp_events +
                  (hl_table$obs_non - hl_table$exp_non)^2 / hl_table$exp_non)
  p_value <- 1 - pchisq(chi_sq, df = g - 2)
  return(list(statistic = chi_sq, p.value = p_value, table = hl_table))
}
hl_result <- hl_test(hf_complete$readmit_30, hf_complete$pred_prob)
brier_score <- mean((hf_complete$pred_prob - hf_complete$readmit_30)^2)
sink("output/model_performance.txt")
cat("=== Model Performance Metrics ===\n\n")
cat("--- Discrimination ---\n")
cat("AUC:", round(auc_val, 3), "\n")
cat("95% CI:", round(auc_ci[1], 3), "-", round(auc_ci[3], 3), "\n\n")
cat("--- Calibration ---\n")
cat("Hosmer-Lemeshow test:\n")
cat("  Chi-square:", round(hl_result$statistic, 2), "\n")
cat("  df: 8\n")
cat("  p-value:", round(hl_result$p.value, 3), "\n\n")
cat("--- Overall Performance ---\n")
cat("Brier score:", round(brier_score, 4), "\n")
cat("(Lower is better; 0 = perfect, 0.25 = non-informative for 50% prevalence)\n\n")
cat("--- Calibration Table by Decile ---\n")
cal_table <- hl_result$table %>%
  mutate(
    observed_rate = round(100 * obs_events / n, 1),
    expected_rate = round(100 * exp_events / n, 1)
  ) %>%
  select(decile, n, obs_events, exp_events, observed_rate, expected_rate)
print(as.data.frame(cal_table), row.names = FALSE)
sink()
# ============================================================================
# Section 16.6: Bootstrap Validation
# ============================================================================
cat("Performing bootstrap validation...\n")
set.seed(789)
n_boot <- 200
boot_auc <- numeric(n_boot)
boot_brier <- numeric(n_boot)
for (b in 1:n_boot) {
  boot_idx <- sample(1:nrow(hf_complete), replace = TRUE)
  boot_data <- hf_complete[boot_idx, ]
  boot_model <- glm(readmit_30 ~ age + sex + race + insurance + ejection_fraction +
                      comorbidity_count + prior_hf_admission + length_of_stay +
                      sodium + bnp,
                    family = binomial, data = boot_data)
  orig_pred <- predict(boot_model, newdata = hf_complete, type = "response")
  boot_pred <- predict(boot_model, type = "response")
  boot_auc[b] <- as.numeric(auc(roc(hf_complete$readmit_30, orig_pred, quiet = TRUE)))
  boot_brier[b] <- mean((orig_pred - hf_complete$readmit_30)^2)
}
apparent_auc <- as.numeric(auc_val)
optimism_auc <- mean(boot_auc) - apparent_auc
corrected_auc <- apparent_auc - optimism_auc
apparent_brier <- brier_score
optimism_brier <- mean(boot_brier) - apparent_brier
corrected_brier <- apparent_brier - optimism_brier
sink("output/bootstrap_validation.txt")
cat("=== Bootstrap Internal Validation ===\n\n")
cat("Number of bootstrap samples:", n_boot, "\n\n")
cat("--- AUC ---\n")
cat("Apparent AUC:", round(apparent_auc, 3), "\n")
cat("Optimism:", round(optimism_auc, 3), "\n")
cat("Optimism-corrected AUC:", round(corrected_auc, 3), "\n")
cat("Bootstrap 95% CI:", round(quantile(boot_auc, 0.025), 3), "-",
    round(quantile(boot_auc, 0.975), 3), "\n\n")
cat("--- Brier Score ---\n")
cat("Apparent Brier:", round(apparent_brier, 4), "\n")
cat("Optimism:", round(optimism_brier, 4), "\n")
cat("Optimism-corrected Brier:", round(corrected_brier, 4), "\n")
sink()
# ============================================================================
# Section 16.7: Figures
# ============================================================================
cat("Creating publication-ready figures...\n")
# Figure 16.1: Forest Plot
forest_data <- full_summary %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "age" ~ "Age (per year)",
      term == "sexMale" ~ "Sex (Male vs Female)",
      term == "raceBlack" ~ "Race (Black vs White)",
      term == "raceHispanic" ~ "Race (Hispanic vs White)",
      term == "raceOther" ~ "Race (Other vs White)",
      term == "insurancePrivate" ~ "Insurance (Private vs Medicare)",
      term == "insuranceMedicaid/Uninsured" ~ "Insurance (Medicaid vs Medicare)",
      term == "ejection_fraction" ~ "Ejection Fraction (per %)",
      term == "comorbidity_count" ~ "Comorbidity Count (per 1)",
      term == "prior_hf_admissionYes" ~ "Prior HF Admission (Yes vs No)",
      term == "length_of_stay" ~ "Length of Stay (per day)",
      term == "sodium" ~ "Sodium (per mEq/L)",
      term == "bnp" ~ "BNP (per pg/mL)",
      TRUE ~ term
    )
  ) %>%
  mutate(term = factor(term, levels = rev(term)))
fig16_1 <- ggplot(forest_data, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2,
                 colour = "black") +
  geom_point(size = 3, colour = "black") +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
                limits = c(0.5, 3)) +
  labs(title = "Forest Plot: Adjusted Odds Ratios for 30-Day Readmission",
       subtitle = "Multivariable logistic regression model",
       x = "Odds Ratio (95% CI)", y = "") +
  theme(axis.text.y = element_text(size = 9))
ggsave("figures/fig16_1_forest_plot.jpeg", fig16_1, width = 10, height = 7,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig16_1_forest_plot.jpeg\n")
# Figure 16.2: ROC Curve
roc_data <- data.frame(
  sensitivity = roc_obj$sensitivities,
  specificity = roc_obj$specificities
)
fig16_2 <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_line(colour = "black", linewidth = 1) +
  annotate("text", x = 0.6, y = 0.2,
           label = sprintf("AUC = %.3f\n(95%% CI: %.3f\u2013%.3f)",
                           auc_val, auc_ci[1], auc_ci[3]),
           size = 4) +
  coord_equal() +
  labs(title = "ROC Curve for 30-Day Readmission Prediction Model",
       x = "1 \u2212 Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)")
ggsave("figures/fig16_2_roc_curve.jpeg", fig16_2, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig16_2_roc_curve.jpeg\n")
# Figure 16.3: Calibration Plot
cal_data <- hf_complete %>%
  mutate(decile = ntile(pred_prob, 10)) %>%
  group_by(decile) %>%
  summarise(
    mean_pred = mean(pred_prob),
    mean_obs = mean(readmit_30),
    n = n(),
    se = sqrt(mean_obs * (1 - mean_obs) / n)
  )
fig16_3 <- ggplot(cal_data, aes(x = mean_pred, y = mean_obs)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = mean_obs - 1.96*se, ymax = mean_obs + 1.96*se),
                width = 0.01, colour = "black") +
  geom_point(aes(size = n), colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "black",
              linewidth = 0.8, linetype = "dotted") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  coord_equal(xlim = c(0, 0.35), ylim = c(0, 0.35)) +
  labs(title = "Calibration Plot for 30-Day Readmission Model",
       subtitle = "Points show observed vs predicted by decile; dotted line shows LOESS smooth",
       x = "Mean Predicted Probability",
       y = "Observed Proportion")
ggsave("figures/fig16_3_calibration_plot.jpeg", fig16_3, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig16_3_calibration_plot.jpeg\n")
# Figure 16.4: Distribution of Predicted Probabilities
fig16_4 <- ggplot(hf_complete, aes(x = pred_prob, fill = factor(readmit_30))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50, colour = "white") +
  scale_fill_manual(values = c("0" = "grey70", "1" = "grey30"),
                    labels = c("No Readmission", "Readmission"),
                    name = "Outcome") +
  labs(title = "Distribution of Predicted Probabilities by Outcome",
       x = "Predicted Probability of 30-Day Readmission",
       y = "Count") +
  theme(legend.position = "bottom")
ggsave("figures/fig16_4_pred_distribution.jpeg", fig16_4, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig16_4_pred_distribution.jpeg\n")
# ============================================================================
# Section 16.8: Results Narrative
# ============================================================================
sig_predictors <- full_summary %>%
  filter(term != "(Intercept)", p.value < 0.05) %>%
  arrange(desc(estimate))
sink("output/results_narrative.txt")
cat("=== Results Narrative for Manuscript ===\n\n")
cat("METHODS:\n")
cat("---------\n")
cat("We conducted a retrospective cohort study using hospital administrative\n")
cat("records from 2018-2022. Adults discharged with a primary diagnosis of\n")
cat("heart failure were included. The outcome was unplanned readmission within\n")
cat("30 days of discharge. Multivariable logistic regression was used to\n")
cat("identify independent predictors of readmission. Internal validation was\n")
cat("performed using 200 bootstrap samples. All analyses were conducted in\n")
cat("R version", paste(R.version$major, R.version$minor, sep = "."), ".\n\n")
cat("RESULTS:\n")
cat("---------\n")
cat(sprintf("Of %d patients discharged after HF hospitalization, %d (%.1f%%)\n",
            n_complete, n_events_complete, 100*n_events_complete/n_complete))
cat("were readmitted within 30 days.\n\n")
cat("In multivariable analysis, independent predictors of readmission included:\n")
for (i in 1:nrow(sig_predictors)) {
  cat(sprintf("  - %s (OR %.2f, 95%% CI %.2f-%.2f)\n",
              sig_predictors$term[i],
              sig_predictors$estimate[i],
              sig_predictors$conf.low[i],
              sig_predictors$conf.high[i]))
}
cat(sprintf("\nThe model demonstrated moderate discrimination (AUC %.2f, 95%% CI\n",
            apparent_auc))
cat(sprintf("%.2f-%.2f; optimism-corrected AUC %.2f) and adequate calibration\n",
            auc_ci[1], auc_ci[3], corrected_auc))
cat(sprintf("(Hosmer-Lemeshow p = %.2f).\n", hl_result$p.value))
sink()
# ============================================================================
# Section 16.9: Reporting Checklist
# ============================================================================
sink("output/reporting_checklist.txt")
cat("=== Reporting Checklist for This Analysis ===\n\n")
cat("METHODS SECTION:\n")
cat("[X] Study design clearly described (retrospective cohort)\n")
cat("[X] Data source and time period specified (hospital records, 2018-2022)\n")
cat("[X] Inclusion/exclusion criteria stated\n")
cat("[X] Outcome definition provided (30-day unplanned readmission)\n")
cat("[X] All predictors defined with measurement details\n")
cat("[X] Sample size and number of events reported (n=", n_complete, ", events=", n_events_complete, ")\n", sep="")
cat("[X] Events-per-variable ratio calculated (", round(n_events_complete/10, 1), ")\n", sep="")
cat("[X] Missing data: extent and handling described\n")
cat("[X] Statistical software documented (R)\n")
cat("[X] Model building strategy described\n")
cat("[X] Validation approach specified (bootstrap)\n\n")
cat("RESULTS SECTION:\n")
cat("[X] Participant flow documented\n")
cat("[X] Baseline characteristics table by outcome (Table 1)\n")
cat("[X] Missing data extent reported\n")
cat("[X] Crude ORs reported\n")
cat("[X] Adjusted ORs with 95% CIs reported\n")
cat("[X] Reference categories clearly indicated\n")
cat("[X] Model performance metrics reported (AUC, calibration)\n")
cat("[X] Validation results reported (optimism-corrected AUC)\n\n")
cat("FOR PREDICTION MODELS:\n")
cat("[X] Full model equation provided\n")
cat("[X] Discrimination: AUC with 95% CI\n")
cat("[X] Calibration plot included\n")
cat("[X] Calibration metrics reported (H-L test)\n")
cat("[ ] Clinical utility assessed (decision curve) - OPTIONAL\n")
cat("[ ] External validation results - NOT AVAILABLE\n")
sink()
# ============================================================================
# Section 16.10: Summary
# ============================================================================
sink("output/chapter16_summary.txt")
cat("=== Chapter 16 Summary: Reporting Standards and Domain Applications ===\n\n")
cat("STUDY OVERVIEW:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Research Question: Predictors of 30-day readmission after HF hospitalization\n")
cat("Design: Retrospective cohort study\n")
cat("Sample size:", n_complete, "\n")
cat("Events:", n_events_complete, "(", round(100*n_events_complete/n_complete, 1), "%)\n")
cat("Events per variable:", round(n_events_complete/10, 1), "\n\n")
cat("KEY FINDINGS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Significant predictors of readmission:\n")
for (i in 1:min(5, nrow(sig_predictors))) {
  cat("  ", sig_predictors$term[i], ": OR =", round(sig_predictors$estimate[i], 2), "\n")
}
cat("\nMODEL PERFORMANCE:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Discrimination:\n")
cat("  Apparent AUC:", round(apparent_auc, 3), "\n")
cat("  Optimism-corrected AUC:", round(corrected_auc, 3), "\n")
cat("  95% CI:", round(auc_ci[1], 3), "-", round(auc_ci[3], 3), "\n")
cat("Calibration:\n")
cat("  Hosmer-Lemeshow p-value:", round(hl_result$p.value, 3), "\n")
cat("  Brier score:", round(brier_score, 4), "\n\n")
cat("REPORTING GUIDELINES ADDRESSED:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("TRIPOD: Prediction model reporting requirements met\n")
cat("STROBE: Observational study requirements met\n")
cat("Full model equation, performance metrics, and validation provided\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 16 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig16_1_forest_plot.jpeg\n")
cat("  - fig16_2_roc_curve.jpeg\n")
cat("  - fig16_3_calibration_plot.jpeg\n")
cat("  - fig16_4_pred_distribution.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - study_flow.txt\n")
cat("  - table1_characteristics.txt\n")
cat("  - model_results.txt\n")
cat("  - model_performance.txt\n")
cat("  - bootstrap_validation.txt\n")
cat("  - results_narrative.txt\n")
cat("  - reporting_checklist.txt\n")
cat("  - chapter16_summary.txt\n")