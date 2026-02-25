# ============================================================================
# Chapter 14: Survey Data and Complex Sampling
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 14
# Required packages: survey, tidyverse, ggplot2
# ============================================================================
# Load required packages
library(survey)
library(tidyverse)
library(ggplot2)
library(gridExtra)
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
cat("CHAPTER 14: SURVEY DATA AND COMPLEX SAMPLING\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 14.2: Simulate Complex Survey Data
# ============================================================================
cat("Simulating complex survey data...\n")
# Simulate a realistic survey with:
# - 20 strata (e.g., geographic regions x urban/rural)
# - 3 PSUs per stratum (60 total PSUs, e.g., counties)
# - Variable number of individuals per PSU
# - Oversampling of minority group
n_strata <- 20
n_psu_per_stratum <- 3
n_total_psu <- n_strata * n_psu_per_stratum
# Create PSU-level data
psu_data <- data.frame(
  stratum = rep(1:n_strata, each = n_psu_per_stratum),
  psu = 1:n_total_psu
)
# Generate individuals within PSUs
set.seed(123)
individual_data <- data.frame()
for (i in 1:n_total_psu) {
  # Variable cluster size (30-80 individuals per PSU)
  n_in_psu <- sample(30:80, 1)
  # PSU-level random effect (creates clustering)
  psu_effect <- rnorm(1, 0, 0.5)
  # Stratum characteristics
  stratum_id <- psu_data$stratum[i]
  urban <- stratum_id <= 10  # First 10 strata are urban
  # Generate individual data
  psu_df <- data.frame(
    id = ((i-1) * 100 + 1):((i-1) * 100 + n_in_psu),
    stratum = stratum_id,
    psu = i,
    urban = urban
  )
  # Demographics
  psu_df$age <- round(rnorm(n_in_psu, mean = ifelse(urban, 42, 48), sd = 15))
  psu_df$age <- pmax(18, pmin(85, psu_df$age))
  psu_df$female <- rbinom(n_in_psu, 1, 0.52)
  # Minority status (oversampled)
  base_minority_prob <- 0.15
  psu_df$minority <- rbinom(n_in_psu, 1, 0.30)  # Oversampled
  # BMI with clustering
  psu_df$bmi <- round(rnorm(n_in_psu, mean = 27 + psu_effect, sd = 5), 1)
  psu_df$bmi <- pmax(15, pmin(50, psu_df$bmi))
  # Income (1-5 scale)
  psu_df$income <- sample(1:5, n_in_psu, replace = TRUE,
                          prob = c(0.15, 0.25, 0.30, 0.20, 0.10))
  # Physical activity
  psu_df$phys_active <- rbinom(n_in_psu, 1, ifelse(urban, 0.45, 0.35))
  # Generate outcome: diabetes (with PSU-level clustering)
  lp <- -4.5 +
    0.04 * (psu_df$age - 45) +
    0.08 * (psu_df$bmi - 27) +
    0.3 * psu_df$minority +
    -0.15 * psu_df$female +
    -0.2 * (psu_df$income - 3) +
    -0.4 * psu_df$phys_active +
    psu_effect  # PSU-level clustering
  psu_df$diabetes <- rbinom(n_in_psu, 1, plogis(lp))
  individual_data <- rbind(individual_data, psu_df)
}
# Calculate survey weights
individual_data$base_weight <- ifelse(individual_data$minority == 1, 0.5, 1.0)
psu_sizes <- table(individual_data$psu)
individual_data$psu_size <- psu_sizes[as.character(individual_data$psu)]
pop_multiplier <- 2
individual_data$weight <- individual_data$base_weight * pop_multiplier
individual_data$final_weight <- individual_data$weight
# Summary
n_total <- nrow(individual_data)
n_strata_actual <- length(unique(individual_data$stratum))
n_psu_actual <- length(unique(individual_data$psu))
sink("output/survey_data_summary.txt")
cat("=== Survey Data Summary ===\n\n")
cat("--- Survey Design ---\n")
cat("Total sample size:", n_total, "\n")
cat("Number of strata:", n_strata_actual, "\n")
cat("Number of PSUs:", n_psu_actual, "\n")
cat("PSUs per stratum:", n_psu_per_stratum, "\n")
cat("Average cluster size:", round(n_total / n_psu_actual, 1), "\n\n")
cat("--- Outcome Distribution ---\n")
cat("Diabetes cases:", sum(individual_data$diabetes), "\n")
cat("Diabetes prevalence (unweighted):", round(100 * mean(individual_data$diabetes), 2), "%\n\n")
cat("--- Demographic Distribution (Unweighted) ---\n")
cat("Mean age:", round(mean(individual_data$age), 1), "\n")
cat("Female:", round(100 * mean(individual_data$female), 1), "%\n")
cat("Minority:", round(100 * mean(individual_data$minority), 1), "%\n")
cat("Mean BMI:", round(mean(individual_data$bmi), 1), "\n")
cat("Physically active:", round(100 * mean(individual_data$phys_active), 1), "%\n\n")
cat("--- Weight Distribution ---\n")
cat("Min weight:", round(min(individual_data$final_weight), 2), "\n")
cat("Max weight:", round(max(individual_data$final_weight), 2), "\n")
cat("Mean weight:", round(mean(individual_data$final_weight), 2), "\n")
sink()
# ============================================================================
# Section 14.3: Create Survey Design Object
# ============================================================================
cat("Creating survey design objects...\n")
survey_design <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~final_weight,
  nest = TRUE,
  data = individual_data
)
sink("output/survey_design_summary.txt")
cat("=== Survey Design Specification ===\n\n")
cat("Design: Stratified cluster sampling\n")
cat("Strata:", n_strata_actual, "\n")
cat("PSUs (clusters):", n_psu_actual, "\n")
cat("Observations:", n_total, "\n\n")
cat("--- Design Degrees of Freedom ---\n")
cat("df = PSUs - Strata =", n_psu_actual, "-", n_strata_actual, "=",
    n_psu_actual - n_strata_actual, "\n\n")
cat("--- Weighted vs Unweighted Estimates ---\n")
cat("\nDiabetes prevalence:\n")
cat("  Unweighted:", round(100 * mean(individual_data$diabetes), 2), "%\n")
wt_prev <- svymean(~diabetes, design = survey_design)
cat("  Weighted:", round(100 * coef(wt_prev)[1], 2), "%\n")
cat("  SE:", round(100 * SE(wt_prev)[1], 2), "%\n")
cat("\nMinority proportion:\n")
cat("  Unweighted (oversampled):", round(100 * mean(individual_data$minority), 2), "%\n")
wt_minority <- svymean(~minority, design = survey_design)
cat("  Weighted (population estimate):", round(100 * coef(wt_minority)[1], 2), "%\n")
sink()
# ============================================================================
# Section 14.4: Unweighted vs Weighted Logistic Regression
# ============================================================================
cat("Fitting unweighted and weighted logistic regression models...\n")
# Unweighted model (WRONG for population inference)
unwt_model <- glm(diabetes ~ age + bmi + female + minority + income + phys_active,
                  family = binomial,
                  data = individual_data)
# Survey-weighted model (CORRECT)
svywt_model <- svyglm(diabetes ~ age + bmi + female + minority + income + phys_active,
                      design = survey_design,
                      family = quasibinomial())
# Extract results
unwt_coef <- coef(unwt_model)
unwt_se <- sqrt(diag(vcov(unwt_model)))
unwt_or <- exp(unwt_coef)
unwt_ci <- exp(confint.default(unwt_model))
svywt_coef <- coef(svywt_model)
svywt_se <- SE(svywt_model)
svywt_or <- exp(svywt_coef)
svywt_ci <- exp(confint(svywt_model))
# Calculate design effects
deff <- (svywt_se / unwt_se)^2
sink("output/weighted_vs_unweighted.txt")
cat("=== Weighted vs Unweighted Logistic Regression ===\n\n")
cat("True model (used to generate data):\n")
cat("logit(p) = -4.5 + 0.04*(age-45) + 0.08*(bmi-27) + 0.3*minority\n")
cat("           - 0.15*female - 0.2*(income-3) - 0.4*phys_active + PSU_effect\n\n")
cat("--- Unweighted Model (INCORRECT for population inference) ---\n")
cat("Sample size:", nobs(unwt_model), "\n\n")
unwt_table <- data.frame(
  Variable = names(unwt_coef),
  Coef = round(unwt_coef, 4),
  SE = round(unwt_se, 4),
  OR = round(unwt_or, 3),
  CI_Lower = round(unwt_ci[, 1], 3),
  CI_Upper = round(unwt_ci[, 2], 3)
)
print(unwt_table, row.names = FALSE)
cat("\n--- Survey-Weighted Model (CORRECT) ---\n")
cat("Sample size:", nobs(svywt_model), "\n")
cat("Design df:", svywt_model$df.residual, "\n\n")
svywt_table <- data.frame(
  Variable = names(svywt_coef),
  Coef = round(svywt_coef, 4),
  SE = round(svywt_se, 4),
  OR = round(svywt_or, 3),
  CI_Lower = round(svywt_ci[, 1], 3),
  CI_Upper = round(svywt_ci[, 2], 3)
)
print(svywt_table, row.names = FALSE)
cat("\n--- Design Effects (DEFF) ---\n")
cat("DEFF = (Weighted SE / Unweighted SE)^2\n")
cat("DEFF > 1 indicates clustering inflates variance\n\n")
deff_table <- data.frame(
  Variable = names(deff),
  DEFF = round(deff, 2),
  SE_Inflation = paste0(round((sqrt(deff) - 1) * 100, 1), "%")
)
print(deff_table, row.names = FALSE)
cat("\n--- Key Observations ---\n")
cat("1. Point estimates are similar (selection not strongly related to outcome)\n")
cat("2. Standard errors are", round(mean(sqrt(deff)) * 100 - 100, 0),
    "% larger on average with proper weighting\n")
cat("3. Average DEFF:", round(mean(deff), 2), "\n")
cat("4. This reflects the clustering within PSUs\n")
sink()
# Figure 14.1: Comparison of SEs
se_comparison <- data.frame(
  Variable = rep(names(unwt_coef)[-1], 2),
  SE = c(unwt_se[-1], svywt_se[-1]),
  Method = rep(c("Unweighted", "Survey-Weighted"), each = length(unwt_se) - 1)
)
fig14_1 <- ggplot(se_comparison, aes(x = Variable, y = SE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Unweighted" = "grey70", "Survey-Weighted" = "grey30")) +
  labs(title = "Standard Errors: Unweighted vs Survey-Weighted",
       subtitle = "Survey-weighted SEs are larger due to clustering (design effect)",
       x = "", y = "Standard Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave("figures/fig14_1_se_comparison.jpeg", fig14_1, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_1_se_comparison.jpeg\n")
# Figure 14.2: Design Effects
fig14_2 <- ggplot(deff_table[-1, ], aes(x = reorder(Variable, DEFF), y = DEFF)) +
  geom_bar(stat = "identity", fill = "grey50", colour = "black", alpha = 0.8,
           linewidth = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "black") +
  coord_flip() +
  labs(title = "Design Effects by Variable",
       subtitle = "DEFF > 1 indicates variance inflation due to complex sampling",
       x = "", y = "Design Effect (DEFF)") +
  annotate("text", x = 0.7, y = 1.1, label = "SRS baseline", colour = "black", hjust = 0)
ggsave("figures/fig14_2_design_effects.jpeg", fig14_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_2_design_effects.jpeg\n")
# ============================================================================
# Section 14.5: Odds Ratio Comparison
# ============================================================================
cat("Creating odds ratio comparison...\n")
or_comparison <- data.frame(
  Variable = rep(names(unwt_or)[-1], 2),
  OR = c(unwt_or[-1], svywt_or[-1]),
  Lower = c(unwt_ci[-1, 1], svywt_ci[-1, 1]),
  Upper = c(unwt_ci[-1, 2], svywt_ci[-1, 2]),
  Method = rep(c("Unweighted", "Survey-Weighted"), each = length(unwt_or) - 1)
)
# Figure 14.3: OR Forest Plot
fig14_3 <- ggplot(or_comparison, aes(x = Variable, y = OR, shape = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = 0.5), width = 0.2, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("Unweighted" = 16, "Survey-Weighted" = 17)) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Odds Ratios: Unweighted vs Survey-Weighted",
       subtitle = "Survey-weighted CIs are wider due to design effects",
       x = "", y = "Odds Ratio (log scale)") +
  theme(legend.position = "bottom")
ggsave("figures/fig14_3_or_comparison.jpeg", fig14_3, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_3_or_comparison.jpeg\n")
sink("output/or_comparison.txt")
cat("=== Odds Ratio Comparison ===\n\n")
cat("--- Unweighted Odds Ratios ---\n")
unwt_or_table <- data.frame(
  Variable = names(unwt_or)[-1],
  OR = round(unwt_or[-1], 3),
  CI_Lower = round(unwt_ci[-1, 1], 3),
  CI_Upper = round(unwt_ci[-1, 2], 3)
)
print(unwt_or_table, row.names = FALSE)
cat("\n--- Survey-Weighted Odds Ratios ---\n")
svywt_or_table <- data.frame(
  Variable = names(svywt_or)[-1],
  OR = round(svywt_or[-1], 3),
  CI_Lower = round(svywt_ci[-1, 1], 3),
  CI_Upper = round(svywt_ci[-1, 2], 3)
)
print(svywt_or_table, row.names = FALSE)
cat("\n--- Interpretation ---\n")
cat("Age: Each year increase associated with", round((svywt_or["age"] - 1) * 100, 1),
    "% higher odds of diabetes\n")
cat("BMI: Each unit increase associated with", round((svywt_or["bmi"] - 1) * 100, 1),
    "% higher odds of diabetes\n")
cat("Minority: Associated with", round((svywt_or["minority"] - 1) * 100, 0),
    "% higher odds of diabetes\n")
cat("Female: Associated with", round((1 - svywt_or["female"]) * 100, 0),
    "% lower odds of diabetes\n")
cat("Physical activity: Associated with", round((1 - svywt_or["phys_active"]) * 100, 0),
    "% lower odds of diabetes\n")
sink()
# ============================================================================
# Section 14.6: Variance Estimation Methods
# ============================================================================
cat("Comparing variance estimation methods...\n")
# Taylor series linearization (default)
taylor_model <- svywt_model
# Jackknife
jk_design <- as.svrepdesign(survey_design, type = "JKn")
jk_model <- svyglm(diabetes ~ age + bmi + female + minority + income + phys_active,
                   design = jk_design,
                   family = quasibinomial())
# Bootstrap
set.seed(456)
boot_design <- as.svrepdesign(survey_design, type = "bootstrap", replicates = 100)
boot_model <- svyglm(diabetes ~ age + bmi + female + minority + income + phys_active,
                     design = boot_design,
                     family = quasibinomial())
# Compare SEs
se_taylor <- SE(taylor_model)
se_jk <- SE(jk_model)
se_boot <- SE(boot_model)
sink("output/variance_estimation.txt")
cat("=== Variance Estimation Methods Comparison ===\n\n")
cat("Methods compared:\n")
cat("1. Taylor series linearization (default)\n")
cat("2. Jackknife (JKn for stratified designs)\n")
cat("3. Bootstrap (100 replicates)\n\n")
cat("--- Standard Errors by Method ---\n")
se_methods <- data.frame(
  Variable = names(se_taylor),
  Taylor = round(se_taylor, 4),
  Jackknife = round(se_jk, 4),
  Bootstrap = round(se_boot, 4)
)
print(se_methods, row.names = FALSE)
cat("\n--- Relative to Taylor Series ---\n")
rel_se <- data.frame(
  Variable = names(se_taylor),
  Taylor = 1.00,
  Jackknife = round(se_jk / se_taylor, 3),
  Bootstrap = round(se_boot / se_taylor, 3)
)
print(rel_se, row.names = FALSE)
cat("\n--- Conclusion ---\n")
cat("All methods give similar standard errors.\n")
cat("Taylor series is the default and computationally efficient.\n")
cat("Jackknife and bootstrap are alternatives when design variables are limited.\n")
sink()
# Figure 14.4: Variance estimation comparison
se_var_methods <- data.frame(
  Variable = rep(names(se_taylor)[-1], 3),
  SE = c(se_taylor[-1], se_jk[-1], se_boot[-1]),
  Method = rep(c("Taylor", "Jackknife", "Bootstrap"), each = length(se_taylor) - 1)
)
fig14_4 <- ggplot(se_var_methods, aes(x = Variable, y = SE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Taylor" = "grey80", "Jackknife" = "grey50",
                               "Bootstrap" = "grey20")) +
  labs(title = "Variance Estimation Methods Comparison",
       subtitle = "Taylor series, Jackknife, and Bootstrap give similar results",
       x = "", y = "Standard Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave("figures/fig14_4_variance_methods.jpeg", fig14_4, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_4_variance_methods.jpeg\n")
# ============================================================================
# Section 14.7: Subpopulation (Domain) Analysis
# ============================================================================
cat("Performing subpopulation analysis...\n")
# CORRECT approach: Subset the design object
female_design <- subset(survey_design, female == 1)
male_design <- subset(survey_design, female == 0)
model_female <- svyglm(diabetes ~ age + bmi + minority + income + phys_active,
                       design = female_design,
                       family = quasibinomial())
model_male <- svyglm(diabetes ~ age + bmi + minority + income + phys_active,
                     design = male_design,
                     family = quasibinomial())
# WRONG approach: Subset data first (for comparison)
wrong_female_data <- subset(individual_data, female == 1)
wrong_female_design <- svydesign(
  id = ~psu, strata = ~stratum, weights = ~final_weight,
  nest = TRUE, data = wrong_female_data
)
wrong_model_female <- svyglm(diabetes ~ age + bmi + minority + income + phys_active,
                             design = wrong_female_design,
                             family = quasibinomial())
sink("output/subpopulation_analysis.txt")
cat("=== Subpopulation (Domain) Analysis ===\n\n")
cat("Analyzing diabetes predictors separately by sex.\n\n")
cat("--- Female Subpopulation (CORRECT: subset design) ---\n")
cat("n =", nobs(model_female), "\n")
female_table <- data.frame(
  Variable = names(coef(model_female)),
  OR = round(exp(coef(model_female)), 3),
  SE = round(SE(model_female), 4),
  CI_Lower = round(exp(confint(model_female))[, 1], 3),
  CI_Upper = round(exp(confint(model_female))[, 2], 3)
)
print(female_table, row.names = FALSE)
cat("\n--- Male Subpopulation (CORRECT: subset design) ---\n")
cat("n =", nobs(model_male), "\n")
male_table <- data.frame(
  Variable = names(coef(model_male)),
  OR = round(exp(coef(model_male)), 3),
  SE = round(SE(model_male), 4),
  CI_Lower = round(exp(confint(model_male))[, 1], 3),
  CI_Upper = round(exp(confint(model_male))[, 2], 3)
)
print(male_table, row.names = FALSE)
cat("\n--- WRONG Approach: Subset Data Before Design ---\n")
cat("n =", nobs(wrong_model_female), "\n")
wrong_table <- data.frame(
  Variable = names(coef(wrong_model_female)),
  SE_Correct = round(SE(model_female), 4),
  SE_Wrong = round(SE(wrong_model_female), 4),
  Ratio = round(SE(wrong_model_female) / SE(model_female), 3)
)
print(wrong_table, row.names = FALSE)
cat("\nNote: Wrong approach typically underestimates SEs because it loses\n")
cat("information about the full design (empty strata, reduced PSU count).\n")
sink()
# Figure 14.5: Subpopulation comparison
subpop_or <- data.frame(
  Variable = rep(names(coef(model_female))[-1], 2),
  OR = c(exp(coef(model_female)[-1]), exp(coef(model_male)[-1])),
  Lower = c(exp(confint(model_female)[-1, 1]), exp(confint(model_male)[-1, 1])),
  Upper = c(exp(confint(model_female)[-1, 2]), exp(confint(model_male)[-1, 2])),
  Sex = rep(c("Female", "Male"), each = length(coef(model_female)) - 1)
)
fig14_5 <- ggplot(subpop_or, aes(x = Variable, y = OR, shape = Sex)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = 0.5), width = 0.2, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("Female" = 16, "Male" = 17)) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Subpopulation Analysis: OR by Sex",
       subtitle = "Diabetes predictors analysed separately for males and females",
       x = "", y = "Odds Ratio (log scale)") +
  theme(legend.position = "bottom")
ggsave("figures/fig14_5_subpopulation.jpeg", fig14_5, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_5_subpopulation.jpeg\n")
# ============================================================================
# Section 14.8: Model Diagnostics
# ============================================================================
cat("Computing model diagnostics...\n")
# Wald tests for overall model and individual terms
wald_overall <- regTermTest(svywt_model, ~ age + bmi + female + minority + income + phys_active)
wald_age <- regTermTest(svywt_model, ~age)
wald_bmi <- regTermTest(svywt_model, ~bmi)
wald_female <- regTermTest(svywt_model, ~female)
wald_minority <- regTermTest(svywt_model, ~minority)
wald_income <- regTermTest(svywt_model, ~income)
wald_phys <- regTermTest(svywt_model, ~phys_active)
# Model comparison (nested models)
model_reduced <- svyglm(diabetes ~ age + bmi + female,
                        design = survey_design,
                        family = quasibinomial())
model_full <- svywt_model
sink("output/model_diagnostics.txt")
cat("=== Model Diagnostics for Survey Data ===\n\n")
cat("--- Wald Test for Overall Model ---\n")
cat("H0: All coefficients (except intercept) = 0\n")
print(wald_overall)
cat("\n--- Wald Tests for Individual Terms ---\n")
cat("\nAge:\n")
print(wald_age)
cat("\nBMI:\n")
print(wald_bmi)
cat("\nFemale:\n")
print(wald_female)
cat("\nMinority:\n")
print(wald_minority)
cat("\nIncome:\n")
print(wald_income)
cat("\nPhysical Activity:\n")
print(wald_phys)
cat("\n--- Nested Model Comparison ---\n")
cat("Reduced model: diabetes ~ age + bmi + female\n")
cat("Full model: diabetes ~ age + bmi + female + minority + income + phys_active\n\n")
wald_comp_result <- anova(model_reduced, model_full, method = "Wald")
print(wald_comp_result)
sink()
# Predicted probabilities
pred_probs <- predict(svywt_model, type = "response")
individual_data$pred_prob <- pred_probs
# Figure 14.6: Predicted probabilities by BMI
pred_by_bmi <- individual_data %>%
  mutate(bmi_group = cut(bmi, breaks = c(0, 25, 30, 35, 100),
                         labels = c("<25", "25-30", "30-35", "35+"))) %>%
  group_by(bmi_group) %>%
  summarise(
    mean_pred = mean(pred_prob),
    mean_actual = mean(diabetes),
    n = n()
  )
fig14_6 <- ggplot(pred_by_bmi, aes(x = bmi_group)) +
  geom_bar(aes(y = mean_actual), stat = "identity", fill = "grey70", colour = "black",
           alpha = 0.8, linewidth = 0.3) +
  geom_point(aes(y = mean_pred), colour = "black", size = 4, shape = 17) +
  geom_line(aes(y = mean_pred, group = 1), colour = "black", linewidth = 1,
            linetype = "dashed") +
  labs(title = "Model Calibration by BMI Category",
       subtitle = "Bars = observed prevalence; Triangles/dashed line = predicted probability",
       x = "BMI Category", y = "Diabetes Prevalence") +
  theme(legend.position = "bottom")
ggsave("figures/fig14_6_calibration.jpeg", fig14_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_6_calibration.jpeg\n")
# ============================================================================
# Section 14.9: Weighted Prevalence Estimates
# ============================================================================
cat("Computing weighted prevalence estimates...\n")
prev_overall <- svymean(~diabetes, design = survey_design)
prev_by_minority <- svyby(~diabetes, ~minority, design = survey_design, svymean)
individual_data$age_group <- cut(individual_data$age,
                                 breaks = c(0, 40, 60, 100),
                                 labels = c("18-39", "40-59", "60+"))
survey_design_age <- svydesign(
  id = ~psu, strata = ~stratum, weights = ~final_weight,
  nest = TRUE, data = individual_data
)
prev_by_age <- svyby(~diabetes, ~age_group, design = survey_design_age, svymean)
sink("output/weighted_prevalence.txt")
cat("=== Weighted Prevalence Estimates ===\n\n")
cat("--- Overall Diabetes Prevalence ---\n")
cat("Weighted prevalence:", round(100 * coef(prev_overall), 2), "%\n")
cat("Standard error:", round(100 * SE(prev_overall), 2), "%\n")
cat("95% CI:", round(100 * confint(prev_overall)[1], 2), "% -",
    round(100 * confint(prev_overall)[2], 2), "%\n\n")
cat("--- Prevalence by Minority Status ---\n")
print(data.frame(
  Minority = c("No", "Yes"),
  Prevalence = round(100 * prev_by_minority$diabetes, 2),
  SE = round(100 * prev_by_minority$se, 2)
), row.names = FALSE)
cat("\n--- Prevalence by Age Group ---\n")
print(data.frame(
  Age_Group = prev_by_age$age_group,
  Prevalence = round(100 * prev_by_age$diabetes, 2),
  SE = round(100 * prev_by_age$se, 2)
), row.names = FALSE)
sink()
# Figure 14.7: Weighted prevalence by group
prev_plot_data <- data.frame(
  Group = c(as.character(prev_by_age$age_group), "Non-minority", "Minority"),
  Prevalence = c(prev_by_age$diabetes, prev_by_minority$diabetes) * 100,
  SE = c(prev_by_age$se, prev_by_minority$se) * 100,
  Category = c(rep("Age Group", 3), rep("Minority Status", 2))
)
fig14_7 <- ggplot(prev_plot_data, aes(x = Group, y = Prevalence, fill = Category)) +
  geom_bar(stat = "identity", alpha = 0.8, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Prevalence - 1.96*SE, ymax = Prevalence + 1.96*SE),
                width = 0.2) +
  scale_fill_manual(values = c("Age Group" = "grey70", "Minority Status" = "grey40")) +
  facet_wrap(~Category, scales = "free_x") +
  labs(title = "Weighted Diabetes Prevalence by Subgroup",
       subtitle = "Error bars show 95% confidence intervals",
       x = "", y = "Prevalence (%)") +
  theme(legend.position = "none")
ggsave("figures/fig14_7_weighted_prevalence.jpeg", fig14_7, width = 10, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig14_7_weighted_prevalence.jpeg\n")
# ============================================================================
# Section 14.10: Summary
# ============================================================================
sink("output/chapter14_summary.txt")
cat("=== Chapter 14 Summary: Survey Data and Complex Sampling ===\n\n")
cat("SURVEY DESIGN:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Sample size:", n_total, "\n")
cat("Strata:", n_strata_actual, "\n")
cat("PSUs (clusters):", n_psu_actual, "\n")
cat("Design df:", n_psu_actual - n_strata_actual, "\n\n")
cat("WEIGHTED VS UNWEIGHTED COMPARISON:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Average design effect (DEFF):", round(mean(deff), 2), "\n")
cat("Average SE inflation:", round((mean(sqrt(deff)) - 1) * 100, 0), "%\n\n")
cat("Key odds ratios (survey-weighted):\n")
for (var in names(svywt_or)[-1]) {
  cat("  ", var, ": OR =", round(svywt_or[var], 3),
      "(95% CI:", round(svywt_ci[var, 1], 3), "-", round(svywt_ci[var, 2], 3), ")\n")
}
cat("\nVARIANCE ESTIMATION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Taylor, Jackknife, and Bootstrap methods give similar results.\n")
cat("Average ratio (JK/Taylor):", round(mean(se_jk/se_taylor), 3), "\n")
cat("Average ratio (Boot/Taylor):", round(mean(se_boot/se_taylor), 3), "\n\n")
cat("SUBPOPULATION ANALYSIS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Female n:", nobs(model_female), "\n")
cat("Male n:", nobs(model_male), "\n")
cat("Correct method: subset() on design object, not on data\n\n")
cat("WEIGHTED PREVALENCE:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Overall diabetes:", round(100 * coef(prev_overall), 2), "%\n")
cat("  (95% CI:", round(100 * confint(prev_overall)[1], 2), "% -",
    round(100 * confint(prev_overall)[2], 2), "%)\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 14 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig14_1_se_comparison.jpeg\n")
cat("  - fig14_2_design_effects.jpeg\n")
cat("  - fig14_3_or_comparison.jpeg\n")
cat("  - fig14_4_variance_methods.jpeg\n")
cat("  - fig14_5_subpopulation.jpeg\n")
cat("  - fig14_6_calibration.jpeg\n")
cat("  - fig14_7_weighted_prevalence.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - survey_data_summary.txt\n")
cat("  - survey_design_summary.txt\n")
cat("  - weighted_vs_unweighted.txt\n")
cat("  - or_comparison.txt\n")
cat("  - variance_estimation.txt\n")
cat("  - subpopulation_analysis.txt\n")
cat("  - model_diagnostics.txt\n")
cat("  - weighted_prevalence.txt\n")
cat("  - chapter14_summary.txt\n")
