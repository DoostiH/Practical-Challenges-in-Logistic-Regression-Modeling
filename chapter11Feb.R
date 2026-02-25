# ============================================================================
# Chapter 11: Logistic Regression for Longitudinal Data
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 11
# Required packages: geepack, lme4, tidyverse, ggplot2, gridExtra, pROC
# ============================================================================
# Load required packages
library(geepack)
library(lme4)
library(tidyverse)
library(ggplot2)
library(gridExtra)
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
cat("CHAPTER 11: LOGISTIC REGRESSION FOR LONGITUDINAL DATA\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 11.2: Simulate Longitudinal Data
# ============================================================================
cat("Simulating longitudinal binary data...\n")
simulate_longitudinal_data <- function(n_subjects = 150, max_time = 5,
                                       dropout_prob = 0.08, seed = 123) {
  set.seed(seed)
  # Subject-level variables
  age <- rnorm(n_subjects, mean = 50, sd = 10)
  sex <- rbinom(n_subjects, 1, 0.5)
  treatment <- rbinom(n_subjects, 1, 0.5)
  # Random intercepts for subjects (subject-specific heterogeneity)
  random_intercepts <- rnorm(n_subjects, mean = 0, sd = 0.8)
  # Build dataset
  long_list <- list()
  for (i in 1:n_subjects) {
    # Determine number of time points (allowing for dropout)
    n_times <- max_time
    for (t in 2:max_time) {
      if (runif(1) < dropout_prob) {
        n_times <- t - 1
        break
      }
    }
    for (t in 1:n_times) {
      # Time-varying covariate
      time_varying <- rnorm(1, mean = 0.2 * t, sd = 1)
      # Linear predictor with random intercept
      # True model: treatment effect increases over time (interaction)
      linear_pred <- -1.5 +
        0.02 * age[i] +
        0.4 * sex[i] +
        0.3 * treatment[i] +
        0.15 * t +
        0.12 * treatment[i] * t +  # Treatment x time interaction
        0.3 * time_varying +
        random_intercepts[i]
      prob <- plogis(linear_pred)
      y <- rbinom(1, 1, prob)
      long_list[[length(long_list) + 1]] <- data.frame(
        subject_id = i,
        time = t,
        age = age[i],
        sex = sex[i],
        treatment = treatment[i],
        time_varying = time_varying,
        y = y
      )
    }
  }
  long_data <- do.call(rbind, long_list)
  # Convert to factors
  long_data$subject_id <- factor(long_data$subject_id)
  long_data$sex <- factor(long_data$sex, labels = c("Male", "Female"))
  long_data$treatment <- factor(long_data$treatment, labels = c("Control", "Treatment"))
  return(long_data)
}
# Generate dataset
longitudinal_data <- simulate_longitudinal_data(n_subjects = 150, max_time = 5)
# Data summary
n_subjects <- length(unique(longitudinal_data$subject_id))
n_obs <- nrow(longitudinal_data)
obs_per_subject <- table(longitudinal_data$subject_id)
sink("output/data_summary.txt")
cat("=== Longitudinal Data Summary ===\n\n")
cat("Number of subjects:", n_subjects, "\n")
cat("Total observations:", n_obs, "\n")
cat("Mean observations per subject:", round(mean(obs_per_subject), 2), "\n")
cat("Range of observations per subject:", min(obs_per_subject), "-", max(obs_per_subject), "\n\n")
cat("Overall event rate:", round(mean(longitudinal_data$y), 3), "\n\n")
cat("Event rate by time:\n")
event_by_time <- longitudinal_data %>%
  group_by(time) %>%
  summarise(n = n(), events = sum(y), rate = mean(y), .groups = "drop")
print(as.data.frame(event_by_time))
cat("\nEvent rate by treatment:\n")
event_by_trt <- longitudinal_data %>%
  group_by(treatment) %>%
  summarise(n = n(), events = sum(y), rate = mean(y), .groups = "drop")
print(as.data.frame(event_by_trt))
cat("\nTrue model:\n")
cat("logit(p) = -1.5 + 0.02*age + 0.4*sex + 0.3*treatment + 0.15*time\n")
cat("           + 0.12*treatment*time + 0.3*time_varying + random_intercept\n")
cat("Random intercept SD: 0.8\n")
sink()
# Figure 11.1: Event rates over time by treatment
event_time_trt <- longitudinal_data %>%
  group_by(time, treatment) %>%
  summarise(rate = mean(y), n = n(), se = sqrt(rate * (1 - rate) / n), .groups = "drop")
fig11_1 <- ggplot(event_time_trt, aes(x = time, y = rate, linetype = treatment,
                                      shape = treatment, group = treatment)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_point(size = 3, colour = "black") +
  geom_errorbar(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se),
                width = 0.1, colour = "black") +
  scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "dashed")) +
  scale_shape_manual(values = c("Control" = 16, "Treatment" = 17)) +
  labs(title = "Observed Event Rates Over Time by Treatment",
       subtitle = "Error bars show 95% confidence intervals",
       x = "Time", y = "Event Rate", linetype = "Treatment", shape = "Treatment") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig11_1_event_rates_time.jpeg", fig11_1, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_1_event_rates_time.jpeg\n")
# ============================================================================
# Section 11.3: GEE Models
# ============================================================================
cat("Fitting GEE models with different correlation structures...\n")
# Fit GEE with independence
gee_indep <- geeglm(y ~ age + sex + treatment * time + time_varying,
                    id = subject_id,
                    data = longitudinal_data,
                    family = binomial(link = "logit"),
                    corstr = "independence")
# Fit GEE with exchangeable correlation
gee_exch <- geeglm(y ~ age + sex + treatment * time + time_varying,
                   id = subject_id,
                   data = longitudinal_data,
                   family = binomial(link = "logit"),
                   corstr = "exchangeable")
# Fit GEE with AR(1) correlation
gee_ar1 <- geeglm(y ~ age + sex + treatment * time + time_varying,
                  id = subject_id,
                  data = longitudinal_data,
                  family = binomial(link = "logit"),
                  corstr = "ar1")
# Fit GEE with unstructured correlation
gee_unstr <- geeglm(y ~ age + sex + treatment * time + time_varying,
                    id = subject_id,
                    data = longitudinal_data,
                    family = binomial(link = "logit"),
                    corstr = "unstructured")
# Compare QIC
qic_comparison <- data.frame(
  Correlation = c("Independence", "Exchangeable", "AR(1)", "Unstructured"),
  QIC = c(QIC(gee_indep)[1], QIC(gee_exch)[1], QIC(gee_ar1)[1], QIC(gee_unstr)[1])
)
qic_comparison$Delta_QIC <- qic_comparison$QIC - min(qic_comparison$QIC)
sink("output/gee_comparison.txt")
cat("=== GEE Model Comparison ===\n\n")
cat("--- QIC Comparison ---\n")
print(qic_comparison, row.names = FALSE)
cat("\nBest model by QIC:", qic_comparison$Correlation[which.min(qic_comparison$QIC)], "\n\n")
cat("--- Estimated Correlation Parameters ---\n")
cat("Exchangeable alpha:", round(summary(gee_exch)$corr[1, 1], 4), "\n")
cat("AR(1) alpha:", round(summary(gee_ar1)$corr[1, 1], 4), "\n\n")
cat("--- Best GEE Model (Exchangeable) Summary ---\n")
print(summary(gee_exch))
sink()
# Extract coefficients and OR from best model
gee_coefs <- coef(gee_exch)
gee_se <- summary(gee_exch)$coefficients[, "Std.err"]
gee_or <- exp(gee_coefs)
gee_or_lower <- exp(gee_coefs - 1.96 * gee_se)
gee_or_upper <- exp(gee_coefs + 1.96 * gee_se)
gee_pval <- summary(gee_exch)$coefficients[, "Pr(>|W|)"]
gee_results <- data.frame(
  Variable = names(gee_coefs),
  Estimate = round(gee_coefs, 4),
  SE = round(gee_se, 4),
  OR = round(gee_or, 4),
  OR_Lower = round(gee_or_lower, 4),
  OR_Upper = round(gee_or_upper, 4),
  p_value = round(gee_pval, 4)
)
sink("output/gee_results.txt")
cat("=== GEE Results (Exchangeable Correlation) ===\n\n")
print(gee_results, row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Treatment main effect: OR =", round(gee_or["treatmentTreatment"], 3), "\n")
cat("- Time effect: OR =", round(gee_or["time"], 3), "per time unit\n")
cat("- Treatment x Time interaction: OR =", round(gee_or["treatmentTreatment:time"], 3), "\n")
cat("  (Treatment effect increases by", round((gee_or["treatmentTreatment:time"] - 1) * 100, 1),
    "% per time unit)\n")
sink()
# Figure 11.2: QIC comparison
fig11_2 <- ggplot(qic_comparison, aes(x = reorder(Correlation, QIC), y = QIC)) +
  geom_bar(stat = "identity", fill = "grey50", colour = "black", alpha = 0.8,
           linewidth = 0.3) +
  geom_text(aes(label = round(QIC, 1)), vjust = -0.5, size = 3.5) +
  labs(title = "GEE Model Comparison by QIC",
       subtitle = "Lower QIC indicates better fit",
       x = "Correlation Structure", y = "QIC") +
  coord_cartesian(ylim = c(min(qic_comparison$QIC) * 0.95, max(qic_comparison$QIC) * 1.05))
ggsave("figures/fig11_2_qic_comparison.jpeg", fig11_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_2_qic_comparison.jpeg\n")
# Figure 11.3: OR forest plot
or_plot_data <- gee_results[-1, ]  # Remove intercept
or_plot_data$Variable <- factor(or_plot_data$Variable,
                                levels = or_plot_data$Variable[order(or_plot_data$OR)])
fig11_3 <- ggplot(or_plot_data, aes(x = Variable, y = OR)) +
  geom_point(size = 3, colour = "black") +
  geom_errorbar(aes(ymin = OR_Lower, ymax = OR_Upper), width = 0.2, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  scale_y_log10() +
  labs(title = "GEE Odds Ratios with 95% Confidence Intervals",
       subtitle = "Population-averaged effects",
       x = "", y = "Odds Ratio (log scale)")
ggsave("figures/fig11_3_gee_forest_plot.jpeg", fig11_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_3_gee_forest_plot.jpeg\n")
# ============================================================================
# Section 11.4: Mixed-Effects Models (GLMM)
# ============================================================================
cat("Fitting mixed-effects logistic regression models...\n")
# Random intercept model
glmm_ri <- glmer(y ~ age + sex + treatment * time + time_varying + (1 | subject_id),
                 data = longitudinal_data,
                 family = binomial(link = "logit"),
                 control = glmerControl(optimizer = "bobyqa"))
# Random intercept and slope model
glmm_ris <- glmer(y ~ age + sex + treatment * time + time_varying + (1 + time | subject_id),
                  data = longitudinal_data,
                  family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa"))
# Compare models
model_comparison <- anova(glmm_ri, glmm_ris)
sink("output/glmm_results.txt")
cat("=== Mixed-Effects Logistic Regression Results ===\n\n")
cat("--- Random Intercept Model ---\n")
print(summary(glmm_ri))
cat("\n--- Random Intercept and Slope Model ---\n")
print(summary(glmm_ris))
cat("\n--- Model Comparison (LRT) ---\n")
print(model_comparison)
cat("\nConclusion:",
    ifelse(model_comparison$`Pr(>Chisq)`[2] < 0.05,
           "Random slope model significantly better",
           "Random intercept model sufficient"), "\n")
sink()
# Extract GLMM coefficients
glmm_coefs <- fixef(glmm_ri)
glmm_se <- sqrt(diag(vcov(glmm_ri)))
glmm_or <- exp(glmm_coefs)
glmm_or_lower <- exp(glmm_coefs - 1.96 * glmm_se)
glmm_or_upper <- exp(glmm_coefs + 1.96 * glmm_se)
glmm_results <- data.frame(
  Variable = names(glmm_coefs),
  Estimate = round(glmm_coefs, 4),
  SE = round(glmm_se, 4),
  OR = round(glmm_or, 4),
  OR_Lower = round(glmm_or_lower, 4),
  OR_Upper = round(glmm_or_upper, 4)
)
# Random effects variance
re_var <- as.data.frame(VarCorr(glmm_ri))
sink("output/glmm_coefficients.txt")
cat("=== GLMM Coefficients (Random Intercept Model) ===\n\n")
cat("--- Fixed Effects ---\n")
print(glmm_results, row.names = FALSE)
cat("\n--- Random Effects ---\n")
cat("Random intercept SD:", round(re_var$sdcor[1], 4), "\n")
cat("Random intercept variance:", round(re_var$vcov[1], 4), "\n")
cat("\nNote: GLMM provides subject-specific (conditional) effects.\n")
cat("These are larger than population-averaged (GEE) effects.\n")
sink()
# Figure 11.4: Random effects distribution
random_effects <- ranef(glmm_ri)$subject_id
random_effects$subject_id <- rownames(random_effects)
colnames(random_effects)[1] <- "Intercept"
fig11_4 <- ggplot(random_effects, aes(x = Intercept)) +
  geom_histogram(bins = 25, fill = "grey60", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Distribution of Random Intercepts",
       subtitle = paste0("SD = ", round(re_var$sdcor[1], 3)),
       x = "Random Intercept", y = "Frequency")
ggsave("figures/fig11_4_random_effects.jpeg", fig11_4, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_4_random_effects.jpeg\n")
# ============================================================================
# Section 11.5: GEE vs GLMM Comparison
# ============================================================================
cat("Comparing GEE and GLMM estimates...\n")
comparison_table <- data.frame(
  Variable = names(gee_coefs),
  GEE_Est = round(gee_coefs, 4),
  GEE_SE = round(gee_se, 4),
  GLMM_Est = round(glmm_coefs, 4),
  GLMM_SE = round(glmm_se, 4)
)
comparison_table$Ratio <- round(comparison_table$GLMM_Est / comparison_table$GEE_Est, 3)
sink("output/gee_vs_glmm.txt")
cat("=== GEE vs GLMM Comparison ===\n\n")
print(comparison_table, row.names = FALSE)
cat("\nKey differences:\n")
cat("- GLMM estimates are generally larger (further from 0) than GEE\n")
cat("- This is expected: GLMM gives subject-specific effects, GEE gives population-averaged\n")
cat("- For random intercept variance sigma^2 =", round(re_var$vcov[1], 3), "\n")
cat("- Expected ratio: approximately", round(sqrt(1 + (16*sqrt(3)/(15*pi))^2 * re_var$vcov[1]), 3), "\n")
sink()
# Figure 11.5: GEE vs GLMM coefficients
comparison_long <- comparison_table[-1, ] %>%
  select(Variable, GEE_Est, GLMM_Est) %>%
  pivot_longer(cols = c(GEE_Est, GLMM_Est), names_to = "Model", values_to = "Estimate") %>%
  mutate(Model = case_match(Model,
                            "GEE_Est" ~ "GEE (Population-Averaged)",
                            "GLMM_Est" ~ "GLMM (Subject-Specific)"))
fig11_5 <- ggplot(comparison_long, aes(x = Variable, y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("GEE (Population-Averaged)" = "grey70",
                               "GLMM (Subject-Specific)" = "grey30")) +
  coord_flip() +
  labs(title = "GEE vs. GLMM Coefficient Estimates",
       subtitle = "GLMM subject-specific effects are larger than GEE population-averaged effects",
       x = "", y = "Coefficient Estimate") +
  theme(legend.position = "bottom")
ggsave("figures/fig11_5_gee_vs_glmm.jpeg", fig11_5, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_5_gee_vs_glmm.jpeg\n")
# ============================================================================
# Section 11.6: Predicted Probabilities
# ============================================================================
cat("Computing predicted probabilities...\n")
# Create prediction grid
pred_grid <- expand.grid(
  time = 1:5,
  treatment = c("Control", "Treatment"),
  age = mean(longitudinal_data$age),
  sex = "Female",
  time_varying = 0
)
# GEE predictions (population-averaged)
pred_grid$gee_pred <- predict(gee_exch, newdata = pred_grid, type = "response")
# GLMM predictions (population-averaged approximation)
# Use re.form = NA to get population-averaged predictions
pred_grid$glmm_pred <- predict(glmm_ri, newdata = pred_grid, type = "response", re.form = NA)
sink("output/predicted_probabilities.txt")
cat("=== Predicted Probabilities by Time and Treatment ===\n\n")
cat("(At mean age, female, time_varying = 0)\n\n")
pred_print <- pred_grid %>%
  mutate(gee_pred = round(gee_pred, 4), glmm_pred = round(glmm_pred, 4))
print(as.data.frame(pred_print))
sink()
# Figure 11.6: Predicted probabilities over time
pred_long <- pred_grid %>%
  select(time, treatment, gee_pred, glmm_pred) %>%
  pivot_longer(cols = c(gee_pred, glmm_pred), names_to = "Model", values_to = "Probability") %>%
  mutate(Model = case_match(Model, "gee_pred" ~ "GEE", "glmm_pred" ~ "GLMM"))
fig11_6 <- ggplot(pred_long, aes(x = time, y = Probability, linetype = treatment,
                                 shape = Model)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_point(size = 2.5, colour = "black") +
  scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "dashed")) +
  scale_shape_manual(values = c("GEE" = 16, "GLMM" = 1)) +
  labs(title = "Predicted Probability Over Time by Treatment",
       subtitle = "Comparing GEE and GLMM predictions",
       x = "Time", y = "Predicted Probability",
       linetype = "Treatment", shape = "Model") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig11_6_predicted_probabilities.jpeg", fig11_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_6_predicted_probabilities.jpeg\n")
# ============================================================================
# Section 11.7: Transition Models
# ============================================================================
cat("Fitting transition model...\n")
# Create lagged outcome
longitudinal_data_lag <- longitudinal_data %>%
  arrange(subject_id, time) %>%
  group_by(subject_id) %>%
  mutate(
    y_prev = lag(y, 1),
    time_prev = lag(time, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(y_prev), time_prev == time - 1)  # Consecutive observations only
# Fit transition model
transition_model <- glm(y ~ y_prev + age + sex + treatment * time + time_varying,
                        family = binomial(link = "logit"),
                        data = longitudinal_data_lag)
trans_coefs <- coef(transition_model)
trans_se <- sqrt(diag(vcov(transition_model)))
trans_or <- exp(trans_coefs)
trans_or_lower <- exp(trans_coefs - 1.96 * trans_se)
trans_or_upper <- exp(trans_coefs + 1.96 * trans_se)
trans_pval <- summary(transition_model)$coefficients[, 4]
trans_results <- data.frame(
  Variable = names(trans_coefs),
  Estimate = round(trans_coefs, 4),
  SE = round(trans_se, 4),
  OR = round(trans_or, 4),
  OR_Lower = round(trans_or_lower, 4),
  OR_Upper = round(trans_or_upper, 4),
  p_value = round(trans_pval, 4)
)
sink("output/transition_model.txt")
cat("=== Transition Model Results ===\n\n")
cat("Model: P(Y_t = 1 | Y_{t-1}, X)\n\n")
print(trans_results, row.names = FALSE)
cat("\nKey finding:\n")
cat("- Previous state (y_prev) OR =", round(trans_or["y_prev"], 3), "\n")
cat("  Strong state dependence: subjects in state 1 are",
    round(trans_or["y_prev"], 1), "times more likely to remain in state 1\n")
sink()
# Figure 11.7: Transition probabilities
trans_pred_grid <- expand.grid(
  y_prev = c(0, 1),
  time = 2:5,
  treatment = c("Control", "Treatment"),
  age = mean(longitudinal_data_lag$age),
  sex = "Female",
  time_varying = 0
)
trans_pred_grid$pred_prob <- predict(transition_model, newdata = trans_pred_grid, type = "response")
trans_pred_grid$y_prev_label <- factor(trans_pred_grid$y_prev,
                                       labels = c("From State 0", "From State 1"))
fig11_7 <- ggplot(trans_pred_grid, aes(x = time, y = pred_prob,
                                       linetype = treatment, shape = y_prev_label)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_point(size = 2.5, colour = "black") +
  scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "dashed")) +
  scale_shape_manual(values = c("From State 0" = 16, "From State 1" = 17)) +
  labs(title = "Transition Probabilities to State 1",
       subtitle = "P(Y_t = 1 | Y_{t-1}, covariates)",
       x = "Time", y = "Probability of Transitioning to State 1",
       linetype = "Treatment", shape = "Previous State") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig11_7_transition_probabilities.jpeg", fig11_7, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_7_transition_probabilities.jpeg\n")
# ============================================================================
# Section 11.8: Missing Data Analysis
# ============================================================================
cat("Analyzing impact of missing data...\n")
# Create dataset with MAR missing data
set.seed(789)
longitudinal_missing <- longitudinal_data %>%
  group_by(subject_id) %>%
  mutate(
    y_prev = lag(y, 1),
    # Missingness depends on previous outcome (MAR)
    miss_prob = ifelse(!is.na(y_prev) & y_prev == 1, 0.25, 0.08),
    y_observed = ifelse(runif(n()) < miss_prob, NA, y)
  ) %>%
  ungroup()
n_missing <- sum(is.na(longitudinal_missing$y_observed))
pct_missing <- round(100 * n_missing / nrow(longitudinal_missing), 1)
# Complete case analysis
complete_data <- longitudinal_missing %>% filter(!is.na(y_observed))
gee_complete <- geeglm(y_observed ~ age + sex + treatment * time + time_varying,
                       id = subject_id,
                       data = complete_data,
                       family = binomial(link = "logit"),
                       corstr = "exchangeable")
# Compare with full data analysis
comparison_missing <- data.frame(
  Variable = names(coef(gee_exch)),
  Full_Data = round(coef(gee_exch), 4),
  Complete_Case = round(coef(gee_complete), 4)
)
comparison_missing$Difference <- round(comparison_missing$Complete_Case - comparison_missing$Full_Data, 4)
comparison_missing$Pct_Bias <- round(100 * comparison_missing$Difference / comparison_missing$Full_Data, 1)
sink("output/missing_data_analysis.txt")
cat("=== Missing Data Analysis ===\n\n")
cat("Missing data mechanism: MAR (depends on previous outcome)\n")
cat("Total observations:", nrow(longitudinal_missing), "\n")
cat("Missing observations:", n_missing, "(", pct_missing, "%)\n\n")
cat("--- Coefficient Comparison ---\n")
print(comparison_missing, row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Complete case analysis may be biased under MAR\n")
cat("- Bias is typically toward null (coefficients attenuated)\n")
cat("- Consider multiple imputation or weighted GEE for MAR data\n")
sink()
# Figure 11.8: Missing data impact
missing_plot_data <- comparison_missing[-1, ] %>%
  select(Variable, Full_Data, Complete_Case) %>%
  pivot_longer(cols = c(Full_Data, Complete_Case), names_to = "Analysis", values_to = "Estimate")
fig11_8 <- ggplot(missing_plot_data, aes(x = Variable, y = Estimate, fill = Analysis)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Complete_Case" = "grey70", "Full_Data" = "grey30"),
                    labels = c("Complete Case", "Full Data")) +
  coord_flip() +
  labs(title = "Impact of Missing Data on Coefficient Estimates",
       subtitle = paste0(pct_missing, "% missing under MAR mechanism"),
       x = "", y = "Coefficient Estimate") +
  theme(legend.position = "bottom")
ggsave("figures/fig11_8_missing_data_impact.jpeg", fig11_8, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_8_missing_data_impact.jpeg\n")
# ============================================================================
# Section 11.9: Model Diagnostics
# ============================================================================
cat("Performing model diagnostics...\n")
# Residual analysis for GEE
longitudinal_data$gee_fitted <- predict(gee_exch, type = "response")
longitudinal_data$gee_resid <- residuals(gee_exch, type = "pearson")
# Marginal predicted vs observed by decile
longitudinal_data$pred_decile <- cut(longitudinal_data$gee_fitted,
                                     breaks = quantile(longitudinal_data$gee_fitted, probs = seq(0, 1, 0.1)),
                                     include.lowest = TRUE, labels = 1:10)
calibration_data <- longitudinal_data %>%
  group_by(pred_decile) %>%
  summarise(
    n = n(),
    observed = mean(y),
    predicted = mean(gee_fitted),
    .groups = "drop"
  )
sink("output/model_diagnostics.txt")
cat("=== Model Diagnostics ===\n\n")
cat("--- GEE Calibration by Decile ---\n")
print(as.data.frame(calibration_data))
cat("\n--- Residual Summary ---\n")
cat("Mean Pearson residual:", round(mean(longitudinal_data$gee_resid), 4), "\n")
cat("SD Pearson residual:", round(sd(longitudinal_data$gee_resid), 4), "\n")
sink()
# Figure 11.9: Calibration plot
fig11_9 <- ggplot(calibration_data, aes(x = predicted, y = observed)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "GEE Calibration Plot",
       subtitle = "Observed vs. predicted probabilities by decile",
       x = "Mean Predicted Probability", y = "Observed Proportion",
       size = "n") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1))
ggsave("figures/fig11_9_calibration.jpeg", fig11_9, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig11_9_calibration.jpeg\n")
# ============================================================================
# Section 11.10: Summary
# ============================================================================
sink("output/chapter11_summary.txt")
cat("=== Chapter 11 Summary: Logistic Regression for Longitudinal Data ===\n\n")
cat("SIMULATED DATA:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Subjects:", n_subjects, "\n")
cat("Total observations:", n_obs, "\n")
cat("Mean obs per subject:", round(mean(obs_per_subject), 2), "\n")
cat("Overall event rate:", round(mean(longitudinal_data$y), 3), "\n\n")
cat("GEE RESULTS (Exchangeable Correlation):\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Estimated correlation:", round(summary(gee_exch)$corr[1, 1], 4), "\n")
cat("Key effects:\n")
cat("  Treatment: OR =", round(exp(coef(gee_exch)["treatmentTreatment"]), 3), "\n")
cat("  Time: OR =", round(exp(coef(gee_exch)["time"]), 3), "\n")
cat("  Treatment x Time: OR =", round(exp(coef(gee_exch)["treatmentTreatment:time"]), 3), "\n\n")
cat("GLMM RESULTS (Random Intercept):\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Random intercept SD:", round(re_var$sdcor[1], 4), "\n")
cat("Key effects:\n")
cat("  Treatment: OR =", round(exp(fixef(glmm_ri)["treatmentTreatment"]), 3), "\n")
cat("  Time: OR =", round(exp(fixef(glmm_ri)["time"]), 3), "\n")
cat("  Treatment x Time: OR =", round(exp(fixef(glmm_ri)["treatmentTreatment:time"]), 3), "\n\n")
cat("TRANSITION MODEL:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Previous state (y_prev) OR:", round(trans_or["y_prev"], 3), "\n")
cat("Strong state dependence observed\n\n")
cat("MISSING DATA ANALYSIS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Missing rate:", pct_missing, "%\n")
cat("Complete case analysis shows potential bias under MAR\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 11 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig11_1_event_rates_time.jpeg\n")
cat("  - fig11_2_qic_comparison.jpeg\n")
cat("  - fig11_3_gee_forest_plot.jpeg\n")
cat("  - fig11_4_random_effects.jpeg\n")
cat("  - fig11_5_gee_vs_glmm.jpeg\n")
cat("  - fig11_6_predicted_probabilities.jpeg\n")
cat("  - fig11_7_transition_probabilities.jpeg\n")
cat("  - fig11_8_missing_data_impact.jpeg\n")
cat("  - fig11_9_calibration.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - data_summary.txt\n")
cat("  - gee_comparison.txt\n")
cat("  - gee_results.txt\n")
cat("  - glmm_results.txt\n")
cat("  - glmm_coefficients.txt\n")
cat("  - gee_vs_glmm.txt\n")
cat("  - predicted_probabilities.txt\n")
cat("  - transition_model.txt\n")
cat("  - missing_data_analysis.txt\n")
cat("  - model_diagnostics.txt\n")
cat("  - chapter11_summary.txt\n")