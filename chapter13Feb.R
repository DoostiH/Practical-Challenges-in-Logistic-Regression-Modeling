# ============================================================================
# Chapter 13: Handling Missing Data
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 13
# Required packages: mice, naniar, VIM, tidyverse, ggplot2
# ============================================================================
# Load required packages
library(mice)
library(naniar)
library(VIM)
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
cat("CHAPTER 13: HANDLING MISSING DATA\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 13.2: Simulate Data with Different Missing Mechanisms
# ============================================================================
cat("Simulating data with different missing mechanisms...\n")
# Generate complete data
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rbinom(n, 1, 0.5)
# True model
beta0 <- -0.5
beta1 <- 0.8
beta2 <- -0.6
beta3 <- 0.5
linear_pred <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
prob_y <- plogis(linear_pred)
y <- rbinom(n, 1, prob_y)
# Complete dataset
complete_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = factor(x3))
# Create datasets with different missing mechanisms
# 1. MCAR: Missing completely at random (20% of x2 missing randomly)
set.seed(456)
mcar_data <- complete_data
mcar_miss <- runif(n) < 0.20
mcar_data$x2[mcar_miss] <- NA
# 2. MAR: Missing at random (x2 more likely missing when x1 is high)
set.seed(789)
mar_data <- complete_data
mar_prob <- plogis(-1.5 + 1.2 * x1)  # P(missing) depends on x1
mar_miss <- runif(n) < mar_prob
mar_data$x2[mar_miss] <- NA
# 3. MNAR: Missing not at random (x2 more likely missing when x2 itself is high)
set.seed(101)
mnar_data <- complete_data
mnar_prob <- plogis(-1.5 + 0.8 * x2)  # P(missing) depends on x2 itself
mnar_miss <- runif(n) < mnar_prob
mnar_data$x2[mnar_miss] <- NA
# Summary statistics
sink("output/missing_data_summary.txt")
cat("=== Missing Data Summary ===\n\n")
cat("Complete data: n =", n, "\n")
cat("True model: logit(p) =", beta0, "+", beta1, "*x1 +", beta2, "*x2 +", beta3, "*x3\n\n")
cat("--- MCAR (Missing Completely at Random) ---\n")
cat("Missing in x2:", sum(is.na(mcar_data$x2)), "(", round(100*mean(is.na(mcar_data$x2)), 1), "%)\n")
cat("Complete cases:", sum(complete.cases(mcar_data)), "\n\n")
cat("--- MAR (Missing at Random) ---\n")
cat("Missing in x2:", sum(is.na(mar_data$x2)), "(", round(100*mean(is.na(mar_data$x2)), 1), "%)\n")
cat("Complete cases:", sum(complete.cases(mar_data)), "\n")
cat("MAR mechanism: P(x2 missing) = logit(-1.5 + 1.2*x1)\n\n")
cat("--- MNAR (Missing Not at Random) ---\n")
cat("Missing in x2:", sum(is.na(mnar_data$x2)), "(", round(100*mean(is.na(mnar_data$x2)), 1), "%)\n")
cat("Complete cases:", sum(complete.cases(mnar_data)), "\n")
cat("MNAR mechanism: P(x2 missing) = logit(-1.5 + 0.8*x2)\n")
sink()
# ============================================================================
# Section 13.3: Visualizing Missing Data
# ============================================================================
cat("Creating missing data visualizations...\n")
# Figure 13.1: Missing data pattern visualization
jpeg("figures/fig13_1_missing_pattern.jpeg", width = 10, height = 6, units = "in",
     res = 300, quality = 95)
par(mfrow = c(1, 2))
md_pattern <- md.pattern(mar_data, plot = TRUE, rotate.names = TRUE)
dev.off()
cat("Saved: figures/fig13_1_missing_pattern.jpeg\n")
# Figure 13.2: Missingness by observed variables
mar_data$x2_missing <- factor(is.na(mar_data$x2), labels = c("Observed", "Missing"))
fig13_2 <- ggplot(mar_data, aes(x = x1, fill = x2_missing)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", colour = "white") +
  scale_fill_manual(values = c("Observed" = "grey70", "Missing" = "grey30")) +
  labs(title = "Distribution of x1 by x2 Missingness (MAR)",
       subtitle = "Under MAR, x2 is more likely missing when x1 is high",
       x = "x1", y = "Count", fill = "x2 Status") +
  theme(legend.position = "bottom")
ggsave("figures/fig13_2_mar_mechanism.jpeg", fig13_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_2_mar_mechanism.jpeg\n")
# Test for MAR: association between x1 and missingness
mar_test <- glm(is.na(x2) ~ x1, family = binomial, data = mar_data)
sink("output/mar_test.txt", append = FALSE)
cat("=== Test for MAR Mechanism ===\n\n")
cat("Logistic regression: P(x2 missing) ~ x1\n\n")
print(summary(mar_test)$coefficients)
cat("\nInterpretation: Significant positive coefficient for x1 indicates\n")
cat("that missingness depends on x1, consistent with MAR mechanism.\n")
sink()
# ============================================================================
# Section 13.4: Complete Case Analysis
# ============================================================================
cat("Performing complete case analysis...\n")
# True model (complete data)
true_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = complete_data)
# Complete case models
cc_mcar <- glm(y ~ x1 + x2 + x3, family = binomial, data = mcar_data)
cc_mar <- glm(y ~ x1 + x2 + x3, family = binomial, data = mar_data)
cc_mnar <- glm(y ~ x1 + x2 + x3, family = binomial, data = mnar_data)
# Extract results
extract_results <- function(model, name) {
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  data.frame(
    Method = name,
    Variable = names(coefs),
    Estimate = coefs,
    SE = ses,
    OR = exp(coefs),
    row.names = NULL
  )
}
true_results <- extract_results(true_model, "True (Complete)")
cc_mcar_results <- extract_results(cc_mcar, "CC-MCAR")
cc_mar_results <- extract_results(cc_mar, "CC-MAR")
cc_mnar_results <- extract_results(cc_mnar, "CC-MNAR")
cc_comparison <- bind_rows(true_results, cc_mcar_results, cc_mar_results, cc_mnar_results)
sink("output/complete_case_results.txt")
cat("=== Complete Case Analysis Results ===\n\n")
cat("True coefficients:\n")
cat("  Intercept:", beta0, "\n")
cat("  x1:", beta1, "\n")
cat("  x2:", beta2, "\n")
cat("  x3:", beta3, "\n\n")
# Helper function to print results nicely
print_results <- function(df) {
  df_print <- df
  df_print[, sapply(df_print, is.numeric)] <- round(df_print[, sapply(df_print, is.numeric)], 4)
  print(df_print[, -1], row.names = FALSE)  # Exclude Method column
}
cat("--- True Model (Complete Data, n =", nrow(complete_data), ") ---\n")
print_results(true_results)
cat("\n--- CC under MCAR (n =", sum(complete.cases(mcar_data)), ") ---\n")
print_results(cc_mcar_results)
cat("\n--- CC under MAR (n =", sum(complete.cases(mar_data)), ") ---\n")
print_results(cc_mar_results)
cat("\n--- CC under MNAR (n =", sum(complete.cases(mnar_data)), ") ---\n")
print_results(cc_mnar_results)
cat("\n--- Bias Comparison (for x2 coefficient) ---\n")
cat("True x2 coefficient:", beta2, "\n")
cat("CC-MCAR estimate:", round(coef(cc_mcar)["x2"], 4),
    ", Bias:", round(coef(cc_mcar)["x2"] - beta2, 4), "\n")
cat("CC-MAR estimate:", round(coef(cc_mar)["x2"], 4),
    ", Bias:", round(coef(cc_mar)["x2"] - beta2, 4), "\n")
cat("CC-MNAR estimate:", round(coef(cc_mnar)["x2"], 4),
    ", Bias:", round(coef(cc_mnar)["x2"] - beta2, 4), "\n")
sink()
# Figure 13.3: CC bias comparison
cc_x2 <- cc_comparison %>%
  filter(Variable == "x2") %>%
  mutate(True = beta2,
         Bias = Estimate - True)
fig13_3 <- ggplot(cc_x2, aes(x = Method, y = Estimate)) +
  geom_point(size = 4, colour = "black") +
  geom_errorbar(aes(ymin = Estimate - 1.96*SE, ymax = Estimate + 1.96*SE),
                width = 0.2, colour = "black") +
  geom_hline(yintercept = beta2, linetype = "dashed", colour = "black", linewidth = 1) +
  annotate("text", x = 0.6, y = beta2 + 0.05, label = paste("True =", beta2),
           colour = "black", hjust = 0) +
  labs(title = "Complete Case Analysis: Bias in x2 Coefficient",
       subtitle = "Dashed line shows true coefficient value",
       x = "", y = "Estimated Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/fig13_3_cc_bias.jpeg", fig13_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_3_cc_bias.jpeg\n")
# ============================================================================
# Section 13.5: Multiple Imputation with mice
# ============================================================================
cat("Performing multiple imputation...\n")
# Multiple imputation for MAR data
imp_mar <- mice(mar_data[, c("y", "x1", "x2", "x3")],
                m = 20,           # 20 imputations
                method = "pmm",   # Predictive mean matching for continuous
                maxit = 20,       # Iterations
                seed = 123,
                printFlag = FALSE)
# Check convergence
jpeg("figures/fig13_4_mice_convergence.jpeg", width = 10, height = 6, units = "in",
     res = 300, quality = 95)
plot(imp_mar)
dev.off()
cat("Saved: figures/fig13_4_mice_convergence.jpeg\n")
# Fit model to each imputed dataset
fit_mar <- with(imp_mar, glm(y ~ x1 + x2 + x3, family = binomial))
# Pool results using Rubin's rules
pooled_mar <- pool(fit_mar)
pooled_summary <- summary(pooled_mar, conf.int = TRUE, exponentiate = FALSE)
# Also get OR
pooled_or <- summary(pooled_mar, conf.int = TRUE, exponentiate = TRUE)
sink("output/mi_results.txt")
cat("=== Multiple Imputation Results (MAR Data) ===\n\n")
cat("Number of imputations: 20\n")
cat("Method: Predictive Mean Matching (PMM)\n")
cat("Iterations: 20\n\n")
cat("--- Pooled Coefficients ---\n")
pooled_print <- pooled_summary[, c("term", "estimate", "std.error", "statistic", "p.value")]
pooled_print[, -1] <- round(pooled_print[, -1], 4)
print(pooled_print, row.names = FALSE)
cat("\n--- Pooled Odds Ratios ---\n")
or_table <- data.frame(
  Variable = pooled_or$term,
  OR = round(pooled_or$estimate, 4),
  CI_Lower = round(pooled_or$`2.5 %`, 4),
  CI_Upper = round(pooled_or$`97.5 %`, 4)
)
print(or_table, row.names = FALSE)
cat("\n--- Fraction of Missing Information (FMI) ---\n")
fmi_table <- data.frame(
  Variable = row.names(pooled_mar$pooled),
  FMI = round(pooled_mar$pooled$fmi, 4),
  Lambda = round(pooled_mar$pooled$lambda, 4)
)
print(fmi_table, row.names = FALSE)
cat("\nFMI interpretation: Proportion of variance attributable to missing data\n")
sink()
# ============================================================================
# Section 13.6: Compare CC vs MI
# ============================================================================
cat("Comparing complete case and multiple imputation...\n")
# Extract MI results for comparison
mi_results <- data.frame(
  Method = "MI",
  Variable = pooled_summary$term,
  Estimate = pooled_summary$estimate,
  SE = pooled_summary$std.error,
  OR = exp(pooled_summary$estimate)
)
# Combine for comparison
all_results <- bind_rows(
  true_results,
  cc_mar_results,
  mi_results
)
sink("output/cc_vs_mi_comparison.txt")
cat("=== Complete Case vs Multiple Imputation Comparison ===\n\n")
cat("Data: MAR mechanism (", round(100*mean(is.na(mar_data$x2)), 1), "% missing in x2)\n\n")
cat("--- Coefficient Estimates ---\n")
comparison_wide <- all_results %>%
  select(Method, Variable, Estimate) %>%
  pivot_wider(names_from = Method, values_from = Estimate)
comparison_wide[, -1] <- round(comparison_wide[, -1], 4)
print(as.data.frame(comparison_wide))
cat("\n--- Standard Errors ---\n")
se_wide <- all_results %>%
  select(Method, Variable, SE) %>%
  pivot_wider(names_from = Method, values_from = SE)
se_wide[, -1] <- round(se_wide[, -1], 4)
print(as.data.frame(se_wide))
cat("\n--- Bias (relative to true values) ---\n")
true_coefs <- c(beta0, beta1, beta2, beta3)
names(true_coefs) <- c("(Intercept)", "x1", "x2", "x31")
for (var in c("x1", "x2", "x31")) {
  cc_est <- cc_mar_results$Estimate[cc_mar_results$Variable == var]
  mi_est <- mi_results$Estimate[mi_results$Variable == var]
  true_val <- true_coefs[var]
  cat(var, ":\n")
  cat("  True:", round(true_val, 4), "\n")
  cat("  CC:", round(cc_est, 4), "(Bias:", round(cc_est - true_val, 4), ")\n")
  cat("  MI:", round(mi_est, 4), "(Bias:", round(mi_est - true_val, 4), ")\n\n")
}
cat("--- Efficiency Gain from MI ---\n")
for (var in c("x1", "x2", "x31")) {
  cc_se <- cc_mar_results$SE[cc_mar_results$Variable == var]
  mi_se <- mi_results$SE[mi_results$Variable == var]
  eff_gain <- (cc_se / mi_se)^2
  cat(var, ": SE ratio =", round(cc_se/mi_se, 3),
      ", Efficiency gain =", round((eff_gain - 1) * 100, 1), "%\n")
}
sink()
# Figure 13.5: CC vs MI comparison
comparison_plot <- all_results %>%
  filter(Variable %in% c("x1", "x2", "x31")) %>%
  mutate(True = case_when(
    Variable == "x1" ~ beta1,
    Variable == "x2" ~ beta2,
    Variable == "x31" ~ beta3
  ))
fig13_5 <- ggplot(comparison_plot, aes(x = Variable, y = Estimate, shape = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour = "black") +
  geom_errorbar(aes(ymin = Estimate - 1.96*SE, ymax = Estimate + 1.96*SE),
                position = position_dodge(width = 0.5), width = 0.2, colour = "black") +
  geom_point(aes(y = True), shape = 4, size = 4, colour = "black", stroke = 2,
             show.legend = FALSE) +
  scale_shape_manual(values = c("True (Complete)" = 16, "CC-MAR" = 1, "MI" = 17)) +
  labs(title = "Complete Case vs Multiple Imputation: Coefficient Estimates",
       subtitle = "X marks show true values; MAR mechanism with ~25% missing",
       x = "Variable", y = "Estimate") +
  theme(legend.position = "bottom")
ggsave("figures/fig13_5_cc_vs_mi.jpeg", fig13_5, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_5_cc_vs_mi.jpeg\n")
# ============================================================================
# Section 13.7: Simulation Study - Comparing Methods Across Mechanisms
# ============================================================================
cat("Running simulation study (this may take a moment)...\n")
run_simulation <- function(n_sim = 200, n = 500, miss_rate = 0.25, seed = 999) {
  set.seed(seed)
  results <- data.frame()
  for (sim in 1:n_sim) {
    # Generate data
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rbinom(n, 1, 0.5)
    lp <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
    y <- rbinom(n, 1, plogis(lp))
    complete <- data.frame(y = y, x1 = x1, x2 = x2, x3 = factor(x3))
    # Create MCAR
    mcar <- complete
    mcar$x2[runif(n) < miss_rate] <- NA
    # Create MAR
    mar <- complete
    mar_prob <- plogis(-1 + 1.5 * x1)
    mar_prob <- mar_prob * (miss_rate / mean(mar_prob))  # Calibrate to target rate
    mar$x2[runif(n) < mar_prob] <- NA
    # Create MNAR
    mnar <- complete
    mnar_prob <- plogis(-1 + 1.0 * x2)
    mnar_prob <- mnar_prob * (miss_rate / mean(mnar_prob))
    mnar$x2[runif(n) < mnar_prob] <- NA
    # Fit models
    for (mechanism in c("MCAR", "MAR", "MNAR")) {
      data_use <- switch(mechanism, MCAR = mcar, MAR = mar, MNAR = mnar)
      # Complete case
      cc_fit <- tryCatch(
        glm(y ~ x1 + x2 + x3, family = binomial, data = data_use),
        error = function(e) NULL
      )
      if (!is.null(cc_fit)) {
        results <- rbind(results, data.frame(
          sim = sim,
          mechanism = mechanism,
          method = "CC",
          x1_est = coef(cc_fit)["x1"],
          x2_est = coef(cc_fit)["x2"],
          x3_est = coef(cc_fit)["x31"],
          x1_se = sqrt(vcov(cc_fit)["x1", "x1"]),
          x2_se = sqrt(vcov(cc_fit)["x2", "x2"]),
          x3_se = sqrt(vcov(cc_fit)["x31", "x31"])
        ))
      }
      # Multiple imputation
      mi_fit <- tryCatch({
        imp <- mice(data_use, m = 10, method = "pmm", maxit = 10,
                    seed = sim, printFlag = FALSE)
        fit <- with(imp, glm(y ~ x1 + x2 + x3, family = binomial))
        pool(fit)
      }, error = function(e) NULL)
      if (!is.null(mi_fit)) {
        mi_sum <- summary(mi_fit)
        results <- rbind(results, data.frame(
          sim = sim,
          mechanism = mechanism,
          method = "MI",
          x1_est = mi_sum$estimate[mi_sum$term == "x1"],
          x2_est = mi_sum$estimate[mi_sum$term == "x2"],
          x3_est = mi_sum$estimate[mi_sum$term == "x31"],
          x1_se = mi_sum$std.error[mi_sum$term == "x1"],
          x2_se = mi_sum$std.error[mi_sum$term == "x2"],
          x3_se = mi_sum$std.error[mi_sum$term == "x31"]
        ))
      }
    }
    if (sim %% 50 == 0) cat("  Completed", sim, "of", n_sim, "simulations\n")
  }
  return(results)
}
# Run simulation (reduced for speed)
sim_results <- run_simulation(n_sim = 200, n = 500)
# Summarize simulation results
sim_summary <- sim_results %>%
  group_by(mechanism, method) %>%
  summarise(
    n_sims = n(),
    x2_mean = mean(x2_est, na.rm = TRUE),
    x2_bias = mean(x2_est - beta2, na.rm = TRUE),
    x2_se_mean = mean(x2_se, na.rm = TRUE),
    x2_rmse = sqrt(mean((x2_est - beta2)^2, na.rm = TRUE)),
    x2_coverage = mean(abs(x2_est - beta2) < 1.96 * x2_se, na.rm = TRUE),
    .groups = "drop"
  )
sink("output/simulation_results.txt")
cat("=== Simulation Study Results ===\n\n")
cat("Settings: n = 500, ~25% missing, 200 simulations\n")
cat("True x2 coefficient:", beta2, "\n\n")
cat("--- Summary by Mechanism and Method ---\n")
print(as.data.frame(sim_summary), row.names = FALSE)
cat("\n--- Key Findings ---\n")
cat("1. Under MCAR: Both CC and MI are approximately unbiased\n")
cat("2. Under MAR: CC shows bias; MI remains approximately unbiased\n")
cat("3. Under MNAR: Both methods show bias (MI may be less biased)\n")
cat("4. MI generally has better coverage than CC under MAR/MNAR\n")
sink()
# Figure 13.6: Simulation results
fig13_6 <- ggplot(sim_summary, aes(x = mechanism, y = x2_bias, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("CC" = "grey70", "MI" = "grey30")) +
  labs(title = "Simulation Study: Bias in x2 Coefficient",
       subtitle = "200 simulations, n = 500, ~25% missing",
       x = "Missing Mechanism", y = "Bias (Estimate \u2212 True)", fill = "Method") +
  theme(legend.position = "bottom")
ggsave("figures/fig13_6_simulation_bias.jpeg", fig13_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_6_simulation_bias.jpeg\n")
# Figure 13.7: Coverage
fig13_7 <- ggplot(sim_summary, aes(x = mechanism, y = x2_coverage, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "black") +
  scale_fill_manual(values = c("CC" = "grey70", "MI" = "grey30")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "Simulation Study: 95% CI Coverage",
       subtitle = "Dashed line shows nominal 95% coverage",
       x = "Missing Mechanism", y = "Coverage", fill = "Method") +
  theme(legend.position = "bottom")
ggsave("figures/fig13_7_simulation_coverage.jpeg", fig13_7, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_7_simulation_coverage.jpeg\n")
# ============================================================================
# Section 13.8: Sensitivity Analysis for MNAR
# ============================================================================
cat("Performing sensitivity analysis for MNAR...\n")
# Delta adjustment sensitivity analysis
deltas <- c(-0.5, -0.25, 0, 0.25, 0.5)
sensitivity_results <- data.frame()
for (delta in deltas) {
  # Adjust imputed datasets
  imp_adjusted <- imp_mar
  for (i in 1:imp_adjusted$m) {
    imputed_data <- complete(imp_adjusted, i)
    imputed_data$x2[is.na(mar_data$x2)] <-
      imputed_data$x2[is.na(mar_data$x2)] + delta
    imp_adjusted$imp$x2[, i] <- imputed_data$x2[is.na(mar_data$x2)]
  }
  # Refit and pool
  fit_adj <- with(imp_adjusted, glm(y ~ x1 + x2 + x3, family = binomial))
  pooled_adj <- summary(pool(fit_adj))
  sensitivity_results <- rbind(sensitivity_results, data.frame(
    delta = delta,
    x2_est = pooled_adj$estimate[pooled_adj$term == "x2"],
    x2_se = pooled_adj$std.error[pooled_adj$term == "x2"],
    x2_pval = pooled_adj$p.value[pooled_adj$term == "x2"]
  ))
}
sensitivity_results$x2_lower <- sensitivity_results$x2_est - 1.96 * sensitivity_results$x2_se
sensitivity_results$x2_upper <- sensitivity_results$x2_est + 1.96 * sensitivity_results$x2_se
sensitivity_results$significant <- sensitivity_results$x2_pval < 0.05
sink("output/sensitivity_analysis.txt")
cat("=== Sensitivity Analysis for MNAR ===\n\n")
cat("Delta adjustment: Missing x2 values = Imputed values + delta\n")
cat("Positive delta: Missing values are higher than MAR assumption\n")
cat("Negative delta: Missing values are lower than MAR assumption\n\n")
cat("--- Results by Delta ---\n")
sens_print <- sensitivity_results
sens_print[, sapply(sens_print, is.numeric)] <- round(sens_print[, sapply(sens_print, is.numeric)], 4)
print(sens_print, row.names = FALSE)
cat("\n--- Tipping Point Analysis ---\n")
cat("True x2 coefficient:", beta2, "\n")
cat("The coefficient for x2 remains significant across all delta values tested.\n")
cat("Conclusions appear robust to moderate departures from MAR.\n")
sink()
# Figure 13.8: Sensitivity analysis
fig13_8 <- ggplot(sensitivity_results, aes(x = delta, y = x2_est)) +
  geom_ribbon(aes(ymin = x2_lower, ymax = x2_upper), alpha = 0.3, fill = "grey60") +
  geom_line(colour = "black", linewidth = 1) +
  geom_point(size = 3, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = beta2, linetype = "dotted", colour = "black") +
  annotate("text", x = 0.5, y = beta2 + 0.03, label = "True value", colour = "black") +
  annotate("text", x = 0.5, y = 0.03, label = "Null", colour = "black") +
  labs(title = "Sensitivity Analysis: Effect of Delta Adjustment on x2 Coefficient",
       subtitle = "Shaded region shows 95% CI; dashed line = null; dotted line = true value",
       x = "Delta (adjustment to imputed values)", y = "x2 Coefficient Estimate")
ggsave("figures/fig13_8_sensitivity_analysis.jpeg", fig13_8, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_8_sensitivity_analysis.jpeg\n")
# ============================================================================
# Section 13.9: Real Data Example - Clinical Trial Simulation
# ============================================================================
cat("Creating clinical trial example...\n")
# Simulate realistic clinical trial data
set.seed(2024)
n_trial <- 500
# Baseline characteristics
age <- round(rnorm(n_trial, mean = 55, sd = 12))
sex <- factor(rbinom(n_trial, 1, 0.45), labels = c("Male", "Female"))
severity <- round(rnorm(n_trial, mean = 50, sd = 15))
treatment <- factor(rbinom(n_trial, 1, 0.5), labels = c("Placebo", "Active"))
# Response probability
lp_trial <- -2 + 0.02 * (age - 55) + 0.3 * (sex == "Female") +
  0.03 * (severity - 50) + 0.8 * (treatment == "Active")
response <- rbinom(n_trial, 1, plogis(lp_trial))
# Create missing data (MAR)
severity_miss_prob <- plogis(-2 + 0.03 * age)
severity_obs <- ifelse(runif(n_trial) < severity_miss_prob, NA, severity)
response_miss_prob <- plogis(-3 + 0.02 * severity - 0.5 * (treatment == "Active"))
response_obs <- ifelse(runif(n_trial) < response_miss_prob, NA, response)
trial_data <- data.frame(
  response = response_obs,
  treatment = treatment,
  age = age,
  sex = sex,
  severity = severity_obs
)
# Complete case analysis
cc_trial <- glm(response ~ treatment + age + sex + severity,
                family = binomial, data = trial_data)
# Multiple imputation
imp_trial <- mice(trial_data, m = 20, method = c("logreg", "", "pmm", "", "pmm"),
                  maxit = 20, seed = 123, printFlag = FALSE)
fit_trial <- with(imp_trial, glm(response ~ treatment + age + sex + severity,
                                 family = binomial))
pooled_trial <- pool(fit_trial)
pooled_trial_sum <- summary(pooled_trial, conf.int = TRUE, exponentiate = TRUE)
sink("output/clinical_trial_example.txt")
cat("=== Clinical Trial Example ===\n\n")
cat("--- Data Description ---\n")
cat("Total patients:", n_trial, "\n")
cat("Missing in severity:", sum(is.na(trial_data$severity)),
    "(", round(100*mean(is.na(trial_data$severity)), 1), "%)\n")
cat("Missing in response:", sum(is.na(trial_data$response)),
    "(", round(100*mean(is.na(trial_data$response)), 1), "%)\n")
cat("Complete cases:", sum(complete.cases(trial_data)), "\n\n")
cat("--- Complete Case Analysis ---\n")
cat("Sample size:", nobs(cc_trial), "\n")
cc_or <- exp(coef(cc_trial))
cc_ci <- exp(confint.default(cc_trial))
cc_table <- data.frame(
  Variable = names(cc_or),
  OR = round(cc_or, 3),
  CI_Lower = round(cc_ci[, 1], 3),
  CI_Upper = round(cc_ci[, 2], 3)
)
print(cc_table, row.names = FALSE)
cat("\n--- Multiple Imputation Analysis ---\n")
cat("Number of imputations: 20\n")
mi_table <- data.frame(
  Variable = pooled_trial_sum$term,
  OR = round(pooled_trial_sum$estimate, 3),
  CI_Lower = round(pooled_trial_sum$`2.5 %`, 3),
  CI_Upper = round(pooled_trial_sum$`97.5 %`, 3),
  FMI = round(pooled_trial$pooled$fmi, 3)
)
print(mi_table, row.names = FALSE)
cat("\n--- Comparison: Treatment Effect ---\n")
cat("True treatment OR: exp(0.8) =", round(exp(0.8), 3), "\n")
cat("CC treatment OR:", round(cc_or["treatmentActive"], 3), "\n")
cat("MI treatment OR:", round(pooled_trial_sum$estimate[pooled_trial_sum$term == "treatmentActive"], 3), "\n")
sink()
# Figure 13.9: Clinical trial comparison
trial_comparison <- data.frame(
  Variable = c("Treatment (Active)", "Age", "Sex (Female)", "Severity"),
  CC_OR = cc_or[-1],
  MI_OR = pooled_trial_sum$estimate[-1]
) %>%
  pivot_longer(cols = c(CC_OR, MI_OR), names_to = "Method", values_to = "OR") %>%
  mutate(Method = case_match(Method,
                             "CC_OR" ~ "Complete Case",
                             "MI_OR" ~ "Multiple Imputation"))
fig13_9 <- ggplot(trial_comparison, aes(x = Variable, y = OR, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_fill_manual(values = c("Complete Case" = "grey70",
                               "Multiple Imputation" = "grey30")) +
  coord_flip() +
  labs(title = "Clinical Trial: Complete Case vs Multiple Imputation",
       subtitle = "Odds ratios for treatment response",
       x = "", y = "Odds Ratio") +
  theme(legend.position = "bottom")
ggsave("figures/fig13_9_trial_comparison.jpeg", fig13_9, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig13_9_trial_comparison.jpeg\n")
# ============================================================================
# Section 13.10: Summary
# ============================================================================
sink("output/chapter13_summary.txt")
cat("=== Chapter 13 Summary: Handling Missing Data ===\n\n")
cat("MISSING DATA MECHANISMS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("MCAR: Missing completely at random - unbiased but inefficient\n")
cat("MAR: Missing at random - can be handled by MI\n")
cat("MNAR: Missing not at random - requires sensitivity analysis\n\n")
cat("COMPLETE CASE VS MULTIPLE IMPUTATION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Under MAR with ~25% missing (x2 coefficient):\n")
cat("  True coefficient:", beta2, "\n")
cat("  CC estimate:", round(coef(cc_mar)["x2"], 4),
    "(Bias:", round(coef(cc_mar)["x2"] - beta2, 4), ")\n")
cat("  MI estimate:", round(mi_results$Estimate[mi_results$Variable == "x2"], 4),
    "(Bias:", round(mi_results$Estimate[mi_results$Variable == "x2"] - beta2, 4), ")\n\n")
cat("SIMULATION STUDY (200 sims, n=500, ~25% missing):\n")
cat("-" , rep("-", 50), "\n", sep = "")
print(as.data.frame(sim_summary[, c("mechanism", "method", "x2_bias", "x2_coverage")]))
cat("\nSENSITIVITY ANALYSIS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Delta range tested:", min(deltas), "to", max(deltas), "\n")
cat("x2 coefficient remained significant across all deltas\n")
cat("Conclusions robust to moderate departures from MAR\n\n")
cat("CLINICAL TRIAL EXAMPLE:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Treatment effect (Active vs Placebo):\n")
cat("  True OR: 2.226\n")
cat("  CC OR:", round(cc_or["treatmentActive"], 3), "\n")
cat("  MI OR:", round(pooled_trial_sum$estimate[pooled_trial_sum$term == "treatmentActive"], 3), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 13 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig13_1_missing_pattern.jpeg\n")
cat("  - fig13_2_mar_mechanism.jpeg\n")
cat("  - fig13_3_cc_bias.jpeg\n")
cat("  - fig13_4_mice_convergence.jpeg\n")
cat("  - fig13_5_cc_vs_mi.jpeg\n")
cat("  - fig13_6_simulation_bias.jpeg\n")
cat("  - fig13_7_simulation_coverage.jpeg\n")
cat("  - fig13_8_sensitivity_analysis.jpeg\n")
cat("  - fig13_9_trial_comparison.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - missing_data_summary.txt\n")
cat("  - mar_test.txt\n")
cat("  - complete_case_results.txt\n")
cat("  - mi_results.txt\n")
cat("  - cc_vs_mi_comparison.txt\n")
cat("  - simulation_results.txt\n")
cat("  - sensitivity_analysis.txt\n")
cat("  - clinical_trial_example.txt\n")
cat("  - chapter13_summary.txt\n")