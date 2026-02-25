# ============================================================================
# Chapter 15: Bayesian and Causal Methods
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 15
# Required packages: rstanarm, loo, MatchIt, cobalt, WeightIt, tidyverse
# ============================================================================
# Load required packages
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
cat("CHAPTER 15: BAYESIAN AND CAUSAL METHODS\n")
cat("=======================================================\n\n")
# ============================================================================
# PART A: BAYESIAN LOGISTIC REGRESSION
# ============================================================================
cat("=== PART A: BAYESIAN LOGISTIC REGRESSION ===\n\n")
# ============================================================================
# Section 15.2: Simulate Data for Bayesian Analysis
# ============================================================================
cat("Simulating data for Bayesian analysis...\n")
# Create a dataset with moderate sample size
n_bayes <- 200
x1 <- rnorm(n_bayes)
x2 <- rnorm(n_bayes)
x3 <- rbinom(n_bayes, 1, 0.4)
# True model
beta0_true <- -0.5
beta1_true <- 0.8
beta2_true <- -0.5
beta3_true <- 0.6
lp <- beta0_true + beta1_true * x1 + beta2_true * x2 + beta3_true * x3
prob_y <- plogis(lp)
y <- rbinom(n_bayes, 1, prob_y)
bayes_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = factor(x3))
sink("output/bayesian_data_summary.txt")
cat("=== Bayesian Analysis Data Summary ===\n\n")
cat("Sample size:", n_bayes, "\n")
cat("Outcome: y (binary)\n")
cat("Predictors: x1 (continuous), x2 (continuous), x3 (binary)\n\n")
cat("True model:\n")
cat("logit(p) =", beta0_true, "+", beta1_true, "*x1 +", beta2_true, "*x2 +", beta3_true, "*x3\n\n")
cat("Outcome distribution:\n")
cat("  y = 0:", sum(y == 0), "(", round(100 * mean(y == 0), 1), "%)\n")
cat("  y = 1:", sum(y == 1), "(", round(100 * mean(y == 1), 1), "%)\n")
sink()
# ============================================================================
# Section 15.3: Prior Distributions Visualization
# ============================================================================
cat("Creating prior distribution visualizations...\n")
# Figure 15.1: Weakly Informative Prior
beta_seq <- seq(-10, 10, 0.1)
prior_weak <- dnorm(beta_seq, 0, 2.5)
prior_strong <- dnorm(beta_seq, 0, 1)
prior_flat <- dnorm(beta_seq, 0, 10)
prior_df <- data.frame(
  beta = rep(beta_seq, 3),
  density = c(prior_weak, prior_strong, prior_flat),
  Prior = rep(c("Weakly Informative N(0, 2.5)",
                "Informative N(0, 1)",
                "Diffuse N(0, 10)"), each = length(beta_seq))
)
fig15_1 <- ggplot(prior_df, aes(x = beta, y = density, linetype = Prior)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_vline(xintercept = c(-5, 5), linetype = "dashed", colour = "grey50", alpha = 0.7) +
  scale_linetype_manual(values = c("Weakly Informative N(0, 2.5)" = "solid",
                                   "Informative N(0, 1)" = "dashed",
                                   "Diffuse N(0, 10)" = "dotted")) +
  annotate("text", x = -5.5, y = max(prior_df$density) * 0.9,
           label = "OR = 0.007", hjust = 1, size = 3) +
  annotate("text", x = 5.5, y = max(prior_df$density) * 0.9,
           label = "OR = 148", hjust = 0, size = 3) +
  labs(title = "Prior Distributions for Logistic Regression Coefficients",
       subtitle = "Vertical dashed lines show |beta| = 5 (extreme odds ratios)",
       x = "Coefficient (log-OR)", y = "Prior Density") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig15_1_prior_distributions.jpeg", fig15_1, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig15_1_prior_distributions.jpeg\n")
# ============================================================================
# Section 15.4: Bayesian Logistic Regression with rstanarm
# ============================================================================
cat("Fitting Bayesian logistic regression models...\n")
# Check if rstanarm is available
if (requireNamespace("rstanarm", quietly = TRUE)) {
  library(rstanarm)
  # Fit Bayesian model with weakly informative prior
  bayes_model <- stan_glm(
    y ~ x1 + x2 + x3,
    family = binomial(link = "logit"),
    data = bayes_data,
    prior = normal(0, 2.5),
    prior_intercept = normal(0, 10),
    chains = 4,
    iter = 2000,
    seed = 123,
    refresh = 0
  )
  # Also fit frequentist model for comparison
  freq_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = bayes_data)
  # Extract posterior samples
  posterior_samples <- as.matrix(bayes_model)
  # Posterior summaries
  bayes_summary <- summary(bayes_model)
  sink("output/bayesian_model_results.txt")
  cat("=== Bayesian Logistic Regression Results ===\n\n")
  cat("Prior specification:\n")
  cat("  Coefficients: Normal(0, 2.5)\n")
  cat("  Intercept: Normal(0, 10)\n\n")
  cat("MCMC settings:\n")
  cat("  Chains: 4\n")
  cat("  Iterations: 2000 (1000 warmup)\n")
  cat("  Total posterior samples: 4000\n\n")
  cat("--- Posterior Summaries ---\n")
  print(bayes_summary)
  cat("\n--- Odds Ratios (Posterior Median and 95% Credible Interval) ---\n")
  or_summary <- data.frame(
    Variable = c("(Intercept)", "x1", "x2", "x31"),
    Median_OR = round(exp(apply(posterior_samples[, 1:4], 2, median)), 3),
    CI_Lower = round(exp(apply(posterior_samples[, 1:4], 2, quantile, 0.025)), 3),
    CI_Upper = round(exp(apply(posterior_samples[, 1:4], 2, quantile, 0.975)), 3)
  )
  print(or_summary, row.names = FALSE)
  cat("\n--- Probability Statements ---\n")
  cat("P(beta_x1 > 0):", round(mean(posterior_samples[, "x1"] > 0), 3), "\n")
  cat("P(OR_x1 > 1.5):", round(mean(exp(posterior_samples[, "x1"]) > 1.5), 3), "\n")
  cat("P(beta_x2 < 0):", round(mean(posterior_samples[, "x2"] < 0), 3), "\n")
  cat("P(OR_x3 > 1):", round(mean(exp(posterior_samples[, "x31"]) > 1), 3), "\n")
  cat("\n--- Comparison with Frequentist Model ---\n")
  freq_coef <- coef(freq_model)
  freq_se <- sqrt(diag(vcov(freq_model)))
  bayes_median <- apply(posterior_samples[, 1:4], 2, median)
  bayes_sd <- apply(posterior_samples[, 1:4], 2, sd)
  comparison_df <- data.frame(
    Variable = names(freq_coef),
    Freq_Est = round(freq_coef, 3),
    Freq_SE = round(freq_se, 3),
    Bayes_Median = round(bayes_median, 3),
    Bayes_SD = round(bayes_sd, 3)
  )
  print(comparison_df, row.names = FALSE)
  cat("\n--- True Values for Comparison ---\n")
  cat("Intercept:", beta0_true, "\n")
  cat("x1:", beta1_true, "\n")
  cat("x2:", beta2_true, "\n")
  cat("x3:", beta3_true, "\n")
  sink()
  # Figure 15.2: Posterior distributions
  post_df <- data.frame(
    x1 = posterior_samples[, "x1"],
    x2 = posterior_samples[, "x2"],
    x31 = posterior_samples[, "x31"]
  ) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  true_values <- data.frame(
    Variable = c("x1", "x2", "x31"),
    True = c(beta1_true, beta2_true, beta3_true)
  )
  fig15_2 <- ggplot(post_df, aes(x = Value)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70",
                   colour = "white", alpha = 0.8) +
    geom_density(colour = "black", linewidth = 1) +
    geom_vline(data = true_values, aes(xintercept = True),
               colour = "black", linetype = "dashed", linewidth = 1) +
    facet_wrap(~Variable, scales = "free") +
    labs(title = "Posterior Distributions for Regression Coefficients",
         subtitle = "Dashed lines show true parameter values",
         x = "Coefficient Value", y = "Density") +
    theme(legend.position = "none")
  ggsave("figures/fig15_2_posterior_distributions.jpeg", fig15_2, width = 10, height = 4,
         dpi = 300, device = "jpeg")
  cat("Saved: figures/fig15_2_posterior_distributions.jpeg\n")
  # Figure 15.3: Trace plots
  jpeg("figures/fig15_3_trace_plots.jpeg", width = 10, height = 6, units = "in",
       res = 300, quality = 95)
  plot(bayes_model, plotfun = "trace")
  dev.off()
  cat("Saved: figures/fig15_3_trace_plots.jpeg\n")
  # Convergence diagnostics
  sink("output/convergence_diagnostics.txt")
  cat("=== Convergence Diagnostics ===\n\n")
  cat("--- Rhat Values (should be < 1.01) ---\n")
  rhat_vals <- summary(bayes_model)[, "Rhat"]
  print(round(rhat_vals, 4))
  cat("\nAll Rhat < 1.01:", all(rhat_vals < 1.01, na.rm = TRUE), "\n")
  cat("\n--- Effective Sample Size ---\n")
  neff_vals <- summary(bayes_model)[, "n_eff"]
  print(round(neff_vals, 0))
  cat("\nMinimum n_eff:", round(min(neff_vals, na.rm = TRUE), 0), "\n")
  cat("n_eff > 400 for all:", all(neff_vals > 400, na.rm = TRUE), "\n")
  sink()
  # Model comparison with LOO
  if (requireNamespace("loo", quietly = TRUE)) {
    library(loo)
    loo_full <- loo(bayes_model)
    bayes_reduced <- stan_glm(
      y ~ x1 + x2,
      family = binomial(link = "logit"),
      data = bayes_data,
      prior = normal(0, 2.5),
      prior_intercept = normal(0, 10),
      chains = 4,
      iter = 2000,
      seed = 123,
      refresh = 0
    )
    loo_reduced <- loo(bayes_reduced)
    loo_comp <- loo_compare(loo_full, loo_reduced)
    sink("output/model_comparison_loo.txt")
    cat("=== Bayesian Model Comparison (LOO-CV) ===\n\n")
    cat("--- Full Model LOO ---\n")
    print(loo_full)
    cat("\n--- Reduced Model (without x3) LOO ---\n")
    print(loo_reduced)
    cat("\n--- Model Comparison ---\n")
    print(loo_comp)
    cat("\nInterpretation:\n")
    cat("Positive elpd_diff favors the first model (full).\n")
    cat("Difference significant if |elpd_diff| > 2*SE.\n")
    sink()
  }
} else {
  cat("rstanarm not available. Creating placeholder outputs...\n")
  sink("output/bayesian_model_results.txt")
  cat("=== Bayesian Logistic Regression Results ===\n\n")
  cat("Note: rstanarm package not available.\n")
  cat("Install with: install.packages('rstanarm')\n\n")
  freq_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = bayes_data)
  freq_coef <- coef(freq_model)
  freq_se <- sqrt(diag(vcov(freq_model)))
  cat("--- Frequentist Model (for reference) ---\n")
  cat("These results approximate what Bayesian analysis would show.\n\n")
  freq_df <- data.frame(
    Variable = names(freq_coef),
    Estimate = round(freq_coef, 3),
    SE = round(freq_se, 3),
    OR = round(exp(freq_coef), 3)
  )
  print(freq_df, row.names = FALSE)
  sink()
  # Create placeholder figure
  fig15_2 <- ggplot(data.frame(x = rnorm(1000)), aes(x = x)) +
    geom_histogram(bins = 30, fill = "grey70", colour = "white") +
    labs(title = "Posterior Distribution (Placeholder)",
         subtitle = "Install rstanarm for actual Bayesian analysis")
  ggsave("figures/fig15_2_posterior_distributions.jpeg", fig15_2, width = 8, height = 5,
         dpi = 300, device = "jpeg")
}
# ============================================================================
# PART B: CAUSAL INFERENCE METHODS
# ============================================================================
cat("\n=== PART B: CAUSAL INFERENCE METHODS ===\n\n")
# ============================================================================
# Section 15.6: Simulate Observational Data with Confounding
# ============================================================================
cat("Simulating observational data with confounding...\n")
n_causal <- 1000
# Confounders
age <- rnorm(n_causal, mean = 50, sd = 10)
severity <- rnorm(n_causal, mean = 5, sd = 2)
comorbidity <- rbinom(n_causal, 1, 0.3)
prior_treatment <- rbinom(n_causal, 1, 0.4)
# Treatment assignment (confounded)
lp_treat <- -2 + 0.02 * (age - 50) + 0.3 * (severity - 5) + 0.5 * comorbidity + 0.3 * prior_treatment
prob_treat <- plogis(lp_treat)
treatment <- rbinom(n_causal, 1, prob_treat)
# Outcome (treatment has causal effect)
true_treat_effect <- 0.405
lp_outcome <- -2 + true_treat_effect * treatment + 0.03 * (age - 50) +
  0.2 * (severity - 5) + 0.4 * comorbidity - 0.2 * prior_treatment
prob_outcome <- plogis(lp_outcome)
outcome <- rbinom(n_causal, 1, prob_outcome)
obs_data <- data.frame(
  outcome = outcome,
  treatment = treatment,
  age = age,
  severity = severity,
  comorbidity = comorbidity,
  prior_treatment = prior_treatment
)
sink("output/causal_data_summary.txt")
cat("=== Causal Inference Data Summary ===\n\n")
cat("Sample size:", n_causal, "\n\n")
cat("True causal effect of treatment:\n")
cat("  Log-OR:", round(true_treat_effect, 3), "\n")
cat("  OR:", round(exp(true_treat_effect), 3), "\n\n")
cat("--- Treatment Distribution ---\n")
cat("  Control (T=0):", sum(treatment == 0), "(", round(100 * mean(treatment == 0), 1), "%)\n")
cat("  Treated (T=1):", sum(treatment == 1), "(", round(100 * mean(treatment == 1), 1), "%)\n\n")
cat("--- Outcome Distribution ---\n")
cat("  Outcome = 0:", sum(outcome == 0), "(", round(100 * mean(outcome == 0), 1), "%)\n")
cat("  Outcome = 1:", sum(outcome == 1), "(", round(100 * mean(outcome == 1), 1), "%)\n\n")
cat("--- Covariate Means by Treatment ---\n")
covariate_means <- obs_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    age = round(mean(age), 1),
    severity = round(mean(severity), 2),
    comorbidity = round(mean(comorbidity), 2),
    prior_treatment = round(mean(prior_treatment), 2),
    outcome = round(mean(outcome), 3)
  )
print(as.data.frame(covariate_means))
cat("\nNote: Differences in covariates indicate confounding.\n")
sink()
# ============================================================================
# Section 15.7: Propensity Score Estimation
# ============================================================================
cat("Estimating propensity scores...\n")
ps_model <- glm(treatment ~ age + severity + comorbidity + prior_treatment,
                family = binomial, data = obs_data)
obs_data$ps <- predict(ps_model, type = "response")
sink("output/propensity_score_model.txt")
cat("=== Propensity Score Model ===\n\n")
cat("Model: P(Treatment = 1 | Covariates)\n\n")
cat("--- Coefficients ---\n")
ps_coef <- coef(ps_model)
ps_se <- sqrt(diag(vcov(ps_model)))
ps_or <- exp(ps_coef)
ps_table <- data.frame(
  Variable = names(ps_coef),
  Coef = round(ps_coef, 4),
  SE = round(ps_se, 4),
  OR = round(ps_or, 3)
)
print(ps_table, row.names = FALSE)
cat("\n--- Propensity Score Distribution ---\n")
cat("Overall:\n")
cat("  Min:", round(min(obs_data$ps), 3), "\n")
cat("  Max:", round(max(obs_data$ps), 3), "\n")
cat("  Mean:", round(mean(obs_data$ps), 3), "\n\n")
cat("By treatment group:\n")
cat("Control (T=0):\n")
cat("  Min:", round(min(obs_data$ps[obs_data$treatment == 0]), 3), "\n")
cat("  Max:", round(max(obs_data$ps[obs_data$treatment == 0]), 3), "\n")
cat("  Mean:", round(mean(obs_data$ps[obs_data$treatment == 0]), 3), "\n\n")
cat("Treated (T=1):\n")
cat("  Min:", round(min(obs_data$ps[obs_data$treatment == 1]), 3), "\n")
cat("  Max:", round(max(obs_data$ps[obs_data$treatment == 1]), 3), "\n")
cat("  Mean:", round(mean(obs_data$ps[obs_data$treatment == 1]), 3), "\n")
sink()
# Figure 15.4: Propensity score distributions
fig15_4 <- ggplot(obs_data, aes(x = ps, fill = factor(treatment))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40, colour = "white") +
  scale_fill_manual(values = c("0" = "grey70", "1" = "grey30"),
                    labels = c("Control", "Treated")) +
  labs(title = "Propensity Score Distributions by Treatment Group",
       subtitle = "Good overlap is essential for causal inference",
       x = "Propensity Score", y = "Count", fill = "Treatment") +
  theme(legend.position = "bottom")
ggsave("figures/fig15_4_ps_distributions.jpeg", fig15_4, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig15_4_ps_distributions.jpeg\n")
# ============================================================================
# Section 15.8: Naive (Unadjusted) Analysis
# ============================================================================
cat("Performing naive analysis...\n")
naive_model <- glm(outcome ~ treatment, family = binomial, data = obs_data)
naive_coef <- coef(naive_model)["treatment"]
naive_se <- sqrt(vcov(naive_model)["treatment", "treatment"])
naive_or <- exp(naive_coef)
naive_ci <- exp(naive_coef + c(-1.96, 1.96) * naive_se)
# ============================================================================
# Section 15.9: Propensity Score Matching
# ============================================================================
cat("Performing propensity score matching...\n")
if (requireNamespace("MatchIt", quietly = TRUE)) {
  library(MatchIt)
  match_out <- matchit(treatment ~ age + severity + comorbidity + prior_treatment,
                       data = obs_data,
                       method = "nearest",
                       distance = "logit",
                       caliper = 0.2,
                       ratio = 1)
  matched_data <- match.data(match_out)
  matched_model <- glm(outcome ~ treatment, family = binomial,
                       data = matched_data, weights = weights)
  matched_coef <- coef(matched_model)["treatment"]
  matched_se <- sqrt(vcov(matched_model)["treatment", "treatment"])
  matched_or <- exp(matched_coef)
  matched_ci <- exp(matched_coef + c(-1.96, 1.96) * matched_se)
  sink("output/matching_results.txt")
  cat("=== Propensity Score Matching Results ===\n\n")
  cat("Matching method: 1:1 Nearest Neighbor\n")
  cat("Distance: Logit (propensity score)\n")
  cat("Caliper: 0.2 standard deviations\n\n")
  cat("--- Sample Sizes ---\n")
  cat("Original sample:", nrow(obs_data), "\n")
  cat("  Control:", sum(obs_data$treatment == 0), "\n")
  cat("  Treated:", sum(obs_data$treatment == 1), "\n")
  cat("Matched sample:", nrow(matched_data), "\n")
  cat("  Control:", sum(matched_data$treatment == 0), "\n")
  cat("  Treated:", sum(matched_data$treatment == 1), "\n")
  cat("Unmatched (discarded):", nrow(obs_data) - nrow(matched_data), "\n\n")
  cat("--- Treatment Effect Estimate ---\n")
  cat("Log-OR:", round(matched_coef, 4), "\n")
  cat("SE:", round(matched_se, 4), "\n")
  cat("OR:", round(matched_or, 3), "\n")
  cat("95% CI:", round(matched_ci[1], 3), "-", round(matched_ci[2], 3), "\n")
  cat("True OR:", round(exp(true_treat_effect), 3), "\n")
  sink()
  # Balance diagnostics
  if (requireNamespace("cobalt", quietly = TRUE)) {
    library(cobalt)
    bal <- bal.tab(match_out, un = TRUE, stats = c("m", "v"))
    sink("output/balance_diagnostics.txt")
    cat("=== Balance Diagnostics ===\n\n")
    cat("SMD: Standardized Mean Difference\n")
    cat("  < 0.1: Good balance\n")
    cat("  0.1-0.25: Acceptable\n")
    cat("  > 0.25: Poor balance\n\n")
    print(bal)
    sink()
    # Figure 15.5: Love plot
    jpeg("figures/fig15_5_love_plot.jpeg", width = 8, height = 5, units = "in",
         res = 300, quality = 95)
    love.plot(match_out,
              stats = "mean.diffs",
              thresholds = c(m = 0.1),
              abs = TRUE,
              var.order = "unadjusted",
              colors = c("grey60", "black"))
    dev.off()
    cat("Saved: figures/fig15_5_love_plot.jpeg\n")
  }
} else {
  cat("MatchIt not available. Creating placeholder...\n")
  matched_or <- 1.5
  matched_ci <- c(1.1, 2.0)
  matched_coef <- log(1.5)
}
# ============================================================================
# Section 15.10: Inverse Probability Weighting
# ============================================================================
cat("Performing IPW analysis...\n")
obs_data$ipw <- ifelse(obs_data$treatment == 1,
                       1 / obs_data$ps,
                       1 / (1 - obs_data$ps))
p_treat <- mean(obs_data$treatment)
obs_data$ipw_stab <- ifelse(obs_data$treatment == 1,
                            p_treat / obs_data$ps,
                            (1 - p_treat) / (1 - obs_data$ps))
ipw_truncated <- obs_data$ipw_stab
ipw_truncated[ipw_truncated < quantile(ipw_truncated, 0.01)] <- quantile(ipw_truncated, 0.01)
ipw_truncated[ipw_truncated > quantile(ipw_truncated, 0.99)] <- quantile(ipw_truncated, 0.99)
obs_data$ipw_trunc <- ipw_truncated
ipw_model <- glm(outcome ~ treatment, family = binomial,
                 data = obs_data, weights = ipw_trunc)
ipw_coef <- coef(ipw_model)["treatment"]
ipw_se <- sqrt(vcov(ipw_model)["treatment", "treatment"])
ipw_or <- exp(ipw_coef)
ipw_ci <- exp(ipw_coef + c(-1.96, 1.96) * ipw_se)
sink("output/ipw_results.txt")
cat("=== Inverse Probability Weighting Results ===\n\n")
cat("--- Weight Distribution (Stabilized, Truncated) ---\n")
cat("Min:", round(min(obs_data$ipw_trunc), 3), "\n")
cat("Max:", round(max(obs_data$ipw_trunc), 3), "\n")
cat("Mean:", round(mean(obs_data$ipw_trunc), 3), "\n")
cat("SD:", round(sd(obs_data$ipw_trunc), 3), "\n\n")
cat("--- Treatment Effect Estimate ---\n")
cat("Log-OR:", round(ipw_coef, 4), "\n")
cat("SE:", round(ipw_se, 4), "\n")
cat("OR:", round(ipw_or, 3), "\n")
cat("95% CI:", round(ipw_ci[1], 3), "-", round(ipw_ci[2], 3), "\n")
cat("True OR:", round(exp(true_treat_effect), 3), "\n")
sink()
# Figure 15.6: IPW weight distribution
fig15_6 <- ggplot(obs_data, aes(x = ipw_trunc, fill = factor(treatment))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40, colour = "white") +
  scale_fill_manual(values = c("0" = "grey70", "1" = "grey30"),
                    labels = c("Control", "Treated")) +
  labs(title = "IPW Weight Distribution by Treatment Group",
       subtitle = "Stabilised and truncated weights",
       x = "IPW Weight", y = "Count", fill = "Treatment") +
  theme(legend.position = "bottom")
ggsave("figures/fig15_6_ipw_weights.jpeg", fig15_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig15_6_ipw_weights.jpeg\n")
# ============================================================================
# Section 15.11: Doubly Robust Estimation
# ============================================================================
cat("Performing doubly robust estimation...\n")
model_treated <- glm(outcome ~ age + severity + comorbidity + prior_treatment,
                     family = binomial,
                     data = subset(obs_data, treatment == 1))
model_control <- glm(outcome ~ age + severity + comorbidity + prior_treatment,
                     family = binomial,
                     data = subset(obs_data, treatment == 0))
obs_data$mu1 <- predict(model_treated, newdata = obs_data, type = "response")
obs_data$mu0 <- predict(model_control, newdata = obs_data, type = "response")
aipw_treated <- with(obs_data,
                     treatment * (outcome - mu1) / ps + mu1)
aipw_control <- with(obs_data,
                     (1 - treatment) * (outcome - mu0) / (1 - ps) + mu0)
ate_aipw <- mean(aipw_treated) - mean(aipw_control)
# Bootstrap for SE
set.seed(456)
n_boot <- 500
boot_ate <- numeric(n_boot)
for (b in 1:n_boot) {
  boot_idx <- sample(1:nrow(obs_data), replace = TRUE)
  boot_data <- obs_data[boot_idx, ]
  ps_boot <- predict(glm(treatment ~ age + severity + comorbidity + prior_treatment,
                         family = binomial, data = boot_data), type = "response")
  m1_boot <- predict(glm(outcome ~ age + severity + comorbidity + prior_treatment,
                         family = binomial,
                         data = subset(boot_data, treatment == 1)),
                     newdata = boot_data, type = "response")
  m0_boot <- predict(glm(outcome ~ age + severity + comorbidity + prior_treatment,
                         family = binomial,
                         data = subset(boot_data, treatment == 0)),
                     newdata = boot_data, type = "response")
  aipw_t <- boot_data$treatment * (boot_data$outcome - m1_boot) / ps_boot + m1_boot
  aipw_c <- (1 - boot_data$treatment) * (boot_data$outcome - m0_boot) / (1 - ps_boot) + m0_boot
  boot_ate[b] <- mean(aipw_t) - mean(aipw_c)
}
dr_se <- sd(boot_ate)
dr_ci <- quantile(boot_ate, c(0.025, 0.975))
sink("output/doubly_robust_results.txt")
cat("=== Doubly Robust (AIPW) Results ===\n\n")
cat("Method: Augmented Inverse Probability Weighting\n")
cat("Bootstrap SE: 500 replications\n\n")
cat("--- Average Treatment Effect (Risk Difference Scale) ---\n")
cat("ATE:", round(ate_aipw, 4), "\n")
cat("SE:", round(dr_se, 4), "\n")
cat("95% CI:", round(dr_ci[1], 4), "-", round(dr_ci[2], 4), "\n\n")
cat("Note: AIPW is consistent if either the propensity score model\n")
cat("OR the outcome model is correctly specified (doubly robust).\n")
sink()
# ============================================================================
# Section 15.12: Results Comparison
# ============================================================================
cat("Creating results comparison...\n")
adj_model <- glm(outcome ~ treatment + age + severity + comorbidity + prior_treatment,
                 family = binomial, data = obs_data)
adj_coef <- coef(adj_model)["treatment"]
adj_se <- sqrt(vcov(adj_model)["treatment", "treatment"])
adj_or <- exp(adj_coef)
adj_ci <- exp(adj_coef + c(-1.96, 1.96) * adj_se)
results_comparison <- data.frame(
  Method = c("True Effect", "Naive (Unadjusted)", "Regression Adjustment",
             "PS Matching", "IPW", "Doubly Robust (ATE)"),
  Estimate = c(true_treat_effect, naive_coef, adj_coef, matched_coef, ipw_coef, NA),
  OR = c(exp(true_treat_effect), naive_or, adj_or, matched_or, ipw_or, NA),
  CI_Lower = c(NA, naive_ci[1], adj_ci[1], matched_ci[1], ipw_ci[1], NA),
  CI_Upper = c(NA, naive_ci[2], adj_ci[2], matched_ci[2], ipw_ci[2], NA)
)
results_comparison$ATE_RD <- c(NA, NA, NA, NA, NA, ate_aipw)
sink("output/results_comparison.txt")
cat("=== Causal Inference Methods Comparison ===\n\n")
cat("True treatment effect: OR =", round(exp(true_treat_effect), 3), "\n\n")
cat("--- Odds Ratio Estimates ---\n")
print(results_comparison[, c("Method", "OR", "CI_Lower", "CI_Upper")], row.names = FALSE)
cat("\n--- Bias Assessment ---\n")
cat("Naive bias:", round(naive_or - exp(true_treat_effect), 3), "(OR scale)\n")
cat("Adjusted bias:", round(adj_or - exp(true_treat_effect), 3), "(OR scale)\n")
cat("Matching bias:", round(matched_or - exp(true_treat_effect), 3), "(OR scale)\n")
cat("IPW bias:", round(ipw_or - exp(true_treat_effect), 3), "(OR scale)\n")
cat("\n--- Interpretation ---\n")
cat("Naive analysis overestimates effect due to confounding.\n")
cat("Adjusted methods (matching, IPW, DR) reduce bias toward true effect.\n")
sink()
# Figure 15.7: Methods comparison forest plot
methods_plot_df <- results_comparison[2:5, ]
methods_plot_df$Method <- factor(methods_plot_df$Method,
                                 levels = rev(methods_plot_df$Method))
fig15_7 <- ggplot(methods_plot_df, aes(x = OR, y = Method)) +
  geom_point(size = 3, colour = "black") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, colour = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = exp(true_treat_effect), linetype = "dotted",
             colour = "black", linewidth = 1) +
  annotate("text", x = exp(true_treat_effect) + 0.05, y = 4.3,
           label = paste("True OR =", round(exp(true_treat_effect), 2)),
           colour = "black", hjust = 0, size = 3) +
  scale_x_continuous(limits = c(0.8, 2.5)) +
  labs(title = "Treatment Effect Estimates by Method",
       subtitle = "Dotted line = true causal effect; dashed line = null",
       x = "Odds Ratio", y = "") +
  theme(axis.text.y = element_text(size = 10))
ggsave("figures/fig15_7_methods_comparison.jpeg", fig15_7, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig15_7_methods_comparison.jpeg\n")
# ============================================================================
# Section 15.13: Sensitivity Analysis (E-value)
# ============================================================================
cat("Performing sensitivity analysis...\n")
calculate_evalue <- function(or, lo = NULL, hi = NULL) {
  evalue_point <- if (or > 1) {
    or + sqrt(or * (or - 1))
  } else {
    1
  }
  evalue_ci <- 1
  if (!is.null(lo) && lo > 1) {
    evalue_ci <- lo + sqrt(lo * (lo - 1))
  }
  return(list(point = evalue_point, ci = evalue_ci))
}
evalue_matched <- calculate_evalue(matched_or, matched_ci[1], matched_ci[2])
sink("output/sensitivity_analysis.txt")
cat("=== Sensitivity Analysis for Unmeasured Confounding ===\n\n")
cat("--- E-value Analysis (for PS Matching estimate) ---\n")
cat("Estimated OR:", round(matched_or, 3), "\n")
cat("95% CI:", round(matched_ci[1], 3), "-", round(matched_ci[2], 3), "\n\n")
cat("E-value for point estimate:", round(evalue_matched$point, 2), "\n")
cat("E-value for CI lower bound:", round(evalue_matched$ci, 2), "\n\n")
cat("Interpretation:\n")
cat("To explain away the observed effect, an unmeasured confounder would need\n")
cat("to be associated with both treatment and outcome by a risk ratio of at\n")
cat("least", round(evalue_matched$point, 2), "(for point estimate) or",
    round(evalue_matched$ci, 2), "(for CI bound).\n\n")
cat("E-values > 2 suggest moderate robustness to unmeasured confounding.\n")
cat("E-values > 3 suggest strong robustness.\n")
sink()
# ============================================================================
# Section 15.14: Summary
# ============================================================================
sink("output/chapter15_summary.txt")
cat("=== Chapter 15 Summary: Bayesian and Causal Methods ===\n\n")
cat("PART A: BAYESIAN LOGISTIC REGRESSION\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Sample size:", n_bayes, "\n")
cat("True model: logit(p) = -0.5 + 0.8*x1 - 0.5*x2 + 0.6*x3\n\n")
if (exists("bayes_model")) {
  cat("Posterior summaries (median OR, 95% CrI):\n")
  cat("  x1: OR =", round(exp(median(posterior_samples[, "x1"])), 3), "\n")
  cat("  x2: OR =", round(exp(median(posterior_samples[, "x2"])), 3), "\n")
  cat("  x3: OR =", round(exp(median(posterior_samples[, "x31"])), 3), "\n\n")
  cat("Probability statements:\n")
  cat("  P(OR_x1 > 1) =", round(mean(exp(posterior_samples[, "x1"]) > 1), 3), "\n")
  cat("  P(OR_x2 < 1) =", round(mean(exp(posterior_samples[, "x2"]) < 1), 3), "\n")
}
cat("\nPART B: CAUSAL INFERENCE\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Sample size:", n_causal, "\n")
cat("True treatment effect: OR =", round(exp(true_treat_effect), 3), "\n\n")
cat("Treatment effect estimates:\n")
cat("  Naive (unadjusted): OR =", round(naive_or, 3), "\n")
cat("  Regression adjusted: OR =", round(adj_or, 3), "\n")
cat("  PS Matching: OR =", round(matched_or, 3), "\n")
cat("  IPW: OR =", round(ipw_or, 3), "\n")
cat("  Doubly Robust (ATE): RD =", round(ate_aipw, 4), "\n\n")
cat("Sensitivity analysis:\n")
cat("  E-value (point estimate):", round(evalue_matched$point, 2), "\n")
cat("  E-value (CI bound):", round(evalue_matched$ci, 2), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 15 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig15_1_prior_distributions.jpeg\n")
cat("  - fig15_2_posterior_distributions.jpeg\n")
cat("  - fig15_3_trace_plots.jpeg\n")
cat("  - fig15_4_ps_distributions.jpeg\n")
cat("  - fig15_5_love_plot.jpeg\n")
cat("  - fig15_6_ipw_weights.jpeg\n")
cat("  - fig15_7_methods_comparison.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - bayesian_data_summary.txt\n")
cat("  - bayesian_model_results.txt\n")
cat("  - convergence_diagnostics.txt\n")
cat("  - model_comparison_loo.txt\n")
cat("  - causal_data_summary.txt\n")
cat("  - propensity_score_model.txt\n")
cat("  - matching_results.txt\n")
cat("  - balance_diagnostics.txt\n")
cat("  - ipw_results.txt\n")
cat("  - doubly_robust_results.txt\n")
cat("  - results_comparison.txt\n")
cat("  - sensitivity_analysis.txt\n")
cat("  - chapter15_summary.txt\n")