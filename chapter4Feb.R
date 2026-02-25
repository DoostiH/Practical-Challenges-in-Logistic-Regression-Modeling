# =============================================================================
# Chapter 4: Overdispersion and Multiple Link Functions
# Figures and Analysis Code
# =============================================================================
# Install required packages
#install.packages(c("tidyverse", "dglm", "gamlss", "lme4",
#                  "pROC", "cowplot", "scales", "MASS"))
# Load required packages
library(tidyverse)
library(dglm)              # Double generalized linear models
library(gamlss)            # GAMLSS framework
library(lme4)              # Mixed-effects models
library(pROC)              # ROC analysis
library(cowplot)           # For combining plots
library(scales)            # For plot formatting
library(MASS)              # For additional distributions
# Resolve namespace conflicts
select <- dplyr::select
filter <- dplyr::filter
# Set seed for reproducibility
set.seed(123)
# Create output directories if they don't exist
dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
# Set consistent B&W theme for all plots
theme_chapter <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
# =============================================================================
# Figure 4.1: Illustrating Overdispersion Concept
# =============================================================================
# Create data showing binomial vs overdispersed data
set.seed(123)
n_groups <- 50
trials <- 20
true_prob <- 0.5
# Binomial data (no overdispersion)
y_binomial <- rbinom(n_groups, trials, true_prob)
# Overdispersed data (beta-binomial-like)
# Simulate by varying the probability for each group
prob_varied <- rbeta(n_groups, 2, 2)  # Beta(2,2) gives variation around 0.5
y_overdispersed <- rbinom(n_groups, trials, prob_varied)
# Calculate statistics
binom_var <- var(y_binomial)
binom_expected_var <- trials * true_prob * (1 - true_prob)
overdisp_var <- var(y_overdispersed)
phi_estimate <- overdisp_var / binom_expected_var
cat("Binomial data:\n")
cat("  Observed variance:", round(binom_var, 2), "\n")
cat("  Expected variance:", binom_expected_var, "\n")
cat("  Dispersion:", round(binom_var/binom_expected_var, 2), "\n\n")
cat("Overdispersed data:\n")
cat("  Observed variance:", round(overdisp_var, 2), "\n")
cat("  Expected variance:", binom_expected_var, "\n")
cat("  Dispersion:", round(phi_estimate, 2), "\n")
# Create comparison data frame
binom_label <- expression("Binomial (" * phi %~~% 1 * ")")
od_label_val <- round(phi_estimate, 1)

comparison_data <- data.frame(
  y = c(y_binomial, y_overdispersed),
  Type = rep(c("Binomial", "Overdispersed"), each = n_groups)
)
comparison_data$Type <- factor(comparison_data$Type, levels = c("Binomial", "Overdispersed"))

fig_4_1a <- ggplot(comparison_data, aes(x = y, fill = Type)) +
  geom_histogram(binwidth = 1, position = "dodge", colour = "black", alpha = 0.8,
                 linewidth = 0.3) +
  geom_vline(xintercept = trials * true_prob, linetype = "dashed", colour = "grey30") +
  annotate("text", x = trials * true_prob + 0.5, y = Inf, vjust = 2, hjust = 0,
           label = "Expected mean", size = 3) +
  scale_fill_manual(values = c("Binomial" = "grey75", "Overdispersed" = "grey35"),
                    labels = c(paste0("Binomial (\u03C6 \u2248 1)"),
                               paste0("Overdispersed (\u03C6 \u2248 ", od_label_val, ")"))) +
  labs(title = "Distribution of Successes: Binomial vs. Overdispersed",
       subtitle = paste0("n = ", n_groups, " groups, ", trials, " trials each, true p = ", true_prob),
       x = "Number of Successes", y = "Count", fill = "") +
  theme_chapter +
  theme(legend.position = "bottom")
# Variance comparison
var_data <- data.frame(
  Type = c("Expected\n(Binomial)", "Observed\n(Binomial)", "Observed\n(Overdispersed)"),
  Variance = c(binom_expected_var, binom_var, overdisp_var)
)
var_data$Type <- factor(var_data$Type, levels = var_data$Type)
fig_4_1b <- ggplot(var_data, aes(x = Type, y = Variance, fill = Type)) +
  geom_bar(stat = "identity", width = 0.6, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("grey65", "grey45", "grey20")) +
  labs(title = "Variance Comparison",
       subtitle = "Overdispersion = excess variance beyond binomial expectation",
       x = "", y = "Variance") +
  theme_chapter +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(var_data$Variance) * 1.15))
fig_4_1 <- plot_grid(fig_4_1a, fig_4_1b, ncol = 2, labels = c("A", "B"), rel_widths = c(1.3, 1))
ggsave("figures/fig_4_1_overdispersion_concept.jpeg", fig_4_1,
       width = 11, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 4.1 saved: Overdispersion concept\n")
# =============================================================================
# Create Simulated Dataset with Overdispersion
# =============================================================================
set.seed(456)
n <- 200
trials <- 20
# Predictors
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)  # This will also affect dispersion
# True coefficients for mean model
beta0 <- -0.5
beta1 <- 0.8
beta2 <- -0.5
beta3 <- 0.4
# Linear predictor for mean
lin_pred <- beta0 + beta1*x1 + beta2*x2 + beta3*x3
prob_mean <- plogis(lin_pred)
# Create varying dispersion based on x3
# Higher x3 = higher dispersion
gamma0 <- 0.5   # Baseline log-dispersion
gamma1 <- 0.4   # Effect of x3 on log-dispersion
log_dispersion <- gamma0 + gamma1 * x3
dispersion <- exp(log_dispersion)
# Generate overdispersed responses using beta-binomial-like approach
# For each observation, draw probability from a distribution centered on prob_mean
# with variance controlled by dispersion
alpha_beta_sum <- 1 / (dispersion - 1)  # Concentration parameter
alpha_beta_sum <- pmax(alpha_beta_sum, 2)  # Ensure valid parameters
alpha_param <- prob_mean * alpha_beta_sum
beta_param <- (1 - prob_mean) * alpha_beta_sum
# Draw actual probabilities
prob_actual <- rbeta(n, alpha_param, beta_param)
prob_actual <- pmax(pmin(prob_actual, 0.999), 0.001)
# Generate responses
y <- rbinom(n, trials, prob_actual)
# Create dataset
overdisp_data <- data.frame(
  y = y,
  trials = trials,
  x1 = x1,
  x2 = x2,
  x3 = x3,
  prob_mean = prob_mean,
  prob_actual = prob_actual,
  dispersion = dispersion
)
# Save data summary
sink("output/ch4_data_summary.txt")
cat("Overdispersion Simulation Data Summary\n")
cat("======================================\n\n")
cat("Sample size:", n, "\n")
cat("Trials per observation:", trials, "\n")
cat("Mean success rate:", round(mean(y/trials), 3), "\n\n")
cat("True mean model coefficients:\n")
cat("  Intercept:", beta0, "\n")
cat("  x1:", beta1, "\n")
cat("  x2:", beta2, "\n")
cat("  x3:", beta3, "\n\n")
cat("True dispersion model coefficients:\n")
cat("  Intercept (gamma0):", gamma0, "\n")
cat("  x3 (gamma1):", gamma1, "\n")
cat("\nDispersion range:", round(min(dispersion), 2), "to", round(max(dispersion), 2), "\n")
sink()
# =============================================================================
# Figure 4.2: Visualizing Varying Dispersion in the Data
# =============================================================================
fig_4_2a <- ggplot(overdisp_data, aes(x = x3, y = y/trials)) +
  geom_point(aes(fill = dispersion), alpha = 0.7, size = 2, shape = 21, colour = "black",
             stroke = 0.3) +
  geom_smooth(method = "loess", colour = "black", se = FALSE, linewidth = 1) +
  scale_fill_gradient(low = "white", high = "black", name = "Dispersion") +
  labs(title = "Observed Proportions vs. x3",
       subtitle = "Shading indicates local dispersion level",
       x = expression(x[3]), y = "Observed Proportion (y/n)") +
  theme_chapter
# Residual spread by x3
overdisp_data$x3_bin <- cut(overdisp_data$x3, breaks = 5)
fig_4_2b <- ggplot(overdisp_data, aes(x = x3_bin, y = (y/trials - prob_mean))) +
  geom_boxplot(fill = "grey70", colour = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Residual Spread by x3 Quintile",
       subtitle = "Increasing spread indicates overdispersion varies with x3",
       x = expression(x[3] ~ "Quintile"), y = "Residual (Observed \u2212 Expected)") +
  theme_chapter +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig_4_2 <- plot_grid(fig_4_2a, fig_4_2b, ncol = 2, labels = c("A", "B"))
ggsave("figures/fig_4_2_varying_dispersion.jpeg", fig_4_2,
       width = 11, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 4.2 saved: Varying dispersion visualization\n")
# =============================================================================
# Model Fitting: Standard, Quasi-binomial, and DGLM
# =============================================================================
# Standard binomial model
model_standard <- glm(cbind(y, trials - y) ~ x1 + x2 + x3,
                      family = binomial(link = "logit"),
                      data = overdisp_data)
# Calculate dispersion parameter
pearson_resid <- residuals(model_standard, type = "pearson")
phi_hat <- sum(pearson_resid^2) / model_standard$df.residual
sink("output/ch4_standard_model.txt")
cat("Standard Binomial Model\n")
cat("=======================\n\n")
print(summary(model_standard))
cat("\nEstimated dispersion parameter:", round(phi_hat, 3), "\n")
cat("(Values > 1 indicate overdispersion)\n")
sink()
# Quasi-binomial model
model_quasi <- glm(cbind(y, trials - y) ~ x1 + x2 + x3,
                   family = quasibinomial(link = "logit"),
                   data = overdisp_data)
sink("output/ch4_quasi_model.txt")
cat("Quasi-binomial Model\n")
cat("====================\n\n")
print(summary(model_quasi))
sink()
# Double GLM (dual-link model)
model_dglm <- dglm(cbind(y, trials - y) ~ x1 + x2 + x3,
                   dformula = ~ x3,
                   family = binomial(link = "logit"),
                   dlink = "log",
                   data = overdisp_data)
sink("output/ch4_dglm_model.txt")
cat("Double GLM (Dual-Link Model)\n")
cat("============================\n\n")
cat("Mean Model:\n")
print(summary(model_dglm))
cat("\nDispersion Model:\n")
print(summary(model_dglm$dispersion.fit))
sink()
cat("Model outputs saved\n")
# =============================================================================
# Figure 4.3: Overdispersion Diagnostics
# =============================================================================
# Diagnostic plot: squared Pearson residuals vs fitted values
diag_data <- data.frame(
  fitted = fitted(model_standard),
  pearson_sq = pearson_resid^2,
  x3 = overdisp_data$x3
)
fig_4_3a <- ggplot(diag_data, aes(x = fitted, y = pearson_sq)) +
  geom_point(aes(fill = x3), alpha = 0.6, shape = 21, colour = "black", stroke = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 1) +
  geom_smooth(method = "loess", colour = "black", se = FALSE, linewidth = 1,
              linetype = "longdash") +
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Overdispersion Diagnostic Plot",
       subtitle = "Squared Pearson residuals should average \u2248 1 under correct specification",
       x = "Fitted Values", y = "Squared Pearson Residuals",
       fill = expression(x[3])) +
  theme_chapter +
  annotate("text", x = 0.2, y = max(diag_data$pearson_sq) * 0.9,
           label = paste0("Mean = ", round(mean(diag_data$pearson_sq), 2)),
           colour = "black", size = 4)
# Q-Q plot of deviance residuals
dev_resid <- residuals(model_standard, type = "deviance")
qq_data <- data.frame(
  theoretical = qnorm(ppoints(length(dev_resid))),
  observed = sort(dev_resid)
)
fig_4_3b <- ggplot(qq_data, aes(x = theoretical, y = observed)) +
  geom_point(alpha = 0.6, colour = "grey40") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "black") +
  labs(title = "Q-Q Plot of Deviance Residuals",
       subtitle = "Deviations from line indicate model misspecification",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_chapter
fig_4_3 <- plot_grid(fig_4_3a, fig_4_3b, ncol = 2, labels = c("A", "B"))
ggsave("figures/fig_4_3_overdispersion_diagnostics.jpeg", fig_4_3,
       width = 11, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 4.3 saved: Overdispersion diagnostics\n")
# =============================================================================
# Figure 4.4: Standard Error Comparison Across Models
# =============================================================================
# Extract standard errors
se_standard <- sqrt(diag(vcov(model_standard)))
se_quasi <- sqrt(diag(vcov(model_quasi)))
se_dglm <- sqrt(diag(vcov(model_dglm)))
se_comparison <- data.frame(
  Parameter = names(se_standard),
  Standard = se_standard,
  `Quasi-binomial` = se_quasi,
  DGLM = se_dglm,
  check.names = FALSE
)
# Calculate ratio to standard
se_comparison$`Quasi/Standard` <- se_comparison$`Quasi-binomial` / se_comparison$Standard
se_comparison$`DGLM/Standard` <- se_comparison$DGLM / se_comparison$Standard
sink("output/ch4_se_comparison.txt")
cat("Standard Error Comparison\n")
cat("=========================\n\n")
print(se_comparison, digits = 4)
cat("\nNote: Ratios > 1 indicate larger (more conservative) SEs\n")
cat("Expected ratio for quasi-binomial ~ sqrt(phi) =", round(sqrt(phi_hat), 3), "\n")
sink()
# Reshape for plotting
se_long <- se_comparison %>%
  dplyr::select(Parameter, Standard, `Quasi-binomial`, DGLM) %>%
  pivot_longer(cols = -Parameter, names_to = "Method", values_to = "SE") %>%
  mutate(Method = factor(Method, levels = c("Standard", "Quasi-binomial", "DGLM")))
fig_4_4 <- ggplot(se_long, aes(x = Parameter, y = SE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Standard" = "grey85", "Quasi-binomial" = "grey55",
                               "DGLM" = "grey25")) +
  labs(title = "Standard Errors by Method",
       subtitle = paste0("Dispersion \u03C6 \u2248 ", round(phi_hat, 2),
                         "; quasi-binomial and DGLM account for overdispersion"),
       x = "Parameter", y = "Standard Error", fill = "Method") +
  theme_chapter +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/fig_4_4_se_comparison.jpeg", fig_4_4,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 4.4 saved: Standard error comparison\n")
# =============================================================================
# Figure 4.5: Coefficient Comparison
# =============================================================================
# True coefficients
true_coefs <- c(beta0, beta1, beta2, beta3)
names(true_coefs) <- c("(Intercept)", "x1", "x2", "x3")
coef_comparison <- data.frame(
  Parameter = names(true_coefs),
  True = true_coefs,
  Standard = coef(model_standard),
  `Quasi-binomial` = coef(model_quasi),
  DGLM = coef(model_dglm),
  check.names = FALSE
)
coef_long <- coef_comparison %>%
  pivot_longer(cols = c(Standard, `Quasi-binomial`, DGLM),
               names_to = "Method", values_to = "Estimate") %>%
  mutate(Method = factor(Method, levels = c("Standard", "Quasi-binomial", "DGLM")))
fig_4_5 <- ggplot(coef_long, aes(x = Parameter, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_point(data = coef_comparison, aes(x = Parameter, y = True),
             inherit.aes = FALSE, shape = 4, size = 4, stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Standard" = "grey85", "Quasi-binomial" = "grey55",
                               "DGLM" = "grey25")) +
  labs(title = "Coefficient Estimates: Mean Model",
       subtitle = "\u00D7 marks true values; all methods give similar point estimates",
       x = "Parameter", y = "Estimate", fill = "Method") +
  theme_chapter +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/fig_4_5_coefficient_comparison.jpeg", fig_4_5,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 4.5 saved: Coefficient comparison\n")
# =============================================================================
# Figure 4.6: DGLM Dispersion Model Results
# =============================================================================
# Extract dispersion model coefficients
disp_coef <- coef(model_dglm$dispersion.fit)
disp_se <- sqrt(diag(vcov(model_dglm$dispersion.fit)))
# True dispersion coefficients
true_disp <- c(gamma0, gamma1)
names(true_disp) <- c("(Intercept)", "x3")
disp_comparison <- data.frame(
  Parameter = names(disp_coef),
  True = true_disp,
  Estimate = disp_coef,
  SE = disp_se
)
disp_comparison$Lower <- disp_comparison$Estimate - 1.96 * disp_comparison$SE
disp_comparison$Upper <- disp_comparison$Estimate + 1.96 * disp_comparison$SE
sink("output/ch4_dispersion_model.txt")
cat("Dispersion Model Results\n")
cat("========================\n\n")
cat("True coefficients:\n")
print(true_disp)
cat("\nEstimated coefficients:\n")
print(disp_comparison)
cat("\nInterpretation:\n")
cat("- Baseline dispersion: exp(", round(disp_coef[1], 3), ") =", round(exp(disp_coef[1]), 3), "\n")
cat("- Effect of x3: exp(", round(disp_coef[2], 3), ") =", round(exp(disp_coef[2]), 3), "\n")
cat("  (1-unit increase in x3 multiplies dispersion by", round(exp(disp_coef[2]), 3), ")\n")
sink()
fig_4_6 <- ggplot(disp_comparison, aes(x = Parameter, y = Estimate)) +
  geom_bar(stat = "identity", fill = "grey50", width = 0.5, colour = "black",
           linewidth = 0.3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 0.8) +
  geom_point(aes(y = True), shape = 4, size = 5, stroke = 2, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Dispersion Model Coefficients",
       subtitle = "\u00D7 marks true values; bars show DGLM estimates with 95% CI",
       x = "Parameter", y = "Coefficient (log scale)") +
  theme_chapter +
  annotate("text", x = 1.5, y = max(disp_comparison$Upper) * 0.9,
           label = paste0("exp(\u03B3\u2081) = ", round(exp(disp_coef[2]), 2),
                          "\n(dispersion multiplier per unit x3)"),
           size = 3.5)
ggsave("figures/fig_4_6_dispersion_model.jpeg", fig_4_6,
       width = 7, height = 5, dpi = 300, device = "jpeg")
cat("Figure 4.6 saved: Dispersion model results\n")
# =============================================================================
# Figure 4.7: Predicted Probabilities with Varying Dispersion
# =============================================================================
# Create prediction grid
pred_grid <- expand.grid(
  x1 = seq(-2, 2, length.out = 100),
  x2 = 0,
  x3 = c(-1.5, 0, 1.5)
)
# Get predictions from DGLM
pred_grid$pred <- predict(model_dglm, newdata = pred_grid, type = "response")
# Calculate dispersion at each x3 level
pred_grid$dispersion <- exp(disp_coef[1] + disp_coef[2] * pred_grid$x3)
# Get standard errors on link scale
pred_link <- predict(model_dglm, newdata = pred_grid, type = "link", se.fit = TRUE)
# Adjust SE for dispersion
pred_grid$se_adj <- pred_link$se.fit * sqrt(pred_grid$dispersion)
# Calculate confidence intervals on probability scale
pred_grid$lower <- plogis(pred_link$fit - 1.96 * pred_grid$se_adj)
pred_grid$upper <- plogis(pred_link$fit + 1.96 * pred_grid$se_adj)
pred_grid$x3_label <- factor(pred_grid$x3,
                             labels = c("x3 = -1.5 (low dispersion)",
                                        "x3 = 0 (medium dispersion)",
                                        "x3 = 1.5 (high dispersion)"))
fig_4_7 <- ggplot(pred_grid, aes(x = x1, y = pred, linetype = x3_label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = x3_label), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1, colour = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  scale_fill_manual(values = c("grey80", "grey55", "grey30")) +
  labs(title = "Predicted Probabilities with Dispersion-Adjusted Confidence Intervals",
       subtitle = "Wider intervals at higher x3 reflect greater dispersion",
       x = expression(x[1]), y = "Predicted Probability",
       linetype = "", fill = "") +
  theme_chapter +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.3)))
ggsave("figures/fig_4_7_predictions_with_dispersion.jpeg", fig_4_7,
       width = 9, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 4.7 saved: Predictions with varying dispersion\n")
# =============================================================================
# GAMLSS Implementation
# =============================================================================
# Fit GAMLSS beta-binomial model
model_gamlss <- gamlss(cbind(y, trials - y) ~ x1 + x2 + x3,
                       sigma.formula = ~ x3,
                       family = BB(),
                       data = overdisp_data,
                       trace = FALSE)
sink("output/ch4_gamlss_model.txt")
cat("GAMLSS Beta-Binomial Model\n")
cat("==========================\n\n")
print(summary(model_gamlss))
sink()
# =============================================================================
# Figure 4.8: GAMLSS vs DGLM Comparison
# =============================================================================
# Extract coefficients from both models
gamlss_mu_coef <- coef(model_gamlss, what = "mu")
gamlss_sigma_coef <- coef(model_gamlss, what = "sigma")
comparison_models <- data.frame(
  Component = c(rep("Mean", 4), rep("Dispersion", 2)),
  Parameter = c("(Intercept)", "x1", "x2", "x3", "(Intercept)", "x3"),
  DGLM = c(coef(model_dglm), coef(model_dglm$dispersion.fit)),
  GAMLSS = c(gamlss_mu_coef, gamlss_sigma_coef)
)
sink("output/ch4_model_comparison.txt")
cat("DGLM vs GAMLSS Coefficient Comparison\n")
cat("=====================================\n\n")
print(comparison_models, digits = 4)
sink()
comp_long <- comparison_models %>%
  pivot_longer(cols = c(DGLM, GAMLSS), names_to = "Model", values_to = "Estimate")
fig_4_8 <- ggplot(comp_long, aes(x = Parameter, y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6,
           colour = "black", linewidth = 0.3) +
  facet_wrap(~Component, scales = "free") +
  scale_fill_manual(values = c("DGLM" = "grey65", "GAMLSS" = "grey30")) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Coefficient Comparison: DGLM vs. GAMLSS",
       subtitle = "Both approaches model mean and dispersion; estimates should be similar",
       x = "Parameter", y = "Estimate") +
  theme_chapter +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/fig_4_8_dglm_gamlss_comparison.jpeg", fig_4_8,
       width = 10, height = 5, dpi = 300, device = "jpeg")
cat("Figure 4.8 saved: DGLM vs GAMLSS comparison\n")
# =============================================================================
# Real Data Example: Clinical Trial with Clustering
# =============================================================================
set.seed(789)
n_clinics <- 8
patients_per_clinic <- 25
n_total <- n_clinics * patients_per_clinic
# Create clinic-level random effects (source of overdispersion)
clinic_effects <- rnorm(n_clinics, 0, 0.8)
# Patient-level data
clinic_id <- rep(1:n_clinics, each = patients_per_clinic)
age <- rnorm(n_total, 50, 10)
treatment <- rep(c(0, 1), length.out = n_total)
severity <- rnorm(n_total, 0, 1)
# Generate outcomes with clinic-level variation
lin_pred_clinical <- -0.5 + 0.02 * (age - 50) + 0.8 * treatment - 0.3 * severity +
  clinic_effects[clinic_id]
prob_clinical <- plogis(lin_pred_clinical)
improved <- rbinom(n_total, 1, prob_clinical)
clinical_data <- data.frame(
  improved = improved,
  clinic_id = factor(clinic_id),
  age = age,
  treatment = factor(treatment, labels = c("Control", "Treatment")),
  severity = severity
)
# Fit models
clinical_standard <- glm(improved ~ age + treatment + severity,
                         family = binomial, data = clinical_data)
clinical_quasi <- glm(improved ~ age + treatment + severity,
                      family = quasibinomial, data = clinical_data)
clinical_mixed <- glmer(improved ~ age + treatment + severity + (1|clinic_id),
                        family = binomial, data = clinical_data)
# Calculate dispersion
pearson_clinical <- residuals(clinical_standard, type = "pearson")
phi_clinical <- sum(pearson_clinical^2) / clinical_standard$df.residual
sink("output/ch4_clinical_example.txt")
cat("Clinical Trial Example\n")
cat("======================\n\n")
cat("Data structure:\n")
cat("- Clinics:", n_clinics, "\n")
cat("- Patients per clinic:", patients_per_clinic, "\n")
cat("- Total patients:", n_total, "\n\n")
cat("Improvement by clinic:\n")
print(table(clinical_data$clinic_id, clinical_data$improved))
cat("\nDispersion parameter:", round(phi_clinical, 3), "\n\n")
cat("Standard model:\n")
print(summary(clinical_standard)$coefficients)
cat("\nQuasi-binomial model:\n")
print(summary(clinical_quasi)$coefficients)
cat("\nMixed-effects model:\n")
print(summary(clinical_mixed)$coefficients)
cat("\nRandom effects variance:", VarCorr(clinical_mixed)$clinic_id[1], "\n")
sink()
# =============================================================================
# Figure 4.9: Clinical Trial - Clinic-Level Variation
# =============================================================================
# Summarize by clinic
clinic_summary <- clinical_data %>%
  group_by(clinic_id) %>%
  summarise(
    n = n(),
    improved = sum(improved),
    proportion = mean(improved),
    .groups = "drop"
  )
overall_prop <- mean(clinical_data$improved)
fig_4_9a <- ggplot(clinic_summary, aes(x = clinic_id, y = proportion)) +
  geom_bar(stat = "identity", fill = "grey60", width = 0.7, colour = "black",
           linewidth = 0.3) +
  geom_hline(yintercept = overall_prop, linetype = "dashed", colour = "black", linewidth = 1) +
  geom_text(aes(label = paste0(improved, "/", n)), vjust = -0.3, size = 3.5) +
  annotate("text", x = n_clinics, y = overall_prop + 0.03,
           label = paste0("Overall: ", round(overall_prop, 2)),
           hjust = 1, colour = "black") +
  labs(title = "Improvement Rate by Clinic",
       subtitle = "Variation exceeds binomial expectation (overdispersion)",
       x = "Clinic", y = "Proportion Improved") +
  theme_chapter +
  coord_cartesian(ylim = c(0, 1))
# SE comparison for treatment effect
se_clinical <- data.frame(
  Model = c("Standard", "Quasi-binomial", "Mixed-effects"),
  SE = c(
    summary(clinical_standard)$coefficients["treatmentTreatment", "Std. Error"],
    summary(clinical_quasi)$coefficients["treatmentTreatment", "Std. Error"],
    summary(clinical_mixed)$coefficients["treatmentTreatment", "Std. Error"]
  )
)
se_clinical$Model <- factor(se_clinical$Model,
                            levels = c("Standard", "Quasi-binomial", "Mixed-effects"))
fig_4_9b <- ggplot(se_clinical, aes(x = Model, y = SE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = round(SE, 3)), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("Standard" = "grey85", "Quasi-binomial" = "grey55",
                               "Mixed-effects" = "grey25")) +
  labs(title = "SE of Treatment Effect",
       subtitle = "Accounting for clustering increases uncertainty",
       x = "", y = "Standard Error") +
  theme_chapter +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(se_clinical$SE) * 1.2))
fig_4_9 <- plot_grid(fig_4_9a, fig_4_9b, ncol = 2, labels = c("A", "B"))
ggsave("figures/fig_4_9_clinical_example.jpeg", fig_4_9,
       width = 11, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 4.9 saved: Clinical trial example\n")
# =============================================================================
# Figure 4.10: Confidence Interval Comparison
# =============================================================================
# Extract treatment effect estimates and CIs
get_ci <- function(model, param = "treatmentTreatment") {
  if (inherits(model, "glmerMod")) {
    est <- fixef(model)[param]
    se <- sqrt(vcov(model)[param, param])
  } else {
    est <- coef(model)[param]
    se <- sqrt(vcov(model)[param, param])
  }
  c(estimate = est, lower = est - 1.96*se, upper = est + 1.96*se)
}
ci_comparison <- data.frame(
  Model = c("Standard", "Quasi-binomial", "Mixed-effects"),
  rbind(
    get_ci(clinical_standard),
    get_ci(clinical_quasi),
    get_ci(clinical_mixed)
  )
)
ci_comparison$Model <- factor(ci_comparison$Model,
                              levels = c("Standard", "Quasi-binomial", "Mixed-effects"))
# Convert to OR scale
ci_comparison$OR <- exp(ci_comparison$estimate)
ci_comparison$OR_lower <- exp(ci_comparison$lower)
ci_comparison$OR_upper <- exp(ci_comparison$upper)
fig_4_10 <- ggplot(ci_comparison, aes(x = Model, y = OR, shape = Model)) +
  geom_point(size = 4, colour = "black") +
  geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), width = 0.2, linewidth = 1,
                colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("Standard" = 16, "Quasi-binomial" = 17,
                                "Mixed-effects" = 15)) +
  scale_y_log10() +
  labs(title = "Treatment Effect: Odds Ratio with 95% CI",
       subtitle = "Standard model underestimates uncertainty; corrected models give wider CIs",
       x = "", y = "Odds Ratio (log scale)") +
  theme_chapter +
  theme(legend.position = "none")
ggsave("figures/fig_4_10_ci_comparison.jpeg", fig_4_10,
       width = 7, height = 5, dpi = 300, device = "jpeg")
cat("Figure 4.10 saved: CI comparison\n")
# =============================================================================
# Summary Statistics
# =============================================================================
sink("output/ch4_chapter_summary.txt")
cat("Chapter 4 Summary Statistics\n")
cat("============================\n\n")
cat("Simulated Overdispersion Data:\n")
cat("- Sample size:", n, "\n")
cat("- Trials per observation:", trials, "\n")
cat("- Estimated dispersion (phi):", round(phi_hat, 3), "\n\n")
cat("DGLM Dispersion Model:\n")
cat("- Baseline dispersion: exp(", round(disp_coef[1], 3), ") =", round(exp(disp_coef[1]), 3), "\n")
cat("- x3 effect: exp(", round(disp_coef[2], 3), ") =", round(exp(disp_coef[2]), 3), "\n\n")
cat("Clinical Trial Example:\n")
cat("- Clinics:", n_clinics, "\n")
cat("- Total patients:", n_total, "\n")
cat("- Dispersion:", round(phi_clinical, 3), "\n")
cat("- Random effects SD:", round(sqrt(VarCorr(clinical_mixed)$clinic_id[1]), 3), "\n\n")
cat("Key Findings:\n")
cat("1. Overdispersion inflates Type I error when ignored\n")
cat("2. Quasi-binomial adjusts SEs by factor of sqrt(phi)\n")
cat("3. DGLM allows modeling factors affecting dispersion\n")
cat("4. Mixed-effects models appropriate for clustered data\n")
cat("5. GAMLSS provides flexible alternative framework\n")
sink()
cat("\n=== All Chapter 4 figures and outputs generated successfully ===\n")
cat("Figures saved to: figures/\n")
cat("  fig_4_1_overdispersion_concept.jpeg\n")
cat("  fig_4_2_varying_dispersion.jpeg\n")
cat("  fig_4_3_overdispersion_diagnostics.jpeg\n")
cat("  fig_4_4_se_comparison.jpeg\n")
cat("  fig_4_5_coefficient_comparison.jpeg\n")
cat("  fig_4_6_dispersion_model.jpeg\n")
cat("  fig_4_7_predictions_with_dispersion.jpeg\n")
cat("  fig_4_8_dglm_gamlss_comparison.jpeg\n")
cat("  fig_4_9_clinical_example.jpeg\n")
cat("  fig_4_10_ci_comparison.jpeg\n")
cat("Outputs saved to: output/\n")