# =============================================================================
# Chapter 2: Complete and Quasi-Complete Separation
# Figures and Analysis Code
# =============================================================================
# Install required Packages
#install.packages(c("tidyverse", "logistf", "brglm2", "detectseparation",
#                  "arm", "cowplot", "reshape2", "pROC", "scales"))
# Load required packages
library(tidyverse)
library(logistf)           # For Firth logistic regression
library(brglm2)            # For bias-reduction methods
library(detectseparation)  # For detecting separation
library(arm)               # For Bayesian GLM
library(cowplot)           # For combining plots
library(reshape2)          # For data reshaping
library(pROC)              # For ROC analysis
library(scales)            # For plot formatting
# Resolve namespace conflicts (MASS masks dplyr functions)
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
# Figure 2.1: Complete Separation Visualization
# =============================================================================
# Simulate data with complete separation
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- ifelse(x1 > 0, 1, 0)  # Perfect separation by x1
sep_data <- data.frame(y = y, x1 = x1, x2 = x2)
# Create the plot
fig_2_1 <- ggplot(sep_data, aes(x = x1, y = x2, shape = factor(y))) +
  geom_point(size = 3, alpha = 0.7, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 1) +
  annotate("text", x = -1.5, y = 2.5, label = "All Y = 0", fontface = "bold", size = 4) +
  annotate("text", x = 1.5, y = 2.5, label = "All Y = 1", fontface = "bold", size = 4) +
  annotate("text", x = 0.15, y = -2.5, label = "Separation\nboundary",
           hjust = 0, size = 3.5, colour = "grey30") +
  scale_shape_manual(values = c("0" = 1, "1" = 17),
                     labels = c("Y = 0", "Y = 1")) +
  labs(title = "Complete Separation Example",
       subtitle = expression("The predictor " * x[1] * " perfectly separates the outcome classes"),
       x = expression(x[1]),
       y = expression(x[2]),
       shape = "Outcome") +
  theme_chapter +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
ggsave("figures/fig_2_1_complete_separation.jpeg", fig_2_1,
       width = 7, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 2.1 saved: Complete separation visualization\n")
# =============================================================================
# Figure 2.2: Quasi-Complete Separation Visualization
# =============================================================================
set.seed(123)
# Simulate data with quasi-complete separation
z <- ifelse(x1 > 0, 1, 0)
# Add some overlap (flip 5 observations)
overlap_indices <- c(which(x1 > 0 & x1 < 0.3)[1:3], which(x1 < 0 & x1 > -0.3)[1:2])
overlap_indices <- overlap_indices[!is.na(overlap_indices)]
z[overlap_indices] <- 1 - z[overlap_indices]
qsep_data <- data.frame(z = z, x1 = x1, x2 = x2)
# Identify overlap points
qsep_data$overlap <- FALSE
qsep_data$overlap[overlap_indices] <- TRUE
fig_2_2 <- ggplot(qsep_data, aes(x = x1, y = x2, shape = factor(z))) +
  geom_point(data = subset(qsep_data, !overlap), size = 3, alpha = 0.7, colour = "black") +
  geom_point(data = subset(qsep_data, overlap), size = 4, alpha = 1, stroke = 1.5, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 1) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -3, ymax = 3,
           alpha = 0.15, fill = "grey70") +
  annotate("text", x = 0, y = -2.8, label = "Overlap\nregion", size = 3.5, colour = "grey30") +
  scale_shape_manual(values = c("0" = 1, "1" = 17),
                     labels = c("Y = 0", "Y = 1")) +
  labs(title = "Quasi-Complete Separation Example",
       subtitle = "Near-perfect separation with some overlap at the boundary",
       x = expression(x[1]),
       y = expression(x[2]),
       shape = "Outcome") +
  theme_chapter +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
ggsave("figures/fig_2_2_quasi_separation.jpeg", fig_2_2,
       width = 7, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 2.2 saved: Quasi-complete separation visualization\n")
# =============================================================================
# Model Fitting and Output Capture
# =============================================================================
# Attempt standard logistic regression (will produce warnings)
sep_model <- suppressWarnings(
  glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
      control = glm.control(maxit = 50))
)
# Capture the summary output
sink("output/ch2_sep_model_summary.txt")
cat("Standard Logistic Regression with Complete Separation\n")
cat("=====================================================\n\n")
summary(sep_model)
sink()
# Fit Firth's penalized logistic regression
firth_model <- logistf(y ~ x1 + x2, data = sep_data)
sink("output/ch2_firth_model_summary.txt")
cat("Firth's Penalized Logistic Regression\n")
cat("=====================================\n\n")
summary(firth_model)
sink()
# Fit using brglm2
br_model <- glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
                method = "brglmFit")
sink("output/ch2_brglm_model_summary.txt")
cat("Bias-Reduced GLM (brglm2)\n")
cat("=========================\n\n")
summary(br_model)
sink()
# Fit Bayesian model
bayes_model <- bayesglm(y ~ x1 + x2, family = binomial(link = "logit"),
                        data = sep_data,
                        prior.scale = 2.5,
                        prior.df = 1)
sink("output/ch2_bayes_model_summary.txt")
cat("Bayesian GLM with Weakly Informative Priors\n")
cat("============================================\n\n")
summary(bayes_model)
sink()
# Formal separation detection
sep_check <- glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
                 method = "detect_separation")
sink("output/ch2_separation_check.txt")
cat("Separation Detection Results\n")
cat("============================\n\n")
print(sep_check)
sink()
cat("Model outputs saved to output/ directory\n")
# =============================================================================
# Figure 2.3: Coefficient Comparison Across Methods
# =============================================================================
# Create coefficient comparison table
coef_comparison <- data.frame(
  Method = c("Standard MLE", "Firth", "BR-GLM", "Bayesian"),
  Intercept = c(coef(sep_model)[1], coef(firth_model)[1],
                coef(br_model)[1], coef(bayes_model)[1]),
  x1 = c(coef(sep_model)[2], coef(firth_model)[2],
         coef(br_model)[2], coef(bayes_model)[2]),
  x2 = c(coef(sep_model)[3], coef(firth_model)[3],
         coef(br_model)[3], coef(bayes_model)[3])
)
# Save coefficient comparison
sink("output/ch2_coefficient_comparison.txt")
cat("Coefficient Comparison Across Methods\n")
cat("=====================================\n\n")
print(coef_comparison, digits = 3)
sink()
# Reshape for plotting
coef_long <- coef_comparison %>%
  pivot_longer(cols = c(Intercept, x1, x2),
               names_to = "Coefficient",
               values_to = "Estimate") %>%
  mutate(Method = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")))
# Create the comparison plot
fig_2_3 <- ggplot(coef_long, aes(x = Coefficient, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Standard MLE" = "grey90", "Firth" = "grey60",
                               "BR-GLM" = "grey35", "Bayesian" = "grey10")) +
  labs(title = "Coefficient Estimates by Method",
       subtitle = "Standard MLE produces extreme estimates under separation",
       x = "Coefficient",
       y = "Estimate",
       fill = "Method") +
  theme_chapter +
  theme(axis.text.x = element_text(size = 10)) +
  coord_cartesian(ylim = c(-5, 30))
ggsave("figures/fig_2_3_coefficient_comparison.jpeg", fig_2_3,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.3 saved: Coefficient comparison\n")
# =============================================================================
# Figure 2.4: Likelihood Surface Illustration
# =============================================================================
# Create a grid for the likelihood surface (simplified 2D case)
beta1_seq <- seq(-5, 25, length.out = 100)
loglik <- sapply(beta1_seq, function(b1) {
  eta <- b1 * sep_data$x1
  p <- plogis(eta)
  # Avoid log(0)
  p <- pmax(pmin(p, 1 - 1e-10), 1e-10)
  sum(sep_data$y * log(p) + (1 - sep_data$y) * log(1 - p))
})
# Penalized log-likelihood (Firth)
pen_loglik <- sapply(beta1_seq, function(b1) {
  eta <- b1 * sep_data$x1
  p <- plogis(eta)
  p <- pmax(pmin(p, 1 - 1e-10), 1e-10)
  # Standard log-likelihood
  ll <- sum(sep_data$y * log(p) + (1 - sep_data$y) * log(1 - p))
  # Jeffrey's prior penalty (simplified)
  penalty <- -0.5 * log(1 + b1^2 / 10)
  ll + penalty * 5  # Scale for visualization
})
likelihood_data <- data.frame(
  beta1 = rep(beta1_seq, 2),
  loglik = c(loglik, pen_loglik),
  Type = rep(c("Standard", "Penalized (Firth)"), each = length(beta1_seq))
)
fig_2_4 <- ggplot(likelihood_data, aes(x = beta1, y = loglik, linetype = Type)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_vline(xintercept = coef(firth_model)[2], linetype = "dotted",
             colour = "grey40", linewidth = 0.8) +
  annotate("text", x = coef(firth_model)[2] + 1, y = -60,
           label = "Firth\nestimate", hjust = 0, size = 3, colour = "grey40") +
  annotate("segment", x = 20, xend = 24, y = -20, yend = -20,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 17, y = -20, label = expression("MLE " %->% " \u221E"),
           colour = "black", size = 3.5) +
  scale_linetype_manual(values = c("Standard" = "solid", "Penalized (Firth)" = "dashed")) +
  labs(title = "Log-Likelihood Under Separation",
       subtitle = expression("Standard likelihood is monotonic; penalized likelihood has finite maximum"),
       x = expression("Coefficient " * beta[1]),
       y = "Log-likelihood",
       linetype = "Method") +
  theme_chapter
ggsave("figures/fig_2_4_likelihood_surface.jpeg", fig_2_4,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.4 saved: Likelihood surface illustration\n")
# =============================================================================
# Figure 2.5: Predicted Probabilities Comparison
# =============================================================================
# Helper function for logistf predictions (handles different package versions)
predict_logistf <- function(model, newdata) {
  # Get coefficients
  beta <- coef(model)
  # Create model matrix from newdata
  formula_terms <- attr(terms(model$formula), "term.labels")
  X <- model.matrix(as.formula(paste("~", paste(formula_terms, collapse = "+"))), data = newdata)
  # Compute linear predictor and transform to probability
  eta <- as.vector(X %*% beta)
  plogis(eta)
}
# Create prediction grid
grid_data <- data.frame(x1 = seq(-3, 3, length.out = 200), x2 = 0)
# Get predictions from each model
pred_standard <- predict(sep_model, newdata = grid_data, type = "response")
pred_firth <- predict_logistf(firth_model, newdata = grid_data)
pred_brglm <- predict(br_model, newdata = grid_data, type = "response")
pred_bayes <- predict(bayes_model, newdata = grid_data, type = "response")
predictions <- data.frame(
  x1 = grid_data$x1,
  `Standard MLE` = pred_standard,
  `Firth` = pred_firth,
  `BR-GLM` = pred_brglm,
  `Bayesian` = pred_bayes,
  check.names = FALSE
)
pred_long <- predictions %>%
  pivot_longer(cols = -x1, names_to = "Method", values_to = "Probability") %>%
  mutate(Method = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")))
# Add data points
data_points <- sep_data %>%
  dplyr::select(x1, y) %>%
  mutate(y_jitter = y + runif(dplyr::n(), -0.03, 0.03))
fig_2_5 <- ggplot() +
  geom_point(data = data_points, aes(x = x1, y = y_jitter, shape = factor(y)),
             alpha = 0.4, size = 2, colour = "grey50", show.legend = FALSE) +
  geom_line(data = pred_long, aes(x = x1, y = Probability, linetype = Method),
            linewidth = 1, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_shape_manual(values = c("0" = 1, "1" = 17)) +
  scale_linetype_manual(values = c("Standard MLE" = "solid", "Firth" = "dashed",
                                   "BR-GLM" = "dotdash", "Bayesian" = "twodash")) +
  labs(title = "Predicted Probabilities by Method",
       subtitle = "Standard MLE produces step-function predictions; alternatives are smoother",
       x = expression(x[1]),
       y = "Predicted Probability",
       linetype = "Method") +
  theme_chapter +
  theme(legend.position = "right")
ggsave("figures/fig_2_5_predicted_probabilities.jpeg", fig_2_5,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.5 saved: Predicted probabilities comparison\n")
# =============================================================================
# Figure 2.6: Standard Errors Comparison
# =============================================================================
# Extract standard errors
se_standard <- summary(sep_model)$coefficients[, 2]
se_firth <- sqrt(diag(vcov(firth_model)))
se_brglm <- summary(br_model)$coefficients[, 2]
se_bayes <- summary(bayes_model)$coefficients[, 2]
se_comparison <- data.frame(
  Method = rep(c("Standard MLE", "Firth", "BR-GLM", "Bayesian"), each = 3),
  Coefficient = rep(c("Intercept", "x1", "x2"), 4),
  SE = c(se_standard, se_firth, se_brglm, se_bayes)
)
se_comparison$Method <- factor(se_comparison$Method,
                               levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian"))
# Cap extreme values for visualization
se_comparison$SE_capped <- pmin(se_comparison$SE, 50)
se_comparison$truncated <- se_comparison$SE > 50
fig_2_6 <- ggplot(se_comparison, aes(x = Coefficient, y = SE_capped, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_text(data = subset(se_comparison, truncated),
            aes(label = sprintf("%.0f", SE)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Standard MLE" = "grey90", "Firth" = "grey60",
                               "BR-GLM" = "grey35", "Bayesian" = "grey10")) +
  labs(title = "Standard Errors by Method",
       subtitle = "Standard MLE produces inflated SEs under separation; alternatives provide stability",
       x = "Coefficient",
       y = "Standard Error",
       fill = "Method") +
  theme_chapter +
  coord_cartesian(ylim = c(0, 55)) +
  annotate("text", x = 2, y = 52, label = "(truncated)", size = 3, colour = "grey50")
ggsave("figures/fig_2_6_standard_errors.jpeg", fig_2_6,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.6 saved: Standard errors comparison\n")
# Save SE comparison table
sink("output/ch2_se_comparison.txt")
cat("Standard Error Comparison Across Methods\n")
cat("=========================================\n\n")
se_wide <- se_comparison %>%
  dplyr::select(-SE_capped, -truncated) %>%
  pivot_wider(names_from = Coefficient, values_from = SE)
print(se_wide, digits = 3)
sink()
# =============================================================================
# Real Data Example: Endometrial Cancer Dataset
# =============================================================================
# The endometrial cancer dataset from Heinze & Schemper (2002)
# We'll create a similar dataset structure
set.seed(456)
# Simulate data similar to endometrial cancer study
n_endo <- 79
NV <- rbinom(n_endo, 1, 0.4)  # Neovascularization
PI <- runif(n_endo, 0.05, 0.95)  # Prognostic Index
EH <- rbinom(n_endo, 1, 0.45)  # Endometrial Hyperplasia
# Create outcome with near-perfect separation on NV
prob_hg <- plogis(-2 + 3.5*NV + 1.5*PI + 0.5*EH)
HG <- rbinom(n_endo, 1, prob_hg)
# Ensure near-separation: make all NV=1 cases have HG=1 except one
HG[NV == 1] <- 1
HG[which(NV == 1)[1]] <- 0  # One exception for quasi-separation
endo_data <- data.frame(HG = HG, NV = NV, PI = PI, EH = EH)
# Check separation
table(endo_data$NV, endo_data$HG)
sink("output/ch2_endo_crosstab.txt")
cat("Cross-tabulation: Neovascularization vs. Histology Grade\n")
cat("=========================================================\n\n")
print(table(NV = endo_data$NV, HG = endo_data$HG))
cat("\nNote: Only 1 case with NV=1 has HG=0, indicating quasi-complete separation\n")
sink()
# Fit models
endo_glm <- suppressWarnings(
  glm(HG ~ NV + PI + EH, family = binomial, data = endo_data)
)
endo_firth <- logistf(HG ~ NV + PI + EH, data = endo_data)
endo_brglm <- glm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
                  method = "brglmFit")
endo_bayes <- bayesglm(HG ~ NV + PI + EH, family = binomial, data = endo_data)
# Check for separation
endo_sep <- glm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
                method = "detect_separation")
sink("output/ch2_endo_separation.txt")
cat("Separation Detection for Endometrial Data\n")
cat("==========================================\n\n")
print(endo_sep)
sink()
# =============================================================================
# Figure 2.7: Endometrial Data - Coefficient Forest Plot
# =============================================================================
# Create forest plot data
get_or_ci <- function(model, method_name) {
  if (inherits(model, "logistf")) {
    coefs <- coef(model)
    ci <- confint(model)
    data.frame(
      Method = method_name,
      Variable = names(coefs),
      OR = exp(coefs),
      Lower = exp(ci[, 1]),
      Upper = exp(ci[, 2])
    )
  } else {
    coefs <- coef(model)
    se <- summary(model)$coefficients[, 2]
    ci_low <- coefs - 1.96 * se
    ci_high <- coefs + 1.96 * se
    data.frame(
      Method = method_name,
      Variable = names(coefs),
      OR = exp(coefs),
      Lower = exp(ci_low),
      Upper = exp(ci_high)
    )
  }
}
forest_data <- bind_rows(
  get_or_ci(endo_glm, "Standard MLE"),
  get_or_ci(endo_firth, "Firth"),
  get_or_ci(endo_brglm, "BR-GLM"),
  get_or_ci(endo_bayes, "Bayesian")
) %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  mutate(
    Method = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")),
    Variable = factor(Variable, levels = c("NV", "PI", "EH"),
                      labels = c("Neovascularization", "Prognostic Index", "Endometrial Hyperplasia"))
  )
# Cap extreme CIs for visualization
forest_data$Upper_capped <- pmin(forest_data$Upper, 500)
forest_data$OR_capped <- pmin(forest_data$OR, 500)
fig_2_7 <- ggplot(forest_data, aes(x = OR_capped, y = Method, shape = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper_capped), height = 0.2, linewidth = 0.8,
                 colour = "black") +
  geom_point(size = 3, colour = "black") +
  facet_wrap(~Variable, ncol = 1, scales = "free_x") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 50, 100, 500),
                labels = c("0.1", "0.5", "1", "2", "5", "10", "50", "100", "500+")) +
  scale_shape_manual(values = c("Standard MLE" = 16, "Firth" = 17,
                                "BR-GLM" = 15, "Bayesian" = 18)) +
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       subtitle = "Endometrial cancer data: NV exhibits quasi-complete separation",
       x = "Odds Ratio (log scale)",
       y = "") +
  theme_chapter +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))
ggsave("figures/fig_2_7_forest_plot.jpeg", fig_2_7,
       width = 8, height = 6, dpi = 300, device = "jpeg")
cat("Figure 2.7 saved: Forest plot for endometrial data\n")
# Save odds ratio comparison
sink("output/ch2_endo_odds_ratios.txt")
cat("Odds Ratio Comparison - Endometrial Cancer Data\n")
cat("================================================\n\n")
or_wide <- forest_data %>%
  mutate(OR_CI = sprintf("%.2f (%.2f, %.2f)", OR, Lower, pmin(Upper, 999))) %>%
  dplyr::select(Variable, Method, OR_CI) %>%
  pivot_wider(names_from = Method, values_from = OR_CI)
print(or_wide, width = 120)
sink()
# =============================================================================
# Figure 2.8: Profile Likelihood for NV Coefficient
# =============================================================================
# Compute profile likelihood for the NV coefficient
nv_range <- seq(0, 8, length.out = 100)
profile_ll <- sapply(nv_range, function(b_nv) {
  # Fix NV coefficient and optimize others
  offset_term <- b_nv * endo_data$NV
  tryCatch({
    fit <- glm(HG ~ offset(offset_term) + PI + EH,
               family = binomial, data = endo_data)
    logLik(fit)
  }, error = function(e) NA)
})
# Penalized profile (simplified)
profile_ll_pen <- sapply(nv_range, function(b_nv) {
  offset_term <- b_nv * endo_data$NV
  tryCatch({
    fit <- glm(HG ~ offset(offset_term) + PI + EH,
               family = binomial, data = endo_data)
    as.numeric(logLik(fit)) - 0.5 * b_nv^2 / 16  # Penalty term
  }, error = function(e) NA)
})
profile_data <- data.frame(
  beta_NV = rep(nv_range, 2),
  loglik = c(profile_ll, profile_ll_pen),
  Type = rep(c("Standard", "Penalized"), each = length(nv_range))
) %>%
  dplyr::filter(!is.na(loglik))
# Normalize for comparison
profile_data <- profile_data %>%
  group_by(Type) %>%
  mutate(loglik_norm = loglik - max(loglik, na.rm = TRUE))
fig_2_8 <- ggplot(profile_data, aes(x = beta_NV, y = loglik_norm, linetype = Type)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_hline(yintercept = -1.92, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 6, y = -1.5, label = "95% CI boundary\n(\u03C7\u00B2 = 3.84)",
           size = 3, colour = "grey50") +
  scale_linetype_manual(values = c("Standard" = "solid", "Penalized" = "dashed")) +
  labs(title = "Profile Log-Likelihood for Neovascularization Coefficient",
       subtitle = "Standard profile extends to infinity; penalized profile achieves maximum",
       x = expression("Coefficient " * beta[NV]),
       y = "Normalized Log-likelihood",
       linetype = "Method") +
  theme_chapter +
  coord_cartesian(xlim = c(0, 8), ylim = c(-5, 0.5))
ggsave("figures/fig_2_8_profile_likelihood.jpeg", fig_2_8,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.8 saved: Profile likelihood\n")
# =============================================================================
# Figure 2.9: Decision Flowchart Data (for TikZ in LaTeX)
# =============================================================================
# This will be created in LaTeX using TikZ, but we save the logic
sink("output/ch2_decision_flowchart.txt")
cat("Decision Flowchart for Handling Separation\n")
cat("==========================================\n\n")
cat("1. Fit standard logistic regression\n")
cat("2. Check for warning signs:\n")
cat("   - Large coefficients (|β| > 10)\n")
cat("   - Large standard errors (SE > 10)\n")
cat("   - Non-convergence warnings\n")
cat("3. Run formal separation test (detectseparation)\n")
cat("4. If separation detected:\n")
cat("   a. Primary: Use Firth's method (logistf)\n")
cat("   b. Alternative: Bayesian with weakly informative priors\n")
cat("   c. Alternative: BR-GLM (brglm2)\n")
cat("5. Report both detection and method used\n")
sink()
# =============================================================================
# Figure 2.10: Simulation Study - Bias Under Separation
# =============================================================================
set.seed(789)
n_sims <- 500
sample_sizes <- c(50, 100, 200)
results_list <- list()
for (n_sim in sample_sizes) {
  bias_standard <- numeric(n_sims)
  bias_firth <- numeric(n_sims)
  converged <- logical(n_sims)
  for (i in 1:n_sims) {
    # Generate data with near-separation
    x <- rnorm(n_sim)
    true_beta <- 2
    prob <- plogis(true_beta * x)
    y <- rbinom(n_sim, 1, prob)
    # Fit models
    fit_std <- suppressWarnings(
      glm(y ~ x, family = binomial)
    )
    fit_firth <- suppressWarnings(
      logistf(y ~ x, data = data.frame(y = y, x = x), pl = FALSE)
    )
    bias_standard[i] <- coef(fit_std)[2] - true_beta
    bias_firth[i] <- coef(fit_firth)[2] - true_beta
    converged[i] <- fit_std$converged
  }
  results_list[[as.character(n_sim)]] <- data.frame(
    n = n_sim,
    Method = rep(c("Standard MLE", "Firth"), each = n_sims),
    Bias = c(bias_standard, bias_firth)
  )
}
sim_results <- bind_rows(results_list) %>%
  mutate(n = factor(n, levels = sample_sizes, labels = paste("n =", sample_sizes)))
fig_2_10 <- ggplot(sim_results, aes(x = Method, y = Bias, fill = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, colour = "black") +
  facet_wrap(~n) +
  scale_fill_manual(values = c("Standard MLE" = "grey80", "Firth" = "grey40")) +
  labs(title = "Bias in Coefficient Estimates: Simulation Study",
       subtitle = expression("True " * beta * " = 2; 500 simulations per sample size"),
       x = "",
       y = expression("Bias (" * hat(beta) * " - " * beta * ")")) +
  theme_chapter +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-2, 5))
ggsave("figures/fig_2_10_simulation_bias.jpeg", fig_2_10,
       width = 9, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.10 saved: Simulation study results\n")
# Save simulation summary
sink("output/ch2_simulation_summary.txt")
cat("Simulation Study Summary\n")
cat("========================\n\n")
sim_summary <- sim_results %>%
  group_by(n, Method) %>%
  summarise(
    Mean_Bias = mean(Bias, na.rm = TRUE),
    Median_Bias = median(Bias, na.rm = TRUE),
    SD_Bias = sd(Bias, na.rm = TRUE),
    RMSE = sqrt(mean(Bias^2, na.rm = TRUE)),
    .groups = "drop"
  )
print(sim_summary, digits = 3)
sink()
# =============================================================================
# Summary Statistics for Chapter
# =============================================================================
sink("output/ch2_chapter_summary.txt")
cat("Chapter 2 Summary Statistics\n")
cat("============================\n\n")
cat("Complete Separation Example:\n")
cat("- Sample size: ", n, "\n")
cat("- Outcome prevalence: ", mean(sep_data$y), "\n")
cat("- Standard MLE x1 coefficient: ", coef(sep_model)[2], "\n")
cat("- Firth x1 coefficient: ", coef(firth_model)[2], "\n\n")
cat("Endometrial Cancer Example:\n")
cat("- Sample size: ", n_endo, "\n")
cat("- Outcome prevalence: ", mean(endo_data$HG), "\n")
cat("- Cases with NV=1 and HG=0: ", sum(endo_data$NV == 1 & endo_data$HG == 0), "\n")
cat("- Cases with NV=1 and HG=1: ", sum(endo_data$NV == 1 & endo_data$HG == 1), "\n\n")
cat("Key Recommendations:\n")
cat("1. Always check for separation with detectseparation()\n")
cat("2. Use Firth's method as primary solution\n")
cat("3. Bayesian approaches with weakly informative priors as alternative\n")
cat("4. Report separation detection and correction method\n")
sink()
cat("\n=== All Chapter 2 figures and outputs generated successfully ===\n")
cat("Figures saved to: figures/\n")
cat("  fig_2_1_complete_separation.jpeg\n")
cat("  fig_2_2_quasi_separation.jpeg\n")
cat("  fig_2_3_coefficient_comparison.jpeg\n")
cat("  fig_2_4_likelihood_surface.jpeg\n")
cat("  fig_2_5_predicted_probabilities.jpeg\n")
cat("  fig_2_6_standard_errors.jpeg\n")
cat("  fig_2_7_forest_plot.jpeg\n")
cat("  fig_2_8_profile_likelihood.jpeg\n")
cat("  fig_2_10_simulation_bias.jpeg\n")
cat("Outputs saved to: output/\n")
