# =============================================================================
# Chapter 3: Rare Events Bias
# Figures and Analysis Code
# =============================================================================
# Load required packages
library(tidyverse)
library(logistf)           # For Firth logistic regression
library(brglm2)            # For bias-reduction methods
library(ROSE)              # For ROSE sampling
library(pROC)              # For ROC analysis
library(cowplot)           # For combining plots
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
# Figure 3.1: Visualizing Rare Events Data
# =============================================================================
# Simulate rare events data with known parameters
n <- 2000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
# True coefficients
beta0_true <- -4      # Low intercept creates rare events
beta1_true <- 0.8
beta2_true <- -0.5
beta3_true <- 0.6
# Generate outcome with ~3% event rate
linear_pred <- beta0_true + beta1_true*x1 + beta2_true*x2 + beta3_true*x3
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
rare_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
# Calculate event rate
event_rate <- mean(y)
n_events <- sum(y)
cat("Event rate:", round(event_rate * 100, 2), "%\n")
cat("Number of events:", n_events, "out of", n, "\n")
# Save event rate info
sink("output/ch3_event_rate.txt")
cat("Rare Events Data Summary\n")
cat("========================\n\n")
cat("Total observations:", n, "\n")
cat("Number of events:", n_events, "\n")
cat("Event rate:", round(event_rate * 100, 2), "%\n")
cat("\nTrue coefficients:\n")
cat("  Intercept:", beta0_true, "\n")
cat("  x1:", beta1_true, "\n")
cat("  x2:", beta2_true, "\n")
cat("  x3:", beta3_true, "\n")
sink()
# Visualize the class imbalance
class_dist <- data.frame(
  Outcome = factor(c("Non-event (Y=0)", "Event (Y=1)"),
                   levels = c("Non-event (Y=0)", "Event (Y=1)")),
  Count = c(sum(y == 0), sum(y == 1)),
  Proportion = c(mean(y == 0), mean(y == 1))
)
fig_3_1a <- ggplot(class_dist, aes(x = Outcome, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", width = 0.6, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0(Count, "\n(", round(Proportion*100, 1), "%)")),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("Non-event (Y=0)" = "grey70", "Event (Y=1)" = "grey30")) +
  labs(title = "Class Distribution in Rare Events Data",
       subtitle = paste0("Event rate: ", round(event_rate*100, 1), "% (", n_events, " events out of ", n, " observations)"),
       x = "", y = "Count") +
  theme_chapter +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(class_dist$Count) * 1.15))
# Distribution of predicted probabilities
fig_3_1b <- ggplot(rare_data, aes(x = prob)) +
  geom_histogram(bins = 50, fill = "grey60", colour = "white", alpha = 0.7) +
  geom_vline(xintercept = event_rate, linetype = "dashed", colour = "black", linewidth = 1) +
  annotate("text", x = event_rate + 0.02, y = Inf, vjust = 2, hjust = 0,
           label = paste0("Mean event rate = ", round(event_rate*100, 1), "%"),
           colour = "black", size = 3.5) +
  labs(title = "Distribution of True Probabilities",
       subtitle = "Most observations have very low probability of the event",
       x = "True Probability P(Y=1)", y = "Count") +
  theme_chapter +
  scale_x_continuous(labels = percent_format())
fig_3_1 <- plot_grid(fig_3_1a, fig_3_1b, ncol = 2, labels = c("A", "B"))
ggsave("figures/fig_3_1_rare_events_distribution.jpeg", fig_3_1,
       width = 10, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 3.1 saved: Rare events distribution\n")
# =============================================================================
# Model Fitting: Standard, Firth, and BR-GLM
# =============================================================================
# Standard logistic regression
standard_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = rare_data)
sink("output/ch3_standard_model.txt")
cat("Standard Logistic Regression\n")
cat("============================\n\n")
print(summary(standard_model))
sink()
# Firth's penalized logistic regression
firth_model <- logistf(y ~ x1 + x2 + x3, data = rare_data)
sink("output/ch3_firth_model.txt")
cat("Firth's Penalized Logistic Regression\n")
cat("=====================================\n\n")
print(summary(firth_model))
sink()
# Bias-reduced GLM
br_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = rare_data,
                method = "brglmFit")
sink("output/ch3_brglm_model.txt")
cat("Bias-Reduced GLM (brglm2)\n")
cat("=========================\n\n")
print(summary(br_model))
sink()
cat("Model outputs saved\n")
# =============================================================================
# Figure 3.2: Coefficient Bias Comparison
# =============================================================================
# True coefficients
true_coefs <- c(beta0_true, beta1_true, beta2_true, beta3_true)
names(true_coefs) <- c("Intercept", "x1", "x2", "x3")
# Extract estimates
coef_comparison <- data.frame(
  Parameter = c("Intercept", "x1", "x2", "x3"),
  True = true_coefs,
  Standard = coef(standard_model),
  Firth = coef(firth_model),
  `BR-GLM` = coef(br_model),
  check.names = FALSE
)
# Calculate bias
bias_comparison <- data.frame(
  Parameter = coef_comparison$Parameter,
  Standard = coef_comparison$Standard - coef_comparison$True,
  Firth = coef_comparison$Firth - coef_comparison$True,
  `BR-GLM` = coef_comparison$`BR-GLM` - coef_comparison$True,
  check.names = FALSE
)
# Save comparison
sink("output/ch3_coefficient_comparison.txt")
cat("Coefficient Comparison\n")
cat("======================\n\n")
cat("Estimates:\n")
print(coef_comparison, digits = 4)
cat("\nBias (Estimate - True):\n")
print(bias_comparison, digits = 4)
sink()
# Reshape for plotting
coef_long <- coef_comparison %>%
  pivot_longer(cols = c(Standard, Firth, `BR-GLM`),
               names_to = "Method", values_to = "Estimate") %>%
  mutate(Method = factor(Method, levels = c("Standard", "Firth", "BR-GLM")))
fig_3_2 <- ggplot(coef_long, aes(x = Parameter, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_point(data = coef_comparison, aes(x = Parameter, y = True),
             inherit.aes = FALSE, shape = 4, size = 4, stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Standard" = "grey85", "Firth" = "grey55", "BR-GLM" = "grey25")) +
  labs(title = "Coefficient Estimates vs. True Values",
       subtitle = "\u00D7 marks indicate true parameter values; bars show estimates",
       x = "Parameter", y = "Estimate", fill = "Method") +
  theme_chapter
ggsave("figures/fig_3_2_coefficient_bias.jpeg", fig_3_2,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 3.2 saved: Coefficient bias comparison\n")
# =============================================================================
# Figure 3.3: Bias as a Function of Event Rate (Simulation)
# =============================================================================
set.seed(456)
n_sims <- 200
event_rates <- c(0.01, 0.02, 0.03, 0.05, 0.10, 0.20, 0.30, 0.50)
n_obs <- 1000
true_beta1 <- 0.8
bias_results <- data.frame()
for (er in event_rates) {
  # Calculate intercept needed to achieve target event rate
  # Using approximation: P(Y=1) ≈ expit(beta0) when predictors centered
  target_intercept <- qlogis(er)
  bias_std <- numeric(n_sims)
  bias_firth <- numeric(n_sims)
  for (i in 1:n_sims) {
    x <- rnorm(n_obs)
    lp <- target_intercept + true_beta1 * x
    p <- plogis(lp)
    y_sim <- rbinom(n_obs, 1, p)
    # Skip if no events or all events
    if (sum(y_sim) < 3 || sum(y_sim) > n_obs - 3) next
    # Fit models
    fit_std <- suppressWarnings(glm(y_sim ~ x, family = binomial))
    fit_firth <- suppressWarnings(logistf(y_sim ~ x, data = data.frame(y_sim, x), pl = FALSE))
    bias_std[i] <- coef(fit_std)[2] - true_beta1
    bias_firth[i] <- coef(fit_firth)[2] - true_beta1
  }
  bias_results <- rbind(bias_results, data.frame(
    EventRate = er,
    Method = rep(c("Standard MLE", "Firth"), each = n_sims),
    Bias = c(bias_std, bias_firth)
  ))
}
# Remove NAs
bias_results <- bias_results %>% dplyr::filter(!is.na(Bias))
# Summarize
bias_summary <- bias_results %>%
  group_by(EventRate, Method) %>%
  summarise(
    MeanBias = mean(Bias, na.rm = TRUE),
    MedianBias = median(Bias, na.rm = TRUE),
    SE = sd(Bias, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
fig_3_3 <- ggplot(bias_summary, aes(x = EventRate, y = MeanBias, linetype = Method, shape = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1, colour = "black") +
  geom_point(size = 3, colour = "black") +
  geom_errorbar(aes(ymin = MeanBias - 1.96*SE, ymax = MeanBias + 1.96*SE),
                width = 0.01, colour = "black") +
  scale_x_continuous(labels = percent_format(), breaks = event_rates) +
  scale_linetype_manual(values = c("Standard MLE" = "solid", "Firth" = "dashed")) +
  scale_shape_manual(values = c("Standard MLE" = 16, "Firth" = 17)) +
  labs(title = "Bias in Coefficient Estimates by Event Rate",
       subtitle = paste0("Simulation: n = ", n_obs, ", true \u03B2 = ", true_beta1, ", ", n_sims, " replications per event rate"),
       x = "Event Rate",
       y = expression("Mean Bias (" * hat(beta) * " - " * beta * ")"),
       linetype = "Method", shape = "Method") +
  theme_chapter +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/fig_3_3_bias_by_event_rate.jpeg", fig_3_3,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 3.3 saved: Bias by event rate\n")
# Save simulation summary
sink("output/ch3_bias_simulation.txt")
cat("Bias Simulation Summary\n")
cat("=======================\n\n")
print(bias_summary %>% pivot_wider(names_from = Method, values_from = c(MeanBias, MedianBias, SE)),
      width = 120)
sink()
# =============================================================================
# Case-Control Sampling Implementation
# =============================================================================
# All cases, sample of controls
cases <- which(rare_data$y == 1)
controls <- which(rare_data$y == 0)
# Sample controls at 3:1 ratio
set.seed(123)
n_controls_sample <- min(3 * length(cases), length(controls))
sampled_controls <- sample(controls, n_controls_sample)
cc_data <- rare_data[c(cases, sampled_controls), ]
cat("Case-control sample:\n")
cat("  Cases:", length(cases), "\n")
cat("  Controls:", n_controls_sample, "\n")
cat("  Sample event rate:", round(mean(cc_data$y) * 100, 1), "%\n")
# Fit model on case-control sample
cc_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = cc_data)
# Calculate intercept correction
# Correction = log((p_sample/(1-p_sample)) / (p_pop/(1-p_pop)))
p_sample <- mean(cc_data$y)
p_pop <- mean(rare_data$y)
intercept_correction <- log((p_sample/(1-p_sample)) / (p_pop/(1-p_pop)))
# Corrected coefficients
cc_coef_corrected <- coef(cc_model)
cc_coef_corrected[1] <- cc_coef_corrected[1] - intercept_correction
sink("output/ch3_case_control.txt")
cat("Case-Control Sampling Analysis\n")
cat("==============================\n\n")
cat("Population event rate:", round(p_pop * 100, 2), "%\n")
cat("Sample event rate:", round(p_sample * 100, 2), "%\n")
cat("Intercept correction:", round(intercept_correction, 4), "\n\n")
cat("Uncorrected coefficients:\n")
print(coef(cc_model))
cat("\nCorrected coefficients:\n")
print(cc_coef_corrected)
sink()
# =============================================================================
# Figure 3.4: Case-Control Sampling Illustration
# =============================================================================
# Create visualization data
cc_viz_data <- rare_data %>%
  mutate(
    Sampled = ifelse(row_number() %in% c(cases, sampled_controls), "Sampled", "Not sampled"),
    Outcome = ifelse(y == 1, "Event", "Non-event")
  )
fig_3_4a <- ggplot(cc_viz_data, aes(x = x1, y = x2, shape = Outcome, alpha = Sampled)) +
  geom_point(size = 1.5, colour = "black") +
  scale_shape_manual(values = c("Event" = 17, "Non-event" = 1)) +
  scale_alpha_manual(values = c("Sampled" = 1, "Not sampled" = 0.1)) +
  labs(title = "Case-Control Sampling",
       subtitle = "All events + random sample of non-events",
       x = expression(x[1]), y = expression(x[2])) +
  theme_chapter +
  guides(alpha = guide_legend(override.aes = list(size = 3)))
# Compare distributions
cc_coef_comparison <- data.frame(
  Parameter = c("Intercept", "x1", "x2", "x3"),
  True = true_coefs,
  Population = coef(standard_model),
  `CC Uncorrected` = coef(cc_model),
  `CC Corrected` = cc_coef_corrected,
  check.names = FALSE
)
cc_coef_long <- cc_coef_comparison %>%
  pivot_longer(cols = c(Population, `CC Uncorrected`, `CC Corrected`),
               names_to = "Method", values_to = "Estimate") %>%
  mutate(Method = factor(Method, levels = c("Population", "CC Uncorrected", "CC Corrected")))
fig_3_4b <- ggplot(cc_coef_long, aes(x = Parameter, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_point(data = cc_coef_comparison, aes(x = Parameter, y = True),
             inherit.aes = FALSE, shape = 4, size = 4, stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Population" = "grey75", "CC Uncorrected" = "grey45",
                               "CC Corrected" = "grey15")) +
  labs(title = "Coefficient Estimates: Case-Control",
       subtitle = "\u00D7 = true values; intercept correction restores proper estimates",
       x = "Parameter", y = "Estimate", fill = "Method") +
  theme_chapter
fig_3_4 <- plot_grid(fig_3_4a, fig_3_4b, ncol = 2, labels = c("A", "B"), rel_widths = c(1, 1.2))
ggsave("figures/fig_3_4_case_control.jpeg", fig_3_4,
       width = 11, height = 4.5, dpi = 300, device = "jpeg")
cat("Figure 3.4 saved: Case-control sampling\n")
# =============================================================================
# ROSE Resampling
# =============================================================================
set.seed(123)
rose_result <- ROSE(y ~ x1 + x2 + x3, data = rare_data, seed = 123)
rose_data <- rose_result$data
cat("\nROSE resampling:\n")
cat("  Original event rate:", round(mean(rare_data$y) * 100, 1), "%\n")
cat("  ROSE event rate:", round(mean(rose_data$y) * 100, 1), "%\n")
# Fit model on ROSE data
rose_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = rose_data)
sink("output/ch3_rose_model.txt")
cat("ROSE Resampling Analysis\n")
cat("========================\n\n")
cat("Original distribution:\n")
print(table(rare_data$y))
cat("\nROSE distribution:\n")
print(table(rose_data$y))
cat("\nModel on ROSE data:\n")
print(summary(rose_model))
sink()
# =============================================================================
# Figure 3.5: ROSE Resampling Visualization
# =============================================================================
# Compare original and ROSE distributions
orig_dens <- data.frame(
  x1 = rare_data$x1,
  Dataset = "Original",
  Outcome = factor(rare_data$y, labels = c("Non-event", "Event"))
)
rose_dens <- data.frame(
  x1 = rose_data$x1,
  Dataset = "ROSE",
  Outcome = factor(rose_data$y, labels = c("Non-event", "Event"))
)
combined_dens <- rbind(orig_dens, rose_dens)
fig_3_5a <- ggplot(combined_dens, aes(x = x1, fill = Outcome)) +
  geom_density(alpha = 0.5, colour = "black") +
  facet_wrap(~Dataset, ncol = 1) +
  scale_fill_manual(values = c("Non-event" = "grey75", "Event" = "grey35")) +
  labs(title = "Distribution of x1 by Outcome",
       subtitle = "ROSE creates synthetic balanced samples",
       x = expression(x[1]), y = "Density") +
  theme_chapter
# Class balance comparison
balance_data <- data.frame(
  Dataset = rep(c("Original", "ROSE"), each = 2),
  Outcome = rep(c("Non-event", "Event"), 2),
  Count = c(sum(rare_data$y == 0), sum(rare_data$y == 1),
            sum(rose_data$y == 0), sum(rose_data$y == 1))
)
fig_3_5b <- ggplot(balance_data, aes(x = Dataset, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Non-event" = "grey75", "Event" = "grey35")) +
  labs(title = "Class Balance: Original vs. ROSE",
       x = "", y = "Count") +
  theme_chapter +
  coord_cartesian(ylim = c(0, max(balance_data$Count) * 1.1))
fig_3_5 <- plot_grid(fig_3_5a, fig_3_5b, ncol = 2, labels = c("A", "B"), rel_widths = c(1.2, 1))
ggsave("figures/fig_3_5_rose_resampling.jpeg", fig_3_5,
       width = 11, height = 5, dpi = 300, device = "jpeg")
cat("Figure 3.5 saved: ROSE resampling\n")
# =============================================================================
# Figure 3.6: All Methods Coefficient Comparison
# =============================================================================
all_coef <- data.frame(
  Parameter = c("Intercept", "x1", "x2", "x3"),
  True = true_coefs,
  Standard = coef(standard_model),
  Firth = coef(firth_model),
  `BR-GLM` = coef(br_model),
  `Case-Control` = cc_coef_corrected,
  ROSE = coef(rose_model),
  check.names = FALSE
)
all_coef_long <- all_coef %>%
  pivot_longer(cols = -c(Parameter, True), names_to = "Method", values_to = "Estimate") %>%
  mutate(
    Method = factor(Method, levels = c("Standard", "Firth", "BR-GLM", "Case-Control", "ROSE")),
    Bias = Estimate - rep(true_coefs, 5)
  )
fig_3_6 <- ggplot(all_coef_long, aes(x = Parameter, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), width = 0.8,
           colour = "black", linewidth = 0.3) +
  geom_point(data = all_coef, aes(x = Parameter, y = True),
             inherit.aes = FALSE, shape = 4, size = 4, stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Standard" = "grey90", "Firth" = "grey70",
                               "BR-GLM" = "grey50", "Case-Control" = "grey30",
                               "ROSE" = "grey10")) +
  labs(title = "Coefficient Estimates: All Methods",
       subtitle = "\u00D7 marks true parameter values",
       x = "Parameter", y = "Estimate", fill = "Method") +
  theme_chapter +
  theme(legend.position = "bottom")
ggsave("figures/fig_3_6_all_methods_comparison.jpeg", fig_3_6,
       width = 9, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 3.6 saved: All methods comparison\n")
# Save comprehensive comparison
sink("output/ch3_all_methods_comparison.txt")
cat("All Methods Coefficient Comparison\n")
cat("===================================\n\n")
print(all_coef, digits = 4)
cat("\nBias (Estimate - True):\n")
bias_all <- all_coef
bias_all[, 3:7] <- all_coef[, 3:7] - all_coef$True
print(bias_all[, -2], digits = 4)
sink()
# =============================================================================
# Model Performance Evaluation
# =============================================================================
# Helper function for predictions from logistf
predict_logistf <- function(model, newdata) {
  beta <- coef(model)
  formula_terms <- attr(terms(model$formula), "term.labels")
  X <- model.matrix(as.formula(paste("~", paste(formula_terms, collapse = "+"))), data = newdata)
  plogis(as.vector(X %*% beta))
}
# Get predictions on original data
pred_standard <- predict(standard_model, type = "response")
pred_firth <- predict_logistf(firth_model, rare_data)
pred_brglm <- predict(br_model, type = "response")
pred_rose <- predict(rose_model, newdata = rare_data, type = "response")
# Case-control with corrected intercept
cc_model_corrected <- cc_model
cc_model_corrected$coefficients[1] <- cc_coef_corrected[1]
pred_cc <- predict(cc_model_corrected, newdata = rare_data, type = "response")
# Calculate ROC curves
roc_standard <- roc(rare_data$y, pred_standard, quiet = TRUE)
roc_firth <- roc(rare_data$y, pred_firth, quiet = TRUE)
roc_brglm <- roc(rare_data$y, pred_brglm, quiet = TRUE)
roc_cc <- roc(rare_data$y, pred_cc, quiet = TRUE)
roc_rose <- roc(rare_data$y, pred_rose, quiet = TRUE)
# =============================================================================
# Figure 3.7: ROC Curves Comparison
# =============================================================================
# Create ROC data for ggplot
create_roc_data <- function(roc_obj, method_name) {
  data.frame(
    Specificity = roc_obj$specificities,
    Sensitivity = roc_obj$sensitivities,
    Method = method_name
  )
}
roc_plot_data <- rbind(
  create_roc_data(roc_standard, paste0("Standard (AUC=", round(auc(roc_standard), 3), ")")),
  create_roc_data(roc_firth, paste0("Firth (AUC=", round(auc(roc_firth), 3), ")")),
  create_roc_data(roc_brglm, paste0("BR-GLM (AUC=", round(auc(roc_brglm), 3), ")")),
  create_roc_data(roc_cc, paste0("Case-Control (AUC=", round(auc(roc_cc), 3), ")")),
  create_roc_data(roc_rose, paste0("ROSE (AUC=", round(auc(roc_rose), 3), ")"))
)
# Assign distinct linetypes
roc_methods <- unique(roc_plot_data$Method)
roc_linetypes <- setNames(c("solid", "dashed", "dotted", "dotdash", "longdash"), roc_methods)

fig_3_7 <- ggplot(roc_plot_data, aes(x = 1 - Specificity, y = Sensitivity, linetype = Method)) +
  geom_line(linewidth = 0.9, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  scale_linetype_manual(values = roc_linetypes) +
  labs(title = "ROC Curves: Comparison of Methods",
       subtitle = "Evaluated on original (imbalanced) test data",
       x = "False Positive Rate (1 \u2212 Specificity)",
       y = "True Positive Rate (Sensitivity)",
       linetype = "Method") +
  theme_chapter +
  theme(legend.position = c(0.7, 0.25),
        legend.background = element_rect(fill = "white", colour = "grey80"),
        legend.key.width = unit(1.5, "cm")) +
  coord_equal()
ggsave("figures/fig_3_7_roc_comparison.jpeg", fig_3_7,
       width = 7, height = 6, dpi = 300, device = "jpeg")
cat("Figure 3.7 saved: ROC curves comparison\n")
# =============================================================================
# Figure 3.8: Precision-Recall Curves
# =============================================================================
# Calculate precision-recall for each method
calc_pr <- function(y_true, y_pred) {
  thresholds <- sort(unique(y_pred), decreasing = TRUE)
  pr_data <- data.frame(threshold = numeric(), precision = numeric(), recall = numeric())
  for (thresh in thresholds) {
    pred_pos <- y_pred >= thresh
    tp <- sum(pred_pos & y_true == 1)
    fp <- sum(pred_pos & y_true == 0)
    fn <- sum(!pred_pos & y_true == 1)
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    pr_data <- rbind(pr_data, data.frame(threshold = thresh, precision = precision, recall = recall))
  }
  return(pr_data)
}
pr_standard <- calc_pr(rare_data$y, pred_standard) %>% mutate(Method = "Standard")
pr_firth <- calc_pr(rare_data$y, pred_firth) %>% mutate(Method = "Firth")
pr_rose <- calc_pr(rare_data$y, pred_rose) %>% mutate(Method = "ROSE")
pr_data <- rbind(pr_standard, pr_firth, pr_rose)
fig_3_8 <- ggplot(pr_data, aes(x = recall, y = precision, linetype = Method)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_hline(yintercept = event_rate, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 0.8, y = event_rate + 0.02,
           label = paste0("Baseline (", round(event_rate*100, 1), "%)"),
           colour = "grey50", size = 3) +
  scale_linetype_manual(values = c("Standard" = "solid", "Firth" = "dashed", "ROSE" = "dotdash")) +
  labs(title = "Precision-Recall Curves",
       subtitle = "More informative than ROC for rare events",
       x = "Recall (Sensitivity)",
       y = "Precision (PPV)",
       linetype = "Method") +
  theme_chapter +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
ggsave("figures/fig_3_8_precision_recall.jpeg", fig_3_8,
       width = 7, height = 5, dpi = 300, device = "jpeg")
cat("Figure 3.8 saved: Precision-recall curves\n")
# =============================================================================
# Figure 3.9: Calibration Comparison
# =============================================================================
# Create calibration data
create_calibration <- function(y_true, y_pred, method_name, n_bins = 10) {
  bins <- cut(y_pred, breaks = seq(0, max(y_pred) * 1.01, length.out = n_bins + 1),
              include.lowest = TRUE)
  cal_data <- data.frame(y_true, y_pred, bins) %>%
    group_by(bins) %>%
    summarise(
      mean_pred = mean(y_pred),
      mean_obs = mean(y_true),
      n = n(),
      se = sqrt(mean_obs * (1 - mean_obs) / n),
      .groups = "drop"
    ) %>%
    mutate(Method = method_name)
  return(cal_data)
}
cal_standard <- create_calibration(rare_data$y, pred_standard, "Standard")
cal_firth <- create_calibration(rare_data$y, pred_firth, "Firth")
cal_rose <- create_calibration(rare_data$y, pred_rose, "ROSE")
cal_data <- rbind(cal_standard, cal_firth, cal_rose)
fig_3_9 <- ggplot(cal_data, aes(x = mean_pred, y = mean_obs, shape = Method, linetype = Method)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), alpha = 0.7, colour = "black") +
  geom_line(linewidth = 0.8, colour = "black") +
  geom_errorbar(aes(ymin = pmax(0, mean_obs - 1.96*se), ymax = pmin(1, mean_obs + 1.96*se)),
                width = 0.005, alpha = 0.5, colour = "black") +
  scale_shape_manual(values = c("Standard" = 16, "Firth" = 17, "ROSE" = 15)) +
  scale_linetype_manual(values = c("Standard" = "solid", "Firth" = "dashed", "ROSE" = "dotdash")) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(title = "Calibration Plot",
       subtitle = "Predicted vs. observed probabilities by decile",
       x = "Mean Predicted Probability",
       y = "Observed Proportion",
       shape = "Method", linetype = "Method") +
  theme_chapter +
  coord_cartesian(xlim = c(0, 0.25), ylim = c(0, 0.25))
ggsave("figures/fig_3_9_calibration.jpeg", fig_3_9,
       width = 7, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 3.9 saved: Calibration comparison\n")
# =============================================================================
# Real Data Example: Simulated Fraud Detection
# =============================================================================
set.seed(42)
n_transactions <- 10000
fraud_rate <- 0.005  # 0.5% fraud rate
# Generate features
amount <- exp(rnorm(n_transactions, 4, 1))
hour <- runif(n_transactions, 0, 24)
weekend <- rbinom(n_transactions, 1, 0.3)
foreign <- rbinom(n_transactions, 1, 0.1)
online <- rbinom(n_transactions, 1, 0.4)
# Generate fraud with higher probability for certain patterns
fraud_logit <- -6 + 0.8*foreign + 0.5*online + 0.003*amount +
  0.1*(hour > 22 | hour < 6) - 0.3*weekend
fraud_prob <- plogis(fraud_logit)
fraud <- rbinom(n_transactions, 1, fraud_prob)
fraud_data <- data.frame(
  fraud = fraud,
  amount = amount,
  hour = hour,
  weekend = weekend,
  foreign = foreign,
  online = online
)
actual_fraud_rate <- mean(fraud)
cat("\nFraud detection example:\n")
cat("  Transactions:", n_transactions, "\n")
cat("  Fraud cases:", sum(fraud), "\n")
cat("  Fraud rate:", round(actual_fraud_rate * 100, 2), "%\n")
# Split data
set.seed(123)
train_idx <- sample(1:nrow(fraud_data), 0.7 * nrow(fraud_data))
train_data <- fraud_data[train_idx, ]
test_data <- fraud_data[-train_idx, ]
# Fit models
fraud_standard <- glm(fraud ~ amount + hour + weekend + foreign + online,
                      family = binomial, data = train_data)
fraud_firth <- logistf(fraud ~ amount + hour + weekend + foreign + online,
                       data = train_data)
fraud_rose <- ROSE(fraud ~ amount + hour + weekend + foreign + online,
                   data = train_data, seed = 123)$data
fraud_rose_model <- glm(fraud ~ amount + hour + weekend + foreign + online,
                        family = binomial, data = fraud_rose)
# Evaluate on test set
pred_fraud_std <- predict(fraud_standard, newdata = test_data, type = "response")
pred_fraud_firth <- predict_logistf(fraud_firth, test_data)
pred_fraud_rose <- predict(fraud_rose_model, newdata = test_data, type = "response")
roc_fraud_std <- roc(test_data$fraud, pred_fraud_std, quiet = TRUE)
roc_fraud_firth <- roc(test_data$fraud, pred_fraud_firth, quiet = TRUE)
roc_fraud_rose <- roc(test_data$fraud, pred_fraud_rose, quiet = TRUE)
# =============================================================================
# Figure 3.10: Fraud Detection Results
# =============================================================================
# Create method labels with AUC values
fraud_method_labels <- c(
  paste0("Standard (AUC=", round(auc(roc_fraud_std), 3), ")"),
  paste0("Firth (AUC=", round(auc(roc_fraud_firth), 3), ")"),
  paste0("ROSE (AUC=", round(auc(roc_fraud_rose), 3), ")")
)
fraud_roc_data <- rbind(
  create_roc_data(roc_fraud_std, fraud_method_labels[1]),
  create_roc_data(roc_fraud_firth, fraud_method_labels[2]),
  create_roc_data(roc_fraud_rose, fraud_method_labels[3])
)
# Create named linetype vector
fraud_linetypes <- setNames(c("solid", "dashed", "dotdash"), fraud_method_labels)

fig_3_10 <- ggplot(fraud_roc_data, aes(x = 1 - Specificity, y = Sensitivity, linetype = Method)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  scale_linetype_manual(values = fraud_linetypes) +
  labs(title = "Fraud Detection: ROC Curves",
       subtitle = paste0("Test set evaluation (fraud rate: ", round(mean(test_data$fraud)*100, 2), "%)"),
       x = "False Positive Rate",
       y = "True Positive Rate",
       linetype = "Method") +
  theme_chapter +
  theme(legend.position = c(0.7, 0.25),
        legend.background = element_rect(fill = "white", colour = "grey80"),
        legend.key.width = unit(1.5, "cm")) +
  coord_equal()
ggsave("figures/fig_3_10_fraud_detection.jpeg", fig_3_10,
       width = 7, height = 6, dpi = 300, device = "jpeg")
cat("Figure 3.10 saved: Fraud detection results\n")
# Save fraud detection results
sink("output/ch3_fraud_detection.txt")
cat("Fraud Detection Example\n")
cat("=======================\n\n")
cat("Data summary:\n")
cat("  Training set:", nrow(train_data), "transactions,", sum(train_data$fraud), "frauds\n")
cat("  Test set:", nrow(test_data), "transactions,", sum(test_data$fraud), "frauds\n\n")
cat("Model performance (AUC):\n")
cat("  Standard:", round(auc(roc_fraud_std), 4), "\n")
cat("  Firth:", round(auc(roc_fraud_firth), 4), "\n")
cat("  ROSE:", round(auc(roc_fraud_rose), 4), "\n\n")
cat("Standard model coefficients:\n")
print(summary(fraud_standard)$coefficients)
sink()
# =============================================================================
# Summary Statistics
# =============================================================================
sink("output/ch3_chapter_summary.txt")
cat("Chapter 3 Summary Statistics\n")
cat("============================\n\n")
cat("Simulated Rare Events Data:\n")
cat("- Sample size:", n, "\n")
cat("- Event rate:", round(event_rate * 100, 2), "%\n")
cat("- Number of events:", n_events, "\n\n")
cat("AUC Comparison:\n")
cat("- Standard:", round(auc(roc_standard), 4), "\n")
cat("- Firth:", round(auc(roc_firth), 4), "\n")
cat("- BR-GLM:", round(auc(roc_brglm), 4), "\n")
cat("- Case-Control:", round(auc(roc_cc), 4), "\n")
cat("- ROSE:", round(auc(roc_rose), 4), "\n\n")
cat("Key Findings:\n")
cat("1. Standard MLE shows positive bias in coefficient estimates for rare events\n")
cat("2. Firth's method and BR-GLM provide less biased estimates\n")
cat("3. Case-control sampling requires intercept correction\n")
cat("4. ROSE creates balanced samples but may alter coefficient estimates\n")
cat("5. All methods perform similarly in terms of discrimination (AUC)\n")
sink()
cat("\n=== All Chapter 3 figures and outputs generated successfully ===\n")
cat("Figures saved to: figures/\n")
cat("  fig_3_1_rare_events_distribution.jpeg\n")
cat("  fig_3_2_coefficient_bias.jpeg\n")
cat("  fig_3_3_bias_by_event_rate.jpeg\n")
cat("  fig_3_4_case_control.jpeg\n")
cat("  fig_3_5_rose_resampling.jpeg\n")
cat("  fig_3_6_all_methods_comparison.jpeg\n")
cat("  fig_3_7_roc_comparison.jpeg\n")
cat("  fig_3_8_precision_recall.jpeg\n")
cat("  fig_3_9_calibration.jpeg\n")
cat("  fig_3_10_fraud_detection.jpeg\n")
cat("Outputs saved to: output/\n")
