# ============================================================================
# Chapter 12: Multinomial and Ordinal Logistic Regression
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 12
# Required packages: nnet, MASS, VGAM, ordinal, tidyverse, ggplot2
# ============================================================================
# Load required packages
library(nnet)
library(MASS)
library(VGAM)
library(ordinal)
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
cat("CHAPTER 12: MULTINOMIAL AND ORDINAL LOGISTIC REGRESSION\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 12.2: Simulate Multinomial Data
# ============================================================================
cat("Simulating multinomial data...\n")
simulate_multinomial_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  # Predictors
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rbinom(n, 1, 0.5)
  # Linear predictors for each category (relative to reference Category 1)
  lp2 <- 0.5 + 1.2 * x1 - 0.8 * x2 + 0.5 * x3
  lp3 <- -0.8 + 0.3 * x1 + 1.1 * x2 - 0.7 * x3
  # Calculate multinomial probabilities
  denom <- 1 + exp(lp2) + exp(lp3)
  p1 <- 1 / denom
  p2 <- exp(lp2) / denom
  p3 <- exp(lp3) / denom
  # Generate category based on probabilities
  probs <- cbind(p1, p2, p3)
  y <- apply(probs, 1, function(p) sample(1:3, 1, prob = p))
  data <- data.frame(
    y = factor(y, levels = 1:3, labels = c("Category1", "Category2", "Category3")),
    x1 = x1,
    x2 = x2,
    x3 = factor(x3, labels = c("No", "Yes"))
  )
  return(data)
}
multi_data <- simulate_multinomial_data(n = 1000)
# Data summary
sink("output/multinomial_data_summary.txt")
cat("=== Multinomial Data Summary ===\n\n")
cat("Sample size:", nrow(multi_data), "\n\n")
cat("Outcome distribution:\n")
print(table(multi_data$y))
cat("\nProportions:\n")
print(round(prop.table(table(multi_data$y)), 3))
cat("\nTrue model:\n")
cat("Category 2 vs 1: logit = 0.5 + 1.2*x1 - 0.8*x2 + 0.5*x3\n")
cat("Category 3 vs 1: logit = -0.8 + 0.3*x1 + 1.1*x2 - 0.7*x3\n")
sink()
# Figure 12.1: Predictors by category
fig12_1a <- ggplot(multi_data, aes(x = y, y = x1, fill = y)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Category1" = "grey80", "Category2" = "grey55",
                               "Category3" = "grey30")) +
  labs(title = "x1 by Category", x = "Category", y = "x1") +
  theme(legend.position = "none")
fig12_1b <- ggplot(multi_data, aes(x = y, y = x2, fill = y)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Category1" = "grey80", "Category2" = "grey55",
                               "Category3" = "grey30")) +
  labs(title = "x2 by Category", x = "Category", y = "x2") +
  theme(legend.position = "none")
fig12_1 <- grid.arrange(fig12_1a, fig12_1b, ncol = 2,
                        top = "Distribution of Continuous Predictors by Outcome Category")
ggsave("figures/fig12_1_predictors_by_category.jpeg", fig12_1, width = 10, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_1_predictors_by_category.jpeg\n")
# ============================================================================
# Section 12.3: Multinomial Logistic Regression
# ============================================================================
cat("Fitting multinomial logistic regression...\n")
# Fit multinomial model (Category1 is reference)
multi_model <- multinom(y ~ x1 + x2 + x3, data = multi_data, trace = FALSE)
# Extract coefficients and standard errors
multi_coef <- coef(multi_model)
multi_se <- summary(multi_model)$standard.errors
# Calculate z-values and p-values
z_values <- multi_coef / multi_se
p_values <- 2 * (1 - pnorm(abs(z_values)))
# Calculate relative risk ratios (RRR) and confidence intervals
rrr <- exp(multi_coef)
rrr_lower <- exp(multi_coef - 1.96 * multi_se)
rrr_upper <- exp(multi_coef + 1.96 * multi_se)
sink("output/multinomial_results.txt")
cat("=== Multinomial Logistic Regression Results ===\n\n")
cat("Reference category: Category1\n\n")
cat("--- Coefficients ---\n")
cat("\nCategory2 vs Category1:\n")
cat("  (Intercept):", round(multi_coef[1, 1], 4), "(SE:", round(multi_se[1, 1], 4),
    ", p =", round(p_values[1, 1], 4), ")\n")
cat("  x1:", round(multi_coef[1, 2], 4), "(SE:", round(multi_se[1, 2], 4),
    ", p =", round(p_values[1, 2], 4), ")\n")
cat("  x2:", round(multi_coef[1, 3], 4), "(SE:", round(multi_se[1, 3], 4),
    ", p =", round(p_values[1, 3], 4), ")\n")
cat("  x3Yes:", round(multi_coef[1, 4], 4), "(SE:", round(multi_se[1, 4], 4),
    ", p =", round(p_values[1, 4], 4), ")\n")
cat("\nCategory3 vs Category1:\n")
cat("  (Intercept):", round(multi_coef[2, 1], 4), "(SE:", round(multi_se[2, 1], 4),
    ", p =", round(p_values[2, 1], 4), ")\n")
cat("  x1:", round(multi_coef[2, 2], 4), "(SE:", round(multi_se[2, 2], 4),
    ", p =", round(p_values[2, 2], 4), ")\n")
cat("  x2:", round(multi_coef[2, 3], 4), "(SE:", round(multi_se[2, 3], 4),
    ", p =", round(p_values[2, 3], 4), ")\n")
cat("  x3Yes:", round(multi_coef[2, 4], 4), "(SE:", round(multi_se[2, 4], 4),
    ", p =", round(p_values[2, 4], 4), ")\n")
cat("\n--- Relative Risk Ratios (RRR) ---\n")
cat("\nCategory2 vs Category1:\n")
cat("  x1: RRR =", round(rrr[1, 2], 3), "(95% CI:", round(rrr_lower[1, 2], 3), "-",
    round(rrr_upper[1, 2], 3), ")\n")
cat("  x2: RRR =", round(rrr[1, 3], 3), "(95% CI:", round(rrr_lower[1, 3], 3), "-",
    round(rrr_upper[1, 3], 3), ")\n")
cat("  x3Yes: RRR =", round(rrr[1, 4], 3), "(95% CI:", round(rrr_lower[1, 4], 3), "-",
    round(rrr_upper[1, 4], 3), ")\n")
cat("\nCategory3 vs Category1:\n")
cat("  x1: RRR =", round(rrr[2, 2], 3), "(95% CI:", round(rrr_lower[2, 2], 3), "-",
    round(rrr_upper[2, 2], 3), ")\n")
cat("  x2: RRR =", round(rrr[2, 3], 3), "(95% CI:", round(rrr_lower[2, 3], 3), "-",
    round(rrr_upper[2, 3], 3), ")\n")
cat("  x3Yes: RRR =", round(rrr[2, 4], 3), "(95% CI:", round(rrr_lower[2, 4], 3), "-",
    round(rrr_upper[2, 4], 3), ")\n")
cat("\n--- Model Fit ---\n")
cat("Log-likelihood:", round(as.numeric(logLik(multi_model)), 2), "\n")
cat("AIC:", round(AIC(multi_model), 2), "\n")
cat("BIC:", round(BIC(multi_model), 2), "\n")
sink()
# Figure 12.2: RRR forest plot
rrr_plot_data <- data.frame(
  Comparison = rep(c("Category2 vs 1", "Category3 vs 1"), each = 3),
  Variable = rep(c("x1", "x2", "x3 (Yes)"), 2),
  RRR = c(rrr[1, 2:4], rrr[2, 2:4]),
  Lower = c(rrr_lower[1, 2:4], rrr_lower[2, 2:4]),
  Upper = c(rrr_upper[1, 2:4], rrr_upper[2, 2:4])
)
fig12_2 <- ggplot(rrr_plot_data, aes(x = Variable, y = RRR, shape = Comparison)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, colour = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(width = 0.5), width = 0.2, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("Category2 vs 1" = 16, "Category3 vs 1" = 17)) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Multinomial Logistic Regression: Relative Risk Ratios",
       subtitle = "Reference category: Category1",
       x = "", y = "Relative Risk Ratio (log scale)") +
  theme(legend.position = "bottom")
ggsave("figures/fig12_2_multinomial_rrr.jpeg", fig12_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_2_multinomial_rrr.jpeg\n")
# ============================================================================
# Section 12.4: Multinomial Predicted Probabilities
# ============================================================================
cat("Computing multinomial predicted probabilities...\n")
# Create prediction grid
x1_range <- seq(-3, 3, length.out = 100)
pred_grid <- expand.grid(
  x1 = x1_range,
  x2 = 0,
  x3 = "No"
)
# Get predicted probabilities
multi_probs <- predict(multi_model, newdata = pred_grid, type = "probs")
pred_grid <- cbind(pred_grid, multi_probs)
# Convert to long format
pred_long <- pred_grid %>%
  pivot_longer(cols = c(Category1, Category2, Category3),
               names_to = "Category", values_to = "Probability")
# Figure 12.3: Predicted probabilities
fig12_3 <- ggplot(pred_long, aes(x = x1, y = Probability, linetype = Category)) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(values = c("Category1" = "solid", "Category2" = "dashed",
                                   "Category3" = "dotted")) +
  labs(title = "Multinomial Model: Predicted Probabilities by x1",
       subtitle = "At x2 = 0, x3 = No",
       x = "x1", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig12_3_multinomial_predicted_probs.jpeg", fig12_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_3_multinomial_predicted_probs.jpeg\n")
# ============================================================================
# Section 12.5: Multinomial Model Evaluation
# ============================================================================
cat("Evaluating multinomial model...\n")
# Confusion matrix
multi_pred_class <- predict(multi_model, type = "class")
multi_conf <- table(Actual = multi_data$y, Predicted = multi_pred_class)
multi_accuracy <- sum(diag(multi_conf)) / sum(multi_conf)
# Per-class metrics
multi_metrics <- data.frame(
  Category = levels(multi_data$y),
  Precision = NA,
  Recall = NA,
  F1 = NA
)
for (i in 1:3) {
  tp <- multi_conf[i, i]
  fp <- sum(multi_conf[, i]) - tp
  fn <- sum(multi_conf[i, ]) - tp
  multi_metrics$Precision[i] <- tp / (tp + fp)
  multi_metrics$Recall[i] <- tp / (tp + fn)
  multi_metrics$F1[i] <- 2 * multi_metrics$Precision[i] * multi_metrics$Recall[i] /
    (multi_metrics$Precision[i] + multi_metrics$Recall[i])
}
# Cross-validation
set.seed(456)
cv_folds <- 5
fold_idx <- sample(rep(1:cv_folds, length.out = nrow(multi_data)))
cv_accuracy <- numeric(cv_folds)
for (fold in 1:cv_folds) {
  train <- multi_data[fold_idx != fold, ]
  test <- multi_data[fold_idx == fold, ]
  cv_model <- multinom(y ~ x1 + x2 + x3, data = train, trace = FALSE)
  cv_pred <- predict(cv_model, newdata = test, type = "class")
  cv_accuracy[fold] <- mean(cv_pred == test$y)
}
sink("output/multinomial_evaluation.txt")
cat("=== Multinomial Model Evaluation ===\n\n")
cat("--- Confusion Matrix ---\n")
print(multi_conf)
cat("\nOverall Accuracy:", round(multi_accuracy, 4), "\n")
cat("\n--- Per-Class Metrics ---\n")
print(multi_metrics, row.names = FALSE)
cat("\n--- 5-Fold Cross-Validation ---\n")
cat("Fold accuracies:", round(cv_accuracy, 4), "\n")
cat("Mean CV Accuracy:", round(mean(cv_accuracy), 4), "\n")
cat("SD:", round(sd(cv_accuracy), 4), "\n")
sink()
# ============================================================================
# Section 12.6: Simulate Ordinal Data
# ============================================================================
cat("Simulating ordinal data...\n")
simulate_ordinal_data <- function(n = 1000, seed = 456) {
  set.seed(seed)
  # Predictors
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rbinom(n, 1, 0.5)
  # Latent continuous variable
  latent <- 1.2 * x1 - 0.8 * x2 + 0.5 * x3 + rlogis(n)
  # Cutpoints for 3 categories
  cutpoints <- c(-1, 1)
  # Create ordered outcome
  y <- cut(latent, breaks = c(-Inf, cutpoints, Inf), labels = FALSE)
  data <- data.frame(
    y = ordered(y, levels = 1:3, labels = c("Low", "Medium", "High")),
    x1 = x1,
    x2 = x2,
    x3 = factor(x3, labels = c("No", "Yes"))
  )
  return(data)
}
ord_data <- simulate_ordinal_data(n = 1000)
sink("output/ordinal_data_summary.txt")
cat("=== Ordinal Data Summary ===\n\n")
cat("Sample size:", nrow(ord_data), "\n\n")
cat("Outcome distribution:\n")
print(table(ord_data$y))
cat("\nProportions:\n")
print(round(prop.table(table(ord_data$y)), 3))
cat("\nTrue model (latent variable):\n")
cat("y* = 1.2*x1 - 0.8*x2 + 0.5*x3 + logistic_error\n")
cat("Cutpoints: -1, 1\n")
sink()
# Figure 12.4: Ordinal predictors
fig12_4a <- ggplot(ord_data, aes(x = y, y = x1, fill = y)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Low" = "grey80", "Medium" = "grey55",
                               "High" = "grey30")) +
  labs(title = "x1 by Ordinal Category", x = "Category", y = "x1") +
  theme(legend.position = "none")
fig12_4b <- ggplot(ord_data, aes(x = y, y = x2, fill = y)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Low" = "grey80", "Medium" = "grey55",
                               "High" = "grey30")) +
  labs(title = "x2 by Ordinal Category", x = "Category", y = "x2") +
  theme(legend.position = "none")
fig12_4 <- grid.arrange(fig12_4a, fig12_4b, ncol = 2,
                        top = "Distribution of Predictors by Ordinal Category")
ggsave("figures/fig12_4_ordinal_predictors.jpeg", fig12_4, width = 10, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_4_ordinal_predictors.jpeg\n")
# ============================================================================
# Section 12.7: Ordinal Logistic Regression (Proportional Odds)
# ============================================================================
cat("Fitting ordinal logistic regression...\n")
# Fit proportional odds model
ord_model <- polr(y ~ x1 + x2 + x3, data = ord_data, Hess = TRUE)
# Extract coefficients
ord_summary <- coef(summary(ord_model))
ord_coef <- coef(ord_model)
ord_se <- ord_summary[1:length(ord_coef), "Std. Error"]
ord_tval <- ord_summary[1:length(ord_coef), "t value"]
ord_pval <- 2 * (1 - pnorm(abs(ord_tval)))
# Thresholds
ord_thresholds <- ord_model$zeta
# Odds ratios (note: polr uses opposite sign convention)
ord_or <- exp(ord_coef)  # For increase in Y
ord_ci <- confint(ord_model)
ord_or_lower <- exp(ord_ci[, 1])
ord_or_upper <- exp(ord_ci[, 2])
sink("output/ordinal_results.txt")
cat("=== Ordinal Logistic Regression Results ===\n\n")
cat("Proportional Odds Model (cumulative logit)\n\n")
cat("--- Coefficients ---\n")
for (i in 1:length(ord_coef)) {
  cat(names(ord_coef)[i], ":", round(ord_coef[i], 4),
      "(SE:", round(ord_se[i], 4), ", p =", round(ord_pval[i], 4), ")\n")
}
cat("\n--- Thresholds (Intercepts) ---\n")
cat("Low|Medium:", round(ord_thresholds[1], 4), "\n")
cat("Medium|High:", round(ord_thresholds[2], 4), "\n")
cat("\n--- Odds Ratios (for higher category) ---\n")
cat("Note: OR > 1 means predictor increases probability of higher category\n\n")
for (i in 1:length(ord_coef)) {
  cat(names(ord_coef)[i], ": OR =", round(ord_or[i], 3),
      "(95% CI:", round(ord_or_lower[i], 3), "-", round(ord_or_upper[i], 3), ")\n")
}
cat("\n--- Model Fit ---\n")
cat("Log-likelihood:", round(as.numeric(logLik(ord_model)), 2), "\n")
cat("AIC:", round(AIC(ord_model), 2), "\n")
cat("BIC:", round(BIC(ord_model), 2), "\n")
sink()
# Figure 12.5: Ordinal OR forest plot
ord_or_plot <- data.frame(
  Variable = names(ord_coef),
  OR = ord_or,
  Lower = ord_or_lower,
  Upper = ord_or_upper
)
fig12_5 <- ggplot(ord_or_plot, aes(x = Variable, y = OR)) +
  geom_point(size = 3, colour = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Ordinal Logistic Regression: Odds Ratios",
       subtitle = "Proportional odds model \u2014 OR for being in higher category",
       x = "", y = "Odds Ratio (log scale)")
ggsave("figures/fig12_5_ordinal_or.jpeg", fig12_5, width = 8, height = 4,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_5_ordinal_or.jpeg\n")
# ============================================================================
# Section 12.8: Ordinal Predicted Probabilities
# ============================================================================
cat("Computing ordinal predicted probabilities...\n")
# Prediction grid
ord_pred_grid <- expand.grid(
  x1 = x1_range,
  x2 = 0,
  x3 = "No"
)
# Get predicted probabilities
ord_probs <- predict(ord_model, newdata = ord_pred_grid, type = "probs")
ord_pred_grid <- cbind(ord_pred_grid, ord_probs)
# Convert to long format
ord_pred_long <- ord_pred_grid %>%
  pivot_longer(cols = c(Low, Medium, High),
               names_to = "Category", values_to = "Probability") %>%
  mutate(Category = factor(Category, levels = c("Low", "Medium", "High")))
# Figure 12.6: Ordinal predicted probabilities
fig12_6 <- ggplot(ord_pred_long, aes(x = x1, y = Probability, linetype = Category)) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(values = c("Low" = "solid", "Medium" = "dashed",
                                   "High" = "dotted")) +
  labs(title = "Ordinal Model: Predicted Probabilities by x1",
       subtitle = "At x2 = 0, x3 = No",
       x = "x1", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig12_6_ordinal_predicted_probs.jpeg", fig12_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_6_ordinal_predicted_probs.jpeg\n")
# ============================================================================
# Section 12.9: Test Proportional Odds Assumption
# ============================================================================
cat("Testing proportional odds assumption...\n")
# Using ordinal package
ord_clm <- clm(y ~ x1 + x2 + x3, data = ord_data)
# Nominal test (tests if coefficients differ across thresholds)
nom_test <- nominal_test(ord_clm)
# Also fit non-proportional odds model with VGAM
nonprop_model <- vglm(y ~ x1 + x2 + x3,
                      family = cumulative(parallel = FALSE),
                      data = ord_data)
# Likelihood ratio test
ll_prop <- logLik(ord_model)
ll_nonprop <- logLik(nonprop_model)
lrt_stat <- -2 * (as.numeric(ll_prop) - as.numeric(ll_nonprop))
df_diff <- attr(ll_nonprop, "df") - attr(ll_prop, "df")
lrt_pval <- pchisq(lrt_stat, df = df_diff, lower.tail = FALSE)
sink("output/proportional_odds_test.txt")
cat("=== Test of Proportional Odds Assumption ===\n\n")
cat("--- Likelihood Ratio Test ---\n")
cat("Comparing: Proportional odds vs Non-proportional odds model\n")
cat("LRT statistic:", round(lrt_stat, 4), "\n")
cat("Degrees of freedom:", df_diff, "\n")
cat("P-value:", round(lrt_pval, 4), "\n\n")
if (length(lrt_pval) > 0 && !is.na(lrt_pval)) {
  if (lrt_pval > 0.05) {
    cat("Conclusion: Proportional odds assumption is NOT rejected (p > 0.05)\n")
    cat("The proportional odds model is appropriate.\n")
  } else {
    cat("Conclusion: Proportional odds assumption IS rejected (p < 0.05)\n")
    cat("Consider partial proportional odds or multinomial model.\n")
  }
} else {
  cat("Warning: Could not calculate p-value for likelihood ratio test.\n")
}
cat("\n--- Nominal Test (per variable) ---\n")
print(nom_test)
sink()
# ============================================================================
# Section 12.10: Ordinal Model Evaluation
# ============================================================================
cat("Evaluating ordinal model...\n")
# Confusion matrix
ord_pred_class <- predict(ord_model, type = "class")
ord_conf <- table(Actual = ord_data$y, Predicted = ord_pred_class)
ord_accuracy <- sum(diag(ord_conf)) / sum(ord_conf)
# Per-class metrics
ord_metrics <- data.frame(
  Category = levels(ord_data$y),
  Precision = NA,
  Recall = NA,
  F1 = NA
)
for (i in 1:3) {
  tp <- ord_conf[i, i]
  fp <- sum(ord_conf[, i]) - tp
  fn <- sum(ord_conf[i, ]) - tp
  ord_metrics$Precision[i] <- tp / (tp + fp)
  ord_metrics$Recall[i] <- tp / (tp + fn)
  ord_metrics$F1[i] <- 2 * ord_metrics$Precision[i] * ord_metrics$Recall[i] /
    (ord_metrics$Precision[i] + ord_metrics$Recall[i])
}
# Cross-validation
set.seed(789)
ord_cv_accuracy <- numeric(cv_folds)
for (fold in 1:cv_folds) {
  train <- ord_data[fold_idx != fold, ]
  test <- ord_data[fold_idx == fold, ]
  cv_model <- polr(y ~ x1 + x2 + x3, data = train, Hess = TRUE)
  cv_pred <- predict(cv_model, newdata = test, type = "class")
  ord_cv_accuracy[fold] <- mean(as.character(cv_pred) == as.character(test$y))
}
sink("output/ordinal_evaluation.txt")
cat("=== Ordinal Model Evaluation ===\n\n")
cat("--- Confusion Matrix ---\n")
print(ord_conf)
cat("\nOverall Accuracy:", round(ord_accuracy, 4), "\n")
cat("\n--- Per-Class Metrics ---\n")
print(ord_metrics, row.names = FALSE)
cat("\n--- 5-Fold Cross-Validation ---\n")
cat("Fold accuracies:", round(ord_cv_accuracy, 4), "\n")
cat("Mean CV Accuracy:", round(mean(ord_cv_accuracy), 4), "\n")
cat("SD:", round(sd(ord_cv_accuracy), 4), "\n")
sink()
# ============================================================================
# Section 12.11: Compare Multinomial vs Ordinal on Same Data
# ============================================================================
cat("Comparing multinomial and ordinal models on ordinal data...\n")
# Fit multinomial to ordinal data (ignoring ordering)
multi_on_ord <- multinom(y ~ x1 + x2 + x3, data = ord_data, trace = FALSE)
# Compare model fit
comparison <- data.frame(
  Model = c("Multinomial", "Ordinal"),
  Parameters = c(8, 5),
  LogLik = c(as.numeric(logLik(multi_on_ord)), as.numeric(logLik(ord_model))),
  AIC = c(AIC(multi_on_ord), AIC(ord_model)),
  BIC = c(BIC(multi_on_ord), BIC(ord_model))
)
# Compare predictions
multi_pred_ord <- predict(multi_on_ord, type = "class")
ord_pred_ord <- predict(ord_model, type = "class")
agreement <- mean(as.character(multi_pred_ord) == as.character(ord_pred_ord))
multi_acc_ord <- mean(as.character(multi_pred_ord) == as.character(ord_data$y))
ord_acc_ord <- mean(as.character(ord_pred_ord) == as.character(ord_data$y))
# Cross-validation comparison
set.seed(999)
cv_comparison <- data.frame(
  Fold = 1:cv_folds,
  Multinomial = NA,
  Ordinal = NA
)
for (fold in 1:cv_folds) {
  train <- ord_data[fold_idx != fold, ]
  test <- ord_data[fold_idx == fold, ]
  cv_multi <- multinom(y ~ x1 + x2 + x3, data = train, trace = FALSE)
  cv_ord <- polr(y ~ x1 + x2 + x3, data = train, Hess = TRUE)
  cv_comparison$Multinomial[fold] <- mean(as.character(predict(cv_multi, newdata = test)) == as.character(test$y))
  cv_comparison$Ordinal[fold] <- mean(as.character(predict(cv_ord, newdata = test)) == as.character(test$y))
}
sink("output/model_comparison.txt")
cat("=== Multinomial vs Ordinal Model Comparison ===\n\n")
cat("(Both models applied to ordinal data)\n\n")
cat("--- Model Fit Statistics ---\n")
print(comparison, row.names = FALSE)
cat("\n--- Prediction Performance ---\n")
cat("Multinomial accuracy:", round(multi_acc_ord, 4), "\n")
cat("Ordinal accuracy:", round(ord_acc_ord, 4), "\n")
cat("Prediction agreement:", round(agreement, 4), "\n")
cat("\n--- Cross-Validation Comparison ---\n")
print(cv_comparison, row.names = FALSE)
cat("\nMean CV Accuracy:\n")
cat("  Multinomial:", round(mean(cv_comparison$Multinomial), 4), "\n")
cat("  Ordinal:", round(mean(cv_comparison$Ordinal), 4), "\n")
cat("\nConclusion:\n")
if (comparison$AIC[2] < comparison$AIC[1]) {
  cat("Ordinal model has lower AIC - preferred for ordinal data\n")
} else {
  cat("Multinomial model has lower AIC\n")
}
cat("Ordinal model uses fewer parameters (", comparison$Parameters[2],
    " vs ", comparison$Parameters[1], ")\n", sep = "")
sink()
# Figure 12.7: Model comparison
fig12_7 <- ggplot(comparison, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.5) +
  scale_fill_manual(values = c("Multinomial" = "grey70", "Ordinal" = "grey30")) +
  labs(title = "Model Comparison: AIC",
       subtitle = "Lower AIC indicates better fit (penalised for complexity)",
       x = "", y = "AIC") +
  theme(legend.position = "none")
ggsave("figures/fig12_7_model_comparison.jpeg", fig12_7, width = 6, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_7_model_comparison.jpeg\n")
# Figure 12.8: CV comparison
cv_long <- cv_comparison %>%
  pivot_longer(cols = c(Multinomial, Ordinal), names_to = "Model", values_to = "Accuracy")
fig12_8 <- ggplot(cv_long, aes(x = factor(Fold), y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Multinomial" = "grey70", "Ordinal" = "grey30")) +
  labs(title = "Cross-Validation: Multinomial vs Ordinal",
       x = "Fold", y = "Accuracy") +
  theme(legend.position = "bottom")
ggsave("figures/fig12_8_cv_comparison.jpeg", fig12_8, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig12_8_cv_comparison.jpeg\n")
# ============================================================================
# Section 12.12: Summary
# ============================================================================
sink("output/chapter12_summary.txt")
cat("=== Chapter 12 Summary: Multinomial and Ordinal Logistic Regression ===\n\n")
cat("MULTINOMIAL LOGISTIC REGRESSION:\n")
cat("-", rep("-", 50), "\n", sep = "")
cat("Data: n = 1000, 3 unordered categories\n")
cat("Category distribution:", paste(table(multi_data$y), collapse = ", "), "\n\n")
cat("Key RRRs (Category2 vs Category1):\n")
cat("  x1: RRR =", round(rrr[1, 2], 3), "\n")
cat("  x2: RRR =", round(rrr[1, 3], 3), "\n")
cat("Key RRRs (Category3 vs Category1):\n")
cat("  x1: RRR =", round(rrr[2, 2], 3), "\n")
cat("  x2: RRR =", round(rrr[2, 3], 3), "\n")
cat("Accuracy:", round(multi_accuracy, 4), "\n")
cat("CV Accuracy:", round(mean(cv_accuracy), 4), "\n\n")
cat("ORDINAL LOGISTIC REGRESSION:\n")
cat("-", rep("-", 50), "\n", sep = "")
cat("Data: n = 1000, 3 ordered categories (Low < Medium < High)\n")
cat("Category distribution:", paste(table(ord_data$y), collapse = ", "), "\n\n")
cat("Key ORs (for higher category):\n")
cat("  x1: OR =", round(ord_or[1], 3), "\n")
cat("  x2: OR =", round(ord_or[2], 3), "\n")
cat("  x3Yes: OR =", round(ord_or[3], 3), "\n")
cat("Thresholds: Low|Medium =", round(ord_thresholds[1], 3),
    ", Medium|High =", round(ord_thresholds[2], 3), "\n")
cat("Accuracy:", round(ord_accuracy, 4), "\n")
cat("CV Accuracy:", round(mean(ord_cv_accuracy), 4), "\n\n")
cat("PROPORTIONAL ODDS TEST:\n")
cat("-", rep("-", 50), "\n", sep = "")
cat("LRT statistic:", round(lrt_stat, 4), "\n")
cat("P-value:", round(lrt_pval, 4), "\n")
cat("Assumption:", ifelse(lrt_pval > 0.05, "Not rejected", "Rejected"), "\n\n")
cat("MODEL COMPARISON (on ordinal data):\n")
cat("-", rep("-", 50), "\n", sep = "")
cat("Multinomial AIC:", round(AIC(multi_on_ord), 2), "\n")
cat("Ordinal AIC:", round(AIC(ord_model), 2), "\n")
cat("Preferred:", ifelse(AIC(ord_model) < AIC(multi_on_ord), "Ordinal", "Multinomial"), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 12 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig12_1_predictors_by_category.jpeg\n")
cat("  - fig12_2_multinomial_rrr.jpeg\n")
cat("  - fig12_3_multinomial_predicted_probs.jpeg\n")
cat("  - fig12_4_ordinal_predictors.jpeg\n")
cat("  - fig12_5_ordinal_or.jpeg\n")
cat("  - fig12_6_ordinal_predicted_probs.jpeg\n")
cat("  - fig12_7_model_comparison.jpeg\n")
cat("  - fig12_8_cv_comparison.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - multinomial_data_summary.txt\n")
cat("  - multinomial_results.txt\n")
cat("  - multinomial_evaluation.txt\n")
cat("  - ordinal_data_summary.txt\n")
cat("  - ordinal_results.txt\n")
cat("  - proportional_odds_test.txt\n")
cat("  - ordinal_evaluation.txt\n")
cat("  - model_comparison.txt\n")
cat("  - chapter12_summary.txt\n")