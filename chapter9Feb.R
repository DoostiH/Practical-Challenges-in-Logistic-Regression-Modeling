# ============================================================================
# Chapter 9: Model Diagnostics and Goodness-of-Fit
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 9
# Required packages: tidyverse, pROC, ggplot2, gridExtra, ResourceSelection
# ============================================================================
# Load required packages
library(tidyverse)
library(pROC)
library(ggplot2)
library(gridExtra)
library(ResourceSelection)
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
cat("CHAPTER 9: MODEL DIAGNOSTICS AND GOODNESS-OF-FIT\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 9.2: Simulated Data - Well-Specified Model
# ============================================================================
cat("Creating simulated data for a well-specified model...\n")
n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
# True model
linear_pred <- -0.5 + 0.8 * x1 - 0.4 * x2 + 0.6 * x3
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
# Create data frame
diag_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
# Fit the model
model <- glm(y ~ x1 + x2 + x3, family = binomial(link = "logit"), data = diag_data)
# Save data summary
sink("output/data_summary.txt")
cat("=== Simulated Data Summary (Well-Specified Model) ===\n\n")
cat("Sample size:", n, "\n")
cat("Event rate:", round(mean(y), 3), "\n\n")
cat("True model:\n")
cat("  logit(p) = -0.5 + 0.8*x1 - 0.4*x2 + 0.6*x3\n\n")
cat("Model summary:\n")
print(summary(model))
sink()
# ============================================================================
# Section 9.3: Types of Residuals
# ============================================================================
cat("Calculating different types of residuals...\n")
# Calculate different residual types
diag_data$raw_resid <- residuals(model, type = "response")
diag_data$pearson_resid <- residuals(model, type = "pearson")
diag_data$deviance_resid <- residuals(model, type = "deviance")
diag_data$pred_prob <- fitted(model)
diag_data$leverage <- hatvalues(model)
diag_data$cooks_dist <- cooks.distance(model)
# Save residual summary
sink("output/residual_summary.txt")
cat("=== Residual Summary Statistics ===\n\n")
cat("Raw Residuals:\n")
cat("  Mean:", round(mean(diag_data$raw_resid), 4), "\n")
cat("  SD:", round(sd(diag_data$raw_resid), 4), "\n")
cat("  Range:", round(min(diag_data$raw_resid), 4), "to", round(max(diag_data$raw_resid), 4), "\n\n")
cat("Pearson Residuals:\n")
cat("  Mean:", round(mean(diag_data$pearson_resid), 4), "\n")
cat("  SD:", round(sd(diag_data$pearson_resid), 4), "\n")
cat("  Range:", round(min(diag_data$pearson_resid), 4), "to", round(max(diag_data$pearson_resid), 4), "\n\n")
cat("Deviance Residuals:\n")
cat("  Mean:", round(mean(diag_data$deviance_resid), 4), "\n")
cat("  SD:", round(sd(diag_data$deviance_resid), 4), "\n")
cat("  Range:", round(min(diag_data$deviance_resid), 4), "to", round(max(diag_data$deviance_resid), 4), "\n\n")
cat("Large residuals (|deviance| > 2):", sum(abs(diag_data$deviance_resid) > 2),
    "(", round(100 * sum(abs(diag_data$deviance_resid) > 2) / n, 1), "%)\n")
sink()
# Figure 9.1: Residual plots (4-panel)
p1 <- ggplot(diag_data, aes(x = pred_prob, y = raw_resid)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "black", linewidth = 1) +
  labs(title = "Raw Residuals vs. Predicted",
       x = "Predicted Probability", y = "Raw Residual") +
  theme(plot.title = element_text(size = 10))
p2 <- ggplot(diag_data, aes(x = pred_prob, y = pearson_resid)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "black", linewidth = 1) +
  labs(title = "Pearson Residuals vs. Predicted",
       x = "Predicted Probability", y = "Pearson Residual") +
  theme(plot.title = element_text(size = 10))
p3 <- ggplot(diag_data, aes(x = pred_prob, y = deviance_resid)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "black", linewidth = 1) +
  labs(title = "Deviance Residuals vs. Predicted",
       x = "Predicted Probability", y = "Deviance Residual") +
  theme(plot.title = element_text(size = 10))
p4 <- ggplot(diag_data, aes(x = 1:n, y = deviance_resid)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", colour = "black") +
  labs(title = "Index Plot of Deviance Residuals",
       x = "Observation Index", y = "Deviance Residual") +
  theme(plot.title = element_text(size = 10))
fig9_1 <- grid.arrange(p1, p2, p3, p4, ncol = 2,
                       top = "Residual Diagnostic Plots (Well-Specified Model)")
ggsave("figures/fig9_1_residual_plots.jpeg", fig9_1, width = 10, height = 8,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_1_residual_plots.jpeg\n")
# ============================================================================
# Section 9.4: Misspecified Model - Missing Quadratic Term
# ============================================================================
cat("Creating misspecified model example...\n")
set.seed(456)
n_miss <- 500
x1_miss <- rnorm(n_miss)
x2_miss <- rnorm(n_miss)
# True model with quadratic effect
linear_pred_miss <- -0.5 + 0.8 * x1_miss + 0.5 * x1_miss^2 - 0.4 * x2_miss
prob_miss <- plogis(linear_pred_miss)
y_miss <- rbinom(n_miss, 1, prob_miss)
miss_data <- data.frame(y = y_miss, x1 = x1_miss, x2 = x2_miss)
# Fit misspecified model (missing quadratic term)
model_miss <- glm(y ~ x1 + x2, family = binomial, data = miss_data)
# Fit correctly specified model
model_correct <- glm(y ~ x1 + I(x1^2) + x2, family = binomial, data = miss_data)
# Calculate residuals for both models
miss_data$dev_resid_miss <- residuals(model_miss, type = "deviance")
miss_data$dev_resid_correct <- residuals(model_correct, type = "deviance")
miss_data$pred_prob_miss <- fitted(model_miss)
miss_data$pred_prob_correct <- fitted(model_correct)
# Save misspecification analysis
sink("output/misspecification_analysis.txt")
cat("=== Model Misspecification Analysis ===\n\n")
cat("True model: logit(p) = -0.5 + 0.8*x1 + 0.5*x1^2 - 0.4*x2\n\n")
cat("--- Misspecified Model (missing x1^2) ---\n")
print(summary(model_miss))
cat("AIC:", round(AIC(model_miss), 2), "\n\n")
cat("--- Correctly Specified Model ---\n")
print(summary(model_correct))
cat("AIC:", round(AIC(model_correct), 2), "\n\n")
cat("AIC improvement:", round(AIC(model_miss) - AIC(model_correct), 2), "\n")
sink()
# Figure 9.2: Residuals for misspecified vs correct model
p_miss_x1 <- ggplot(miss_data, aes(x = x1, y = dev_resid_miss)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "black", linewidth = 1) +
  labs(title = "Misspecified Model",
       subtitle = "Clear U-shaped pattern indicates missing quadratic",
       x = "x1", y = "Deviance Residual") +
  theme(plot.title = element_text(size = 10))
p_correct_x1 <- ggplot(miss_data, aes(x = x1, y = dev_resid_correct)) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "black", linewidth = 1) +
  labs(title = "Correct Model",
       subtitle = "No systematic pattern - model well-specified",
       x = "x1", y = "Deviance Residual") +
  theme(plot.title = element_text(size = 10))
fig9_2 <- grid.arrange(p_miss_x1, p_correct_x1, ncol = 2,
                       top = "Detecting Misspecification: Residuals vs. x1")
ggsave("figures/fig9_2_misspecification.jpeg", fig9_2, width = 10, height = 4,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_2_misspecification.jpeg\n")
# ============================================================================
# Section 9.5: Influence Diagnostics
# ============================================================================
cat("Analyzing influential observations...\n")
# Create dataset with an influential observation
infl_data <- diag_data
# Add an influential point: extreme x1 value with unexpected outcome
infl_data$x1[1] <- 4.5   # Extreme x1 value
infl_data$y[1] <- 0      # Unexpected y value given high x1
# Refit model with influential point
model_infl <- glm(y ~ x1 + x2 + x3, family = binomial, data = infl_data)
model_no_infl <- glm(y ~ x1 + x2 + x3, family = binomial, data = infl_data[-1, ])
# Calculate influence measures
infl_data$leverage <- hatvalues(model_infl)
infl_data$cooks_dist <- cooks.distance(model_infl)
infl_data$deviance_resid <- residuals(model_infl, type = "deviance")
# Calculate DFBETAS
dfbetas_mat <- dfbetas(model_infl)
infl_data$dfbeta_x1 <- dfbetas_mat[, "x1"]
infl_data$dfbeta_total <- sqrt(rowSums(dfbetas_mat^2))
# Identify high influence observations
leverage_threshold <- 2 * (length(coef(model_infl))) / n
cooks_threshold <- 4 / n
high_leverage <- which(infl_data$leverage > leverage_threshold)
high_cooks <- which(infl_data$cooks_dist > cooks_threshold)
# Save influence diagnostics
sink("output/influence_diagnostics.txt")
cat("=== Influence Diagnostics ===\n\n")
cat("Thresholds:\n")
cat("  Leverage threshold (2p/n):", round(leverage_threshold, 4), "\n")
cat("  Cook's distance threshold (4/n):", round(cooks_threshold, 4), "\n\n")
cat("High leverage observations:", length(high_leverage), "\n")
cat("  Indices:", head(high_leverage, 10), "...\n\n")
cat("High Cook's distance observations:", length(high_cooks), "\n")
cat("  Indices:", head(high_cooks, 10), "...\n\n")
cat("Most influential observation (#1):\n")
cat("  Leverage:", round(infl_data$leverage[1], 4), "\n")
cat("  Cook's distance:", round(infl_data$cooks_dist[1], 4), "\n")
cat("  DFBETA for x1:", round(infl_data$dfbeta_x1[1], 4), "\n\n")
cat("Coefficient comparison (with vs without influential obs #1):\n")
coef_compare <- data.frame(
  Variable = names(coef(model_infl)),
  With_Influential = round(coef(model_infl), 4),
  Without_Influential = round(coef(model_no_infl), 4)
)
coef_compare$Percent_Change <- round(100 * (coef_compare$With_Influential - coef_compare$Without_Influential) /
                                       abs(coef_compare$Without_Influential), 2)
print(coef_compare, row.names = FALSE)
sink()
# Figure 9.3: Influence diagnostic plots
p_lev <- ggplot(infl_data, aes(x = 1:n, y = leverage)) +
  geom_point(alpha = 0.5, colour = "grey50") +
  geom_point(data = infl_data[1, ], aes(x = 1, y = leverage),
             colour = "black", size = 3, shape = 17) +
  geom_hline(yintercept = leverage_threshold, linetype = "dashed", colour = "black") +
  labs(title = "Leverage",
       x = "Observation Index", y = "Leverage") +
  annotate("text", x = 1, y = infl_data$leverage[1] + 0.02, label = "Obs #1",
           colour = "black", size = 3)
p_cook <- ggplot(infl_data, aes(x = 1:n, y = cooks_dist)) +
  geom_point(alpha = 0.5, colour = "grey50") +
  geom_point(data = infl_data[1, ], aes(x = 1, y = cooks_dist),
             colour = "black", size = 3, shape = 17) +
  geom_hline(yintercept = cooks_threshold, linetype = "dashed", colour = "black") +
  labs(title = "Cook's Distance",
       x = "Observation Index", y = "Cook's Distance") +
  annotate("text", x = 1, y = infl_data$cooks_dist[1] + 0.01, label = "Obs #1",
           colour = "black", size = 3)
p_dfbeta <- ggplot(infl_data, aes(x = 1:n, y = dfbeta_x1)) +
  geom_point(alpha = 0.5, colour = "grey50") +
  geom_point(data = infl_data[1, ], aes(x = 1, y = dfbeta_x1),
             colour = "black", size = 3, shape = 17) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-2/sqrt(n), 2/sqrt(n)), linetype = "dotted", colour = "black") +
  labs(title = "DFBETA for x1",
       x = "Observation Index", y = "DFBETA") +
  annotate("text", x = 1, y = infl_data$dfbeta_x1[1] - 0.05, label = "Obs #1",
           colour = "black", size = 3)
fig9_3 <- grid.arrange(p_lev, p_cook, p_dfbeta, ncol = 3,
                       top = "Influence Diagnostics with Artificially Influential Observation")
ggsave("figures/fig9_3_influence_diagnostics.jpeg", fig9_3, width = 12, height = 4,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_3_influence_diagnostics.jpeg\n")
# Figure 9.4: Bubble plot - Leverage vs Residuals
fig9_4 <- ggplot(infl_data, aes(x = leverage, y = deviance_resid, size = cooks_dist)) +
  geom_point(alpha = 0.5, colour = "grey50") +
  geom_point(data = infl_data[1, ], aes(x = leverage, y = deviance_resid, size = cooks_dist),
             colour = "black", shape = 17) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = leverage_threshold, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", colour = "grey40") +
  labs(title = "Leverage vs. Deviance Residuals",
       subtitle = "Bubble size proportional to Cook's distance; triangle marks influential observation #1",
       x = "Leverage", y = "Deviance Residual", size = "Cook's\nDistance") +
  theme(legend.position = "right")
ggsave("figures/fig9_4_leverage_residual_bubble.jpeg", fig9_4, width = 9, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_4_leverage_residual_bubble.jpeg\n")
# ============================================================================
# Section 9.6: Goodness-of-Fit Tests
# ============================================================================
cat("Performing goodness-of-fit tests...\n")
# Use the original well-specified model for GOF tests
# Deviance test
deviance_stat <- model$deviance
deviance_df <- model$df.residual
deviance_pval <- 1 - pchisq(deviance_stat, deviance_df)
# Pearson chi-square
pearson_stat <- sum(residuals(model, type = "pearson")^2)
pearson_pval <- 1 - pchisq(pearson_stat, deviance_df)
# Hosmer-Lemeshow test
hl_test <- hoslem.test(diag_data$y, fitted(model), g = 10)
sink("output/goodness_of_fit.txt")
cat("=== Goodness-of-Fit Tests ===\n\n")
cat("--- Deviance Test ---\n")
cat("Deviance statistic:", round(deviance_stat, 2), "\n")
cat("Degrees of freedom:", deviance_df, "\n")
cat("p-value:", round(deviance_pval, 4), "\n")
cat("Conclusion:", ifelse(deviance_pval > 0.05, "No evidence of poor fit", "Evidence of poor fit"), "\n\n")
cat("--- Pearson Chi-Square Test ---\n")
cat("Pearson chi-square:", round(pearson_stat, 2), "\n")
cat("Degrees of freedom:", deviance_df, "\n")
cat("p-value:", round(pearson_pval, 4), "\n")
cat("Conclusion:", ifelse(pearson_pval > 0.05, "No evidence of poor fit", "Evidence of poor fit"), "\n\n")
cat("--- Hosmer-Lemeshow Test ---\n")
cat("Chi-square statistic:", round(hl_test$statistic, 2), "\n")
cat("Degrees of freedom:", hl_test$parameter, "\n")
cat("p-value:", round(hl_test$p.value, 4), "\n")
cat("Conclusion:", ifelse(hl_test$p.value > 0.05, "No evidence of poor fit", "Evidence of poor fit"), "\n\n")
cat("Note: For individual-level binary data, the Hosmer-Lemeshow test\n")
cat("is more appropriate than the deviance or Pearson tests.\n")
sink()
# Figure 9.5: Hosmer-Lemeshow observed vs expected
diag_data$pred_group <- cut(fitted(model), breaks = quantile(fitted(model), probs = seq(0, 1, 0.1)),
                            include.lowest = TRUE, labels = 1:10)
hl_plot_data <- diag_data %>%
  group_by(pred_group) %>%
  summarise(
    n = n(),
    observed = sum(y),
    expected = sum(pred_prob),
    obs_prop = mean(y),
    exp_prop = mean(pred_prob),
    .groups = "drop"
  )
fig9_5 <- ggplot(hl_plot_data, aes(x = as.numeric(pred_group))) +
  geom_bar(aes(y = observed, fill = "Observed"), stat = "identity", alpha = 0.7, width = 0.4,
           position = position_nudge(x = -0.2), colour = "black", linewidth = 0.3) +
  geom_bar(aes(y = expected, fill = "Expected"), stat = "identity", alpha = 0.7, width = 0.4,
           position = position_nudge(x = 0.2), colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Observed" = "grey30", "Expected" = "grey70")) +
  labs(title = "Hosmer-Lemeshow Test: Observed vs. Expected Events",
       subtitle = paste0("Chi-square = ", round(hl_test$statistic, 2),
                         ", df = ", hl_test$parameter,
                         ", p = ", round(hl_test$p.value, 3)),
       x = "Decile of Predicted Probability", y = "Number of Events", fill = "") +
  scale_x_continuous(breaks = 1:10) +
  theme(legend.position = "bottom")
ggsave("figures/fig9_5_hosmer_lemeshow.jpeg", fig9_5, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_5_hosmer_lemeshow.jpeg\n")
# ============================================================================
# Section 9.7: ROC Curve and Classification Metrics
# ============================================================================
cat("Computing ROC curve and classification metrics...\n")
# ROC analysis
roc_obj <- roc(diag_data$y, fitted(model), quiet = TRUE)
auc_val <- auc(roc_obj)
# Find optimal threshold
coords_best <- coords(roc_obj, "best", best.method = "youden")
threshold <- coords_best$threshold
# Classification at optimal threshold
pred_class <- ifelse(fitted(model) >= threshold, 1, 0)
conf_matrix <- table(Actual = diag_data$y, Predicted = pred_class)
# Calculate metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
ppv <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
npv <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
f1_score <- 2 * ppv * sensitivity / (ppv + sensitivity)
sink("output/classification_metrics.txt")
cat("=== ROC Analysis and Classification Metrics ===\n\n")
cat("--- ROC Analysis ---\n")
cat("AUC:", round(auc_val, 4), "\n")
cat("95% CI:", round(ci.auc(roc_obj)[1], 4), "-", round(ci.auc(roc_obj)[3], 4), "\n")
cat("Optimal threshold (Youden's J):", round(threshold, 4), "\n\n")
cat("--- Confusion Matrix (at optimal threshold) ---\n")
print(conf_matrix)
cat("\n")
cat("--- Classification Metrics ---\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Positive Predictive Value (Precision):", round(ppv, 4), "\n")
cat("Negative Predictive Value:", round(npv, 4), "\n")
cat("F1 Score:", round(f1_score, 4), "\n")
sink()
# Figure 9.6: ROC Curve
roc_data <- data.frame(
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities,
  FPR = 1 - roc_obj$specificities
)
fig9_6 <- ggplot(roc_data, aes(x = FPR, y = Sensitivity)) +
  geom_line(colour = "black", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(data = data.frame(FPR = 1 - coords_best$specificity,
                               Sensitivity = coords_best$sensitivity),
             aes(x = FPR, y = Sensitivity), colour = "black", size = 3, shape = 17) +
  labs(title = "ROC Curve",
       subtitle = paste0("AUC = ", round(auc_val, 3),
                         " (95% CI: ", round(ci.auc(roc_obj)[1], 3),
                         "\u2013", round(ci.auc(roc_obj)[3], 3), ")"),
       x = "1 \u2212 Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  annotate("text", x = 0.7, y = 0.3,
           label = paste0("Optimal threshold = ", round(threshold, 3)),
           colour = "black") +
  coord_equal()
ggsave("figures/fig9_6_roc_curve.jpeg", fig9_6, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_6_roc_curve.jpeg\n")
# ============================================================================
# Section 9.8: Calibration Plot
# ============================================================================
cat("Creating calibration plot...\n")
# Create calibration data
n_bins <- 10
diag_data$cal_bin <- cut(fitted(model),
                         breaks = quantile(fitted(model), probs = seq(0, 1, length.out = n_bins + 1)),
                         include.lowest = TRUE, labels = FALSE)
cal_data <- diag_data %>%
  group_by(cal_bin) %>%
  summarise(
    n = n(),
    observed_prop = mean(y),
    predicted_prob = mean(pred_prob),
    se = sqrt(observed_prop * (1 - observed_prop) / n),
    .groups = "drop"
  )
cal_data$lower <- cal_data$observed_prop - 1.96 * cal_data$se
cal_data$upper <- cal_data$observed_prop + 1.96 * cal_data$se
# Calculate calibration metrics
cal_slope <- coef(lm(observed_prop ~ predicted_prob, data = cal_data))[2]
cal_intercept <- coef(lm(observed_prop ~ predicted_prob, data = cal_data))[1]
# Brier score
brier_score <- mean((fitted(model) - diag_data$y)^2)
sink("output/calibration_metrics.txt")
cat("=== Calibration Metrics ===\n\n")
cat("Brier Score:", round(brier_score, 4), "\n")
cat("  (Lower is better; 0 = perfect, 0.25 = uninformative for 50% prevalence)\n\n")
cat("Calibration slope:", round(cal_slope, 4), "\n")
cat("  (Ideal = 1; <1 indicates overfitting, >1 indicates underfitting)\n\n")
cat("Calibration intercept:", round(cal_intercept, 4), "\n")
cat("  (Ideal = 0; >0 indicates underprediction, <0 indicates overprediction)\n\n")
cat("Calibration by decile:\n")
print(cal_data, n = 10)
sink()
# Figure 9.7: Calibration plot
fig9_7 <- ggplot(cal_data, aes(x = predicted_prob, y = observed_prop)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "grey30") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "solid") +
  labs(title = "Calibration Plot",
       subtitle = paste0("Brier score = ", round(brier_score, 4),
                         ", Calibration slope = ", round(cal_slope, 2)),
       x = "Mean Predicted Probability", y = "Observed Proportion",
       size = "n per bin") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(legend.position = "right")
ggsave("figures/fig9_7_calibration_plot.jpeg", fig9_7, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_7_calibration_plot.jpeg\n")
# ============================================================================
# Section 9.9: Model Comparison
# ============================================================================
cat("Comparing nested models...\n")
# Fit alternative models
model1 <- glm(y ~ x1, family = binomial, data = diag_data)
model2 <- glm(y ~ x1 + x2, family = binomial, data = diag_data)
model3 <- glm(y ~ x1 + x2 + x3, family = binomial, data = diag_data)
model4 <- glm(y ~ x1 + x2 + x3 + x1:x2, family = binomial, data = diag_data)
# Likelihood ratio tests
lrt_1_2 <- anova(model1, model2, test = "Chisq")
lrt_2_3 <- anova(model2, model3, test = "Chisq")
lrt_3_4 <- anova(model3, model4, test = "Chisq")
# Model comparison table
model_comparison <- data.frame(
  Model = c("x1 only", "x1 + x2", "x1 + x2 + x3", "x1 + x2 + x3 + x1:x2"),
  Deviance = round(c(model1$deviance, model2$deviance, model3$deviance, model4$deviance), 2),
  DF = c(model1$df.residual, model2$df.residual, model3$df.residual, model4$df.residual),
  AIC = round(c(AIC(model1), AIC(model2), AIC(model3), AIC(model4)), 2),
  BIC = round(c(BIC(model1), BIC(model2), BIC(model3), BIC(model4)), 2)
)
model_comparison$Delta_AIC <- round(model_comparison$AIC - min(model_comparison$AIC), 2)
model_comparison$Delta_BIC <- round(model_comparison$BIC - min(model_comparison$BIC), 2)
sink("output/model_comparison.txt")
cat("=== Model Comparison ===\n\n")
cat("--- Information Criteria ---\n")
print(model_comparison, row.names = FALSE)
cat("\n--- Likelihood Ratio Tests ---\n\n")
cat("Model 1 (x1) vs Model 2 (x1 + x2):\n")
cat("  Deviance change:", round(lrt_1_2$Deviance[2], 2), "\n")
cat("  p-value:", format(lrt_1_2$`Pr(>Chi)`[2], digits = 4), "\n")
cat("  Significant:", ifelse(lrt_1_2$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n\n")
cat("Model 2 (x1 + x2) vs Model 3 (x1 + x2 + x3):\n")
cat("  Deviance change:", round(lrt_2_3$Deviance[2], 2), "\n")
cat("  p-value:", format(lrt_2_3$`Pr(>Chi)`[2], digits = 4), "\n")
cat("  Significant:", ifelse(lrt_2_3$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n\n")
cat("Model 3 (x1 + x2 + x3) vs Model 4 (+ x1:x2 interaction):\n")
cat("  Deviance change:", round(lrt_3_4$Deviance[2], 2), "\n")
cat("  p-value:", format(lrt_3_4$`Pr(>Chi)`[2], digits = 4), "\n")
cat("  Significant:", ifelse(lrt_3_4$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n")
cat("\nBest model by AIC:", model_comparison$Model[which.min(model_comparison$AIC)], "\n")
cat("Best model by BIC:", model_comparison$Model[which.min(model_comparison$BIC)], "\n")
sink()
# Figure 9.8: AIC/BIC comparison
model_comparison_long <- model_comparison %>%
  select(Model, AIC, BIC) %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Criterion", values_to = "Value")
fig9_8 <- ggplot(model_comparison_long, aes(x = Model, y = Value, fill = Criterion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("AIC" = "grey70", "BIC" = "grey30")) +
  labs(title = "Model Comparison: AIC and BIC",
       subtitle = "Lower values indicate better fit (accounting for complexity)",
       x = "Model", y = "Value") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")
ggsave("figures/fig9_8_model_comparison.jpeg", fig9_8, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_8_model_comparison.jpeg\n")
# ============================================================================
# Section 9.10: Cross-Validation
# ============================================================================
cat("Performing cross-validation...\n")
set.seed(123)
k <- 10
folds <- sample(rep(1:k, length.out = n))
cv_results <- data.frame(
  Fold = integer(),
  Model = character(),
  AUC = numeric(),
  Brier = numeric(),
  Accuracy = numeric()
)
for (fold in 1:k) {
  train_idx <- which(folds != fold)
  test_idx <- which(folds == fold)
  train_data <- diag_data[train_idx, ]
  test_data <- diag_data[test_idx, ]
  # Fit models
  cv_model1 <- glm(y ~ x1, family = binomial, data = train_data)
  cv_model2 <- glm(y ~ x1 + x2, family = binomial, data = train_data)
  cv_model3 <- glm(y ~ x1 + x2 + x3, family = binomial, data = train_data)
  for (m in 1:3) {
    cv_model <- switch(m, cv_model1, cv_model2, cv_model3)
    model_name <- c("x1 only", "x1 + x2", "x1 + x2 + x3")[m]
    pred_prob <- predict(cv_model, newdata = test_data, type = "response")
    # AUC
    roc_cv <- roc(test_data$y, pred_prob, quiet = TRUE)
    auc_cv <- as.numeric(auc(roc_cv))
    # Brier
    brier_cv <- mean((pred_prob - test_data$y)^2)
    # Accuracy at 0.5 threshold
    pred_class_cv <- ifelse(pred_prob >= 0.5, 1, 0)
    acc_cv <- mean(pred_class_cv == test_data$y)
    cv_results <- rbind(cv_results, data.frame(
      Fold = fold,
      Model = model_name,
      AUC = auc_cv,
      Brier = brier_cv,
      Accuracy = acc_cv
    ))
  }
}
cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarise(
    Mean_AUC = mean(AUC),
    SD_AUC = sd(AUC),
    Mean_Brier = mean(Brier),
    SD_Brier = sd(Brier),
    Mean_Accuracy = mean(Accuracy),
    SD_Accuracy = sd(Accuracy),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_AUC))
sink("output/cv_results.txt")
cat("=== Cross-Validation Results (", k, "-fold) ===\n\n", sep = "")
print(as.data.frame(cv_summary), row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Best model by CV-AUC:", cv_summary$Model[1],
    "(", round(cv_summary$Mean_AUC[1], 4), ")\n")
cat("- Adding predictors improves AUC from",
    round(cv_summary$Mean_AUC[cv_summary$Model == "x1 only"], 4), "to",
    round(cv_summary$Mean_AUC[cv_summary$Model == "x1 + x2 + x3"], 4), "\n")
sink()
# Figure 9.9: CV results boxplots
cv_results$Model <- factor(cv_results$Model,
                           levels = c("x1 only", "x1 + x2", "x1 + x2 + x3"))
fig9_9 <- ggplot(cv_results, aes(x = Model, y = AUC, fill = Model)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("x1 only" = "grey80",
                               "x1 + x2" = "grey55",
                               "x1 + x2 + x3" = "grey30")) +
  labs(title = "Cross-Validated AUC by Model",
       subtitle = paste0(k, "-fold cross-validation"),
       x = "Model", y = "AUC") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("figures/fig9_9_cv_auc.jpeg", fig9_9, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_9_cv_auc.jpeg\n")
# ============================================================================
# Section 9.11: Real Data Example
# ============================================================================
cat("Creating real data example...\n")
set.seed(789)
n_real <- 600
# Simulate clinical data
age <- round(rnorm(n_real, mean = 55, sd = 12))
age <- pmax(25, pmin(age, 85))
bmi <- round(rnorm(n_real, mean = 27, sd = 5), 1)
bmi <- pmax(18, pmin(bmi, 45))
smoking <- rbinom(n_real, 1, 0.3)
diabetes <- rbinom(n_real, 1, 0.15)
# True model for disease
linear_pred_real <- -4 + 0.04 * age + 0.08 * bmi + 0.6 * smoking + 0.9 * diabetes
prob_real <- plogis(linear_pred_real)
disease <- rbinom(n_real, 1, prob_real)
real_data <- data.frame(
  disease = disease,
  age = age,
  bmi = bmi,
  smoking = factor(smoking, labels = c("No", "Yes")),
  diabetes = factor(diabetes, labels = c("No", "Yes"))
)
# Fit model
real_model <- glm(disease ~ age + bmi + smoking + diabetes, family = binomial, data = real_data)
# ROC analysis
real_roc <- roc(real_data$disease, fitted(real_model), quiet = TRUE)
real_auc <- auc(real_roc)
# Hosmer-Lemeshow test
real_hl <- hoslem.test(real_data$disease, fitted(real_model), g = 10)
# Calibration
real_data$pred_prob <- fitted(real_model)
real_data$cal_bin <- cut(fitted(real_model),
                         breaks = quantile(fitted(real_model), probs = seq(0, 1, 0.1)),
                         include.lowest = TRUE, labels = FALSE)
real_cal_data <- real_data %>%
  group_by(cal_bin) %>%
  summarise(
    n = n(),
    observed_prop = mean(disease),
    predicted_prob = mean(pred_prob),
    .groups = "drop"
  )
real_brier <- mean((fitted(real_model) - real_data$disease)^2)
sink("output/real_data_analysis.txt")
cat("=== Real Data Example: Disease Risk Model ===\n\n")
cat("Sample size:", n_real, "\n")
cat("Disease prevalence:", round(mean(disease) * 100, 1), "%\n\n")
cat("--- Model Summary ---\n")
print(summary(real_model))
cat("\n--- Model Performance ---\n")
cat("AUC:", round(real_auc, 4), "\n")
cat("95% CI:", round(ci.auc(real_roc)[1], 4), "-", round(ci.auc(real_roc)[3], 4), "\n")
cat("Brier score:", round(real_brier, 4), "\n\n")
cat("--- Hosmer-Lemeshow Test ---\n")
cat("Chi-square:", round(real_hl$statistic, 2), "\n")
cat("p-value:", round(real_hl$p.value, 4), "\n")
cat("Conclusion:", ifelse(real_hl$p.value > 0.05, "Good calibration", "Poor calibration"), "\n")
sink()
# Figure 9.10: Real data ROC curve
real_roc_data <- data.frame(
  Sensitivity = real_roc$sensitivities,
  FPR = 1 - real_roc$specificities
)
fig9_10 <- ggplot(real_roc_data, aes(x = FPR, y = Sensitivity)) +
  geom_line(colour = "black", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  labs(title = "ROC Curve: Disease Risk Model",
       subtitle = paste0("AUC = ", round(real_auc, 3),
                         " (95% CI: ", round(ci.auc(real_roc)[1], 3),
                         "\u2013", round(ci.auc(real_roc)[3], 3), ")"),
       x = "1 \u2212 Specificity", y = "Sensitivity") +
  coord_equal()
ggsave("figures/fig9_10_real_data_roc.jpeg", fig9_10, width = 6, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_10_real_data_roc.jpeg\n")
# Figure 9.11: Real data calibration
fig9_11 <- ggplot(real_cal_data, aes(x = predicted_prob, y = observed_prop)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "Calibration Plot: Disease Risk Model",
       subtitle = paste0("Brier score = ", round(real_brier, 4),
                         ", H-L p = ", round(real_hl$p.value, 3)),
       x = "Mean Predicted Probability", y = "Observed Proportion",
       size = "n") +
  coord_equal(xlim = c(0, 0.8), ylim = c(0, 0.8))
ggsave("figures/fig9_11_real_data_calibration.jpeg", fig9_11, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig9_11_real_data_calibration.jpeg\n")
# ============================================================================
# Section 9.12: Summary
# ============================================================================
sink("output/chapter9_summary.txt")
cat("=== Chapter 9 Summary: Model Diagnostics and Goodness-of-Fit ===\n\n")
cat("SIMULATED DATA RESULTS (Well-Specified Model):\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Sample size:", n, "\n")
cat("Event rate:", round(mean(diag_data$y), 3), "\n\n")
cat("Goodness-of-Fit Tests:\n")
cat("  Hosmer-Lemeshow: Chi-sq =", round(hl_test$statistic, 2),
    ", p =", round(hl_test$p.value, 4), "\n")
cat("  Conclusion:", ifelse(hl_test$p.value > 0.05, "Good fit", "Poor fit"), "\n\n")
cat("Discrimination:\n")
cat("  AUC:", round(auc_val, 4), "(95% CI:", round(ci.auc(roc_obj)[1], 4),
    "-", round(ci.auc(roc_obj)[3], 4), ")\n\n")
cat("Calibration:\n")
cat("  Brier score:", round(brier_score, 4), "\n")
cat("  Calibration slope:", round(cal_slope, 2), "\n\n")
cat("Classification (at optimal threshold", round(threshold, 3), "):\n")
cat("  Accuracy:", round(accuracy, 4), "\n")
cat("  Sensitivity:", round(sensitivity, 4), "\n")
cat("  Specificity:", round(specificity, 4), "\n")
cat("  F1 Score:", round(f1_score, 4), "\n\n")
cat("MISSPECIFICATION DETECTION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("AIC (misspecified):", round(AIC(model_miss), 2), "\n")
cat("AIC (correct):", round(AIC(model_correct), 2), "\n")
cat("Improvement:", round(AIC(model_miss) - AIC(model_correct), 2), "\n\n")
cat("CROSS-VALIDATION (", k, "-fold):\n", sep = "")
cat("-" , rep("-", 50), "\n", sep = "")
for (i in 1:nrow(cv_summary)) {
  cat(" ", cv_summary$Model[i], ": AUC =", round(cv_summary$Mean_AUC[i], 4),
      "(SD:", round(cv_summary$SD_AUC[i], 4), ")\n")
}
cat("\nREAL DATA EXAMPLE:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Sample size:", n_real, "\n")
cat("AUC:", round(real_auc, 4), "\n")
cat("Brier score:", round(real_brier, 4), "\n")
cat("H-L test p-value:", round(real_hl$p.value, 4), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 9 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig9_1_residual_plots.jpeg\n")
cat("  - fig9_2_misspecification.jpeg\n")
cat("  - fig9_3_influence_diagnostics.jpeg\n")
cat("  - fig9_4_leverage_residual_bubble.jpeg\n")
cat("  - fig9_5_hosmer_lemeshow.jpeg\n")
cat("  - fig9_6_roc_curve.jpeg\n")
cat("  - fig9_7_calibration_plot.jpeg\n")
cat("  - fig9_8_model_comparison.jpeg\n")
cat("  - fig9_9_cv_auc.jpeg\n")
cat("  - fig9_10_real_data_roc.jpeg\n")
cat("  - fig9_11_real_data_calibration.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - data_summary.txt\n")
cat("  - residual_summary.txt\n")
cat("  - misspecification_analysis.txt\n")
cat("  - influence_diagnostics.txt\n")
cat("  - goodness_of_fit.txt\n")
cat("  - classification_metrics.txt\n")
cat("  - calibration_metrics.txt\n")
cat("  - model_comparison.txt\n")
cat("  - cv_results.txt\n")
cat("  - real_data_analysis.txt\n")
cat("  - chapter9_summary.txt\n")