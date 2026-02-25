# ============================================================================
# Chapter 10: Model Validation and Prediction
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 10
# Required packages: tidyverse, pROC, ggplot2, gridExtra, boot
# ============================================================================
# Load required packages
library(tidyverse)
library(pROC)
library(ggplot2)
library(gridExtra)
library(boot)
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
cat("CHAPTER 10: MODEL VALIDATION AND PREDICTION\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 10.2: Demonstrating Overfitting
# ============================================================================
cat("Demonstrating overfitting with high-dimensional data...\n")
# Create high-dimensional dataset prone to overfitting
n <- 200  # Small sample
p <- 20   # Many predictors
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
# Only 3 true predictors
true_coef <- rep(0, p)
true_coef[c(1, 5, 10)] <- c(1.0, -0.7, 0.8)
linear_pred <- -0.5 + X %*% true_coef
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
overfit_data <- data.frame(y = y, X)
# Split into training and test
set.seed(123)
train_idx <- sample(1:n, round(0.7 * n))
train_data <- overfit_data[train_idx, ]
test_data <- overfit_data[-train_idx, ]
# Fit full model with all predictors
full_model <- glm(y ~ ., family = binomial, data = train_data)
# Evaluate on training vs test
train_pred <- predict(full_model, type = "response")
test_pred <- predict(full_model, newdata = test_data, type = "response")
train_auc_full <- as.numeric(auc(roc(train_data$y, train_pred, quiet = TRUE)))
test_auc_full <- as.numeric(auc(roc(test_data$y, test_pred, quiet = TRUE)))
# Fit true model (only true predictors)
true_model <- glm(y ~ x1 + x5 + x10, family = binomial, data = train_data)
train_pred_true <- predict(true_model, type = "response")
test_pred_true <- predict(true_model, newdata = test_data, type = "response")
train_auc_true <- as.numeric(auc(roc(train_data$y, train_pred_true, quiet = TRUE)))
test_auc_true <- as.numeric(auc(roc(test_data$y, test_pred_true, quiet = TRUE)))
sink("output/overfitting_demo.txt")
cat("=== Overfitting Demonstration ===\n\n")
cat("Dataset: n =", n, ", p =", p, "\n")
cat("True predictors: x1, x5, x10\n")
cat("Training set:", nrow(train_data), "observations\n")
cat("Test set:", nrow(test_data), "observations\n\n")
cat("--- Full Model (all 20 predictors) ---\n")
cat("Training AUC:", round(train_auc_full, 4), "\n")
cat("Test AUC:", round(test_auc_full, 4), "\n")
cat("Optimism (Train - Test):", round(train_auc_full - test_auc_full, 4), "\n\n")
cat("--- True Model (3 true predictors) ---\n")
cat("Training AUC:", round(train_auc_true, 4), "\n")
cat("Test AUC:", round(test_auc_true, 4), "\n")
cat("Optimism (Train - Test):", round(train_auc_true - test_auc_true, 4), "\n\n")
cat("Interpretation:\n")
cat("- Full model shows large optimism (overfitting)\n")
cat("- True model shows minimal optimism (good generalization)\n")
sink()
# Figure 10.1: ROC curves - Training vs Test
roc_train_full <- roc(train_data$y, train_pred, quiet = TRUE)
roc_test_full <- roc(test_data$y, test_pred, quiet = TRUE)
roc_data <- rbind(
  data.frame(FPR = 1 - roc_train_full$specificities,
             TPR = roc_train_full$sensitivities,
             Set = paste0("Training (AUC = ", round(train_auc_full, 3), ")")),
  data.frame(FPR = 1 - roc_test_full$specificities,
             TPR = roc_test_full$sensitivities,
             Set = paste0("Test (AUC = ", round(test_auc_full, 3), ")"))
)
fig10_1 <- ggplot(roc_data, aes(x = FPR, y = TPR, linetype = Set)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(title = "Overfitting: Training vs. Test ROC Curves",
       subtitle = "Full model with 20 predictors (only 3 are true)",
       x = "1 \u2212 Specificity", y = "Sensitivity", linetype = "") +
  coord_equal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_1_overfitting_roc.jpeg", fig10_1, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_1_overfitting_roc.jpeg\n")
# ============================================================================
# Section 10.3: Model Complexity vs Performance
# ============================================================================
cat("Analyzing model complexity vs performance...\n")
# Build models of increasing complexity
complexity_results <- data.frame(
  n_predictors = 1:p,
  train_auc = NA,
  test_auc = NA
)
# Rank predictors by univariate association
univariate_p <- numeric(p)
for (j in 1:p) {
  var_name <- paste0("x", j)
  formula_uni <- as.formula(paste("y ~", var_name))
  uni_model <- glm(formula_uni, family = binomial, data = train_data)
  univariate_p[j] <- summary(uni_model)$coefficients[2, 4]
}
predictor_order <- order(univariate_p)
for (i in 1:p) {
  selected_vars <- paste0("x", predictor_order[1:i])
  formula_i <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
  model_i <- glm(formula_i, family = binomial, data = train_data)
  train_pred_i <- predict(model_i, type = "response")
  test_pred_i <- predict(model_i, newdata = test_data, type = "response")
  complexity_results$train_auc[i] <- as.numeric(auc(roc(train_data$y, train_pred_i, quiet = TRUE)))
  complexity_results$test_auc[i] <- as.numeric(auc(roc(test_data$y, test_pred_i, quiet = TRUE)))
}
complexity_results$optimism <- complexity_results$train_auc - complexity_results$test_auc
sink("output/complexity_analysis.txt")
cat("=== Model Complexity Analysis ===\n\n")
cat("Predictors added in order of univariate significance\n\n")
print(complexity_results)
cat("\nOptimal complexity (max test AUC):",
    complexity_results$n_predictors[which.max(complexity_results$test_auc)], "predictors\n")
cat("Test AUC at optimal:", round(max(complexity_results$test_auc), 4), "\n")
sink()
# Figure 10.2: Complexity vs AUC
complexity_long <- complexity_results %>%
  select(n_predictors, train_auc, test_auc) %>%
  pivot_longer(cols = c(train_auc, test_auc), names_to = "Set", values_to = "AUC") %>%
  mutate(Set = ifelse(Set == "train_auc", "Training", "Test"))
fig10_2 <- ggplot(complexity_long, aes(x = n_predictors, y = AUC, linetype = Set)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_point(size = 2, aes(shape = Set), colour = "black") +
  geom_vline(xintercept = 3, linetype = "dashed", colour = "grey50") +
  scale_linetype_manual(values = c("Training" = "solid", "Test" = "dashed")) +
  scale_shape_manual(values = c("Training" = 16, "Test" = 1)) +
  annotate("text", x = 3.5, y = 0.55, label = "True model\n(3 predictors)",
           hjust = 0, size = 3, colour = "grey40") +
  labs(title = "Model Complexity vs. Performance",
       subtitle = "Training AUC increases with complexity; test AUC peaks then declines",
       x = "Number of Predictors", y = "AUC", linetype = "", shape = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_2_complexity_vs_auc.jpeg", fig10_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_2_complexity_vs_auc.jpeg\n")
# Figure 10.3: Optimism vs complexity
fig10_3 <- ggplot(complexity_results, aes(x = n_predictors, y = optimism)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_point(size = 2, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Optimism vs. Model Complexity",
       subtitle = "Optimism (overfitting) increases with model complexity",
       x = "Number of Predictors", y = "Optimism (Training AUC \u2212 Test AUC)")
ggsave("figures/fig10_3_optimism_vs_complexity.jpeg", fig10_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_3_optimism_vs_complexity.jpeg\n")
# ============================================================================
# Section 10.4: Cross-Validation
# ============================================================================
cat("Performing cross-validation...\n")
# Use the full dataset with true model
set.seed(456)
k <- 10
n_repeats <- 5
cv_results <- data.frame(
  Repeat = integer(),
  Fold = integer(),
  Train_AUC = numeric(),
  Test_AUC = numeric()
)
for (rep in 1:n_repeats) {
  folds <- sample(rep(1:k, length.out = n))
  for (fold in 1:k) {
    train_cv <- overfit_data[folds != fold, ]
    test_cv <- overfit_data[folds == fold, ]
    model_cv <- glm(y ~ x1 + x5 + x10, family = binomial, data = train_cv)
    train_pred_cv <- predict(model_cv, type = "response")
    test_pred_cv <- predict(model_cv, newdata = test_cv, type = "response")
    cv_results <- rbind(cv_results, data.frame(
      Repeat = rep,
      Fold = fold,
      Train_AUC = as.numeric(auc(roc(train_cv$y, train_pred_cv, quiet = TRUE))),
      Test_AUC = as.numeric(auc(roc(test_cv$y, test_pred_cv, quiet = TRUE)))
    ))
  }
}
cv_results$Optimism <- cv_results$Train_AUC - cv_results$Test_AUC
cv_summary <- cv_results %>%
  summarise(
    Mean_Train_AUC = mean(Train_AUC),
    SD_Train_AUC = sd(Train_AUC),
    Mean_Test_AUC = mean(Test_AUC),
    SD_Test_AUC = sd(Test_AUC),
    Mean_Optimism = mean(Optimism),
    SD_Optimism = sd(Optimism)
  )
sink("output/cv_results.txt")
cat("=== Cross-Validation Results ===\n\n")
cat("Model: y ~ x1 + x5 + x10\n")
cat("k =", k, "folds,", n_repeats, "repeats\n\n")
cat("Summary:\n")
cat("  Mean Training AUC:", round(cv_summary$Mean_Train_AUC, 4),
    "(SD:", round(cv_summary$SD_Train_AUC, 4), ")\n")
cat("  Mean Test AUC:", round(cv_summary$Mean_Test_AUC, 4),
    "(SD:", round(cv_summary$SD_Test_AUC, 4), ")\n")
cat("  Mean Optimism:", round(cv_summary$Mean_Optimism, 4),
    "(SD:", round(cv_summary$SD_Optimism, 4), ")\n\n")
cat("Cross-validated AUC estimate:", round(cv_summary$Mean_Test_AUC, 4), "\n")
sink()
# Figure 10.4: CV results by fold
fig10_4 <- ggplot(cv_results, aes(x = factor(Fold), y = Test_AUC)) +
  geom_boxplot(fill = "grey70", colour = "black", alpha = 0.7) +
  geom_hline(yintercept = cv_summary$Mean_Test_AUC, linetype = "dashed", colour = "black") +
  labs(title = paste0(k, "-Fold Cross-Validation Results"),
       subtitle = paste0("Mean CV-AUC = ", round(cv_summary$Mean_Test_AUC, 3),
                         " (SD = ", round(cv_summary$SD_Test_AUC, 3), ")"),
       x = "Fold", y = "Test AUC")
ggsave("figures/fig10_4_cv_results.jpeg", fig10_4, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_4_cv_results.jpeg\n")
# ============================================================================
# Section 10.5: Bootstrap Validation
# ============================================================================
cat("Performing bootstrap validation...\n")
set.seed(789)
B <- 200
# Fit original model
orig_model <- glm(y ~ x1 + x5 + x10, family = binomial, data = overfit_data)
orig_pred <- predict(orig_model, type = "response")
orig_auc <- as.numeric(auc(roc(overfit_data$y, orig_pred, quiet = TRUE)))
boot_results <- data.frame(
  Boot = 1:B,
  Boot_AUC = NA,
  Test_AUC = NA,
  Optimism = NA
)
for (b in 1:B) {
  boot_idx <- sample(1:n, n, replace = TRUE)
  boot_data <- overfit_data[boot_idx, ]
  boot_model <- glm(y ~ x1 + x5 + x10, family = binomial, data = boot_data)
  # Performance on bootstrap sample
  boot_pred <- predict(boot_model, type = "response")
  boot_results$Boot_AUC[b] <- as.numeric(auc(roc(boot_data$y, boot_pred, quiet = TRUE)))
  # Performance on original data
  test_pred <- predict(boot_model, newdata = overfit_data, type = "response")
  boot_results$Test_AUC[b] <- as.numeric(auc(roc(overfit_data$y, test_pred, quiet = TRUE)))
  boot_results$Optimism[b] <- boot_results$Boot_AUC[b] - boot_results$Test_AUC[b]
}
mean_optimism <- mean(boot_results$Optimism)
corrected_auc <- orig_auc - mean_optimism
boot_summary <- data.frame(
  Metric = c("Original AUC", "Mean Bootstrap AUC", "Mean Test AUC",
             "Mean Optimism", "Optimism-Corrected AUC"),
  Value = c(orig_auc, mean(boot_results$Boot_AUC), mean(boot_results$Test_AUC),
            mean_optimism, corrected_auc)
)
sink("output/bootstrap_validation.txt")
cat("=== Bootstrap Validation Results ===\n\n")
cat("Model: y ~ x1 + x5 + x10\n")
cat("Number of bootstrap samples:", B, "\n\n")
cat("Results:\n")
for (i in 1:nrow(boot_summary)) {
  cat("  ", boot_summary$Metric[i], ":", round(boot_summary$Value[i], 4), "\n")
}
cat("\nInterpretation:\n")
cat("- Original (apparent) AUC:", round(orig_auc, 4), "\n")
cat("- Estimated optimism:", round(mean_optimism, 4), "\n")
cat("- Optimism-corrected AUC:", round(corrected_auc, 4), "\n")
sink()
# Figure 10.5: Bootstrap optimism distribution
fig10_5 <- ggplot(boot_results, aes(x = Optimism)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = mean_optimism, colour = "black", linewidth = 1, linetype = "dashed") +
  labs(title = "Bootstrap Distribution of Optimism",
       subtitle = paste0("Mean optimism = ", round(mean_optimism, 4),
                         " (B = ", B, " bootstrap samples)"),
       x = "Optimism (Bootstrap AUC \u2212 Test AUC)", y = "Frequency") +
  annotate("text", x = mean_optimism + 0.01, y = 25,
           label = paste("Mean =", round(mean_optimism, 4)),
           hjust = 0, colour = "black")
ggsave("figures/fig10_5_bootstrap_optimism.jpeg", fig10_5, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_5_bootstrap_optimism.jpeg\n")
# ============================================================================
# Section 10.6: Well-Specified Model for Performance Metrics
# ============================================================================
cat("Creating well-specified model for performance metrics...\n")
set.seed(444)
n_perf <- 1000
x1_perf <- rnorm(n_perf)
x2_perf <- rnorm(n_perf)
x3_perf <- rnorm(n_perf)
linear_pred_perf <- -0.5 + 1.0 * x1_perf - 0.7 * x2_perf + 0.8 * x3_perf
prob_perf <- plogis(linear_pred_perf)
y_perf <- rbinom(n_perf, 1, prob_perf)
perf_data <- data.frame(y = y_perf, x1 = x1_perf, x2 = x2_perf, x3 = x3_perf)
perf_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = perf_data)
perf_pred <- predict(perf_model, type = "response")
# ============================================================================
# Section 10.7: Discrimination Metrics
# ============================================================================
cat("Computing discrimination metrics...\n")
roc_perf <- roc(perf_data$y, perf_pred, quiet = TRUE)
auc_perf <- as.numeric(auc(roc_perf))
ci_auc_perf <- ci.auc(roc_perf)
# Optimal threshold
coords_best <- coords(roc_perf, "best", best.method = "youden")
threshold_opt <- coords_best$threshold
# Classification at optimal threshold
pred_class <- ifelse(perf_pred >= threshold_opt, 1, 0)
conf_mat <- table(Actual = perf_data$y, Predicted = pred_class)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
sensitivity <- conf_mat[2, 2] / sum(conf_mat[2, ])
specificity <- conf_mat[1, 1] / sum(conf_mat[1, ])
ppv <- conf_mat[2, 2] / sum(conf_mat[, 2])
npv <- conf_mat[1, 1] / sum(conf_mat[, 1])
f1_score <- 2 * ppv * sensitivity / (ppv + sensitivity)
# Discrimination slope
disc_slope <- mean(perf_pred[perf_data$y == 1]) - mean(perf_pred[perf_data$y == 0])
sink("output/discrimination_metrics.txt")
cat("=== Discrimination Metrics ===\n\n")
cat("ROC Analysis:\n")
cat("  AUC:", round(auc_perf, 4), "\n")
cat("  95% CI:", round(ci_auc_perf[1], 4), "-", round(ci_auc_perf[3], 4), "\n")
cat("  Optimal threshold (Youden):", round(threshold_opt, 4), "\n\n")
cat("Confusion Matrix (at threshold =", round(threshold_opt, 3), "):\n")
print(conf_mat)
cat("\n")
cat("Classification Metrics:\n")
cat("  Accuracy:", round(accuracy, 4), "\n")
cat("  Sensitivity:", round(sensitivity, 4), "\n")
cat("  Specificity:", round(specificity, 4), "\n")
cat("  PPV (Precision):", round(ppv, 4), "\n")
cat("  NPV:", round(npv, 4), "\n")
cat("  F1 Score:", round(f1_score, 4), "\n\n")
cat("Discrimination Slope:", round(disc_slope, 4), "\n")
cat("  (Mean predicted prob: events =", round(mean(perf_pred[perf_data$y == 1]), 4),
    ", non-events =", round(mean(perf_pred[perf_data$y == 0]), 4), ")\n")
sink()
# Figure 10.6: ROC curve
roc_plot_data <- data.frame(
  FPR = 1 - roc_perf$specificities,
  TPR = roc_perf$sensitivities
)
fig10_6 <- ggplot(roc_plot_data, aes(x = FPR, y = TPR)) +
  geom_line(colour = "black", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(data = data.frame(FPR = 1 - coords_best$specificity, TPR = coords_best$sensitivity),
             aes(x = FPR, y = TPR), colour = "black", size = 3, shape = 17) +
  labs(title = "ROC Curve",
       subtitle = paste0("AUC = ", round(auc_perf, 3),
                         " (95% CI: ", round(ci_auc_perf[1], 3),
                         "\u2013", round(ci_auc_perf[3], 3), ")"),
       x = "1 \u2212 Specificity", y = "Sensitivity") +
  annotate("text", x = 0.6, y = 0.3,
           label = paste0("Optimal threshold = ", round(threshold_opt, 3)),
           colour = "black") +
  coord_equal()
ggsave("figures/fig10_6_roc_curve.jpeg", fig10_6, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_6_roc_curve.jpeg\n")
# Figure 10.7: Predicted probability distributions
perf_data$pred <- perf_pred
fig10_7 <- ggplot(perf_data, aes(x = pred, linetype = factor(y))) +
  geom_density(fill = NA, colour = "black", linewidth = 0.8) +
  geom_vline(xintercept = threshold_opt, linetype = "dashed", colour = "grey30") +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                        labels = c("Non-event (y=0)", "Event (y=1)")) +
  labs(title = "Distribution of Predicted Probabilities",
       subtitle = paste0("Discrimination slope = ", round(disc_slope, 3)),
       x = "Predicted Probability", y = "Density", linetype = "Outcome") +
  annotate("text", x = threshold_opt + 0.02, y = 2,
           label = paste0("Threshold = ", round(threshold_opt, 3)),
           hjust = 0, size = 3) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_7_pred_distributions.jpeg", fig10_7, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_7_pred_distributions.jpeg\n")
# ============================================================================
# Section 10.8: Calibration Metrics
# ============================================================================
cat("Computing calibration metrics...\n")
# Calibration by deciles
perf_data$pred_decile <- cut(perf_data$pred,
                             breaks = quantile(perf_data$pred, probs = seq(0, 1, 0.1)),
                             include.lowest = TRUE, labels = 1:10)
cal_data <- perf_data %>%
  group_by(pred_decile) %>%
  summarise(
    n = n(),
    observed = mean(y),
    predicted = mean(pred),
    .groups = "drop"
  )
# Calibration slope and intercept
perf_data$logit_pred <- qlogis(pmax(pmin(perf_data$pred, 0.999), 0.001))
cal_model <- glm(y ~ logit_pred, family = binomial, data = perf_data)
cal_intercept <- coef(cal_model)[1]
cal_slope <- coef(cal_model)[2]
# Brier score
brier_score <- mean((perf_data$pred - perf_data$y)^2)
brier_max <- mean(perf_data$y) * (1 - mean(perf_data$y))
scaled_brier <- 1 - brier_score / brier_max
# Hosmer-Lemeshow (manual calculation)
hl_stat <- sum((cal_data$n * (cal_data$observed - cal_data$predicted)^2) /
                 (cal_data$predicted * (1 - cal_data$predicted) + 0.001))
hl_pval <- 1 - pchisq(hl_stat, df = 8)
sink("output/calibration_metrics.txt")
cat("=== Calibration Metrics ===\n\n")
cat("Calibration Slope:", round(cal_slope, 4), "\n")
cat("  (Ideal = 1; <1 indicates overfitting)\n\n")
cat("Calibration Intercept:", round(cal_intercept, 4), "\n")
cat("  (Ideal = 0; >0 indicates underprediction)\n\n")
cat("Brier Score:", round(brier_score, 4), "\n")
cat("  (Lower is better; max =", round(brier_max, 4), "for this prevalence)\n\n")
cat("Scaled Brier Score:", round(scaled_brier, 4), "\n")
cat("  (0 = no information, 1 = perfect)\n\n")
cat("Hosmer-Lemeshow Test:\n")
cat("  Chi-square:", round(hl_stat, 2), "\n")
cat("  p-value:", round(hl_pval, 4), "\n")
cat("  Conclusion:", ifelse(hl_pval > 0.05, "Good calibration", "Poor calibration"), "\n\n")
cat("Calibration by Decile:\n")
print(as.data.frame(cal_data))
sink()
# Figure 10.8: Calibration plot
fig10_8 <- ggplot(cal_data, aes(x = predicted, y = observed)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  labs(title = "Calibration Plot",
       subtitle = paste0("Calibration slope = ", round(cal_slope, 3),
                         ", Brier score = ", round(brier_score, 4)),
       x = "Mean Predicted Probability", y = "Observed Proportion",
       size = "n") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1))
ggsave("figures/fig10_8_calibration_plot.jpeg", fig10_8, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_8_calibration_plot.jpeg\n")
# ============================================================================
# Section 10.9: Threshold Analysis
# ============================================================================
cat("Analyzing classification thresholds...\n")
thresholds <- seq(0.1, 0.9, by = 0.05)
thresh_results <- data.frame(
  Threshold = thresholds,
  Sensitivity = NA,
  Specificity = NA,
  PPV = NA,
  NPV = NA,
  Accuracy = NA,
  F1 = NA,
  Youden = NA
)
for (i in seq_along(thresholds)) {
  th <- thresholds[i]
  pred_th <- ifelse(perf_pred >= th, 1, 0)
  cm <- table(Actual = perf_data$y, Predicted = factor(pred_th, levels = c(0, 1)))
  if (ncol(cm) == 2 && nrow(cm) == 2) {
    thresh_results$Sensitivity[i] <- cm[2, 2] / sum(cm[2, ])
    thresh_results$Specificity[i] <- cm[1, 1] / sum(cm[1, ])
    thresh_results$PPV[i] <- cm[2, 2] / sum(cm[, 2])
    thresh_results$NPV[i] <- cm[1, 1] / sum(cm[, 1])
    thresh_results$Accuracy[i] <- sum(diag(cm)) / sum(cm)
    thresh_results$F1[i] <- 2 * thresh_results$PPV[i] * thresh_results$Sensitivity[i] /
      (thresh_results$PPV[i] + thresh_results$Sensitivity[i])
    thresh_results$Youden[i] <- thresh_results$Sensitivity[i] + thresh_results$Specificity[i] - 1
  }
}
sink("output/threshold_analysis.txt")
cat("=== Threshold Analysis ===\n\n")
print(thresh_results, row.names = FALSE)
cat("\n\nOptimal Thresholds:\n")
cat("  Max Youden's J:", thresh_results$Threshold[which.max(thresh_results$Youden)],
    "(J =", round(max(thresh_results$Youden, na.rm = TRUE), 4), ")\n")
cat("  Max F1 Score:", thresh_results$Threshold[which.max(thresh_results$F1)],
    "(F1 =", round(max(thresh_results$F1, na.rm = TRUE), 4), ")\n")
cat("  Max Accuracy:", thresh_results$Threshold[which.max(thresh_results$Accuracy)],
    "(Acc =", round(max(thresh_results$Accuracy, na.rm = TRUE), 4), ")\n")
sink()
# Figure 10.9: Threshold vs metrics
thresh_long <- thresh_results %>%
  select(Threshold, Sensitivity, Specificity, PPV, NPV) %>%
  pivot_longer(-Threshold, names_to = "Metric", values_to = "Value")
fig10_9 <- ggplot(thresh_long, aes(x = Threshold, y = Value, linetype = Metric)) +
  geom_line(linewidth = 1, colour = "black") +
  scale_linetype_manual(values = c("Sensitivity" = "solid", "Specificity" = "dashed",
                                   "PPV" = "dotted", "NPV" = "dotdash")) +
  labs(title = "Classification Metrics by Threshold",
       subtitle = "Trade-offs between sensitivity, specificity, PPV, and NPV",
       x = "Classification Threshold", y = "Value") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_9_threshold_metrics.jpeg", fig10_9, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_9_threshold_metrics.jpeg\n")
# ============================================================================
# Section 10.10: Decision Curve Analysis
# ============================================================================
cat("Performing decision curve analysis...\n")
thresh_dca <- seq(0.01, 0.99, by = 0.01)
dca_results <- data.frame(
  Threshold = thresh_dca,
  NB_Model = NA,
  NB_All = NA,
  NB_None = 0
)
prevalence <- mean(perf_data$y)
for (i in seq_along(thresh_dca)) {
  th <- thresh_dca[i]
  tp <- sum((perf_pred >= th) & (perf_data$y == 1))
  fp <- sum((perf_pred >= th) & (perf_data$y == 0))
  dca_results$NB_Model[i] <- (tp / n_perf) - (fp / n_perf) * (th / (1 - th))
  dca_results$NB_All[i] <- prevalence - (1 - prevalence) * (th / (1 - th))
}
sink("output/decision_curve.txt")
cat("=== Decision Curve Analysis ===\n\n")
cat("Net benefit compares model-guided decisions to:\n")
cat("- Treat All: treat everyone regardless of model\n")
cat("- Treat None: treat no one\n\n")
cat("Model provides benefit over 'Treat All' when threshold >\n")
benefit_thresh <- min(dca_results$Threshold[dca_results$NB_Model > dca_results$NB_All], na.rm = TRUE)
cat("  ", round(benefit_thresh, 2), "\n")
sink()
# Figure 10.10: Decision curve
dca_long <- dca_results %>%
  pivot_longer(-Threshold, names_to = "Strategy", values_to = "Net_Benefit") %>%
  mutate(Strategy = case_match(Strategy,
                           "NB_Model" ~ "Model",
                           "NB_All" ~ "Treat All",
                           "NB_None" ~ "Treat None"))
fig10_10 <- ggplot(dca_long, aes(x = Threshold, y = Net_Benefit, linetype = Strategy)) +
  geom_line(linewidth = 1, colour = "black") +
  scale_linetype_manual(values = c("Model" = "solid", "Treat All" = "dashed",
                                   "Treat None" = "dotted")) +
  ylim(c(-0.1, max(dca_results$NB_Model, na.rm = TRUE) + 0.05)) +
  labs(title = "Decision Curve Analysis",
       subtitle = "Net benefit of model-guided decisions vs. default strategies",
       x = "Threshold Probability", y = "Net Benefit") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_10_decision_curve.jpeg", fig10_10, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_10_decision_curve.jpeg\n")
# ============================================================================
# Section 10.11: External Validation (Temporal)
# ============================================================================
cat("Simulating external (temporal) validation...\n")
set.seed(555)
n_dev <- 500
n_val <- 300
# Development data
x1_dev <- rnorm(n_dev)
x2_dev <- rnorm(n_dev)
x3_dev <- rnorm(n_dev)
lp_dev <- -0.5 + 1.0 * x1_dev - 0.7 * x2_dev + 0.8 * x3_dev
y_dev <- rbinom(n_dev, 1, plogis(lp_dev))
dev_data <- data.frame(y = y_dev, x1 = x1_dev, x2 = x2_dev, x3 = x3_dev)
# Validation data (slight population shift)
x1_val <- rnorm(n_val, mean = 0.2)  # Slight shift
x2_val <- rnorm(n_val, mean = 0.1)
x3_val <- rnorm(n_val)
lp_val <- -0.7 + 0.9 * x1_val - 0.6 * x2_val + 0.8 * x3_val  # Slight coefficient drift
y_val <- rbinom(n_val, 1, plogis(lp_val))
val_data_ext <- data.frame(y = y_val, x1 = x1_val, x2 = x2_val, x3 = x3_val)
# Fit on development data
ext_model <- glm(y ~ x1 + x2 + x3, family = binomial, data = dev_data)
# Evaluate on both datasets
dev_pred_ext <- predict(ext_model, type = "response")
val_pred_ext <- predict(ext_model, newdata = val_data_ext, type = "response")
dev_auc_ext <- as.numeric(auc(roc(dev_data$y, dev_pred_ext, quiet = TRUE)))
val_auc_ext <- as.numeric(auc(roc(val_data_ext$y, val_pred_ext, quiet = TRUE)))
# Calibration in validation
val_data_ext$pred <- val_pred_ext
val_data_ext$logit_pred <- qlogis(pmax(pmin(val_pred_ext, 0.999), 0.001))
cal_model_ext <- glm(y ~ logit_pred, family = binomial, data = val_data_ext)
cal_slope_ext <- coef(cal_model_ext)[2]
cal_int_ext <- coef(cal_model_ext)[1]
brier_dev <- mean((dev_pred_ext - dev_data$y)^2)
brier_val <- mean((val_pred_ext - val_data_ext$y)^2)
sink("output/external_validation.txt")
cat("=== External (Temporal) Validation ===\n\n")
cat("Development Set: n =", n_dev, "\n")
cat("Validation Set: n =", n_val, "(with population shift)\n\n")
cat("Discrimination:\n")
cat("  Development AUC:", round(dev_auc_ext, 4), "\n")
cat("  Validation AUC:", round(val_auc_ext, 4), "\n")
cat("  AUC drop:", round(dev_auc_ext - val_auc_ext, 4), "\n\n")
cat("Calibration (Validation Set):\n")
cat("  Calibration slope:", round(cal_slope_ext, 4), "\n")
cat("  Calibration intercept:", round(cal_int_ext, 4), "\n\n")
cat("Brier Score:\n")
cat("  Development:", round(brier_dev, 4), "\n")
cat("  Validation:", round(brier_val, 4), "\n\n")
cat("Interpretation:\n")
if (abs(dev_auc_ext - val_auc_ext) < 0.05) {
  cat("  Good transportability: AUC drop < 0.05\n")
} else {
  cat("  Moderate transportability loss: AUC drop >= 0.05\n")
}
sink()
# Figure 10.11: External validation ROC
roc_dev_ext <- roc(dev_data$y, dev_pred_ext, quiet = TRUE)
roc_val_ext <- roc(val_data_ext$y, val_pred_ext, quiet = TRUE)
roc_ext_data <- rbind(
  data.frame(FPR = 1 - roc_dev_ext$specificities, TPR = roc_dev_ext$sensitivities,
             Set = paste0("Development (AUC = ", round(dev_auc_ext, 3), ")")),
  data.frame(FPR = 1 - roc_val_ext$specificities, TPR = roc_val_ext$sensitivities,
             Set = paste0("Validation (AUC = ", round(val_auc_ext, 3), ")"))
)
fig10_11 <- ggplot(roc_ext_data, aes(x = FPR, y = TPR, linetype = Set)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(title = "External Validation: Development vs. Validation ROC",
       subtitle = "Temporal validation with population shift",
       x = "1 \u2212 Specificity", y = "Sensitivity") +
  coord_equal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig10_11_external_validation_roc.jpeg", fig10_11, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig10_11_external_validation_roc.jpeg\n")
# ============================================================================
# Section 10.12: Summary
# ============================================================================
sink("output/chapter10_summary.txt")
cat("=== Chapter 10 Summary: Model Validation and Prediction ===\n\n")
cat("OVERFITTING DEMONSTRATION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Full model (20 predictors, only 3 true):\n")
cat("  Training AUC:", round(train_auc_full, 4), "\n")
cat("  Test AUC:", round(test_auc_full, 4), "\n")
cat("  Optimism:", round(train_auc_full - test_auc_full, 4), "\n\n")
cat("CROSS-VALIDATION (", k, "-fold, ", n_repeats, " repeats):\n", sep = "")
cat("-" , rep("-", 50), "\n", sep = "")
cat("  Mean CV-AUC:", round(cv_summary$Mean_Test_AUC, 4), "\n")
cat("  SD:", round(cv_summary$SD_Test_AUC, 4), "\n\n")
cat("BOOTSTRAP VALIDATION (B =", B, "):\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("  Original AUC:", round(orig_auc, 4), "\n")
cat("  Mean Optimism:", round(mean_optimism, 4), "\n")
cat("  Corrected AUC:", round(corrected_auc, 4), "\n\n")
cat("DISCRIMINATION (n =", n_perf, "):\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("  AUC:", round(auc_perf, 4), "(95% CI:", round(ci_auc_perf[1], 4), "-",
    round(ci_auc_perf[3], 4), ")\n")
cat("  Sensitivity:", round(sensitivity, 4), "\n")
cat("  Specificity:", round(specificity, 4), "\n")
cat("  F1 Score:", round(f1_score, 4), "\n\n")
cat("CALIBRATION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("  Calibration slope:", round(cal_slope, 4), "\n")
cat("  Brier score:", round(brier_score, 4), "\n\n")
cat("EXTERNAL VALIDATION:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("  Development AUC:", round(dev_auc_ext, 4), "\n")
cat("  Validation AUC:", round(val_auc_ext, 4), "\n")
cat("  AUC drop:", round(dev_auc_ext - val_auc_ext, 4), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 10 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig10_1_overfitting_roc.jpeg\n")
cat("  - fig10_2_complexity_vs_auc.jpeg\n")
cat("  - fig10_3_optimism_vs_complexity.jpeg\n")
cat("  - fig10_4_cv_results.jpeg\n")
cat("  - fig10_5_bootstrap_optimism.jpeg\n")
cat("  - fig10_6_roc_curve.jpeg\n")
cat("  - fig10_7_pred_distributions.jpeg\n")
cat("  - fig10_8_calibration_plot.jpeg\n")
cat("  - fig10_9_threshold_metrics.jpeg\n")
cat("  - fig10_10_decision_curve.jpeg\n")
cat("  - fig10_11_external_validation_roc.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - overfitting_demo.txt\n")
cat("  - complexity_analysis.txt\n")
cat("  - cv_results.txt\n")
cat("  - bootstrap_validation.txt\n")
cat("  - discrimination_metrics.txt\n")
cat("  - calibration_metrics.txt\n")
cat("  - threshold_analysis.txt\n")
cat("  - decision_curve.txt\n")
cat("  - external_validation.txt\n")
cat("  - chapter10_summary.txt\n")
