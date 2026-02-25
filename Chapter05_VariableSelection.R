# ============================================================================
# Chapter 5: Variable Selection Methods
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 5
# Required packages: tidyverse, MASS, glmnet, pROC, ggplot2, gridExtra,
#                    scales, mlbench
# ============================================================================
# Load required packages
library(tidyverse)
library(MASS)
library(glmnet)
library(pROC)
library(ggplot2)
library(gridExtra)
library(scales)
# Set seed for reproducibility
set.seed(123)
# Create output directories if they don't exist
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("output")) dir.create("output")
# Helper function for performance metrics
calc_performance <- function(selected, true_vars, p = 20) {
  tp <- length(intersect(selected, true_vars))
  fp <- length(setdiff(selected, true_vars))
  fn <- length(setdiff(true_vars, selected))
  tn <- p - tp - fp - fn
  sensitivity <- tp / length(true_vars)
  specificity <- tn / (tn + fp)
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  f1 <- ifelse(precision + sensitivity > 0,
               2 * precision * sensitivity / (precision + sensitivity), 0)
  return(data.frame(
    True_Positives = tp,
    False_Positives = fp,
    False_Negatives = fn,
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    Precision = round(precision, 3),
    F1_Score = round(f1, 3)
  ))
}
# Set ggplot B&W theme for consistency with other chapters
theme_set(theme_bw(base_size = 11) +
            theme(
              panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              plot.subtitle = element_text(size = 10, color = "grey40"),
              legend.position = "right",
              strip.background = element_rect(fill = "grey95"),
              strip.text = element_text(face = "bold")
            ))
# ============================================================================
# Section 5.2: Simulated Data for Variable Selection
# ============================================================================
cat("\n=======================================================\n")
cat("CHAPTER 5: VARIABLE SELECTION METHODS\n")
cat("=======================================================\n\n")
# Create a simulated dataset with known true predictors
n <- 500
p <- 20  # Number of potential predictors
# Generate predictors (some correlated)
set.seed(123)
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
# Add some correlation between predictors
X[, 2] <- X[, 1] * 0.7 + rnorm(n) * 0.7  # x2 correlated with x1
X[, 6] <- X[, 5] * 0.6 + rnorm(n) * 0.8  # x6 correlated with x5
# True predictors: x1, x5, x10, x15
true_coef <- rep(0, p)
true_coef[c(1, 5, 10, 15)] <- c(1.5, -1.2, 0.8, -1.0)
true_vars <- paste0("x", c(1, 5, 10, 15))
# Create linear predictor and generate response
linear_pred <- -0.5 + X %*% true_coef
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
# Create data frame
sim_data <- data.frame(y = y, X)
# Print data summary
cat("=== Simulated Data Summary ===\n")
cat("Sample size:", n, "\n")
cat("Number of predictors:", p, "\n")
cat("True predictors:", paste(true_vars, collapse = ", "), "\n")
cat("True coefficients:", paste(true_coef[true_coef != 0], collapse = ", "), "\n")
cat("Event rate:", mean(y), "\n\n")
# ============================================================================
# Figure 5.1: Visualization of True vs Noise Predictors
# ============================================================================
# Create data for visualization
predictor_effects <- data.frame(
  Predictor = colnames(X),
  True_Coefficient = true_coef,
  Is_True = ifelse(true_coef != 0, "True Predictor", "Noise Variable")
)
# Calculate univariate associations
univariate_results <- data.frame(
  Variable = character(),
  Coefficient = numeric(),
  P_value = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:p) {
  var_name <- paste0("x", i)
  uni_model <- glm(y ~ sim_data[, var_name], family = binomial(link = "logit"))
  coef_val <- coef(uni_model)[2]
  p_val <- summary(uni_model)$coefficients[2, 4]
  pred <- predict(uni_model, type = "response")
  suppressMessages(auc_val <- as.numeric(auc(roc(y, pred, quiet = TRUE))))
  univariate_results <- rbind(univariate_results,
                              data.frame(Variable = var_name,
                                         Coefficient = coef_val,
                                         P_value = p_val,
                                         AUC = auc_val))
}
univariate_results$True_Predictor <- univariate_results$Variable %in% true_vars
univariate_results$Significant <- univariate_results$P_value < 0.05
# Save univariate results
sink("output/univariate_screening.txt")
cat("=== Univariate Screening Results ===\n\n")
univariate_results_sorted <- univariate_results[order(univariate_results$P_value), ]
print(univariate_results_sorted, row.names = FALSE)
cat("\n\nBonferroni-corrected threshold (alpha = 0.05):", 0.05/p, "\n")
sig_bonferroni <- univariate_results$Variable[univariate_results$P_value < 0.05/p]
cat("Significant predictors after Bonferroni:", paste(sig_bonferroni, collapse = ", "), "\n")
sink()
# Figure 5.1: Univariate P-values
fig5_1 <- ggplot(univariate_results, aes(x = reorder(Variable, -log10(P_value)),
                                         y = -log10(P_value),
                                         fill = True_Predictor)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey40",
             linewidth = 0.8) +
  geom_hline(yintercept = -log10(0.05/p), linetype = "dotted", colour = "black",
             linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "grey35"),
                    labels = c("Noise Variable", "True Predictor")) +
  annotate("text", x = p - 1, y = -log10(0.05) + 0.5, label = "p = 0.05",
           colour = "grey40", size = 3, hjust = 1) +
  annotate("text", x = p - 1, y = -log10(0.05/p) + 0.5, label = "Bonferroni",
           colour = "black", size = 3, hjust = 1) +
  labs(title = "Univariate Screening Results",
       subtitle = "True predictors show larger -log10(p-values)",
       x = "Predictor",
       y = expression(-log[10](p-value)),
       fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
ggsave("figures/fig5_1_univariate_screening.jpeg", fig5_1, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_1_univariate_screening.jpeg\n")
# ============================================================================
# Section 5.3: Traditional Variable Selection Methods
# ============================================================================
# Fit null and full models
null_model <- glm(y ~ 1, family = binomial(link = "logit"), data = sim_data)
full_model <- glm(y ~ ., family = binomial(link = "logit"), data = sim_data)
# Forward Selection
forward_model <- step(null_model,
                      scope = list(lower = formula(null_model),
                                   upper = formula(full_model)),
                      direction = "forward",
                      trace = 0)
selected_forward <- names(coef(forward_model))[-1]
# Backward Elimination
backward_model <- step(full_model,
                       direction = "backward",
                       trace = 0)
selected_backward <- names(coef(backward_model))[-1]
# Stepwise Selection
stepwise_model <- step(null_model,
                       scope = list(lower = formula(null_model),
                                    upper = formula(full_model)),
                       direction = "both",
                       trace = 0)
selected_stepwise <- names(coef(stepwise_model))[-1]
# Save stepwise results
sink("output/stepwise_results.txt")
cat("=== Forward Selection Results ===\n")
cat("Selected variables:", paste(selected_forward, collapse = ", "), "\n")
print(calc_performance(selected_forward, true_vars))
cat("\n=== Backward Elimination Results ===\n")
cat("Selected variables:", paste(selected_backward, collapse = ", "), "\n")
print(calc_performance(selected_backward, true_vars))
cat("\n=== Stepwise Selection Results ===\n")
cat("Selected variables:", paste(selected_stepwise, collapse = ", "), "\n")
print(calc_performance(selected_stepwise, true_vars))
cat("\n=== Stepwise Model Summary ===\n")
print(summary(stepwise_model))
sink()
# ============================================================================
# Section 5.4: Regularization Methods
# ============================================================================
# Prepare data for glmnet
X_matrix <- as.matrix(sim_data[, -1])
y_vector <- sim_data$y
# ----- Ridge Regression (alpha = 0) -----
set.seed(123)
ridge_cv <- cv.glmnet(X_matrix, y_vector, family = "binomial", alpha = 0)
lambda_ridge <- ridge_cv$lambda.min
ridge_model <- glmnet(X_matrix, y_vector, family = "binomial",
                      alpha = 0, lambda = lambda_ridge)
ridge_coef <- as.vector(coef(ridge_model))[-1]
# ----- Lasso Regression (alpha = 1) -----
set.seed(123)
lasso_cv <- cv.glmnet(X_matrix, y_vector, family = "binomial", alpha = 1)
lambda_lasso <- lasso_cv$lambda.min
lambda_lasso_1se <- lasso_cv$lambda.1se
lasso_model <- glmnet(X_matrix, y_vector, family = "binomial",
                      alpha = 1, lambda = lambda_lasso)
lasso_coef <- as.vector(coef(lasso_model))[-1]
selected_lasso <- colnames(X_matrix)[lasso_coef != 0]
# Figure 5.2: Lasso Cross-Validation
jpeg("figures/fig5_2_lasso_cv.jpeg", width = 8, height = 5, units = "in", res = 300)
par(mar = c(5, 4, 4, 2) + 0.1)
plot(lasso_cv)
title(main = "Lasso Cross-Validation", line = 2.5)
dev.off()
cat("Saved: figures/fig5_2_lasso_cv.jpeg\n")
# Figure 5.3: Lasso Coefficient Path
jpeg("figures/fig5_3_lasso_path.jpeg", width = 8, height = 5, units = "in", res = 300)
lasso_full <- glmnet(X_matrix, y_vector, family = "binomial", alpha = 1)
plot(lasso_full, xvar = "lambda", label = TRUE)
abline(v = log(lambda_lasso), lty = 2, col = "black")
abline(v = log(lambda_lasso_1se), lty = 3, col = "grey40")
legend("topright", legend = c("lambda.min", "lambda.1se"),
       col = c("black", "grey40"), lty = c(2, 3), cex = 0.8)
title(main = "Lasso Coefficient Paths", line = 2.5)
dev.off()
cat("Saved: figures/fig5_3_lasso_path.jpeg\n")
# ----- Elastic Net -----
# Find optimal alpha
alpha_grid <- seq(0, 1, by = 0.1)
cv_errors <- numeric(length(alpha_grid))
set.seed(123)
for (i in seq_along(alpha_grid)) {
  cv_model <- cv.glmnet(X_matrix, y_vector, family = "binomial",
                        alpha = alpha_grid[i], nfolds = 10)
  cv_errors[i] <- min(cv_model$cvm)
}
opt_alpha <- alpha_grid[which.min(cv_errors)]
# Fit elastic net with optimal alpha
set.seed(123)
enet_cv <- cv.glmnet(X_matrix, y_vector, family = "binomial", alpha = opt_alpha)
lambda_enet <- enet_cv$lambda.min
enet_model <- glmnet(X_matrix, y_vector, family = "binomial",
                     alpha = opt_alpha, lambda = lambda_enet)
enet_coef <- as.vector(coef(enet_model))[-1]
selected_enet <- colnames(X_matrix)[enet_coef != 0]
# Figure 5.4: Elastic Net Alpha Selection
fig5_4 <- ggplot(data.frame(Alpha = alpha_grid, CV_Error = cv_errors),
                 aes(x = Alpha, y = CV_Error)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_point(size = 3, colour = "black") +
  geom_vline(xintercept = opt_alpha, linetype = "dashed", colour = "grey40") +
  annotate("text", x = opt_alpha + 0.08, y = max(cv_errors),
           label = paste("Optimal \u03B1 =", opt_alpha), colour = "grey40", size = 4) +
  labs(title = "Elastic Net: Optimal Alpha Selection",
       subtitle = "Cross-validated binomial deviance across mixing parameters",
       x = expression(alpha ~ "(mixing parameter)"),
       y = "Cross-Validated Error") +
  annotate("text", x = 0.05, y = min(cv_errors) + 0.001,
           label = "Ridge", size = 3, hjust = 0) +
  annotate("text", x = 0.95, y = min(cv_errors) + 0.001,
           label = "Lasso", size = 3, hjust = 1)
ggsave("figures/fig5_4_elastic_net_alpha.jpeg", fig5_4, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_4_elastic_net_alpha.jpeg\n")
# Save regularization results
sink("output/regularization_results.txt")
cat("=== Ridge Regression Results ===\n")
cat("Optimal lambda:", lambda_ridge, "\n\n")
cat("=== Lasso Regression Results ===\n")
cat("Optimal lambda (min):", lambda_lasso, "\n")
cat("Optimal lambda (1se):", lambda_lasso_1se, "\n")
cat("Selected variables:", paste(selected_lasso, collapse = ", "), "\n")
print(calc_performance(selected_lasso, true_vars))
cat("\n=== Elastic Net Results ===\n")
cat("Optimal alpha:", opt_alpha, "\n")
cat("Optimal lambda:", lambda_enet, "\n")
cat("Selected variables:", paste(selected_enet, collapse = ", "), "\n")
print(calc_performance(selected_enet, true_vars))
sink()
# ============================================================================
# Figure 5.5: Coefficient Comparison Across Methods
# ============================================================================
# Create coefficient comparison data
coef_comparison <- data.frame(
  Variable = colnames(X_matrix),
  True = true_coef,
  Ridge = ridge_coef,
  Lasso = lasso_coef,
  Elastic_Net = enet_coef
)
# Reshape for plotting
coef_long <- coef_comparison %>%
  pivot_longer(cols = c(True, Ridge, Lasso, Elastic_Net),
               names_to = "Method",
               values_to = "Coefficient") %>%
  mutate(Method = factor(Method, levels = c("True", "Ridge", "Lasso", "Elastic_Net")))
# Focus on true predictors and some noise
vars_to_show <- c("x1", "x2", "x5", "x6", "x10", "x15", "x3", "x8")
coef_subset <- coef_long %>%
  filter(Variable %in% vars_to_show) %>%
  mutate(Variable = factor(Variable, levels = vars_to_show))
fig5_5 <- ggplot(coef_subset, aes(x = Variable, y = Coefficient, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("True" = "grey90", "Ridge" = "grey65",
                               "Lasso" = "grey40", "Elastic_Net" = "grey15"),
                    labels = c("True", "Ridge", "Lasso", "Elastic Net")) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "grey40") +
  labs(title = "Coefficient Estimates by Regularisation Method",
       subtitle = "x1, x5, x10, x15 are true predictors; x2 correlated with x1; x6 correlated with x5",
       x = "Predictor",
       y = "Coefficient Estimate",
       fill = "Method") +
  theme(legend.position = "top")
ggsave("figures/fig5_5_coefficient_comparison.jpeg", fig5_5, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_5_coefficient_comparison.jpeg\n")
# ============================================================================
# Section 5.5: Stability Selection
# ============================================================================
# Bootstrap-based stability selection (simplified version)
n_bootstrap <- 200
bootstrap_results <- matrix(0, nrow = n_bootstrap, ncol = p)
colnames(bootstrap_results) <- colnames(X_matrix)
set.seed(123)
for (i in 1:n_bootstrap) {
  # Bootstrap sample
  boot_indices <- sample(1:n, n, replace = TRUE)
  X_boot <- X_matrix[boot_indices, ]
  y_boot <- y_vector[boot_indices]
  # Fit lasso on bootstrap sample
  cv_boot <- cv.glmnet(X_boot, y_boot, family = "binomial", alpha = 1)
  boot_model <- glmnet(X_boot, y_boot, family = "binomial",
                       alpha = 1, lambda = cv_boot$lambda.min)
  # Record non-zero coefficients
  boot_coef <- coef(boot_model)[-1]
  bootstrap_results[i, ] <- as.numeric(boot_coef != 0)
  if (i %% 50 == 0) cat("Bootstrap iteration:", i, "\n")
}
# Calculate selection probabilities
selection_probs <- colMeans(bootstrap_results)
selection_df <- data.frame(
  Variable = colnames(X_matrix),
  Selection_Probability = selection_probs,
  True_Predictor = colnames(X_matrix) %in% true_vars
)
selection_df <- selection_df[order(selection_df$Selection_Probability, decreasing = TRUE), ]
# Select variables with probability > 0.6
selected_stability <- selection_df$Variable[selection_df$Selection_Probability > 0.6]
# Figure 5.6: Stability Selection
fig5_6 <- ggplot(selection_df, aes(x = reorder(Variable, Selection_Probability),
                                   y = Selection_Probability,
                                   fill = True_Predictor)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0.6, linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("text", x = 2, y = 0.65, label = "Selection threshold = 0.6",
           colour = "black", size = 3.5) +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "grey35"),
                    labels = c("Noise Variable", "True Predictor")) +
  coord_flip() +
  labs(title = "Bootstrap Stability Selection",
       subtitle = paste0("Selection probability across ", n_bootstrap, " bootstrap samples"),
       x = "Predictor",
       y = "Selection Probability",
       fill = "") +
  theme(legend.position = "top")
ggsave("figures/fig5_6_stability_selection.jpeg", fig5_6, width = 7, height = 7,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_6_stability_selection.jpeg\n")
# Save stability selection results
sink("output/stability_selection.txt")
cat("=== Stability Selection Results ===\n")
cat("Number of bootstrap samples:", n_bootstrap, "\n")
cat("Selection threshold: 0.6\n\n")
print(selection_df, row.names = FALSE)
cat("\nSelected variables:", paste(selected_stability, collapse = ", "), "\n")
print(calc_performance(selected_stability, true_vars))
sink()
# ============================================================================
# Section 5.6: Method Comparison
# ============================================================================
# Compile all selections
all_methods <- list(
  Forward = selected_forward,
  Backward = selected_backward,
  Stepwise = selected_stepwise,
  Lasso = selected_lasso,
  Elastic_Net = selected_enet,
  Stability = selected_stability
)
# Create selection matrix
selection_matrix <- matrix(0, nrow = p, ncol = length(all_methods))
rownames(selection_matrix) <- colnames(X_matrix)
colnames(selection_matrix) <- names(all_methods)
for (i in seq_along(all_methods)) {
  if (length(all_methods[[i]]) > 0) {
    selection_matrix[all_methods[[i]], i] <- 1
  }
}
# Calculate performance for each method
method_performance <- do.call(rbind, lapply(names(all_methods), function(m) {
  perf <- calc_performance(all_methods[[m]], true_vars)
  perf$Method <- m
  perf$Num_Selected <- length(all_methods[[m]])
  perf
}))
method_performance <- method_performance[, c("Method", "Num_Selected", "True_Positives",
                                             "False_Positives", "False_Negatives",
                                             "Sensitivity", "Precision")]
# Figure 5.7: Selection Heatmap
selection_df_heatmap <- as.data.frame(selection_matrix) %>%
  mutate(Variable = rownames(selection_matrix)) %>%
  pivot_longer(cols = -Variable, names_to = "Method", values_to = "Selected") %>%
  mutate(
    True_Predictor = Variable %in% true_vars,
    Variable = factor(Variable, levels = paste0("x", 1:p)),
    Method = factor(Method, levels = names(all_methods))
  )
fig5_7 <- ggplot(selection_df_heatmap, aes(x = Method, y = Variable, fill = factor(Selected))) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_point(data = filter(selection_df_heatmap, True_Predictor),
             aes(x = Method, y = Variable), shape = 4, size = 2, colour = "black", stroke = 1) +
  scale_fill_manual(values = c("0" = "grey90", "1" = "grey35"),
                    labels = c("Not Selected", "Selected")) +
  labs(title = "Variable Selection Across Methods",
       subtitle = "\u00D7 marks true predictors (x1, x5, x10, x15)",
       x = "Selection Method",
       y = "Predictor",
       fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        panel.grid = element_blank())
ggsave("figures/fig5_7_selection_heatmap.jpeg", fig5_7, width = 8, height = 8,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_7_selection_heatmap.jpeg\n")
# ============================================================================
# Section 5.7: Cross-Validation Performance Comparison
# ============================================================================
set.seed(123)
cv_folds <- 10
fold_indices <- sample(rep(1:cv_folds, length.out = n))
methods_cv <- c("Full", "Stepwise", "Lasso", "Elastic Net", "Ridge")
cv_auc_results <- matrix(0, nrow = cv_folds, ncol = length(methods_cv))
colnames(cv_auc_results) <- methods_cv
for (fold in 1:cv_folds) {
  train_idx <- which(fold_indices != fold)
  test_idx <- which(fold_indices == fold)
  X_train <- X_matrix[train_idx, ]
  y_train <- y_vector[train_idx]
  X_test <- X_matrix[test_idx, ]
  y_test <- y_vector[test_idx]
  train_data <- data.frame(y = y_train, X_train)
  test_data <- data.frame(y = y_test, X_test)
  # 1. Full model
  full <- glm(y ~ ., family = binomial, data = train_data)
  pred_full <- predict(full, newdata = test_data, type = "response")
  # 2. Stepwise
  null <- glm(y ~ 1, family = binomial, data = train_data)
  step_m <- step(null, scope = list(lower = formula(null), upper = formula(full)),
                 direction = "both", trace = 0)
  pred_step <- predict(step_m, newdata = test_data, type = "response")
  # 3. Lasso
  cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
  pred_lasso <- predict(cv_lasso, newx = X_test, s = "lambda.min", type = "response")
  # 4. Elastic Net
  cv_enet <- cv.glmnet(X_train, y_train, family = "binomial", alpha = opt_alpha)
  pred_enet <- predict(cv_enet, newx = X_test, s = "lambda.min", type = "response")
  # 5. Ridge
  cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)
  pred_ridge <- predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response")
  # Calculate AUC
  suppressMessages({
    cv_auc_results[fold, 1] <- as.numeric(auc(roc(y_test, pred_full, quiet = TRUE)))
    cv_auc_results[fold, 2] <- as.numeric(auc(roc(y_test, pred_step, quiet = TRUE)))
    cv_auc_results[fold, 3] <- as.numeric(auc(roc(y_test, as.vector(pred_lasso), quiet = TRUE)))
    cv_auc_results[fold, 4] <- as.numeric(auc(roc(y_test, as.vector(pred_enet), quiet = TRUE)))
    cv_auc_results[fold, 5] <- as.numeric(auc(roc(y_test, as.vector(pred_ridge), quiet = TRUE)))
  })
}
# Summary statistics
cv_summary <- data.frame(
  Method = methods_cv,
  Mean_AUC = colMeans(cv_auc_results),
  SD_AUC = apply(cv_auc_results, 2, sd),
  Min_AUC = apply(cv_auc_results, 2, min),
  Max_AUC = apply(cv_auc_results, 2, max)
)
cv_summary <- cv_summary[order(cv_summary$Mean_AUC, decreasing = TRUE), ]
# Figure 5.8: CV Performance Comparison
cv_long <- as.data.frame(cv_auc_results) %>%
  mutate(Fold = 1:cv_folds) %>%
  pivot_longer(cols = -Fold, names_to = "Method", values_to = "AUC") %>%
  mutate(Method = factor(Method, levels = cv_summary$Method))
fig5_8 <- ggplot(cv_long, aes(x = Method, y = AUC)) +
  geom_boxplot(fill = "grey70", colour = "black", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2, colour = "black") +
  labs(title = "Cross-Validated AUC by Selection Method",
       subtitle = paste0(cv_folds, "-fold cross-validation"),
       x = "Method",
       y = "AUC") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0.7, 0.95))
ggsave("figures/fig5_8_cv_performance.jpeg", fig5_8, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_8_cv_performance.jpeg\n")
# Save CV results
sink("output/cv_comparison.txt")
cat("=== Cross-Validation Performance Comparison ===\n")
cat("Number of folds:", cv_folds, "\n\n")
print(cv_summary, row.names = FALSE)
cat("\n\n=== Detailed CV Results ===\n")
print(round(cv_auc_results, 4))
sink()
# ============================================================================
# Section 5.8: Real Data Example - Pima Indians Diabetes
# ============================================================================
# Load Pima Indians Diabetes dataset
library(mlbench)
data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes
pima$diabetes <- ifelse(pima$diabetes == "pos", 1, 0)
# Split data
set.seed(123)
train_idx <- sample(1:nrow(pima), 0.7 * nrow(pima))
pima_train <- pima[train_idx, ]
pima_test <- pima[-train_idx, ]
# Prepare matrices
X_train_pima <- as.matrix(pima_train[, -9])
y_train_pima <- pima_train$diabetes
X_test_pima <- as.matrix(pima_test[, -9])
y_test_pima <- pima_test$diabetes
# Full model
full_pima <- glm(diabetes ~ ., family = binomial, data = pima_train)
# Stepwise
null_pima <- glm(diabetes ~ 1, family = binomial, data = pima_train)
step_pima <- step(null_pima,
                  scope = list(lower = formula(null_pima), upper = formula(full_pima)),
                  direction = "both", trace = 0)
# Lasso
set.seed(123)
cv_lasso_pima <- cv.glmnet(X_train_pima, y_train_pima, family = "binomial", alpha = 1)
lasso_pima <- glmnet(X_train_pima, y_train_pima, family = "binomial",
                     alpha = 1, lambda = cv_lasso_pima$lambda.min)
# Get predictions
pred_full <- predict(full_pima, newdata = pima_test, type = "response")
pred_step <- predict(step_pima, newdata = pima_test, type = "response")
pred_lasso <- predict(lasso_pima, newx = X_test_pima, type = "response")
# Calculate AUC
suppressMessages({
  auc_full <- as.numeric(auc(roc(y_test_pima, pred_full, quiet = TRUE)))
  auc_step <- as.numeric(auc(roc(y_test_pima, pred_step, quiet = TRUE)))
  auc_lasso <- as.numeric(auc(roc(y_test_pima, as.vector(pred_lasso), quiet = TRUE)))
})
# Figure 5.9: ROC Curves for Pima Data
suppressMessages({
  roc_full <- roc(y_test_pima, pred_full, quiet = TRUE)
  roc_step <- roc(y_test_pima, pred_step, quiet = TRUE)
  roc_lasso <- roc(y_test_pima, as.vector(pred_lasso), quiet = TRUE)
})
roc_df <- rbind(
  data.frame(Specificity = roc_full$specificities, Sensitivity = roc_full$sensitivities,
             Method = paste0("Full Model (AUC = ", round(auc_full, 3), ")")),
  data.frame(Specificity = roc_step$specificities, Sensitivity = roc_step$sensitivities,
             Method = paste0("Stepwise (AUC = ", round(auc_step, 3), ")")),
  data.frame(Specificity = roc_lasso$specificities, Sensitivity = roc_lasso$sensitivities,
             Method = paste0("Lasso (AUC = ", round(auc_lasso, 3), ")"))
)

# Create named linetype vector for ROC
roc_methods <- unique(roc_df$Method)
roc_linetypes <- setNames(c("solid", "dashed", "dotdash"), roc_methods)

fig5_9 <- ggplot(roc_df, aes(x = 1 - Specificity, y = Sensitivity, linetype = Method)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "grey50") +
  scale_linetype_manual(values = roc_linetypes) +
  labs(title = "ROC Curves: Pima Indians Diabetes Dataset",
       subtitle = "Comparison of variable selection methods on test data",
       x = "False Positive Rate (1 \u2212 Specificity)",
       y = "True Positive Rate (Sensitivity)",
       linetype = "") +
  theme(legend.position = c(0.7, 0.25),
        legend.background = element_rect(fill = "white", colour = "grey80"),
        legend.key.width = unit(1.5, "cm")) +
  coord_equal()
ggsave("figures/fig5_9_pima_roc.jpeg", fig5_9, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_9_pima_roc.jpeg\n")
# Coefficient comparison for Pima
pima_coef <- data.frame(
  Variable = names(coef(full_pima))[-1],
  Full = coef(full_pima)[-1],
  Stepwise = ifelse(names(coef(full_pima))[-1] %in% names(coef(step_pima))[-1],
                    coef(step_pima)[names(coef(full_pima))[-1]], NA),
  Lasso = as.vector(coef(lasso_pima))[-1]
)
# Figure 5.10: Coefficient Comparison for Pima Data
pima_coef_long <- pima_coef %>%
  pivot_longer(cols = c(Full, Stepwise, Lasso), names_to = "Method", values_to = "Coefficient") %>%
  filter(!is.na(Coefficient))
fig5_10 <- ggplot(pima_coef_long, aes(x = Variable, y = Coefficient, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Full" = "grey80", "Stepwise" = "grey50", "Lasso" = "grey20")) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "grey40") +
  labs(title = "Coefficient Estimates: Pima Indians Diabetes",
       subtitle = "Comparison across variable selection methods",
       x = "Predictor",
       y = "Coefficient Estimate",
       fill = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
ggsave("figures/fig5_10_pima_coefficients.jpeg", fig5_10, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig5_10_pima_coefficients.jpeg\n")
# Save Pima results
sink("output/pima_results.txt")
cat("=== Pima Indians Diabetes: Variable Selection Results ===\n\n")
cat("Dataset: n =", nrow(pima), "observations,", ncol(pima) - 1, "predictors\n")
cat("Training set:", nrow(pima_train), "observations\n")
cat("Test set:", nrow(pima_test), "observations\n")
cat("Event rate (diabetes):", round(mean(pima$diabetes), 3), "\n\n")
cat("=== Full Model ===\n")
print(summary(full_pima))
cat("\n=== Stepwise Model ===\n")
cat("Selected variables:", paste(names(coef(step_pima))[-1], collapse = ", "), "\n")
print(summary(step_pima))
cat("\n=== Lasso Model ===\n")
lasso_coef_pima <- coef(lasso_pima)
selected_lasso_pima <- rownames(lasso_coef_pima)[lasso_coef_pima[, 1] != 0][-1]
cat("Selected variables:", paste(selected_lasso_pima, collapse = ", "), "\n")
cat("Lambda (min):", cv_lasso_pima$lambda.min, "\n")
print(lasso_coef_pima)
cat("\n=== Test Set Performance ===\n")
cat("Full model AUC:", round(auc_full, 4), "\n")
cat("Stepwise AUC:", round(auc_step, 4), "\n")
cat("Lasso AUC:", round(auc_lasso, 4), "\n")
sink()
# ============================================================================
# Summary Table: Method Performance
# ============================================================================
sink("output/method_comparison_summary.txt")
cat("=== Variable Selection Method Comparison ===\n\n")
cat("Simulated Data: n =", n, ", p =", p, "\n")
cat("True predictors:", paste(true_vars, collapse = ", "), "\n\n")
cat("--- Selection Performance (Simulated Data) ---\n")
print(method_performance, row.names = FALSE)
cat("\n--- Cross-Validation AUC Summary ---\n")
print(cv_summary, row.names = FALSE)
sink()
# ============================================================================
# Print completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 5 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig5_1_univariate_screening.jpeg\n")
cat("  - fig5_2_lasso_cv.jpeg\n")
cat("  - fig5_3_lasso_path.jpeg\n")
cat("  - fig5_4_elastic_net_alpha.jpeg\n")
cat("  - fig5_5_coefficient_comparison.jpeg\n")
cat("  - fig5_6_stability_selection.jpeg\n")
cat("  - fig5_7_selection_heatmap.jpeg\n")
cat("  - fig5_8_cv_performance.jpeg\n")
cat("  - fig5_9_pima_roc.jpeg\n")
cat("  - fig5_10_pima_coefficients.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - univariate_screening.txt\n")
cat("  - stepwise_results.txt\n")
cat("  - regularization_results.txt\n")
cat("  - stability_selection.txt\n")
cat("  - cv_comparison.txt\n")
cat("  - pima_results.txt\n")
cat("  - method_comparison_summary.txt\n")
