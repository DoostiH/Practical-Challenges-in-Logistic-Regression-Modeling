# ============================================================================
# Chapter 6: Multicollinearity
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 6
# Required packages: tidyverse, car, glmnet, pROC, ggplot2, corrplot,
#                    ggcorrplot, gridExtra, MASS, reshape2
# ============================================================================
# Load required packages
library(tidyverse)
library(car)          # For VIF calculation
library(glmnet)       # For ridge regression
library(pROC)         # For ROC analysis
library(ggplot2)
library(corrplot)     # For correlation plots
library(ggcorrplot)   # For ggplot2-based correlation plots
library(gridExtra)
library(MASS)
library(reshape2)
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
cat("CHAPTER 6: MULTICOLLINEARITY\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 6.2: Simulated Data with Multicollinearity
# ============================================================================
cat("Creating simulated data with multicollinearity...\n")
n <- 500
# Generate predictors with known correlation structure
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- 0.8 * x1 + 0.2 * x2 + rnorm(n, 0, 0.5)  # x3 correlated with x1 and x2
x4 <- rnorm(n)
x5 <- 0.9 * x4 + rnorm(n, 0, 0.3)  # x5 highly correlated with x4
# True model: y depends on x1, x2, x4 (not x3 or x5)
linear_pred <- 0.5 + 0.7 * x1 - 0.5 * x2 + 0.3 * x4
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
# Create data frame
collin_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
# Save data summary
sink("output/data_summary.txt")
cat("=== Simulated Data Summary ===\n\n")
cat("Sample size:", n, "\n")
cat("Event rate:", round(mean(y), 3), "\n\n")
cat("True model: logit(p) = 0.5 + 0.7*x1 - 0.5*x2 + 0.3*x4\n")
cat("x3 = 0.8*x1 + 0.2*x2 + noise (correlated with x1, x2)\n")
cat("x5 = 0.9*x4 + noise (highly correlated with x4)\n\n")
cat("Data structure:\n")
str(collin_data)
sink()
# ============================================================================
# Figure 6.1: Correlation Matrix
# ============================================================================
cat("Creating correlation matrix figure...\n")
# Calculate correlation matrix (excluding y)
cor_matrix <- cor(collin_data[, -1])
# Save correlation matrix
sink("output/correlation_matrix.txt")
cat("=== Correlation Matrix of Predictors ===\n\n")
print(round(cor_matrix, 3))
sink()
# Figure 6.1: Correlation heatmap using ggcorrplot (greyscale)
fig6_1 <- ggcorrplot(cor_matrix,
                     type = "lower",
                     lab = TRUE,
                     lab_size = 4,
                     colors = c("grey70", "white", "grey20"),
                     title = "Correlation Matrix of Predictors",
                     ggtheme = theme_bw())
ggsave("figures/fig6_1_correlation_matrix.jpeg", fig6_1, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_1_correlation_matrix.jpeg\n")
# ============================================================================
# Section 6.3: VIF Analysis
# ============================================================================
cat("Computing VIF values...\n")
# Fit logistic regression model with all predictors
full_model <- glm(y ~ x1 + x2 + x3 + x4 + x5,
                  family = binomial(link = "logit"),
                  data = collin_data)
# Calculate VIFs
vif_values <- vif(full_model)
# Save VIF results
sink("output/vif_analysis.txt")
cat("=== Variance Inflation Factor Analysis ===\n\n")
cat("Full Model Summary:\n")
print(summary(full_model))
cat("\n\nVIF Values:\n")
print(round(vif_values, 3))
cat("\n\nInterpretation guidelines:\n")
cat("VIF > 5: Moderate multicollinearity concern\n")
cat("VIF > 10: Serious multicollinearity concern\n\n")
cat("Variables with VIF > 5:", paste(names(vif_values)[vif_values > 5], collapse = ", "), "\n")
cat("Variables with VIF > 10:", paste(names(vif_values)[vif_values > 10], collapse = ", "), "\n")
sink()
# Figure 6.2: VIF Bar Plot
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
)
vif_df$Concern <- ifelse(vif_df$VIF > 10, "Serious",
                         ifelse(vif_df$VIF > 5, "Moderate", "Low"))
fig6_2 <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = Concern)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_hline(yintercept = 10, linetype = "dotted", colour = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("Low" = "grey75", "Moderate" = "grey50", "Serious" = "grey25")) +
  coord_flip() +
  annotate("text", x = 0.7, y = 5.5, label = "Moderate concern",
           colour = "grey40", size = 3, hjust = 0) +
  annotate("text", x = 0.7, y = 10.5, label = "Serious concern",
           colour = "black", size = 3, hjust = 0) +
  labs(title = "Variance Inflation Factors",
       subtitle = "Assessing multicollinearity in logistic regression",
       x = "Predictor",
       y = "VIF",
       fill = "Concern Level") +
  theme(legend.position = "right")
ggsave("figures/fig6_2_vif_barplot.jpeg", fig6_2, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_2_vif_barplot.jpeg\n")
# ============================================================================
# Section 6.4: Effect of Multicollinearity on Estimates
# ============================================================================
cat("Simulating effect of correlation on coefficient estimates...\n")
# Function to simulate multicollinearity effects
simulate_multicollinearity <- function(n = 500, correlation = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # Create x1 and x2 with specified correlation
  x1 <- rnorm(n)
  x2 <- correlation * x1 + sqrt(1 - correlation^2) * rnorm(n)
  # Response depends equally on both predictors (true beta = 1 for both)
  linear_pred <- 0.5 + x1 + x2
  prob <- plogis(linear_pred)
  y <- rbinom(n, 1, prob)
  # Fit model
  model <- glm(y ~ x1 + x2, family = binomial(link = "logit"))
  return(list(
    coefficients = coef(model),
    std_errors = sqrt(diag(vcov(model))),
    actual_correlation = cor(x1, x2)
  ))
}
# Run simulation across different correlation levels
correlations <- seq(0, 0.95, by = 0.05)
set.seed(123)
results_corr <- lapply(correlations, function(corr) {
  simulate_multicollinearity(n = 500, correlation = corr, seed = 123 + round(corr * 100))
})
# Extract results
coef_x1 <- sapply(results_corr, function(x) x$coefficients["x1"])
coef_x2 <- sapply(results_corr, function(x) x$coefficients["x2"])
se_x1 <- sapply(results_corr, function(x) x$std_errors["x1"])
se_x2 <- sapply(results_corr, function(x) x$std_errors["x2"])
# Create summary data frame
correlation_effects <- data.frame(
  Correlation = correlations,
  Coef_x1 = coef_x1,
  Coef_x2 = coef_x2,
  SE_x1 = se_x1,
  SE_x2 = se_x2
)
# Save results
sink("output/correlation_effects.txt")
cat("=== Effect of Correlation on Coefficient Estimates ===\n\n")
cat("True coefficients: beta_x1 = 1, beta_x2 = 1\n\n")
print(round(correlation_effects, 4))
sink()
# Figure 6.3: Effect of correlation on standard errors
corr_long_se <- correlation_effects %>%
  select(Correlation, SE_x1, SE_x2) %>%
  pivot_longer(cols = c(SE_x1, SE_x2), names_to = "Variable", values_to = "SE") %>%
  mutate(Variable = ifelse(Variable == "SE_x1", "x1", "x2"))
fig6_3 <- ggplot(corr_long_se, aes(x = Correlation, y = SE, linetype = Variable,
                                   shape = Variable)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_point(size = 2, colour = "black") +
  scale_linetype_manual(values = c("x1" = "solid", "x2" = "dashed")) +
  scale_shape_manual(values = c("x1" = 16, "x2" = 17)) +
  labs(title = "Effect of Multicollinearity on Standard Errors",
       subtitle = "Standard errors inflate dramatically as correlation increases",
       x = "Correlation between x1 and x2",
       y = "Standard Error of Coefficient",
       linetype = "Predictor", shape = "Predictor") +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", colour = "grey80"))
ggsave("figures/fig6_3_se_vs_correlation.jpeg", fig6_3, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_3_se_vs_correlation.jpeg\n")
# ============================================================================
# Section 6.5: Coefficient Stability Simulation
# ============================================================================
cat("Assessing coefficient stability under multicollinearity...\n")
# Function to assess stability
assess_stability <- function(n = 500, correlation = 0.9, n_samples = 200) {
  coef_x1 <- numeric(n_samples)
  coef_x2 <- numeric(n_samples)
  for (i in 1:n_samples) {
    x1 <- rnorm(n)
    x2 <- correlation * x1 + sqrt(1 - correlation^2) * rnorm(n)
    linear_pred <- 0.5 + x1 + x2
    prob <- plogis(linear_pred)
    y <- rbinom(n, 1, prob)
    model <- glm(y ~ x1 + x2, family = binomial(link = "logit"))
    coef_x1[i] <- coef(model)["x1"]
    coef_x2[i] <- coef(model)["x2"]
  }
  return(data.frame(coef_x1 = coef_x1, coef_x2 = coef_x2))
}
set.seed(456)
stability_low <- assess_stability(correlation = 0.1, n_samples = 200)
stability_high <- assess_stability(correlation = 0.9, n_samples = 200)
# Calculate statistics
stability_stats <- data.frame(
  Correlation = c("Low (r=0.1)", "High (r=0.9)"),
  Mean_x1 = c(mean(stability_low$coef_x1), mean(stability_high$coef_x1)),
  Mean_x2 = c(mean(stability_low$coef_x2), mean(stability_high$coef_x2)),
  SD_x1 = c(sd(stability_low$coef_x1), sd(stability_high$coef_x1)),
  SD_x2 = c(sd(stability_low$coef_x2), sd(stability_high$coef_x2)),
  Corr_estimates = c(cor(stability_low$coef_x1, stability_low$coef_x2),
                     cor(stability_high$coef_x1, stability_high$coef_x2))
)
sink("output/stability_analysis.txt")
cat("=== Coefficient Stability Analysis ===\n\n")
cat("Number of simulations: 200\n")
cat("True coefficients: beta_x1 = 1, beta_x2 = 1\n\n")
# Round only numeric columns
stability_stats_print <- stability_stats
stability_stats_print[, -1] <- round(stability_stats_print[, -1], 4)
print(stability_stats_print)
cat("\n\nInterpretation:\n")
cat("- With high correlation, coefficient estimates are highly variable\n")
cat("- The correlation between estimates becomes strongly negative\n")
cat("- This means x1 and x2 coefficients 'trade off' against each other\n")
sink()
# Figure 6.4: Coefficient stability scatter plots
stability_low$Correlation <- "Low (r = 0.1)"
stability_high$Correlation <- "High (r = 0.9)"
stability_combined <- rbind(stability_low, stability_high)
fig6_4 <- ggplot(stability_combined, aes(x = coef_x1, y = coef_x2)) +
  geom_point(alpha = 0.5, colour = "grey40") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "black") +
  facet_wrap(~ Correlation, scales = "free") +
  labs(title = "Coefficient Stability Under Different Correlation Levels",
       subtitle = "Dashed lines show true coefficient values (\u03B2 = 1)",
       x = "Estimated Coefficient for x1",
       y = "Estimated Coefficient for x2") +
  theme(strip.text = element_text(size = 11, face = "bold"))
ggsave("figures/fig6_4_coefficient_stability.jpeg", fig6_4, width = 10, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_4_coefficient_stability.jpeg\n")
# ============================================================================
# Section 6.6: Solutions - Ridge Regression
# ============================================================================
cat("Fitting ridge regression model...\n")
# Prepare data for glmnet
X_matrix <- as.matrix(collin_data[, -1])
y_vector <- collin_data$y
# Ridge regression with cross-validation
set.seed(123)
ridge_cv <- cv.glmnet(X_matrix, y_vector, family = "binomial", alpha = 0)
lambda_ridge <- ridge_cv$lambda.min
lambda_ridge_1se <- ridge_cv$lambda.1se
# Fit ridge model
ridge_model <- glmnet(X_matrix, y_vector, family = "binomial",
                      alpha = 0, lambda = lambda_ridge)
# Figure 6.5: Ridge CV plot
jpeg("figures/fig6_5_ridge_cv.jpeg", width = 8, height = 5, units = "in", res = 300)
plot(ridge_cv, main = "Ridge Regression Cross-Validation")
dev.off()
cat("Saved: figures/fig6_5_ridge_cv.jpeg\n")
# Compare coefficients
coef_comparison <- data.frame(
  Variable = c("(Intercept)", names(vif_values)),
  Unpenalized = as.vector(coef(full_model)),
  Ridge = as.vector(coef(ridge_model))
)
sink("output/ridge_results.txt")
cat("=== Ridge Regression Results ===\n\n")
cat("Optimal lambda (min):", round(lambda_ridge, 5), "\n")
cat("Optimal lambda (1se):", round(lambda_ridge_1se, 5), "\n\n")
cat("Coefficient Comparison:\n")
# Round only numeric columns
coef_comp_print <- coef_comparison
coef_comp_print[, -1] <- round(coef_comp_print[, -1], 4)
print(coef_comp_print)
sink()
# Figure 6.6: Coefficient comparison
coef_comp_long <- coef_comparison %>%
  filter(Variable != "(Intercept)") %>%
  pivot_longer(cols = c(Unpenalized, Ridge), names_to = "Method", values_to = "Coefficient")
fig6_6 <- ggplot(coef_comp_long, aes(x = Variable, y = Coefficient, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("Unpenalized" = "grey70", "Ridge" = "grey30")) +
  geom_hline(yintercept = 0, colour = "grey40") +
  labs(title = "Coefficient Estimates: Unpenalised vs Ridge Regression",
       subtitle = "Ridge shrinks coefficients toward zero, especially for collinear predictors",
       x = "Predictor",
       y = "Coefficient Estimate",
       fill = "Method") +
  theme(legend.position = "top")
ggsave("figures/fig6_6_ridge_comparison.jpeg", fig6_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_6_ridge_comparison.jpeg\n")
# Figure 6.7: Ridge coefficient path
jpeg("figures/fig6_7_ridge_path.jpeg", width = 8, height = 5, units = "in", res = 300)
ridge_path <- glmnet(X_matrix, y_vector, family = "binomial", alpha = 0)
plot(ridge_path, xvar = "lambda", label = TRUE)
abline(v = log(lambda_ridge), lty = 2, col = "black")
title(main = "Ridge Coefficient Paths", line = 2.5)
legend("topright", legend = "lambda.min", col = "black", lty = 2, cex = 0.8)
dev.off()
cat("Saved: figures/fig6_7_ridge_path.jpeg\n")
# ============================================================================
# Section 6.7: Reduced Model (Variable Selection Approach)
# ============================================================================
cat("Fitting reduced model (removing collinear predictors)...\n")
# Remove x3 (collinear with x1) and x5 (collinear with x4)
reduced_model <- glm(y ~ x1 + x2 + x4,
                     family = binomial(link = "logit"),
                     data = collin_data)
# Compare models
sink("output/model_comparison.txt")
cat("=== Model Comparison: Full vs Reduced ===\n\n")
cat("Full Model (with collinear predictors):\n")
print(summary(full_model))
cat("\n\nReduced Model (collinear predictors removed):\n")
print(summary(reduced_model))
cat("\n\nLikelihood Ratio Test:\n")
print(anova(reduced_model, full_model, test = "Chisq"))
cat("\n\nAIC Comparison:\n")
cat("Full model AIC:", round(AIC(full_model), 2), "\n")
cat("Reduced model AIC:", round(AIC(reduced_model), 2), "\n")
cat("\nVIF for Reduced Model:\n")
print(round(vif(reduced_model), 3))
sink()
# ============================================================================
# Section 6.8: Centering for Structural Multicollinearity
# ============================================================================
cat("Demonstrating effect of centering on polynomial terms...\n")
# Create data with polynomial terms
set.seed(789)
n_quad <- 500
x_raw <- rnorm(n_quad, mean = 5, sd = 2)  # Non-centered predictor
x_centered <- scale(x_raw, scale = FALSE)[, 1]
# Generate response with quadratic effect
linear_pred_quad <- -2 + 0.3 * x_raw - 0.05 * x_raw^2
prob_quad <- plogis(linear_pred_quad)
y_quad <- rbinom(n_quad, 1, prob_quad)
quad_data <- data.frame(
  y = y_quad,
  x = x_raw,
  x_sq = x_raw^2,
  x_c = x_centered,
  x_c_sq = x_centered^2
)
# Fit models with and without centering
model_no_center <- glm(y ~ x + x_sq, family = binomial, data = quad_data)
model_centered <- glm(y ~ x_c + x_c_sq, family = binomial, data = quad_data)
# Calculate correlations and VIFs
cor_no_center <- cor(quad_data$x, quad_data$x_sq)
cor_centered <- cor(quad_data$x_c, quad_data$x_c_sq)
vif_no_center <- vif(model_no_center)
vif_centered <- vif(model_centered)
sink("output/centering_results.txt")
cat("=== Effect of Centering on Polynomial Terms ===\n\n")
cat("Correlation between x and x^2:\n")
cat("  Without centering:", round(cor_no_center, 4), "\n")
cat("  With centering:", round(cor_centered, 4), "\n\n")
cat("VIF Values:\n")
cat("Without centering:\n")
print(round(vif_no_center, 3))
cat("\nWith centering:\n")
print(round(vif_centered, 3))
cat("\n\nModel without centering:\n")
print(summary(model_no_center))
cat("\n\nModel with centering:\n")
print(summary(model_centered))
sink()
# Figure 6.8: Effect of centering
centering_comparison <- data.frame(
  Term = c("Linear", "Quadratic"),
  Without_Centering = as.numeric(vif_no_center),
  With_Centering = as.numeric(vif_centered)
)
centering_long <- centering_comparison %>%
  pivot_longer(cols = c(Without_Centering, With_Centering),
               names_to = "Method", values_to = "VIF") %>%
  mutate(Method = gsub("_", " ", Method))
fig6_8 <- ggplot(centering_long, aes(x = Term, y = VIF, fill = Method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 10, linetype = "dotted", colour = "black") +
  scale_fill_manual(values = c("Without Centering" = "grey70", "With Centering" = "grey30")) +
  labs(title = "Effect of Centering on VIF for Polynomial Terms",
       subtitle = "Centering eliminates structural multicollinearity",
       x = "Term",
       y = "Variance Inflation Factor",
       fill = "") +
  theme(legend.position = "top") +
  annotate("text", x = 2.3, y = 5.5, label = "Moderate", colour = "grey40", size = 3) +
  annotate("text", x = 2.3, y = 10.5, label = "Serious", colour = "black", size = 3)
ggsave("figures/fig6_8_centering_effect.jpeg", fig6_8, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_8_centering_effect.jpeg\n")
# ============================================================================
# Section 6.9: Real Data Example - South African Heart Disease
# ============================================================================
cat("Analyzing real data example...\n")
# Create a realistic heart disease dataset
set.seed(321)
n_heart <- 462
# Generate correlated predictors similar to heart disease data
age <- round(rnorm(n_heart, mean = 45, sd = 12))
age <- pmax(20, pmin(age, 80))
# sbp and tobacco are somewhat correlated
sbp <- round(100 + 0.5 * age + rnorm(n_heart, 0, 15))
tobacco <- pmax(0, 2 + 0.02 * age + rnorm(n_heart, 0, 3))
# ldl and adiposity are correlated
ldl <- pmax(1, 3 + 0.5 * rnorm(n_heart) + rnorm(n_heart, 0, 1))
adiposity <- pmax(10, 20 + 2 * ldl + rnorm(n_heart, 0, 5))
# obesity correlated with adiposity
obesity <- pmax(15, 0.8 * adiposity + rnorm(n_heart, 0, 3))
# family history (binary)
famhist <- rbinom(n_heart, 1, 0.4)
# alcohol
alcohol <- pmax(0, rgamma(n_heart, shape = 2, scale = 10))
# Generate outcome
linear_pred_heart <- -6 + 0.05 * age + 0.02 * sbp + 0.1 * tobacco +
  0.2 * ldl + 0.03 * adiposity + 0.5 * famhist - 0.01 * alcohol
prob_heart <- plogis(linear_pred_heart)
chd <- rbinom(n_heart, 1, prob_heart)
heart_data <- data.frame(
  chd = chd,
  age = age,
  sbp = sbp,
  tobacco = tobacco,
  ldl = ldl,
  adiposity = adiposity,
  obesity = obesity,
  famhist = factor(famhist, labels = c("Absent", "Present")),
  alcohol = alcohol
)
# Correlation matrix of numeric predictors
heart_numeric <- heart_data[, sapply(heart_data, is.numeric)]
heart_numeric <- heart_numeric[, -1]  # Remove chd
cor_heart <- cor(heart_numeric)
# Figure 6.9: Heart data correlation matrix (greyscale)
fig6_9 <- ggcorrplot(cor_heart,
                     type = "lower",
                     lab = TRUE,
                     lab_size = 3,
                     colors = c("grey70", "white", "grey20"),
                     title = "Correlation Matrix: Heart Disease Predictors")
ggsave("figures/fig6_9_heart_correlation.jpeg", fig6_9, width = 8, height = 7,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_9_heart_correlation.jpeg\n")
# Fit full model
heart_full <- glm(chd ~ age + sbp + tobacco + ldl + adiposity + obesity + famhist + alcohol,
                  family = binomial, data = heart_data)
# Calculate VIF
heart_vif <- vif(heart_full)
# Fit ridge model
X_heart <- model.matrix(chd ~ age + sbp + tobacco + ldl + adiposity + obesity + famhist + alcohol,
                        data = heart_data)[, -1]
y_heart <- heart_data$chd
set.seed(123)
ridge_cv_heart <- cv.glmnet(X_heart, y_heart, family = "binomial", alpha = 0)
ridge_heart <- glmnet(X_heart, y_heart, family = "binomial",
                      alpha = 0, lambda = ridge_cv_heart$lambda.min)
# Fit reduced model (remove obesity due to collinearity with adiposity)
heart_reduced <- glm(chd ~ age + sbp + tobacco + ldl + adiposity + famhist + alcohol,
                     family = binomial, data = heart_data)
# Save heart data results
sink("output/heart_data_results.txt")
cat("=== Heart Disease Data Analysis ===\n\n")
cat("Sample size:", n_heart, "\n")
cat("CHD prevalence:", round(mean(chd), 3), "\n\n")
cat("Correlation Matrix:\n")
print(round(cor_heart, 3))
cat("\n\n=== Full Model ===\n")
print(summary(heart_full))
cat("\n\nVIF Values:\n")
print(round(heart_vif, 3))
cat("\n\n=== Reduced Model (obesity removed) ===\n")
print(summary(heart_reduced))
cat("\n\nVIF for Reduced Model:\n")
print(round(vif(heart_reduced), 3))
cat("\n\nAIC Comparison:\n")
cat("Full model:", round(AIC(heart_full), 2), "\n")
cat("Reduced model:", round(AIC(heart_reduced), 2), "\n")
sink()
# Figure 6.10: VIF comparison for heart data
heart_vif_df <- data.frame(
  Variable = names(heart_vif),
  VIF = as.numeric(heart_vif)
)
heart_vif_df$Concern <- ifelse(heart_vif_df$VIF > 10, "Serious",
                               ifelse(heart_vif_df$VIF > 5, "Moderate", "Low"))
fig6_10 <- ggplot(heart_vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = Concern)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 5, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 10, linetype = "dotted", colour = "black") +
  scale_fill_manual(values = c("Low" = "grey75", "Moderate" = "grey50", "Serious" = "grey25")) +
  coord_flip() +
  labs(title = "VIF Values: Heart Disease Predictors",
       subtitle = "Adiposity and obesity show elevated VIF due to collinearity",
       x = "Predictor",
       y = "Variance Inflation Factor",
       fill = "Concern") +
  theme(legend.position = "right")
ggsave("figures/fig6_10_heart_vif.jpeg", fig6_10, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_10_heart_vif.jpeg\n")
# ============================================================================
# Section 6.10: Bootstrap Stability Comparison
# ============================================================================
cat("Running bootstrap stability analysis...\n")
# Split data for evaluation
set.seed(456)
train_idx <- sample(1:n_heart, 0.7 * n_heart)
test_idx <- setdiff(1:n_heart, train_idx)
X_train <- X_heart[train_idx, ]
y_train <- y_heart[train_idx]
X_test <- X_heart[test_idx, ]
y_test <- y_heart[test_idx]
train_data <- heart_data[train_idx, ]
test_data <- heart_data[test_idx, ]
# Fit models on training data
full_train <- glm(chd ~ age + sbp + tobacco + ldl + adiposity + obesity + famhist + alcohol,
                  family = binomial, data = train_data)
ridge_train <- glmnet(X_train, y_train, family = "binomial",
                      alpha = 0, lambda = ridge_cv_heart$lambda.min)
# Predictions
pred_full <- predict(full_train, newdata = test_data, type = "response")
pred_ridge <- predict(ridge_train, newx = X_test, type = "response")[, 1]
# ROC analysis
suppressMessages({
  roc_full <- roc(y_test, pred_full, quiet = TRUE)
  roc_ridge <- roc(y_test, pred_ridge, quiet = TRUE)
})
auc_full <- as.numeric(auc(roc_full))
auc_ridge <- as.numeric(auc(roc_ridge))
# Figure 6.11: ROC comparison
roc_df <- rbind(
  data.frame(Specificity = roc_full$specificities,
             Sensitivity = roc_full$sensitivities,
             Model = paste0("Full Model (AUC = ", round(auc_full, 3), ")")),
  data.frame(Specificity = roc_ridge$specificities,
             Sensitivity = roc_ridge$sensitivities,
             Model = paste0("Ridge (AUC = ", round(auc_ridge, 3), ")"))
)

roc_methods <- unique(roc_df$Model)
roc_linetypes <- setNames(c("solid", "dashed"), roc_methods)

fig6_11 <- ggplot(roc_df, aes(x = 1 - Specificity, y = Sensitivity, linetype = Model)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "grey50") +
  scale_linetype_manual(values = roc_linetypes) +
  labs(title = "ROC Curves: Heart Disease Prediction",
       subtitle = "Comparing full model vs ridge regression",
       x = "False Positive Rate (1 \u2212 Specificity)",
       y = "True Positive Rate (Sensitivity)",
       linetype = "") +
  theme(legend.position = c(0.7, 0.25),
        legend.background = element_rect(fill = "white", colour = "grey80"),
        legend.key.width = unit(1.5, "cm")) +
  coord_equal()
ggsave("figures/fig6_11_heart_roc.jpeg", fig6_11, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_11_heart_roc.jpeg\n")
# Bootstrap coefficient stability
n_boot <- 100
coef_names <- colnames(X_heart)
n_coef <- length(coef_names)
boot_full <- matrix(NA, n_boot, n_coef)
boot_ridge <- matrix(NA, n_boot, n_coef)
colnames(boot_full) <- coef_names
colnames(boot_ridge) <- coef_names
set.seed(789)
for (i in 1:n_boot) {
  boot_idx <- sample(1:n_heart, n_heart, replace = TRUE)
  X_boot <- X_heart[boot_idx, ]
  y_boot <- y_heart[boot_idx]
  # Full model
  full_boot <- glm(y_boot ~ X_boot, family = binomial)
  boot_full[i, ] <- coef(full_boot)[-1]
  # Ridge model
  ridge_boot <- glmnet(X_boot, y_boot, family = "binomial",
                       alpha = 0, lambda = ridge_cv_heart$lambda.min)
  boot_ridge[i, ] <- as.vector(coef(ridge_boot))[-1]
}
# Calculate stability metrics
stability_comparison <- data.frame(
  Variable = coef_names,
  SD_Full = apply(boot_full, 2, sd),
  SD_Ridge = apply(boot_ridge, 2, sd)
)
stability_comparison$Reduction_Pct <- round(
  (stability_comparison$SD_Full - stability_comparison$SD_Ridge) /
    stability_comparison$SD_Full * 100, 1)
stability_comparison <- stability_comparison[order(stability_comparison$Reduction_Pct, decreasing = TRUE), ]
sink("output/bootstrap_stability.txt")
cat("=== Bootstrap Coefficient Stability ===\n\n")
cat("Number of bootstrap samples:", n_boot, "\n\n")
cat("Test set performance:\n")
cat("Full model AUC:", round(auc_full, 4), "\n")
cat("Ridge model AUC:", round(auc_ridge, 4), "\n\n")
cat("Coefficient Stability (Standard Deviation across bootstraps):\n")
# Round only numeric columns
stability_comp_print <- stability_comparison
stability_comp_print[, -1] <- round(stability_comp_print[, -1], 4)
print(stability_comp_print)
sink()
# Figure 6.12: Coefficient stability boxplots
boot_full_long <- as.data.frame(boot_full) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Coefficient") %>%
  mutate(Method = "Full Model")
boot_ridge_long <- as.data.frame(boot_ridge) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Coefficient") %>%
  mutate(Method = "Ridge")
boot_combined <- rbind(boot_full_long, boot_ridge_long)
fig6_12 <- ggplot(boot_combined, aes(x = Variable, y = Coefficient, fill = Method)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Full Model" = "grey70", "Ridge" = "grey30")) +
  labs(title = "Bootstrap Coefficient Stability",
       subtitle = "Ridge regression produces more stable coefficient estimates",
       x = "Predictor",
       y = "Coefficient Estimate",
       fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
ggsave("figures/fig6_12_bootstrap_stability.jpeg", fig6_12, width = 10, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig6_12_bootstrap_stability.jpeg\n")
# ============================================================================
# Summary Table
# ============================================================================
sink("output/chapter6_summary.txt")
cat("=== Chapter 6 Summary: Multicollinearity ===\n\n")
cat("KEY FINDINGS FROM SIMULATED DATA:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Variables with high VIF (>5):", paste(names(vif_values)[vif_values > 5], collapse = ", "), "\n")
cat("Highest VIF:", round(max(vif_values), 2), "(", names(which.max(vif_values)), ")\n\n")
cat("EFFECT OF CORRELATION ON STANDARD ERRORS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("At r=0.0: SE ~", round(correlation_effects$SE_x1[1], 3), "\n")
cat("At r=0.5: SE ~", round(correlation_effects$SE_x1[11], 3), "\n")
cat("At r=0.9: SE ~", round(correlation_effects$SE_x1[19], 3), "\n")
cat("At r=0.95: SE ~", round(correlation_effects$SE_x1[20], 3), "\n\n")
cat("CENTERING EFFECT ON POLYNOMIAL TERMS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("VIF without centering:", round(vif_no_center[1], 2), "\n")
cat("VIF with centering:", round(vif_centered[1], 2), "\n")
cat("Reduction:", round((1 - vif_centered[1]/vif_no_center[1]) * 100, 1), "%\n\n")
cat("HEART DATA ANALYSIS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Full model AIC:", round(AIC(heart_full), 2), "\n")
cat("Reduced model AIC:", round(AIC(heart_reduced), 2), "\n")
cat("Test AUC (Full):", round(auc_full, 4), "\n")
cat("Test AUC (Ridge):", round(auc_ridge, 4), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 6 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig6_1_correlation_matrix.jpeg\n")
cat("  - fig6_2_vif_barplot.jpeg\n")
cat("  - fig6_3_se_vs_correlation.jpeg\n")
cat("  - fig6_4_coefficient_stability.jpeg\n")
cat("  - fig6_5_ridge_cv.jpeg\n")
cat("  - fig6_6_ridge_comparison.jpeg\n")
cat("  - fig6_7_ridge_path.jpeg\n")
cat("  - fig6_8_centering_effect.jpeg\n")
cat("  - fig6_9_heart_correlation.jpeg\n")
cat("  - fig6_10_heart_vif.jpeg\n")
cat("  - fig6_11_heart_roc.jpeg\n")
cat("  - fig6_12_bootstrap_stability.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - data_summary.txt\n")
cat("  - correlation_matrix.txt\n")
cat("  - vif_analysis.txt\n")
cat("  - correlation_effects.txt\n")
cat("  - stability_analysis.txt\n")
cat("  - ridge_results.txt\n")
cat("  - model_comparison.txt\n")
cat("  - centering_results.txt\n")
cat("  - heart_data_results.txt\n")
cat("  - bootstrap_stability.txt\n")
cat("  - chapter6_summary.txt\n")
