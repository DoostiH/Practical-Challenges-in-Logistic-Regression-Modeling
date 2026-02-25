# ============================================================================
# Chapter 7: Non-linearity in Predictors
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 7
# Required packages: tidyverse, splines, mgcv, pROC, ggplot2, gridExtra
# ============================================================================
# Load required packages
library(tidyverse)
library(splines)
library(mgcv)
library(pROC)
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
cat("CHAPTER 7: NON-LINEARITY IN PREDICTORS\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 7.2: Simulated Data with Non-linear Effects
# ============================================================================
cat("Creating simulated data with non-linear effects...\n")
n <- 1000
# Generate predictors
x1 <- runif(n, -3, 3)       # Will have quadratic effect
x2 <- runif(n, 0, 10)       # Will have threshold effect
x3 <- rnorm(n)              # Will have linear effect
# Create non-linear relationships:
# x1: quadratic (U-shaped)
# x2: threshold effect (flat until x2=5, then linear increase)
# x3: simple linear
linear_pred <- -1 + 0.5 * x1 + 0.5 * x1^2 + 0.3 * pmax(0, x2 - 5) + 0.7 * x3
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
# Create data frame
nonlin_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
# Save data summary
sink("output/data_summary.txt")
cat("=== Simulated Data Summary ===\n\n")
cat("Sample size:", n, "\n")
cat("Event rate:", round(mean(y), 3), "\n\n")
cat("True model:\n")
cat("  logit(p) = -1 + 0.5*x1 + 0.5*x1^2 + 0.3*max(0, x2-5) + 0.7*x3\n\n")
cat("Non-linear effects:\n")
cat("  x1: Quadratic (U-shaped) relationship\n")
cat("  x2: Threshold effect (flat until x2=5, then linear)\n")
cat("  x3: Linear relationship\n\n")
cat("Predictor ranges:\n")
cat("  x1:", round(min(x1), 2), "to", round(max(x1), 2), "\n")
cat("  x2:", round(min(x2), 2), "to", round(max(x2), 2), "\n")
cat("  x3:", round(min(x3), 2), "to", round(max(x3), 2), "\n")
sink()
# ============================================================================
# Section 7.3: Fit Linear Model (Misspecified)
# ============================================================================
cat("Fitting misspecified linear model...\n")
linear_model <- glm(y ~ x1 + x2 + x3,
                    family = binomial(link = "logit"),
                    data = nonlin_data)
sink("output/linear_model.txt")
cat("=== Misspecified Linear Model ===\n\n")
print(summary(linear_model))
cat("\nAIC:", round(AIC(linear_model), 2), "\n")
sink()
# ============================================================================
# Section 7.4: Detecting Non-linearity - Empirical Logit Plots
# ============================================================================
cat("Creating empirical logit plots...\n")
# Function to create empirical logit data
create_empirical_logit <- function(data, x_var, y_var, n_bins = 20) {
  data$bin <- cut(data[[x_var]], breaks = n_bins)
  bin_summary <- data %>%
    group_by(bin) %>%
    summarise(
      x_mean = mean(.data[[x_var]]),
      p = mean(.data[[y_var]]),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      empirical_logit = log((p + 0.01) / (1.01 - p))
    ) %>%
    filter(!is.na(bin))
  return(bin_summary)
}
# Create empirical logit data for each predictor
emp_logit_x1 <- create_empirical_logit(nonlin_data, "x1", "y", n_bins = 20)
emp_logit_x2 <- create_empirical_logit(nonlin_data, "x2", "y", n_bins = 20)
emp_logit_x3 <- create_empirical_logit(nonlin_data, "x3", "y", n_bins = 20)
# Figure 7.1: Empirical logit plots
p1 <- ggplot(emp_logit_x1, aes(x = x_mean, y = empirical_logit)) +
  geom_point(aes(size = n), colour = "grey30", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", fill = "grey70",
              alpha = 0.3, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  labs(title = "x1: Quadratic Effect",
       subtitle = "Clear U-shaped pattern indicates non-linearity",
       x = "x1", y = "Empirical Logit") +
  theme(legend.position = "none")
p2 <- ggplot(emp_logit_x2, aes(x = x_mean, y = empirical_logit)) +
  geom_point(aes(size = n), colour = "grey30", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", fill = "grey70",
              alpha = 0.3, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  geom_vline(xintercept = 5, linetype = "dotted", colour = "black") +
  labs(title = "x2: Threshold Effect",
       subtitle = "Flat until x2\u22485, then increasing",
       x = "x2", y = "Empirical Logit") +
  theme(legend.position = "none")
p3 <- ggplot(emp_logit_x3, aes(x = x_mean, y = empirical_logit)) +
  geom_point(aes(size = n), colour = "grey30", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", fill = "grey70",
              alpha = 0.3, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  labs(title = "x3: Linear Effect",
       subtitle = "Smooth and linear trends align well",
       x = "x3", y = "Empirical Logit") +
  theme(legend.position = "none")
fig7_1 <- grid.arrange(p1, p2, p3, ncol = 3,
                       top = "Empirical Logit Plots for Detecting Non-linearity")
ggsave("figures/fig7_1_empirical_logit.jpeg", fig7_1, width = 12, height = 4,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_1_empirical_logit.jpeg\n")
# ============================================================================
# Section 7.5: Statistical Tests for Non-linearity
# ============================================================================
cat("Testing for non-linearity...\n")
# Fit model with quadratic terms
quad_model <- glm(y ~ x1 + I(x1^2) + x2 + I(x2^2) + x3 + I(x3^2),
                  family = binomial(link = "logit"),
                  data = nonlin_data)
# Likelihood ratio test
lrt <- anova(linear_model, quad_model, test = "Chisq")
sink("output/nonlinearity_tests.txt")
cat("=== Statistical Tests for Non-linearity ===\n\n")
cat("--- Quadratic Model ---\n")
print(summary(quad_model))
cat("\n\n--- Likelihood Ratio Test: Linear vs Quadratic ---\n")
print(lrt)
cat("\n\nIndividual quadratic term significance:\n")
quad_coef <- summary(quad_model)$coefficients
cat("  x1^2: p =", format(quad_coef["I(x1^2)", "Pr(>|z|)"], digits = 4),
    ifelse(quad_coef["I(x1^2)", "Pr(>|z|)"] < 0.05, "(significant)", "(not significant)"), "\n")
cat("  x2^2: p =", format(quad_coef["I(x2^2)", "Pr(>|z|)"], digits = 4),
    ifelse(quad_coef["I(x2^2)", "Pr(>|z|)"] < 0.05, "(significant)", "(not significant)"), "\n")
cat("  x3^2: p =", format(quad_coef["I(x3^2)", "Pr(>|z|)"], digits = 4),
    ifelse(quad_coef["I(x3^2)", "Pr(>|z|)"] < 0.05, "(significant)", "(not significant)"), "\n")
sink()
# ============================================================================
# Section 7.6: Modeling Non-linearity - Polynomial Terms
# ============================================================================
cat("Fitting polynomial models...\n")
# Polynomial model (degree 2)
poly_model <- glm(y ~ poly(x1, 2, raw = TRUE) + poly(x2, 2, raw = TRUE) + x3,
                  family = binomial(link = "logit"),
                  data = nonlin_data)
sink("output/polynomial_model.txt")
cat("=== Polynomial Model (Degree 2) ===\n\n")
print(summary(poly_model))
cat("\nAIC:", round(AIC(poly_model), 2), "\n")
cat("AIC improvement over linear:", round(AIC(linear_model) - AIC(poly_model), 2), "\n")
sink()
# ============================================================================
# Section 7.7: Modeling Non-linearity - Splines
# ============================================================================
cat("Fitting spline models...\n")
# Natural splines
ns_model <- glm(y ~ ns(x1, df = 4) + ns(x2, df = 4) + x3,
                family = binomial(link = "logit"),
                data = nonlin_data)
sink("output/spline_model.txt")
cat("=== Natural Spline Model (df = 4) ===\n\n")
print(summary(ns_model))
cat("\nAIC:", round(AIC(ns_model), 2), "\n")
cat("AIC improvement over linear:", round(AIC(linear_model) - AIC(ns_model), 2), "\n")
sink()
# ============================================================================
# Section 7.8: Modeling Non-linearity - GAMs
# ============================================================================
cat("Fitting GAM model...\n")
# GAM model
gam_model <- gam(y ~ s(x1) + s(x2) + x3,
                 family = binomial(link = "logit"),
                 data = nonlin_data)
sink("output/gam_model.txt")
cat("=== Generalized Additive Model ===\n\n")
print(summary(gam_model))
cat("\nAIC:", round(AIC(gam_model), 2), "\n")
cat("AIC improvement over linear:", round(AIC(linear_model) - AIC(gam_model), 2), "\n")
cat("\nEffective degrees of freedom:\n")
cat("  s(x1):", round(sum(gam_model$edf[1]), 2), "\n")
cat("  s(x2):", round(sum(gam_model$edf[2]), 2), "\n")
sink()
# Figure 7.2: GAM smooth functions
jpeg("figures/fig7_2_gam_smooths.jpeg", width = 12, height = 4, units = "in", res = 300)
par(mfrow = c(1, 3))
plot(gam_model, shade = TRUE, shade.col = "grey80",
     select = 1, main = "GAM Smooth: s(x1)", ylab = "s(x1)")
plot(gam_model, shade = TRUE, shade.col = "grey80",
     select = 2, main = "GAM Smooth: s(x2)", ylab = "s(x2)")
# For x3 (linear), create a simple plot
x3_range <- seq(min(nonlin_data$x3), max(nonlin_data$x3), length.out = 100)
x3_effect <- coef(gam_model)["x3"] * x3_range
plot(x3_range, x3_effect, type = "l", lwd = 2, col = "black",
     main = "Linear Effect: x3", xlab = "x3", ylab = "Linear predictor contribution")
abline(h = 0, lty = 2, col = "grey50")
par(mfrow = c(1, 1))
dev.off()
cat("Saved: figures/fig7_2_gam_smooths.jpeg\n")
# ============================================================================
# Section 7.9: Model Comparison - Predicted Curves
# ============================================================================
cat("Creating model comparison plots...\n")
# Create prediction data for x1 (varying x1, holding x2 and x3 at means)
new_data_x1 <- data.frame(
  x1 = seq(min(nonlin_data$x1), max(nonlin_data$x1), length.out = 200),
  x2 = mean(nonlin_data$x2),
  x3 = mean(nonlin_data$x3)
)
# Get predictions from each model
new_data_x1$Linear <- predict(linear_model, newdata = new_data_x1, type = "response")
new_data_x1$Polynomial <- predict(poly_model, newdata = new_data_x1, type = "response")
new_data_x1$Spline <- predict(ns_model, newdata = new_data_x1, type = "response")
new_data_x1$GAM <- predict(gam_model, newdata = new_data_x1, type = "response")
# Calculate true probabilities
new_data_x1$True <- plogis(-1 + 0.5 * new_data_x1$x1 + 0.5 * new_data_x1$x1^2 +
                             0.3 * pmax(0, new_data_x1$x2 - 5) + 0.7 * new_data_x1$x3)
# Reshape for plotting
pred_long_x1 <- new_data_x1 %>%
  pivot_longer(cols = c(Linear, Polynomial, Spline, GAM, True),
               names_to = "Model", values_to = "Probability") %>%
  mutate(Model = factor(Model, levels = c("True", "Linear", "Polynomial", "Spline", "GAM")))
# Figure 7.3: Model comparison for x1
fig7_3 <- ggplot(pred_long_x1, aes(x = x1, y = Probability, linetype = Model)) +
  geom_line(linewidth = 1, colour = "black") +
  scale_linetype_manual(values = c("True" = "solid", "Linear" = "dashed",
                                   "Polynomial" = "dotted", "Spline" = "dotdash",
                                   "GAM" = "longdash")) +
  labs(title = "Model Comparison: Predicted Probability vs x1",
       subtitle = "x2 and x3 held at their means; solid line shows true relationship",
       x = "x1", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig7_3_model_comparison_x1.jpeg", fig7_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_3_model_comparison_x1.jpeg\n")
# Create prediction data for x2 (varying x2, holding x1 and x3 at means)
new_data_x2 <- data.frame(
  x1 = mean(nonlin_data$x1),
  x2 = seq(min(nonlin_data$x2), max(nonlin_data$x2), length.out = 200),
  x3 = mean(nonlin_data$x3)
)
# Get predictions from each model
new_data_x2$Linear <- predict(linear_model, newdata = new_data_x2, type = "response")
new_data_x2$Polynomial <- predict(poly_model, newdata = new_data_x2, type = "response")
new_data_x2$Spline <- predict(ns_model, newdata = new_data_x2, type = "response")
new_data_x2$GAM <- predict(gam_model, newdata = new_data_x2, type = "response")
# Calculate true probabilities
new_data_x2$True <- plogis(-1 + 0.5 * new_data_x2$x1 + 0.5 * new_data_x2$x1^2 +
                             0.3 * pmax(0, new_data_x2$x2 - 5) + 0.7 * new_data_x2$x3)
# Reshape for plotting
pred_long_x2 <- new_data_x2 %>%
  pivot_longer(cols = c(Linear, Polynomial, Spline, GAM, True),
               names_to = "Model", values_to = "Probability") %>%
  mutate(Model = factor(Model, levels = c("True", "Linear", "Polynomial", "Spline", "GAM")))
# Figure 7.4: Model comparison for x2
fig7_4 <- ggplot(pred_long_x2, aes(x = x2, y = Probability, linetype = Model)) +
  geom_line(linewidth = 1, colour = "black") +
  geom_vline(xintercept = 5, linetype = "dotted", colour = "grey40") +
  scale_linetype_manual(values = c("True" = "solid", "Linear" = "dashed",
                                   "Polynomial" = "dotted", "Spline" = "dotdash",
                                   "GAM" = "longdash")) +
  annotate("text", x = 5.2, y = 0.15, label = "Threshold at x2=5",
           hjust = 0, size = 3, colour = "grey40") +
  labs(title = "Model Comparison: Predicted Probability vs x2",
       subtitle = "x1 and x3 held at their means; vertical line marks true threshold",
       x = "x2", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig7_4_model_comparison_x2.jpeg", fig7_4, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_4_model_comparison_x2.jpeg\n")
# ============================================================================
# Section 7.10: Model Comparison - Information Criteria
# ============================================================================
cat("Comparing models using information criteria...\n")
# Create model comparison table
model_comparison <- data.frame(
  Model = c("Linear", "Polynomial", "Natural Spline", "GAM"),
  AIC = c(AIC(linear_model), AIC(poly_model), AIC(ns_model), AIC(gam_model)),
  BIC = c(BIC(linear_model), BIC(poly_model), BIC(ns_model), BIC(gam_model)),
  Deviance = c(deviance(linear_model), deviance(poly_model),
               deviance(ns_model), deviance(gam_model)),
  DF = c(linear_model$df.residual, poly_model$df.residual,
         ns_model$df.residual, gam_model$df.residual)
)
model_comparison <- model_comparison %>%
  mutate(
    Delta_AIC = AIC - min(AIC),
    Delta_BIC = BIC - min(BIC)
  ) %>%
  arrange(AIC)
sink("output/model_comparison.txt")
cat("=== Model Comparison: Information Criteria ===\n\n")
print(model_comparison, row.names = FALSE)
cat("\n\nInterpretation:\n")
cat("- Delta_AIC/BIC: Difference from best model (lower is better)\n")
cat("- Delta > 2: Some evidence against model\n")
cat("- Delta > 10: Strong evidence against model\n")
sink()
# Figure 7.5: AIC/BIC comparison
ic_long <- model_comparison %>%
  select(Model, AIC, BIC) %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Criterion", values_to = "Value")
fig7_5 <- ggplot(ic_long, aes(x = reorder(Model, Value), y = Value, fill = Criterion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("AIC" = "grey70", "BIC" = "grey30")) +
  coord_flip() +
  labs(title = "Model Comparison: Information Criteria",
       subtitle = "Lower values indicate better fit (accounting for complexity)",
       x = "Model", y = "Value") +
  theme(legend.position = "top")
ggsave("figures/fig7_5_information_criteria.jpeg", fig7_5, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_5_information_criteria.jpeg\n")
# ============================================================================
# Section 7.11: Cross-Validation Performance
# ============================================================================
cat("Performing cross-validation...\n")
set.seed(456)
k <- 10  # Number of folds
folds <- sample(rep(1:k, length.out = n))
# Storage for results
cv_results <- data.frame(
  Fold = integer(),
  Model = character(),
  AUC = numeric(),
  Brier = numeric()
)
# Cross-validation loop
for (fold in 1:k) {
  train_idx <- which(folds != fold)
  test_idx <- which(folds == fold)
  train_data <- nonlin_data[train_idx, ]
  test_data <- nonlin_data[test_idx, ]
  # Fit models
  cv_linear <- glm(y ~ x1 + x2 + x3, family = binomial, data = train_data)
  cv_poly <- glm(y ~ poly(x1, 2, raw = TRUE) + poly(x2, 2, raw = TRUE) + x3,
                 family = binomial, data = train_data)
  cv_ns <- glm(y ~ ns(x1, df = 4) + ns(x2, df = 4) + x3,
               family = binomial, data = train_data)
  cv_gam <- gam(y ~ s(x1) + s(x2) + x3, family = binomial, data = train_data)
  # Predictions
  pred_linear <- predict(cv_linear, newdata = test_data, type = "response")
  pred_poly <- predict(cv_poly, newdata = test_data, type = "response")
  pred_ns <- predict(cv_ns, newdata = test_data, type = "response")
  pred_gam <- predict(cv_gam, newdata = test_data, type = "response")
  # Calculate metrics
  y_test <- test_data$y
  # AUC
  auc_linear <- as.numeric(auc(roc(y_test, pred_linear, quiet = TRUE)))
  auc_poly <- as.numeric(auc(roc(y_test, pred_poly, quiet = TRUE)))
  auc_ns <- as.numeric(auc(roc(y_test, pred_ns, quiet = TRUE)))
  auc_gam <- as.numeric(auc(roc(y_test, pred_gam, quiet = TRUE)))
  # Brier score
  brier_linear <- mean((pred_linear - y_test)^2)
  brier_poly <- mean((pred_poly - y_test)^2)
  brier_ns <- mean((pred_ns - y_test)^2)
  brier_gam <- mean((pred_gam - y_test)^2)
  # Store results
  cv_results <- rbind(cv_results, data.frame(
    Fold = fold,
    Model = c("Linear", "Polynomial", "Spline", "GAM"),
    AUC = c(auc_linear, auc_poly, auc_ns, auc_gam),
    Brier = c(brier_linear, brier_poly, brier_ns, brier_gam)
  ))
}
# Summarize CV results
cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarise(
    Mean_AUC = mean(AUC),
    SD_AUC = sd(AUC),
    Mean_Brier = mean(Brier),
    SD_Brier = sd(Brier),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_AUC))
sink("output/cv_results.txt")
cat("=== Cross-Validation Results (", k, "-fold) ===\n\n", sep = "")
cat("Summary:\n")
print(as.data.frame(cv_summary), row.names = FALSE)
cat("\n\nDetailed results by fold:\n")
cv_wide <- cv_results %>%
  pivot_wider(names_from = Model, values_from = c(AUC, Brier))
print(as.data.frame(cv_wide))
sink()
# Figure 7.6: CV AUC boxplot
# Create ordered factor for consistent greyscale assignment
cv_results$Model_ordered <- factor(cv_results$Model,
                                   levels = c("Linear", "Polynomial", "Spline", "GAM"))
fig7_6 <- ggplot(cv_results, aes(x = reorder(Model, AUC, FUN = median), y = AUC,
                                 fill = Model_ordered)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Linear" = "grey85", "Polynomial" = "grey65",
                               "Spline" = "grey45", "GAM" = "grey25")) +
  labs(title = "Cross-Validated AUC by Model",
       subtitle = paste0(k, "-fold cross-validation"),
       x = "Model", y = "AUC") +
  theme(legend.position = "none") +
  coord_flip()
ggsave("figures/fig7_6_cv_auc.jpeg", fig7_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_6_cv_auc.jpeg\n")
# Figure 7.7: CV Brier score boxplot
fig7_7 <- ggplot(cv_results, aes(x = reorder(Model, -Brier, FUN = median), y = Brier,
                                 fill = Model_ordered)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Linear" = "grey85", "Polynomial" = "grey65",
                               "Spline" = "grey45", "GAM" = "grey25")) +
  labs(title = "Cross-Validated Brier Score by Model",
       subtitle = paste0(k, "-fold cross-validation; lower is better"),
       x = "Model", y = "Brier Score") +
  theme(legend.position = "none") +
  coord_flip()
ggsave("figures/fig7_7_cv_brier.jpeg", fig7_7, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_7_cv_brier.jpeg\n")
# ============================================================================
# Section 7.12: Real Data Example - Age and Disease Risk
# ============================================================================
cat("Creating real data example...\n")
# Create a realistic medical dataset with non-linear age effect
set.seed(789)
n_real <- 800
age <- round(runif(n_real, 20, 80))
bmi <- round(rnorm(n_real, mean = 26, sd = 5), 1)
bmi <- pmax(16, pmin(bmi, 45))  # Constrain to realistic range
# Non-linear age effect: risk increases slowly until ~50, then accelerates
age_effect <- -0.02 * (age - 50) + 0.001 * (age - 50)^2
# BMI has a J-shaped relationship (risk at low and high BMI)
bmi_effect <- 0.05 * (bmi - 25)^2 / 100
# Generate outcome
linear_pred_real <- -2 + age_effect + 0.03 * bmi + bmi_effect
prob_real <- plogis(linear_pred_real)
disease <- rbinom(n_real, 1, prob_real)
real_data <- data.frame(
  disease = disease,
  age = age,
  bmi = bmi
)
# Fit models
real_linear <- glm(disease ~ age + bmi, family = binomial, data = real_data)
real_poly <- glm(disease ~ poly(age, 2, raw = TRUE) + poly(bmi, 2, raw = TRUE),
                 family = binomial, data = real_data)
real_ns <- glm(disease ~ ns(age, df = 4) + ns(bmi, df = 4),
               family = binomial, data = real_data)
real_gam <- gam(disease ~ s(age) + s(bmi), family = binomial, data = real_data)
# Save results
sink("output/real_data_results.txt")
cat("=== Real Data Example: Age and Disease Risk ===\n\n")
cat("Sample size:", n_real, "\n")
cat("Disease prevalence:", round(mean(disease), 3), "\n\n")
cat("--- Linear Model ---\n")
print(summary(real_linear))
cat("\n\n--- GAM Model ---\n")
print(summary(real_gam))
cat("\n\nModel Comparison:\n")
cat("Linear AIC:", round(AIC(real_linear), 2), "\n")
cat("Polynomial AIC:", round(AIC(real_poly), 2), "\n")
cat("Natural Spline AIC:", round(AIC(real_ns), 2), "\n")
cat("GAM AIC:", round(AIC(real_gam), 2), "\n")
sink()
# Figure 7.8: Empirical logit plots for real data
emp_logit_age <- create_empirical_logit(real_data, "age", "disease", n_bins = 15)
emp_logit_bmi <- create_empirical_logit(real_data, "bmi", "disease", n_bins = 15)
p_age <- ggplot(emp_logit_age, aes(x = x_mean, y = empirical_logit)) +
  geom_point(aes(size = n), colour = "grey30", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", fill = "grey70",
              alpha = 0.3, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  labs(title = "Age",
       subtitle = "Accelerating risk after age 50",
       x = "Age (years)", y = "Empirical Logit") +
  theme(legend.position = "none")
p_bmi <- ggplot(emp_logit_bmi, aes(x = x_mean, y = empirical_logit)) +
  geom_point(aes(size = n), colour = "grey30", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "black", fill = "grey70",
              alpha = 0.3, linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  labs(title = "BMI",
       subtitle = "J-shaped relationship",
       x = expression("BMI (kg/" * m^2 * ")"), y = "Empirical Logit") +
  theme(legend.position = "none")
fig7_8 <- grid.arrange(p_age, p_bmi, ncol = 2,
                       top = "Empirical Logit Plots: Real Data Example")
ggsave("figures/fig7_8_real_data_empirical_logit.jpeg", fig7_8, width = 10, height = 4,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_8_real_data_empirical_logit.jpeg\n")
# Figure 7.9: GAM smooth functions for real data
jpeg("figures/fig7_9_real_data_gam.jpeg", width = 10, height = 4, units = "in", res = 300)
par(mfrow = c(1, 2))
plot(real_gam, shade = TRUE, shade.col = "grey80",
     select = 1, main = "GAM Smooth: Age Effect", xlab = "Age (years)")
plot(real_gam, shade = TRUE, shade.col = "grey80",
     select = 2, main = "GAM Smooth: BMI Effect", xlab = expression("BMI (kg/" * m^2 * ")"))
par(mfrow = c(1, 1))
dev.off()
cat("Saved: figures/fig7_9_real_data_gam.jpeg\n")
# Figure 7.10: Model predictions for age
new_age <- data.frame(
  age = seq(20, 80, length.out = 100),
  bmi = mean(real_data$bmi)
)
new_age$Linear <- predict(real_linear, newdata = new_age, type = "response")
new_age$Polynomial <- predict(real_poly, newdata = new_age, type = "response")
new_age$Spline <- predict(real_ns, newdata = new_age, type = "response")
new_age$GAM <- predict(real_gam, newdata = new_age, type = "response")
pred_age_long <- new_age %>%
  pivot_longer(cols = c(Linear, Polynomial, Spline, GAM),
               names_to = "Model", values_to = "Probability")
fig7_10 <- ggplot(pred_age_long, aes(x = age, y = Probability, linetype = Model)) +
  geom_line(linewidth = 1, colour = "black") +
  scale_linetype_manual(values = c("Linear" = "dashed", "Polynomial" = "dotted",
                                   "Spline" = "dotdash", "GAM" = "longdash")) +
  labs(title = "Predicted Disease Probability by Age",
       subtitle = "BMI held at mean; non-linear models capture accelerating risk",
       x = "Age (years)", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig7_10_real_data_predictions.jpeg", fig7_10, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig7_10_real_data_predictions.jpeg\n")
# ============================================================================
# Section 7.13: Summary Statistics
# ============================================================================
sink("output/chapter7_summary.txt")
cat("=== Chapter 7 Summary: Non-linearity in Predictors ===\n\n")
cat("SIMULATED DATA RESULTS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("True model: quadratic x1, threshold x2, linear x3\n\n")
cat("Model AIC comparison:\n")
cat("  Linear:     ", round(AIC(linear_model), 2), "\n")
cat("  Polynomial: ", round(AIC(poly_model), 2), " (improvement:", round(AIC(linear_model) - AIC(poly_model), 1), ")\n")
cat("  Spline:     ", round(AIC(ns_model), 2), " (improvement:", round(AIC(linear_model) - AIC(ns_model), 1), ")\n")
cat("  GAM:        ", round(AIC(gam_model), 2), " (improvement:", round(AIC(linear_model) - AIC(gam_model), 1), ")\n\n")
cat("Cross-validated AUC:\n")
for (i in 1:nrow(cv_summary)) {
  cat("  ", cv_summary$Model[i], ": ", round(cv_summary$Mean_AUC[i], 4),
      " (SD: ", round(cv_summary$SD_AUC[i], 4), ")\n", sep = "")
}
cat("\n\nREAL DATA RESULTS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Disease prevalence:", round(mean(disease), 3), "\n\n")
cat("Model AIC comparison:\n")
cat("  Linear:     ", round(AIC(real_linear), 2), "\n")
cat("  Polynomial: ", round(AIC(real_poly), 2), "\n")
cat("  Spline:     ", round(AIC(real_ns), 2), "\n")
cat("  GAM:        ", round(AIC(real_gam), 2), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 7 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig7_1_empirical_logit.jpeg\n")
cat("  - fig7_2_gam_smooths.jpeg\n")
cat("  - fig7_3_model_comparison_x1.jpeg\n")
cat("  - fig7_4_model_comparison_x2.jpeg\n")
cat("  - fig7_5_information_criteria.jpeg\n")
cat("  - fig7_6_cv_auc.jpeg\n")
cat("  - fig7_7_cv_brier.jpeg\n")
cat("  - fig7_8_real_data_empirical_logit.jpeg\n")
cat("  - fig7_9_real_data_gam.jpeg\n")
cat("  - fig7_10_real_data_predictions.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - data_summary.txt\n")
cat("  - linear_model.txt\n")
cat("  - nonlinearity_tests.txt\n")
cat("  - polynomial_model.txt\n")
cat("  - spline_model.txt\n")
cat("  - gam_model.txt\n")
cat("  - model_comparison.txt\n")
cat("  - cv_results.txt\n")
cat("  - real_data_results.txt\n")
cat("  - chapter7_summary.txt\n")
