# ============================================================================
# Chapter 8: Interaction Effects
# R Code for Figures and Analysis
#
# This script generates all figures and outputs for Chapter 8
# Required packages: tidyverse, pROC, ggplot2, gridExtra
# ============================================================================
# Load required packages
library(tidyverse)
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
cat("CHAPTER 8: INTERACTION EFFECTS\n")
cat("=======================================================\n\n")
# ============================================================================
# Section 8.2: Simulated Data with Interactions
# ============================================================================
cat("Creating simulated data with interaction effects...\n")
n <- 1000
# Generate predictors
x1 <- rnorm(n)                    # Continuous predictor
x2 <- rnorm(n)                    # Continuous predictor
x3 <- factor(rbinom(n, 1, 0.5))   # Binary predictor (categorical)
# Create true model with interactions:
# 1. x1 x x2: Continuous x Continuous interaction (coefficient = 0.8)
# 2. x1 x x3: Continuous x Categorical interaction (coefficient = 1.5)
# No x2 x x3 interaction (coefficient = 0)
linear_pred <- -1 + 0.5 * x1 + 0.3 * x2 + 1.2 * as.numeric(as.character(x3)) +
  0.8 * x1 * x2 + 1.5 * x1 * as.numeric(as.character(x3))
prob <- plogis(linear_pred)
y <- rbinom(n, 1, prob)
# Create data frame
interact_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
# Save data summary
sink("output/data_summary.txt")
cat("=== Simulated Data Summary ===\n\n")
cat("Sample size:", n, "\n")
cat("Event rate:", round(mean(y), 3), "\n\n")
cat("True model:\n")
cat("  logit(p) = -1 + 0.5*x1 + 0.3*x2 + 1.2*x3 + 0.8*x1*x2 + 1.5*x1*x3\n\n")
cat("True interaction effects:\n")
cat("  x1 x x2: 0.8 (continuous x continuous)\n")
cat("  x1 x x3: 1.5 (continuous x categorical)\n")
cat("  x2 x x3: 0.0 (no interaction - serves as null test)\n\n")
cat("Predictor distributions:\n")
cat("  x1: mean =", round(mean(x1), 2), ", sd =", round(sd(x1), 2), "\n")
cat("  x2: mean =", round(mean(x2), 2), ", sd =", round(sd(x2), 2), "\n")
cat("  x3: proportion x3=1 =", round(mean(x3 == "1"), 2), "\n")
sink()
# ============================================================================
# Section 8.3: Main Effects Model (Ignoring Interactions)
# ============================================================================
cat("Fitting main effects model (ignoring interactions)...\n")
main_model <- glm(y ~ x1 + x2 + x3,
                  family = binomial(link = "logit"),
                  data = interact_data)
sink("output/main_effects_model.txt")
cat("=== Main Effects Model (Ignoring Interactions) ===\n\n")
print(summary(main_model))
cat("\nAIC:", round(AIC(main_model), 2), "\n")
sink()
# ============================================================================
# Section 8.4: Detecting Interactions - Stratified Analysis
# ============================================================================
cat("Performing stratified analysis to detect interactions...\n")
# Stratified analysis: effect of x1 within each level of x3
strat_x3_0 <- glm(y ~ x1 + x2, family = binomial,
                  data = interact_data[interact_data$x3 == "0", ])
strat_x3_1 <- glm(y ~ x1 + x2, family = binomial,
                  data = interact_data[interact_data$x3 == "1", ])
strat_comparison <- data.frame(
  Group = c("x3 = 0", "x3 = 1"),
  Coef_x1 = c(coef(strat_x3_0)["x1"], coef(strat_x3_1)["x1"]),
  SE_x1 = c(summary(strat_x3_0)$coefficients["x1", "Std. Error"],
            summary(strat_x3_1)$coefficients["x1", "Std. Error"]),
  n = c(sum(interact_data$x3 == "0"), sum(interact_data$x3 == "1"))
)
sink("output/stratified_analysis.txt")
cat("=== Stratified Analysis: Effect of x1 by x3 Group ===\n\n")
cat("This analysis helps detect potential x1 x x3 interaction.\n")
cat("Different slopes across groups suggest interaction.\n\n")
print(strat_comparison, row.names = FALSE)
cat("\nDifference in x1 coefficients:",
    round(strat_comparison$Coef_x1[2] - strat_comparison$Coef_x1[1], 3), "\n")
cat("Expected difference (true x1:x3 coefficient): 1.5\n")
sink()
# Figure 8.1: Stratified relationship visualization
fig8_1 <- ggplot(interact_data, aes(x = x1, y = y, linetype = x3, shape = x3)) +
  geom_point(alpha = 0.2, position = position_jitter(height = 0.05), colour = "grey50") +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = TRUE,
              colour = "black", fill = "grey70", alpha = 0.3) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                        labels = c("x3 = 0", "x3 = 1")) +
  scale_shape_manual(values = c("0" = 16, "1" = 17),
                     labels = c("x3 = 0", "x3 = 1")) +
  labs(title = "Detecting Interaction: Stratified Relationship",
       subtitle = "Different slopes suggest x1 \u00D7 x3 interaction",
       x = "x1", y = "Probability of y = 1",
       linetype = "Group", shape = "Group") +
  theme(legend.position = "bottom")
ggsave("figures/fig8_1_stratified_relationship.jpeg", fig8_1, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_1_stratified_relationship.jpeg\n")
# ============================================================================
# Section 8.5: Detecting Interactions - Statistical Tests
# ============================================================================
cat("Testing for interactions using likelihood ratio tests...\n")
# Fit models with individual interactions
model_x1x2 <- glm(y ~ x1 + x2 + x3 + x1:x2, family = binomial, data = interact_data)
model_x1x3 <- glm(y ~ x1 + x2 + x3 + x1:x3, family = binomial, data = interact_data)
model_x2x3 <- glm(y ~ x1 + x2 + x3 + x2:x3, family = binomial, data = interact_data)
# Full model with all two-way interactions
model_all <- glm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3,
                 family = binomial, data = interact_data)
# True model (x1:x2 and x1:x3 only)
model_true <- glm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3,
                  family = binomial, data = interact_data)
# Likelihood ratio tests
lrt_x1x2 <- anova(main_model, model_x1x2, test = "Chisq")
lrt_x1x3 <- anova(main_model, model_x1x3, test = "Chisq")
lrt_x2x3 <- anova(main_model, model_x2x3, test = "Chisq")
sink("output/interaction_tests.txt")
cat("=== Likelihood Ratio Tests for Interactions ===\n\n")
cat("--- Test for x1 x x2 interaction ---\n")
cat("Deviance reduction:", round(lrt_x1x2$Deviance[2], 2), "\n")
cat("p-value:", format(lrt_x1x2$`Pr(>Chi)`[2], digits = 4), "\n")
cat("Significant:", ifelse(lrt_x1x2$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n\n")
cat("--- Test for x1 x x3 interaction ---\n")
cat("Deviance reduction:", round(lrt_x1x3$Deviance[2], 2), "\n")
cat("p-value:", format(lrt_x1x3$`Pr(>Chi)`[2], digits = 4), "\n")
cat("Significant:", ifelse(lrt_x1x3$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n\n")
cat("--- Test for x2 x x3 interaction ---\n")
cat("Deviance reduction:", round(lrt_x2x3$Deviance[2], 2), "\n")
cat("p-value:", format(lrt_x2x3$`Pr(>Chi)`[2], digits = 4), "\n")
cat("Significant:", ifelse(lrt_x2x3$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n\n")
cat("True model has x1:x2 and x1:x3 interactions only.\n")
sink()
# AIC comparison
aic_comparison <- data.frame(
  Model = c("Main Effects Only",
            "Add x1 \u00D7 x2",
            "Add x1 \u00D7 x3",
            "Add x2 \u00D7 x3",
            "True (x1\u00D7x2 + x1\u00D7x3)",
            "All Interactions"),
  AIC = c(AIC(main_model),
          AIC(model_x1x2),
          AIC(model_x1x3),
          AIC(model_x2x3),
          AIC(model_true),
          AIC(model_all)),
  Parameters = c(4, 5, 5, 5, 6, 7)
)
aic_comparison <- aic_comparison %>%
  mutate(Delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC)
sink("output/model_comparison.txt")
cat("=== Model Comparison: AIC ===\n\n")
print(aic_comparison, row.names = FALSE)
cat("\nBest model by AIC:", aic_comparison$Model[1], "\n")
sink()
# Figure 8.2: AIC comparison
# Assign greyscale fills based on rank
aic_comparison$grey_fill <- seq(from = 0.8, to = 0.3, length.out = nrow(aic_comparison))

fig8_2 <- ggplot(aic_comparison, aes(x = reorder(Model, -AIC), y = AIC)) +
  geom_col(fill = "grey50", colour = "black", linewidth = 0.3) +
  geom_text(aes(label = round(AIC, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Model Comparison: AIC",
       subtitle = "Lower AIC indicates better fit; true model includes x1\u00D7x2 and x1\u00D7x3",
       x = "Model", y = "AIC") +
  theme(legend.position = "none") +
  ylim(c(0, max(aic_comparison$AIC) * 1.1))
ggsave("figures/fig8_2_aic_comparison.jpeg", fig8_2, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_2_aic_comparison.jpeg\n")
# ============================================================================
# Section 8.6: The True Interaction Model
# ============================================================================
cat("Fitting the true interaction model...\n")
interaction_model <- model_true
sink("output/interaction_model.txt")
cat("=== Interaction Model (True Specification) ===\n\n")
print(summary(interaction_model))
cat("\nTrue coefficients:\n")
cat("  Intercept: -1.0, Estimate:", round(coef(interaction_model)[1], 3), "\n")
cat("  x1: 0.5, Estimate:", round(coef(interaction_model)["x1"], 3), "\n")
cat("  x2: 0.3, Estimate:", round(coef(interaction_model)["x2"], 3), "\n")
cat("  x3: 1.2, Estimate:", round(coef(interaction_model)["x31"], 3), "\n")
cat("  x1:x2: 0.8, Estimate:", round(coef(interaction_model)["x1:x2"], 3), "\n")
cat("  x1:x3: 1.5, Estimate:", round(coef(interaction_model)["x1:x31"], 3), "\n")
sink()
# ============================================================================
# Section 8.7: Visualizing Continuous x Continuous Interaction
# ============================================================================
cat("Creating visualizations for continuous x continuous interaction...\n")
# Create prediction grid for x1 at different levels of x2
x2_levels <- quantile(interact_data$x2, probs = c(0.1, 0.5, 0.9))
pred_grid_x1x2 <- expand.grid(
  x1 = seq(min(interact_data$x1), max(interact_data$x1), length.out = 100),
  x2 = x2_levels,
  x3 = "0"  # Hold x3 at reference level
)
pred_grid_x1x2$pred <- predict(interaction_model, newdata = pred_grid_x1x2, type = "response")
pred_grid_x1x2$x2_label <- factor(
  paste0("x2 = ", round(pred_grid_x1x2$x2, 1)),
  levels = paste0("x2 = ", round(x2_levels, 1))
)
# Figure 8.3: Continuous x Continuous interaction
fig8_3 <- ggplot(pred_grid_x1x2, aes(x = x1, y = pred, linetype = x2_label)) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash"),
                        name = "x2 Level",
                        labels = c("Low (10th %ile)", "Median", "High (90th %ile)")) +
  labs(title = "Continuous \u00D7 Continuous Interaction: x1 \u00D7 x2",
       subtitle = "Effect of x1 varies with level of x2; x3 held at reference level",
       x = "x1", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig8_3_cont_cont_interaction.jpeg", fig8_3, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_3_cont_cont_interaction.jpeg\n")
# Figure 8.4: Heatmap of x1 x x2 interaction (greyscale)
heatmap_grid <- expand.grid(
  x1 = seq(-2.5, 2.5, length.out = 50),
  x2 = seq(-2.5, 2.5, length.out = 50),
  x3 = "0"
)
heatmap_grid$pred <- predict(interaction_model, newdata = heatmap_grid, type = "response")
fig8_4 <- ggplot(heatmap_grid, aes(x = x1, y = x2, fill = pred)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", name = "Probability") +
  geom_contour(aes(z = pred), colour = "grey50", alpha = 0.7, bins = 8) +
  labs(title = "Probability Surface: x1 \u00D7 x2 Interaction",
       subtitle = "Contour lines show equal probability; x3 held at reference level",
       x = "x1", y = "x2") +
  coord_fixed()
ggsave("figures/fig8_4_probability_surface.jpeg", fig8_4, width = 7, height = 6,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_4_probability_surface.jpeg\n")
# ============================================================================
# Section 8.8: Visualizing Continuous x Categorical Interaction
# ============================================================================
cat("Creating visualizations for continuous x categorical interaction...\n")
# Prediction grid for x1 by x3
pred_grid_x1x3 <- expand.grid(
  x1 = seq(min(interact_data$x1), max(interact_data$x1), length.out = 100),
  x2 = mean(interact_data$x2),  # Hold x2 at mean
  x3 = c("0", "1")
)
pred_grid_x1x3$pred <- predict(interaction_model, newdata = pred_grid_x1x3, type = "response")
# Add confidence intervals
pred_link <- predict(interaction_model, newdata = pred_grid_x1x3, type = "link", se.fit = TRUE)
pred_grid_x1x3$lower <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
pred_grid_x1x3$upper <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)
# Figure 8.5: Continuous x Categorical interaction
fig8_5 <- ggplot(pred_grid_x1x3, aes(x = x1, y = pred, linetype = x3)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, group = x3),
              fill = "grey80", alpha = 0.4) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"), name = "Group",
                        labels = c("x3 = 0", "x3 = 1")) +
  labs(title = "Continuous \u00D7 Categorical Interaction: x1 \u00D7 x3",
       subtitle = "Effect of x1 is much stronger when x3 = 1; x2 held at mean",
       x = "x1", y = "Predicted Probability") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig8_5_cont_cat_interaction.jpeg", fig8_5, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_5_cont_cat_interaction.jpeg\n")
# ============================================================================
# Section 8.9: Simple Slopes Analysis
# ============================================================================
cat("Performing simple slopes analysis...\n")
# Extract coefficients and variance-covariance matrix
coefs <- coef(interaction_model)
vcov_mat <- vcov(interaction_model)
# Simple slopes for x1 at different levels of x2 (continuous x continuous)
x2_vals <- c(-1.5, 0, 1.5)
simple_slopes_x2 <- data.frame(
  x2_value = x2_vals,
  x2_label = c("Low (x2 = -1.5)", "Mean (x2 = 0)", "High (x2 = 1.5)"),
  simple_slope = coefs["x1"] + coefs["x1:x2"] * x2_vals
)
# Calculate standard errors for simple slopes
var_b1 <- vcov_mat["x1", "x1"]
var_b3 <- vcov_mat["x1:x2", "x1:x2"]
cov_b1b3 <- vcov_mat["x1", "x1:x2"]
simple_slopes_x2$se <- sqrt(var_b1 + simple_slopes_x2$x2_value^2 * var_b3 +
                              2 * simple_slopes_x2$x2_value * cov_b1b3)
simple_slopes_x2$lower <- simple_slopes_x2$simple_slope - 1.96 * simple_slopes_x2$se
simple_slopes_x2$upper <- simple_slopes_x2$simple_slope + 1.96 * simple_slopes_x2$se
simple_slopes_x2$significant <- !((simple_slopes_x2$lower < 0) & (simple_slopes_x2$upper > 0))
simple_slopes_x2$p_value <- 2 * pnorm(-abs(simple_slopes_x2$simple_slope / simple_slopes_x2$se))
# Simple slopes for x1 by x3 (continuous x categorical)
simple_slopes_x3 <- data.frame(
  x3_value = c("0", "1"),
  simple_slope = c(coefs["x1"], coefs["x1"] + coefs["x1:x31"])
)
# Standard errors for x3 groups
var_b1x3 <- vcov_mat["x1", "x1"]
var_b4 <- vcov_mat["x1:x31", "x1:x31"]
cov_b1b4 <- vcov_mat["x1", "x1:x31"]
simple_slopes_x3$se <- c(sqrt(var_b1x3), sqrt(var_b1x3 + var_b4 + 2 * cov_b1b4))
simple_slopes_x3$lower <- simple_slopes_x3$simple_slope - 1.96 * simple_slopes_x3$se
simple_slopes_x3$upper <- simple_slopes_x3$simple_slope + 1.96 * simple_slopes_x3$se
simple_slopes_x3$significant <- !((simple_slopes_x3$lower < 0) & (simple_slopes_x3$upper > 0))
simple_slopes_x3$p_value <- 2 * pnorm(-abs(simple_slopes_x3$simple_slope / simple_slopes_x3$se))
sink("output/simple_slopes.txt")
cat("=== Simple Slopes Analysis ===\n\n")
cat("--- Effect of x1 at Different Levels of x2 ---\n")
cat("(x1 x x2 continuous interaction)\n\n")
ss_x2_print <- simple_slopes_x2
ss_x2_print[, 3:7] <- round(ss_x2_print[, 3:7], 4)
print(ss_x2_print, row.names = FALSE)
cat("\n\n--- Effect of x1 by x3 Group ---\n")
cat("(x1 x x3 categorical interaction)\n\n")
ss_x3_print <- simple_slopes_x3
ss_x3_print[, 2:6] <- round(ss_x3_print[, 2:6], 4)
print(ss_x3_print, row.names = FALSE)
cat("\n\nInterpretation:\n")
cat("- At low x2 (-1.5), effect of x1 is:", round(simple_slopes_x2$simple_slope[1], 3), "\n")
cat("- At high x2 (+1.5), effect of x1 is:", round(simple_slopes_x2$simple_slope[3], 3), "\n")
cat("- For x3=0, effect of x1 is:", round(simple_slopes_x3$simple_slope[1], 3), "\n")
cat("- For x3=1, effect of x1 is:", round(simple_slopes_x3$simple_slope[2], 3), "\n")
sink()
# Figure 8.6: Simple slopes for x1 x x2
fig8_6 <- ggplot(simple_slopes_x2, aes(x = x2_label, y = simple_slope,
                                       ymin = lower, ymax = upper,
                                       shape = significant)) +
  geom_pointrange(size = 1, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("Not Significant", "Significant")) +
  labs(title = "Simple Slopes: Effect of x1 at Different x2 Levels",
       subtitle = "Error bars show 95% confidence intervals",
       x = "x2 Level", y = "Simple Slope (effect of x1 on log-odds)",
       shape = "Significance") +
  theme(legend.position = "bottom")
ggsave("figures/fig8_6_simple_slopes_x2.jpeg", fig8_6, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_6_simple_slopes_x2.jpeg\n")
# Figure 8.7: Simple slopes for x1 x x3
fig8_7 <- ggplot(simple_slopes_x3, aes(x = x3_value, y = simple_slope,
                                       ymin = lower, ymax = upper,
                                       shape = significant)) +
  geom_pointrange(size = 1.2, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("Not Significant", "Significant")) +
  scale_x_discrete(labels = c("x3 = 0", "x3 = 1")) +
  labs(title = "Simple Slopes: Effect of x1 by x3 Group",
       subtitle = "Larger effect for x3 = 1 demonstrates the interaction",
       x = "x3 Group", y = "Simple Slope (effect of x1 on log-odds)",
       shape = "Significance") +
  theme(legend.position = "bottom")
ggsave("figures/fig8_7_simple_slopes_x3.jpeg", fig8_7, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_7_simple_slopes_x3.jpeg\n")
# ============================================================================
# Section 8.10: Johnson-Neyman Technique
# ============================================================================
cat("Performing Johnson-Neyman analysis...\n")
# Calculate effect of x1 across range of x2
jn_x2 <- seq(min(interact_data$x2) - 0.5, max(interact_data$x2) + 0.5, by = 0.02)
jn_effect <- coefs["x1"] + coefs["x1:x2"] * jn_x2
jn_se <- sqrt(var_b1 + jn_x2^2 * var_b3 + 2 * jn_x2 * cov_b1b3)
jn_lower <- jn_effect - 1.96 * jn_se
jn_upper <- jn_effect + 1.96 * jn_se
jn_data <- data.frame(
  x2 = jn_x2,
  effect = jn_effect,
  se = jn_se,
  lower = jn_lower,
  upper = jn_upper,
  significant = !((jn_lower < 0) & (jn_upper > 0))
)
# Find Johnson-Neyman boundaries (where CI crosses zero)
find_jn_boundary <- function(x2_val) {
  effect <- coefs["x1"] + coefs["x1:x2"] * x2_val
  se <- sqrt(var_b1 + x2_val^2 * var_b3 + 2 * x2_val * cov_b1b3)
  return(abs(effect) - 1.96 * se)
}
# Find where the significance changes
sig_changes <- which(diff(jn_data$significant) != 0)
jn_boundaries <- jn_data$x2[sig_changes + 1]
sink("output/johnson_neyman.txt")
cat("=== Johnson-Neyman Analysis ===\n\n")
cat("This analysis identifies regions of x2 where the effect of x1 is significant.\n\n")
if (length(jn_boundaries) > 0) {
  cat("Johnson-Neyman boundaries:\n")
  for (i in seq_along(jn_boundaries)) {
    cat("  x2 =", round(jn_boundaries[i], 3), "\n")
  }
  cat("\nInterpretation:\n")
  if (length(jn_boundaries) == 1) {
    if (jn_data$significant[1]) {
      cat("  Effect of x1 is significant when x2 <", round(jn_boundaries[1], 2), "\n")
    } else {
      cat("  Effect of x1 is significant when x2 >", round(jn_boundaries[1], 2), "\n")
    }
  } else if (length(jn_boundaries) == 2) {
    cat("  Effect of x1 is significant when x2 <", round(jn_boundaries[1], 2),
        "or x2 >", round(jn_boundaries[2], 2), "\n")
  }
} else {
  if (all(jn_data$significant)) {
    cat("Effect of x1 is significant across the entire range of x2.\n")
  } else {
    cat("Effect of x1 is not significant for any value of x2 in the observed range.\n")
  }
}
cat("\nData range of x2:", round(min(interact_data$x2), 2), "to",
    round(max(interact_data$x2), 2), "\n")
cat("Proportion of x2 range where effect is significant:",
    round(mean(jn_data$significant[jn_data$x2 >= min(interact_data$x2) &
                                     jn_data$x2 <= max(interact_data$x2)]) * 100, 1), "%\n")
sink()
# Figure 8.8: Johnson-Neyman plot
fig8_8 <- ggplot(jn_data, aes(x = x2, y = effect)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey75", alpha = 0.5) +
  geom_line(linewidth = 1, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = jn_boundaries, linetype = "dotted", colour = "black",
             linewidth = 1) +
  geom_rug(data = interact_data, aes(x = x2, y = NULL), sides = "b", alpha = 0.3) +
  labs(title = "Johnson-Neyman Plot: Regions of Significance",
       subtitle = "Shaded region shows 95% CI; dotted lines mark boundaries where significance changes",
       x = "x2 (Moderator)", y = "Effect of x1 on Log-Odds") +
  annotate("text", x = min(jn_data$x2), y = max(jn_data$effect) * 0.9,
           label = "Regions where CI excludes zero\nindicate significant effect of x1",
           hjust = 0, size = 3, colour = "grey40")
ggsave("figures/fig8_8_johnson_neyman.jpeg", fig8_8, width = 9, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_8_johnson_neyman.jpeg\n")
# ============================================================================
# Section 8.11: Interpreting on Different Scales
# ============================================================================
cat("Computing effects on different scales...\n")
# Log-odds scale (coefficients)
logodds_effects <- data.frame(
  Scenario = c("Base (x2=0, x3=0)",
               "x2=1, x3=0",
               "x2=0, x3=1",
               "x2=1, x3=1"),
  Effect_x1_logodds = c(
    coefs["x1"],
    coefs["x1"] + coefs["x1:x2"] * 1,
    coefs["x1"] + coefs["x1:x31"],
    coefs["x1"] + coefs["x1:x2"] * 1 + coefs["x1:x31"]
  )
)
# Convert to odds ratios
logodds_effects$OR_per_unit_x1 <- exp(logodds_effects$Effect_x1_logodds)
sink("output/interpretation_scales.txt")
cat("=== Interpreting Interactions on Different Scales ===\n\n")
cat("Effect of a 1-unit increase in x1 at different covariate values:\n\n")
cat("Log-odds Scale:\n")
for (i in 1:nrow(logodds_effects)) {
  cat("  ", logodds_effects$Scenario[i], ": ",
      round(logodds_effects$Effect_x1_logodds[i], 3), "\n", sep = "")
}
cat("\nOdds Ratio Scale:\n")
for (i in 1:nrow(logodds_effects)) {
  cat("  ", logodds_effects$Scenario[i], ": ",
      round(logodds_effects$OR_per_unit_x1[i], 3), "\n", sep = "")
}
cat("\nInterpretation:\n")
cat("- At baseline (x2=0, x3=0), a 1-unit increase in x1 multiplies odds by",
    round(logodds_effects$OR_per_unit_x1[1], 2), "\n")
cat("- When x3=1 (and x2=0), same increase multiplies odds by",
    round(logodds_effects$OR_per_unit_x1[3], 2), "\n")
cat("- The x1 effect is", round(logodds_effects$OR_per_unit_x1[3] / logodds_effects$OR_per_unit_x1[1], 2),
    "times stronger when x3=1 vs x3=0\n")
sink()
# ============================================================================
# Section 8.12: Real Data Example - Treatment Effect Modification
# ============================================================================
cat("Creating real data example...\n")
set.seed(456)
n_real <- 800
# Simulate clinical trial data
age <- round(rnorm(n_real, mean = 55, sd = 12))
age <- pmax(25, pmin(age, 85))
treatment <- factor(rbinom(n_real, 1, 0.5), labels = c("Control", "Treatment"))
severity <- round(rnorm(n_real, mean = 50, sd = 15))
severity <- pmax(10, pmin(severity, 100))
# True model: treatment effect is modified by severity
treatment_num <- as.numeric(treatment == "Treatment")
severity_centered <- severity - 50
linear_pred_real <- -1.5 + 0.02 * (age - 55) + 0.8 * treatment_num +
  0.015 * severity_centered + 0.03 * treatment_num * severity_centered
prob_real <- plogis(linear_pred_real)
response <- rbinom(n_real, 1, prob_real)
real_data <- data.frame(
  response = response,
  age = age,
  treatment = treatment,
  severity = severity,
  severity_centered = severity_centered
)
# Fit models
real_main <- glm(response ~ age + treatment + severity, family = binomial, data = real_data)
real_interact <- glm(response ~ age + treatment * severity, family = binomial, data = real_data)
# Test interaction
lrt_real <- anova(real_main, real_interact, test = "Chisq")
sink("output/real_data_analysis.txt")
cat("=== Real Data Example: Treatment Effect Modification ===\n\n")
cat("Scenario: Clinical trial examining treatment effect\n")
cat("Research question: Does treatment effectiveness vary by disease severity?\n\n")
cat("Sample size:", n_real, "\n")
cat("Response rate:", round(mean(response) * 100, 1), "%\n")
cat("Treatment allocation:", sum(treatment == "Treatment"), "Treatment,",
    sum(treatment == "Control"), "Control\n\n")
cat("--- Main Effects Model ---\n")
print(summary(real_main))
cat("\n--- Interaction Model ---\n")
print(summary(real_interact))
cat("\n--- Likelihood Ratio Test for Interaction ---\n")
cat("Deviance reduction:", round(lrt_real$Deviance[2], 2), "\n")
cat("p-value:", format(lrt_real$`Pr(>Chi)`[2], digits = 4), "\n")
cat("Interaction significant:", ifelse(lrt_real$`Pr(>Chi)`[2] < 0.05, "YES", "NO"), "\n")
cat("\nAIC comparison:\n")
cat("  Main effects model:", round(AIC(real_main), 2), "\n")
cat("  Interaction model:", round(AIC(real_interact), 2), "\n")
cat("  Difference:", round(AIC(real_main) - AIC(real_interact), 2), "\n")
sink()
# Figure 8.9: Real data interaction visualization
severity_levels <- quantile(real_data$severity, probs = c(0.25, 0.5, 0.75))
pred_real <- expand.grid(
  age = mean(real_data$age),
  treatment = levels(real_data$treatment),
  severity = seq(min(real_data$severity), max(real_data$severity), length.out = 100)
)
pred_real$pred <- predict(real_interact, newdata = pred_real, type = "response")
fig8_9 <- ggplot(pred_real, aes(x = severity, y = pred, linetype = treatment)) +
  geom_line(linewidth = 1.2, colour = "black") +
  scale_linetype_manual(values = c("Control" = "solid", "Treatment" = "dashed")) +
  labs(title = "Treatment Effect Modification by Severity",
       subtitle = "Treatment benefit increases with disease severity",
       x = "Disease Severity Score", y = "Predicted Probability of Response",
       linetype = "Group") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))
ggsave("figures/fig8_9_real_data_interaction.jpeg", fig8_9, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_9_real_data_interaction.jpeg\n")
# Simple slopes for real data
real_coefs <- coef(real_interact)
real_vcov <- vcov(real_interact)
# Effect of treatment at different severity levels
sev_vals <- c(25, 50, 75)
real_simple_slopes <- data.frame(
  Severity = sev_vals,
  Treatment_Effect = real_coefs["treatmentTreatment"] +
    real_coefs["treatmentTreatment:severity"] * sev_vals
)
# Recalculate with proper variance
var_trt <- real_vcov["treatmentTreatment", "treatmentTreatment"]
var_int <- real_vcov["treatmentTreatment:severity", "treatmentTreatment:severity"]
cov_trt_int <- real_vcov["treatmentTreatment", "treatmentTreatment:severity"]
real_simple_slopes$se <- sqrt(var_trt + sev_vals^2 * var_int + 2 * sev_vals * cov_trt_int)
real_simple_slopes$lower <- real_simple_slopes$Treatment_Effect - 1.96 * real_simple_slopes$se
real_simple_slopes$upper <- real_simple_slopes$Treatment_Effect + 1.96 * real_simple_slopes$se
real_simple_slopes$OR <- exp(real_simple_slopes$Treatment_Effect)
real_simple_slopes$OR_lower <- exp(real_simple_slopes$lower)
real_simple_slopes$OR_upper <- exp(real_simple_slopes$upper)
real_simple_slopes$significant <- !((real_simple_slopes$lower < 0) & (real_simple_slopes$upper > 0))
sink("output/real_data_simple_slopes.txt")
cat("=== Simple Slopes: Treatment Effect by Severity ===\n\n")
real_ss_print <- real_simple_slopes
real_ss_print[, 2:8] <- round(real_ss_print[, 2:8], 3)
print(real_ss_print, row.names = FALSE)
cat("\nInterpretation:\n")
cat("- At low severity (25), treatment effect (log-odds):",
    round(real_simple_slopes$Treatment_Effect[1], 3),
    ", OR:", round(real_simple_slopes$OR[1], 2), "\n")
cat("- At medium severity (50), treatment effect:",
    round(real_simple_slopes$Treatment_Effect[2], 3),
    ", OR:", round(real_simple_slopes$OR[2], 2), "\n")
cat("- At high severity (75), treatment effect:",
    round(real_simple_slopes$Treatment_Effect[3], 3),
    ", OR:", round(real_simple_slopes$OR[3], 2), "\n")
sink()
# Figure 8.10: Simple slopes for real data
fig8_10 <- ggplot(real_simple_slopes, aes(x = factor(Severity), y = OR,
                                          ymin = OR_lower, ymax = OR_upper)) +
  geom_pointrange(size = 1, colour = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  labs(title = "Treatment Odds Ratio by Disease Severity",
       subtitle = "Error bars show 95% CI; treatment more effective at higher severity",
       x = "Disease Severity Score", y = "Odds Ratio (Treatment vs Control)") +
  scale_y_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8))
ggsave("figures/fig8_10_real_data_or.jpeg", fig8_10, width = 7, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_10_real_data_or.jpeg\n")
# ============================================================================
# Section 8.13: Cross-Validation for Interaction Models
# ============================================================================
cat("Performing cross-validation...\n")
set.seed(789)
k <- 10
folds <- sample(rep(1:k, length.out = n))
cv_results <- data.frame(
  Fold = integer(),
  Model = character(),
  AUC = numeric(),
  Brier = numeric()
)
for (fold in 1:k) {
  train_idx <- which(folds != fold)
  test_idx <- which(folds == fold)
  train_data <- interact_data[train_idx, ]
  test_data <- interact_data[test_idx, ]
  # Fit models
  cv_main <- glm(y ~ x1 + x2 + x3, family = binomial, data = train_data)
  cv_true <- glm(y ~ x1 + x2 + x3 + x1:x2 + x1:x3, family = binomial, data = train_data)
  cv_all <- glm(y ~ x1 * x2 + x1 * x3 + x2 * x3, family = binomial, data = train_data)
  # Predictions
  pred_main <- predict(cv_main, newdata = test_data, type = "response")
  pred_true <- predict(cv_true, newdata = test_data, type = "response")
  pred_all <- predict(cv_all, newdata = test_data, type = "response")
  y_test <- test_data$y
  # Metrics
  cv_results <- rbind(cv_results, data.frame(
    Fold = fold,
    Model = c("Main Effects", "True Interactions", "All Interactions"),
    AUC = c(as.numeric(auc(roc(y_test, pred_main, quiet = TRUE))),
            as.numeric(auc(roc(y_test, pred_true, quiet = TRUE))),
            as.numeric(auc(roc(y_test, pred_all, quiet = TRUE)))),
    Brier = c(mean((pred_main - y_test)^2),
              mean((pred_true - y_test)^2),
              mean((pred_all - y_test)^2))
  ))
}
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
print(as.data.frame(cv_summary), row.names = FALSE)
cat("\nInterpretation:\n")
cat("- Including true interactions improves AUC from",
    round(cv_summary$Mean_AUC[cv_summary$Model == "Main Effects"], 3), "to",
    round(cv_summary$Mean_AUC[cv_summary$Model == "True Interactions"], 3), "\n")
cat("- Overfitting with all interactions shows slight degradation\n")
sink()
# Figure 8.11: CV AUC comparison
cv_results$Model <- factor(cv_results$Model,
                           levels = c("Main Effects", "True Interactions", "All Interactions"))
fig8_11 <- ggplot(cv_results, aes(x = Model, y = AUC, fill = Model)) +
  geom_boxplot(alpha = 0.7, colour = "black") +
  scale_fill_manual(values = c("Main Effects" = "grey80",
                               "True Interactions" = "grey50",
                               "All Interactions" = "grey25")) +
  labs(title = "Cross-Validated AUC by Model",
       subtitle = paste0(k, "-fold cross-validation"),
       x = "Model", y = "AUC") +
  theme(legend.position = "none")
ggsave("figures/fig8_11_cv_auc.jpeg", fig8_11, width = 8, height = 5,
       dpi = 300, device = "jpeg")
cat("Saved: figures/fig8_11_cv_auc.jpeg\n")
# ============================================================================
# Section 8.14: Summary
# ============================================================================
sink("output/chapter8_summary.txt")
cat("=== Chapter 8 Summary: Interaction Effects ===\n\n")
cat("SIMULATED DATA RESULTS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("True model: x1 x x2 (coef=0.8) and x1 x x3 (coef=1.5) interactions\n\n")
cat("Likelihood Ratio Tests:\n")
cat("  x1 x x2: p =", format(lrt_x1x2$`Pr(>Chi)`[2], digits = 4),
    ifelse(lrt_x1x2$`Pr(>Chi)`[2] < 0.05, "(significant)", "(not significant)"), "\n")
cat("  x1 x x3: p =", format(lrt_x1x3$`Pr(>Chi)`[2], digits = 4),
    ifelse(lrt_x1x3$`Pr(>Chi)`[2] < 0.05, "(significant)", "(not significant)"), "\n")
cat("  x2 x x3: p =", format(lrt_x2x3$`Pr(>Chi)`[2], digits = 4),
    ifelse(lrt_x2x3$`Pr(>Chi)`[2] < 0.05, "(significant)", "(not significant)"), "\n\n")
cat("AIC Comparison:\n")
cat("  Main effects only:", round(AIC(main_model), 2), "\n")
cat("  True model (x1 x x2 + x1 x x3):", round(AIC(model_true), 2), "\n")
cat("  All interactions:", round(AIC(model_all), 2), "\n\n")
cat("Simple Slopes (x1 effect by x3 group):\n")
cat("  x3 = 0:", round(simple_slopes_x3$simple_slope[1], 3), "\n")
cat("  x3 = 1:", round(simple_slopes_x3$simple_slope[2], 3), "\n")
cat("  Difference (interaction):", round(diff(simple_slopes_x3$simple_slope), 3), "\n\n")
cat("Cross-Validation AUC:\n")
for (i in 1:nrow(cv_summary)) {
  cat("  ", cv_summary$Model[i], ":", round(cv_summary$Mean_AUC[i], 4),
      "(SD:", round(cv_summary$SD_AUC[i], 4), ")\n")
}
cat("\n\nREAL DATA RESULTS:\n")
cat("-" , rep("-", 50), "\n", sep = "")
cat("Treatment x Severity interaction p-value:",
    format(lrt_real$`Pr(>Chi)`[2], digits = 4), "\n")
cat("AIC improvement:", round(AIC(real_main) - AIC(real_interact), 2), "\n")
sink()
# ============================================================================
# Completion message
# ============================================================================
cat("\n========================================\n")
cat("Chapter 8 R code execution complete!\n")
cat("========================================\n\n")
cat("Figures saved to 'figures/' directory:\n")
cat("  - fig8_1_stratified_relationship.jpeg\n")
cat("  - fig8_2_aic_comparison.jpeg\n")
cat("  - fig8_3_cont_cont_interaction.jpeg\n")
cat("  - fig8_4_probability_surface.jpeg\n")
cat("  - fig8_5_cont_cat_interaction.jpeg\n")
cat("  - fig8_6_simple_slopes_x2.jpeg\n")
cat("  - fig8_7_simple_slopes_x3.jpeg\n")
cat("  - fig8_8_johnson_neyman.jpeg\n")
cat("  - fig8_9_real_data_interaction.jpeg\n")
cat("  - fig8_10_real_data_or.jpeg\n")
cat("  - fig8_11_cv_auc.jpeg\n\n")
cat("Output files saved to 'output/' directory:\n")
cat("  - data_summary.txt\n")
cat("  - main_effects_model.txt\n")
cat("  - stratified_analysis.txt\n")
cat("  - interaction_tests.txt\n")
cat("  - model_comparison.txt\n")
cat("  - interaction_model.txt\n")
cat("  - simple_slopes.txt\n")
cat("  - johnson_neyman.txt\n")
cat("  - interpretation_scales.txt\n")
cat("  - real_data_analysis.txt\n")
cat("  - real_data_simple_slopes.txt\n")
cat("  - cv_results.txt\n")
cat("  - chapter8_summary.txt\n")
