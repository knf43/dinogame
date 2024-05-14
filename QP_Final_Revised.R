# Load the necessary library
library(readr)
library(readxl)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(car)
library(lme4)
library(influence.ME)
library(mgcv)
library(tidyverse)
library(tidygam)
library(MASS) 
library(lmtest)
library(gratia)

# Use file.choose() to open a dialog box for file selection
file_path <- file.choose()

# Read the CSV file into a data frame
dino_data <- read_excel(file_path)

head(dino_data)
names(dino_data)
str(dino_data)

# Renaming variables and ensuring correct data types
dino_data <- dino_data %>%
  rename(
    lex_score = `Proficiency (LexTale Score)`,
    syllable_struct = Syllabic_Structure,
    sent_id = Sentence_ID,
    rt_ms = RTs_MS,
    wm_score = WM_Score,
    phono_freq = Phonotactic_Frequency
  )

dino_data <- dino_data %>%
  mutate(
    ID = as.factor(ID),
    Stress = as.factor(Stress),
    syllable_struct = as.factor(syllable_struct),
    sent_id = as.factor(sent_id),
    Accuracy = as.factor(Accuracy),
    wm_score = as.factor(wm_score), 
    Level = as.numeric(Level),
    Trial = as.numeric(Trial),
    lex_score = as.numeric(lex_score),
    rt_ms = as.numeric(rt_ms),
    phono_freq = as.numeric(phono_freq)
  )

str(dino_data)

# Filter data above 2000 ms 

dino_data_filtered <- dino_data %>%
  filter(rt_ms > 0 & rt_ms <= 2000)

dino_data_filtered <- dino_data_filtered %>%
  group_by(ID) %>%
  mutate(cumulative_rt_ms = cumsum(rt_ms)) %>%
  ungroup()

str(dino_data_filtered)

# Descriptive statistics 

accuracy_summary <- dino_data_filtered %>%
  group_by(Level, Stress, syllable_struct) %>%
  summarize(Accuracy_Percentage = mean(as.numeric(Accuracy == "TRUE")), .groups = "drop")

RT_summary <- dino_data_filtered %>%
  group_by(Level, Stress, syllable_struct) %>%
  summarize(Mean_RT = mean(rt_ms, na.rm = TRUE), .groups = "drop")

# Descriptive plots

dino_data_filtered <- dino_data_filtered %>%
  filter(Level <= 12) 

# Convert Accuracy to a numeric value 
dino_data_filtered$Accuracy_numeric <- as.numeric(dino_data_filtered$Accuracy_binary)

# Accuracy and RT descriptive plots
ggplot(dino_data_filtered, aes(x = as.factor(Level), y = Accuracy_numeric)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "blue") +
  labs(x = "Level", y = "Average Accuracy", title = "Change in Accuracy Across Levels") +
  theme_minimal()

ggplot(dino_data_filtered, aes(x = as.factor(Level), y = Accuracy_numeric, group = 1)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "blue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed", alpha = 0.5, color = "blue") +  # Dashed and semi-transparent line
  labs(x = "Level", y = "Average Accuracy", title = "Change in Accuracy Across Levels") +
  theme_minimal()

ggplot(dino_data_filtered, aes(x = as.factor(Level), y = rt_ms, group = 1)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "blue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed", alpha = 0.5, color = "blue") +  # Dashed and semi-transparent line
  labs(x = "Level", y = "Average Reaction Time (ms)", title = "Change in Reaction Time Across Levels") +
  theme_minimal()

# Prepare a combined factor for faceting
dino_data_filtered$item_type <- interaction(dino_data_filtered$syllable_struct, dino_data_filtered$Stress)

# Reaction Time
ggplot(dino_data_filtered, aes(x = as.factor(Level), y = rt_ms, group = 1)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "blue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed", alpha = 0.5, color = "blue") +
  facet_wrap(~ item_type, scales = "free_y") +
  labs(x = "Level", y = "Average Reaction Time (ms)", title = "Reaction Time by Item Type Across Levels") +
  theme_minimal()

# Accuracy
ggplot(dino_data_filtered, aes(x = as.factor(Level), y = Accuracy_binary, group = 1)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", color = "blue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.y = mean, geom = "line", linetype = "dashed", alpha = 0.5, color = "blue") +
  facet_wrap(~ item_type, scales = "free_y") +
  labs(x = "Level", y = "Average Accuracy", title = "Accuracy by Item Type Across Levels") +
  theme_minimal()

# Transform data for models

# Log transform right-skewed cumulative RT
dino_data_filtered$log_cumulative_rt_ms <- log1p(dino_data_filtered$cumulative_rt_ms)

# Scale phonotactic frequency
dino_data_filtered$scaled_phono_freq <- scale(dino_data_filtered$phono_freq)

# Log transform rt_ms for slight right skewness
dino_data_filtered$transformed_rt_ms <- log(dino_data_filtered$rt_ms + 1)

str(dino_data_filtered)

#RT Models

# Filter accurate data - new dataset 
accurate_data <- dino_data_filtered %>%
  filter(Accuracy == "TRUE")

# Verify data only contains accurate responses 
str(accurate_data)

# View the first few rows of the new dataset to confirm the change
head(accurate_data)

#GAM FOR RTs

# Basic model with just the cumulative RT as a smooth term
model_1 <- gam(transformed_rt_ms ~ s(log_cumulative_rt_ms) +
                 s(ID, bs = "re") + s(sent_id, bs = "re"),
               , data = accurate_data, method = "REML")

summary(model_1)

gam_1_preds <- predict_gam(model_1)
plot(gam_1_preds, series = "log_cumulative_rt_ms")

# Create Model 1 Plot

model_plot_1 <- gam(transformed_rt_ms ~ s(log_cumulative_rt_ms), data = accurate_data, method = "REML")

summary(model_plot_1)

# Create a prediction data frame
pred_data <- data.frame(log_cumulative_rt_ms = seq(from = min(accurate_data$log_cumulative_rt_ms),
                                                   to = max(accurate_data$log_cumulative_rt_ms), length.out = 100))

# Predict transformed RT and convert back to original scale
pred_data$predicted_log_rt_ms <- predict(model_plot_1, newdata = pred_data, type = "response")
pred_data$predicted_rt_ms <- exp(pred_data$predicted_log_rt_ms) - 1  # Back-transform

# Adjust pred_data to include a 'minutes' column for the log_cumulative_rt_ms
pred_data$minutes = exp(pred_data$log_cumulative_rt_ms) / 60000  # Convert ms to minutes

minute_breaks <- pretty(pred_data$minutes, n = 5)  
labels_minutes <- sapply(minute_breaks, function(x) format(round(x, 2), nsmall = 2)) 

ggplot(pred_data, aes(x = minutes, y = predicted_rt_ms)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "skyblue") +
  scale_x_continuous(name = "Log of Cumulative RT (minutes)",
                     labels = scales::comma,  # Ensures nice formatting of numbers
                     breaks = pretty(pred_data$minutes, n = 5)) +
  labs(y = "Predicted Reaction Time (ms)",
       title = "Predicted Reaction Time vs. Log of Cumulative RT",
       subtitle = "Smoothed trend line with confidence interval") +
  theme_minimal()

#Alternative plots with level and trial as predictors
# Original Model for reference
model_plot_1 <- gam(transformed_rt_ms ~ s(log_cumulative_rt_ms), data = accurate_data, method = "REML")
summary(model_plot_1)

gam_1_model_preds <- predict_gam(model_plot_1)
plot(gam_1_model_preds, series = "log_cumulative_rt_ms")

# Model 2: Adding 'Level'
model_plot_2 <- gam(transformed_rt_ms ~ s(Level, bs = "re"), data = accurate_data, method = "REML")
summary(model_plot_2)

gam_2_model_preds <- predict_gam(model_plot_2)
plot(gam_2_model_preds, series = "Level")

# Model 3: Adding 'Trial'
model_plot_3 <- gam(transformed_rt_ms ~ s(Trial, bs = "re"), data = accurate_data, method = "REML")
summary(model_plot_3)

gam_3_model_preds <- predict_gam(model_plot_3)
plot(gam_3_model_preds, series = "Trial")

# Compare Model 1 with Model 2
anova_result_1_vs_2 <- anova(model_plot_1, model_plot_2, test = "Chisq")

# Compare Model 1 with Model 3
anova_result_1_vs_3 <- anova(model_plot_1, model_plot_3, test = "Chisq")

# Print the results
print(anova_result_1_vs_2)
print(anova_result_1_vs_3)c

# Predict using the GAM 3 model
# Fit a GAM with the original rt_ms as the response
model_original <- gam(rt_ms ~ s(Trial, bs = "cs"), data = accurate_data, method = "REML")

# Check the summary to understand the effect of Trial
summary(model_original)

pred_data_original <- data.frame(Trial = seq(min(accurate_data$Trial), max(accurate_data$Trial), length.out = 100))
pred_data_original$predicted_rt_ms <- predict(model_original, newdata = pred_data_original, type = "response")

ggplot(pred_data_original, aes(x = Trial, y = predicted_rt_ms)) +
  geom_line() + 
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "skyblue") +
  labs(x = "Trial", y = "Reaction Time (ms)", 
       title = "Reaction Time vs. Trial", 
       subtitle = "Trend and Smoothed Confidence Interval") +
  theme_minimal()

# Original vs transformed rt _ ms residuals

# Calculate residuals and fitted values for the original model
residuals_original <- residuals(model_original)
fitted_original <- fitted(model_original)

# Plot residuals vs. fitted values
ggplot(data = data.frame(Fitted = fitted_original, Residuals = residuals_original), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Original Model", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data = data.frame(Residuals = residuals_original), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Residuals for Original Model") +
  theme_minimal()

# Calculate residuals and fitted values for the transformed model
residuals_transformed <- residuals(model_plot_3)
fitted_transformed <- fitted(model_plot_3)

# Plot residuals vs. fitted values
ggplot(data = data.frame(Fitted = fitted_transformed, Residuals = residuals_transformed), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Transformed Model", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data = data.frame(Residuals = residuals_transformed), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Residuals for Transformed Model") +
  theme_minimal()

# For the Original Model (rt_ms)
bp_test_original <- bptest(model_original)

# For the Transformed Model (transformed_rt_ms)
bp_test_transformed <- bptest(model_plot_3)

# Output the results
print("Breusch-Pagan Test for Original Model:")
print(bp_test_original)

# original = significant heteroscedasticity 

print("Breusch-Pagan Test for Transformed Model:")
print(bp_test_transformed)

# Compare other transformations
# Fit a temporary model to use in the boxcox function
temp_model <- lm(rt_ms ~ Trial, data = accurate_data)

# Use boxcox to find the optimal lambda
lambda_opt <- boxcox(temp_model, lambda = seq(-2, 2, by = 0.1))$x[which.max(boxcox(temp_model, lambda = seq(-2, 2, by = 0.1))$y)]

# Step 2: Apply the Box-Cox transformation manually
accurate_data$bc_rt_ms <- (accurate_data$rt_ms^lambda_opt - 1) / lambda_opt

# Fit the GAM model with the Box-Cox transformed response
model_bc <- gam(bc_rt_ms ~ s(Trial, bs = "cs"), data = accurate_data, method = "REML")

# Print the summary of the Box-Cox model
summary(model_bc)

# square root transformation
accurate_data$sqrt_rt_ms <- sqrt(accurate_data$rt_ms)

# Fit models
model_sqrt <- gam(sqrt_rt_ms ~ s(Trial, bs = "cs"), data = accurate_data, method = "REML")
model_bc <- gam(bc_rt_ms ~ s(Trial, bs = "cs"), data = accurate_data, method = "REML")

# Check diagnostics for each model
summary(model_sqrt)
summary(model_bc)
summary(model_plot_3)

# Extract residuals and fitted values
data_sqrt <- data.frame(Fitted = fitted(model_sqrt), Residuals = residuals(model_sqrt), Model = "Square Root")
data_bc <- data.frame(Fitted = fitted(model_bc), Residuals = residuals(model_bc), Model = "Box-Cox")
data_log <- data.frame(Fitted = fitted(model_plot_3), Residuals = residuals(model_plot_3), Model = "Log Transformed")

# Combine data
residuals_data <- rbind(data_sqrt, data_bc, data_log)

# Create the plot
ggplot(residuals_data, aes(x = Fitted, y = Residuals, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~ Model, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values for Different Transformations",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "bottom")

#test for heteroscedasticity 
# Breusch-Pagan test for the Box-Cox Model
bp_test_bc <- bptest(model_bc)

# Breusch-Pagan test for the Log Transformed Model
bp_test_log <- bptest(model_plot_3)

# Breusch-Pagan test for the Square Root Model
bp_test_sqrt <- bptest(model_sqrt)

# Print the results
print("Breusch-Pagan Test for Box-Cox Model:")
print(bp_test_bc)

print("Breusch-Pagan Test for Log Transformed Model:")
print(bp_test_log)

print("Breusch-Pagan Test for Square Root Model:")
print(bp_test_sqrt)

# Log transformed RT_MS = most stable in terms  of residual varial consistency 
# Base plot - Model 1

rt_plot_1 <- gam(transformed_rt_ms ~ Trial + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_plot_1)

# Model 2: Keep "Trial" as a linear measure of learning, Add Stress as a non-linear term
rt_model_2 <- gam(transformed_rt_ms ~ Trial + s(Stress, bs = "fs") + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_model_2)

# Perform ANOVA to compare the models
anova_comparison <- anova(rt_plot_1, rt_model_2, test = "Chisq")

# Print the analysis of deviance table
print(anova_comparison)


# Model with Stress as a linear term
model_2_linear <- gam(transformed_rt_ms ~ Trial + Stress +
                        s(ID, bs = "re") + s(sent_id, bs = "re"),
                      data = accurate_data, method = "REML")

summary(model_2_linear)

# Compare the new linear model to the previous non-linear model
anova(rt_plot_1, model_2_linear, test = "Chisq")

# Model 3
rt_plot_3 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_plot_3)

# plot syllable structure 
new_data <- data.frame(
  Trial = mean(accurate_data$Trial),
  Stress = levels(accurate_data$Stress)[1],  # Fixing Stress to one level for simplicity
  syllable_struct = levels(accurate_data$syllable_struct),
  ID = unique(accurate_data$ID)[1],  # Assuming random effect levels are fixed
  sent_id = unique(accurate_data$sent_id)[1]  # Assuming random effect levels are fixed
)

# Generate predictions with standard errors
predictions <- predict(rt_plot_3, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- predictions$fit
new_data$se.fit <- predictions$se.fit

# Adjust y-axis limits for better visibility
ggplot(new_data, aes(x = syllable_struct, y = fit, fill = syllable_struct)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), width = 0.2, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(min(new_data$fit - 1.96 * new_data$se.fit), max(new_data$fit + 1.96 * new_data$se.fit))) +
  labs(title = "Effect of Syllable Structure on Transformed RT",
       x = "Syllable Structure",
       y = "Predicted Transformed RT") +
  theme_minimal()

# Fit the new model with syllable_struct as a parametric term and name it rt_plot_3
rt_plot_3 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Generate predictions for the original dataset
accurate_data$predicted_rt <- predict(rt_plot_3, newdata = accurate_data, type = "response")

jitter_outliers <- function(outliers, width = 0.1) {
  n <- length(outliers)
  jittered <- outliers + runif(n, -width, width)
  return(jittered)
}

# Create the box plot with jitter
ggplot(accurate_data, aes(x = syllable_struct, y = predicted_rt, fill = syllable_struct)) +
  geom_boxplot(outlier.shape = NA) +  # Remove default outliers
  geom_jitter(width = 0.2, alpha = 0.3, color = "black", size = 0.1) +  # Jittered points
  stat_summary(fun.data = function(x) {
    outliers <- boxplot.stats(x)$out
    if (length(outliers) > 0) {
      data.frame(y = jitter_outliers(outliers, width = 0.2))
    } else {
      data.frame(y = numeric(0))
    }
  }, geom = "point", color = "black", size = 1) +  # Custom jittered outliers
  labs(title = "Effect of Syllable Structure on Transformed RT",
       x = "Syllable Structure",
       y = "Predicted Transformed RT") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if not needed

violin_plot <- ggplot(accurate_data, aes(x = syllable_struct, y = predicted_rt, fill = syllable_struct)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black", size = 0.1) +  # Jittered points
  labs(title = "Effect of Syllable Structure on Transformed RT (Violin Plot)",
       x = "Syllable Structure",
       y = "Predicted Transformed RT") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if not needed

print(violin_plot)

anova(model_2_linear, rt_plot_3, test = "Chisq")

# Add lex score
rt_plot_4 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + lex_score + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_4)

# Perform ANOVA to compare rt_plot_3 and rt_plot_4
anova_comparison <- anova(rt_plot_3, rt_plot_4, test = "Chisq")

# Print the analysis of deviance table
print(anova_comparison)

# Add WM score
rt_plot_5 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + lex_score + s(wm_score_num, bs = "re") + s(Trial, bs = "re") + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_5)

anova(rt_plot_4, rt_plot_5, test = "Chisq")

# Add phono freq
rt_plot_6 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + lex_score + s(wm_score_num, bs = "re") + phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_6)

# Perform ANOVA to compare rt_plot_5 and rt_plot_6
anova_comparison <- anova(rt_plot_5, rt_plot_6, test = "Chisq")

# Print the analysis of deviance table
print(anova_comparison)

# interaction stress and trial
rt_plot_7 <- gam(transformed_rt_ms ~ s(Trial, by = Stress) + syllable_struct + lex_score + s(wm_score_num, bs = "re") + phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_7)

# Generate predictions using predict_gam
rt_plot_7_preds <- predict_gam(rt_plot_7, length_out = 25, exclude_terms = c("s(ID)", "s(sent_id)"))

str(rt_plot_7_preds)

colnames(rt_plot_7_preds)

#plot
interaction_plot <- ggplot(rt_plot_7_preds, aes(x = Trial, y = transformed_rt_ms, color = Stress)) +
  geom_line(aes(group = Stress)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Stress), alpha = 0.2) +
  labs(title = "Interaction Between Trial and Stress",
       x = "Trial",
       y = "Predicted Transformed RT") +
  theme_minimal() +
  theme(legend.position = "right")

# Display the plot
print(interaction_plot)

#Anova
anova(rt_plot_6, rt_plot_7, test = "Chisq")

# Add interaction between syllable structure and trial 
rt_plot_8 <- gam(transformed_rt_ms ~ s(Trial, by = syllable_struct) + (Trial, by = Stress) + syllable_struct + lex_score + s(wm_score_num, bs = "re") + phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_8)

#
anova(rt_plot_7, rt_plot_8, test = "Chisq")

# Add interaction between syllable structure and trial 
rt_plot_8 <- gam(transformed_rt_ms ~ s(Trial, by = syllable_struct) + (Trial, by = Stress) + syllable_struct + lex_score + s(wm_score_num, bs = "re") + phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

# Summarize the updated model
summary(rt_plot_8)
