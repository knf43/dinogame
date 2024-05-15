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

# Read the Excel file into a data frame
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

#normalize lex score
dino_data_filtered$lex_score_norm <- scale(dino_data_filtered$lex_score)

# Convert wm_score to numeric
dino_data_filtered$wm_score_num <- as.numeric(as.character(dino_data_filtered$wm_score))

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

# Base plot - Model 1

rt_plot_1 <- gam(transformed_rt_ms ~ Trial + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_plot_1)

# Model 2: Add Stress
model_2_linear <- gam(transformed_rt_ms ~ Trial + Stress +
                        s(ID, bs = "re") + s(sent_id, bs = "re"),
                      data = accurate_data, method = "REML")

summary(model_2_linear)

# Compare the new linear model to the previous non-linear model
anova(rt_plot_1, model_2_linear, test = "Chisq")

# Model 3: Add syllable_struct
rt_plot_3 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_plot_3)

anova(model_2_linear, rt_plot_3, test = "Chisq")

# Plot
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

# Model 4: Add lex_score_norm
rt_plot_4 <- gam(transformed_rt_ms ~ Trial + Stress + syllable_struct + lex_score_norm + s(ID, bs = "re") + s(sent_id, bs = "re"), data = accurate_data, method = "REML")

summary(rt_plot_4)

anova(rt_plot_3, rt_plot_4, test = "Chisq")

# Model 5: Add wm_score_num
rt_plot_5 <- gam(transformed_rt_ms ~ s(Trial) + Stress + syllable_struct + lex_score_norm + wm_score_num + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                 data = accurate_data, method = "REML")

summary(rt_plot_5)

anova(rt_plot_4, rt_plot_5, test = "Chisq")

# Model 6: Add scaled_phono_freq
rt_plot_6 <- gam(transformed_rt_ms ~ s(Trial) + Stress + syllable_struct + lex_score_norm + wm_score_num + scaled_phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                 data = accurate_data, method = "REML")

summary(rt_plot_6)

anova(rt_plot_5, rt_plot_6, test = "Chisq")

# Model 7: Interaction Trial:Stress
rt_plot_7 <- gam(transformed_rt_ms ~ s(Trial, by = Stress) + Stress + syllable_struct + lex_score_norm + wm_score_num + scaled_phono_freq + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                 data = accurate_data, method = "REML")


summary(rt_plot_7)

anova(rt_plot_6, rt_plot_7, test = "Chisq")

# Model 8: Interaction Trial: syllable_struct
rt_plot_8 <- gam(transformed_rt_ms ~ 
                   s(Trial, by = syllable_struct) + 
                   s(Trial, by = Stress) + 
                   Stress +
                   syllable_struct + 
                   lex_score_norm + 
                   wm_score_num + 
                   scaled_phono_freq + 
                   s(ID, bs = "re") + 
                   s(sent_id, bs = "re"), 
                 data = accurate_data, method = "REML")

summary(rt_plot_8)

anova(rt_plot_7, rt_plot_8, test = "Chisq")

# Model 9: Interaction Trial: lex_score_norm
rt_plot_9 <- gam(transformed_rt_ms ~ 
                   s(Trial, by = lex_score_norm) + 
                   s(Trial, by = syllable_struct) + 
                   s(Trial, by = Stress) + 
                   Stress + 
                   syllable_struct + 
                   lex_score_norm + 
                   wm_score_num + 
                   scaled_phono_freq + 
                   s(ID, bs = "re") + 
                   s(sent_id, bs = "re"), 
                 data = accurate_data, method = "REML")

summary(rt_plot_9)

anova(rt_plot_8, rt_plot_9, test = "Chisq")

# Model 10: Interaction Trial: wm_score_num 
rt_plot_10 <- gam(transformed_rt_ms ~ 
                    s(Trial, by = wm_score_num) + 
                    s(Trial, by = lex_score_norm) + 
                    s(Trial, by = syllable_struct) + 
                    s(Trial, by = Stress) + 
                    Stress + 
                    syllable_struct + 
                    lex_score_norm + 
                    wm_score_num + 
                    scaled_phono_freq + 
                    s(ID, bs = "re") + 
                    s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML")

summary(rt_plot_10)

anova(rt_plot_9, rt_plot_10, test = "Chisq")

# Model 11: Interaction Trial: scaled_phono_freq
rt_plot_11 <- gam(transformed_rt_ms ~ 
                    s(Trial, by = scaled_phono_freq) + 
                    s(Trial, by = wm_score_num) + 
                    s(Trial, by = lex_score_norm) + 
                    s(Trial, by = syllable_struct) + 
                    s(Trial, by = Stress) + 
                    Stress + 
                    syllable_struct + 
                    lex_score + 
                    wm_score_num + 
                    scaled_phono_freq + 
                    s(ID, bs = "re") + 
                    s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML")

summary(rt_plot_11)

anova(rt_plot_10, rt_plot_11, test = "Chisq")

# Accuracy GAMs
str(dino_data_filtered)

# Base plot - Model 1
acc_plot_1 <- gam(Accuracy ~ Trial + s(ID, bs = "re") + s(sent_id, bs = "re"), data = dino_data_filtered, method = "REML", family = binomial)

# Summarize the model
summary(acc_plot_1)

acc_plot_1_nonlinear <- gam(Accuracy ~ s(Trial) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = dino_data_filtered, method = "REML", family = binomial)

# Summarize the model
summary(acc_plot_1_nonlinear)

# Model 2: Add Stress
acc_plot_2 <- gam(Accuracy ~ s(Trial) + s(Stress) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_2)

anova(acc_plot_1, acc_plot_2, test = "Chisq")

# Model 3: Add syllable_struct

acc_plot_3 <- gam(Accuracy ~ s(Trial) + s(Stress) + s(syllable_struct) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_3)

anova(acc_plot_2, acc_plot_3, test = "Chisq")

# Model 4: Add lex_score_norm

acc_plot_4 <- gam(Accuracy ~ s(Trial) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_4)

anova(acc_plot_3, acc_plot_4, test = "Chisq")

# Model 5: Add wm_score_num

acc_plot_5 <- gam(Accuracy ~ s(Trial) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_5)

anova(acc_plot_4, acc_plot_5, test = "Chisq")

# Model 6: Add scaled_phono_freq
acc_plot_6 <- gam(Accuracy ~ s(Trial) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_6)

anova(acc_plot_5, acc_plot_6, test = "Chisq")


# Model 7: Interaction Trial:Stress
acc_plot_7 <- gam(Accuracy ~ s(Trial, by = Stress) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_7)

anova(acc_plot_6, acc_plot_7, test = "Chisq")


# Model 8: Interaction Trial: syllable_struct
acc_plot_8 <- gam(Accuracy ~ s(Trial, by = syllable_struct) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_8)

anova(acc_plot_7, acc_plot_8, test = "Chisq")


# Model 9: Interaction Trial:lex_score_norm
acc_plot_9 <- gam(Accuracy ~ s(Trial, by = lex_score_norm) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                  data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_9)

anova(acc_plot_8, acc_plot_9, test = "Chisq")


# Model 10: Interaction Trial:wm_score_num
acc_plot_10 <- gam(Accuracy ~ s(Trial, by = wm_score_num, k = 5) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                   data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_10)

anova(acc_plot_9, acc_plot_10, test = "Chisq")


# Model 11: Interaction Trial: scaled_phono_freq

acc_plot_11 <- gam(Accuracy ~ s(Trial, by = scaled_phono_freq) + s(Stress) + s(syllable_struct) + s(lex_score_norm) + s(wm_score_num, k = 5) + s(scaled_phono_freq) + s(ID, bs = "re") + s(sent_id, bs = "re"), 
                   data = accurate_data, method = "REML", family = binomial)

summary(acc_plot_11)

anova(acc_plot_10, acc_plot_11, test = "Chisq")


