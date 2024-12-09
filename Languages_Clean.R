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
library(tidymv)
library(emmeans)
library(patchwork)
library(gridExtra)
library(janitor)
library(skimr)

# Use file.choose() to open a dialog box for file selection
file_path <- file.choose()
file_path_2 <- file.choose()
file_path_3 <- file.choose()

# File selection
dino_data <- read_excel(file_path)
lang_use <- read_excel(file_path_2)
frequency_data <- read_excel(file_path_3)

# Initial data checks
glimpse(dino_data)
glimpse(lang_use)
glimpse(frequency_data)

# Data cleaning
# Convert Sentence_ID columns to factors
dino_data <- dino_data %>%
  mutate(Sentence_ID = as.factor(Sentence_ID))

frequency_data <- frequency_data %>%
  mutate(Sentence_ID = as.factor(Sentence_ID))

# Clean column names in frequency data
frequency_data <- frequency_data %>% clean_names()
colnames(frequency_data)

# Left join for language use and frequency data
merged_data <- dino_data %>%
  left_join(lang_use, by = "ID")

merged_data <- merged_data %>%
  left_join(
    frequency_data %>% dplyr::select(sentence_id, frq, log_frq),
    by = c("Sentence_ID" = "sentence_id")
  )

# Inspect merged_data 
glimpse(merged_data)
summary(merged_data)
str(merged_data)

# Tidy column names in merged 
merged_data <- merged_data %>%
  clean_names()

str(merged_data)

# Convert variables to correct data type
merged_data <- merged_data %>%
  mutate(
    sentence_id = as.factor(sentence_id),
    stress = as.factor(stress),
    syllabic_structure = as.factor(syllabic_structure),
    accuracy = as.numeric(accuracy),
    r_ts_ms = as.numeric(r_ts_ms),
    wm_score = as.numeric(wm_score),
    phonotactic_frequency = as.numeric(phonotactic_frequency),
    id = as.factor(id)
  )

# Save cleaned data 
write.csv(merged_data, "merged_data_cleaned.csv", row.names = FALSE)

saveRDS(merged_data, "merged_data_cleaned.rds")

# START HERE to load the cleaned data
# To load from a CSV file
merged_data <- read.csv("merged_data_cleaned.csv")

# To load from an RDS file
merged_data <- readRDS("merged_data_cleaned.rds")

# Check structure of data file
str(merged_data)

# Check distribution of continuous variables 
skim(merged_data %>% dplyr::select(r_ts_ms, wm_score, phonotactic_frequency, frq, log_frq))

# More tidying of the data
# Filter out rows where r_ts_ms > 2000
filtered_data <- merged_data %>%
  filter(r_ts_ms <= 2000)

# Histograms 
numerical_vars <- c("r_ts_ms", "wm_score", "phonotactic_frequency", "lang_use", "proficiency_lex_tale_score")

# Plots
for (var in numerical_vars) {
  print(
    ggplot(filtered_data, aes_string(x = var)) +
      geom_histogram(binwidth = ifelse(var == "r_ts_ms", 100, 
                                       ifelse(var %in% c("lang_use", "proficiency_lex_tale_score"), 0.5, 0.1)), 
                     fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
  )
}

# Correlation between Proficiency and L2 Use 
ggplot(filtered_data, aes(x = lang_use, y = proficiency_lex_tale_score)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot of L2 Use vs Lexical Score",
       x = "L2 Use", y = "Lexical Score") +
  theme_minimal()

# For non-linear relationships
cor.test(filtered_data$lang_use, filtered_data$proficiency_lex_tale_score, method = "spearman")

str(filtered_data)

# Apply transformations and scaling
filtered_data <- filtered_data %>%
  mutate(
    log_lang_use = log(lang_use + 1),                    # Log-transform L2 Use
    log_phono_freq = log(phonotactic_frequency + 1),     # Log-transform Phonotactic Frequency
    log_rt_ms = log(r_ts_ms),                            # Log-transform Reaction Times
    scaled_lex_score = scale(proficiency_lex_tale_score), # Scale Lexical Score
    scaled_wm_score = scale(wm_score),                   # Scale WM Score
    wm_score_z = (wm_score - mean(wm_score, na.rm = TRUE)) / sd(wm_score, na.rm = TRUE) # Z-score for WM Score
  )

# Verify transformations
summary(filtered_data %>% 
          dplyr::select(log_lang_use, log_phono_freq, log_rt_ms, 
                        scaled_lex_score, scaled_wm_score, wm_score_z))

str(filtered_data)

# Save filtered and transformed data set
write.csv(filtered_data, "filtered_data_transformed.csv", row.names = FALSE)

saveRDS(filtered_data, "filtered_data_transformed.rds")

# START here to open the TIDY data
# Load the transformed dataset from a CSV file
filtered_data <- read.csv("filtered_data_transformed.csv")

# OR 
# Load the transformed dataset from an RDS file
filtered_data <- readRDS("filtered_data_transformed.rds")

# Create New WM variable and New Verb type variable 
# Create a combined factor stress and syllable structure
filtered_data <- filtered_data %>%
  mutate(
    stress_syllable = interaction(stress, syllabic_structure, sep = "_", drop = TRUE)
  )

# Make sure it is a factor
filtered_data$stress_syllable <- as.factor(filtered_data$stress_syllable)

# Check levels
levels(filtered_data$stress_syllable)

# Create a new variable for WM group based on the mean
mean_wm <- mean(filtered_data$wm_score, na.rm = TRUE)

filtered_data <- filtered_data %>%
  mutate(wm_group = ifelse(wm_score > mean_wm, "high_WM", "low_WM"))

# Convert to factor for modeling
filtered_data$wm_group <- as.factor(filtered_data$wm_group)

# Verify the split
table(filtered_data$wm_group)

str(filtered_data)

# Ensure variables are the correct type
filtered_data <- filtered_data %>%
  mutate(
    id = as.factor(id),
    level = as.numeric(level),
    trial = as.numeric(trial),
    proficiency_lex_tale_score = as.numeric(proficiency_lex_tale_score),
    stress = as.factor(stress),
    syllabic_structure = as.factor(syllabic_structure),
    sentence_id = as.factor(sentence_id),
    accuracy = as.numeric(accuracy),
    r_ts_ms = as.numeric(r_ts_ms),
    wm_score = as.numeric(wm_score),
    phonotactic_frequency = as.numeric(phonotactic_frequency),
    lang_use = as.numeric(lang_use),
    frq = as.numeric(frq),
    log_frq = as.numeric(log_frq),
    log_lang_use = as.numeric(log_lang_use),
    log_phono_freq = as.numeric(log_phono_freq),
    log_rt_ms = as.numeric(log_rt_ms),
    scaled_lex_score = as.numeric(scaled_lex_score),
    scaled_wm_score = as.numeric(scaled_wm_score),
    wm_score_z = as.numeric(wm_score_z),
    stress_syllable = as.factor(stress_syllable),
    wm_group = as.factor(wm_group)
  )

# Descriptive stats
# Step 1: Aggregate data at the player level
player_stats <- filtered_data %>%
  group_by(id, level) %>%
  summarise(
    avg_wm_score = mean(wm_score, na.rm = TRUE),                 # Average WM score per player
    avg_proficiency = mean(proficiency_lex_tale_score, na.rm = TRUE), # Average proficiency score per player
    avg_language_use = mean(lang_use, na.rm = TRUE)              # Average language use per player
  )

# Step 2: Aggregate across all players for each level
level_stats <- player_stats %>%
  group_by(level) %>%
  summarise(
    num_players = n_distinct(id),           # Count unique players per level
    avg_wm_score = mean(avg_wm_score, na.rm = TRUE),          # Average WM score for all players
    avg_proficiency = mean(avg_proficiency, na.rm = TRUE),    # Average proficiency score for all players
    avg_language_use = mean(avg_language_use, na.rm = TRUE)   # Average language use for all players
  )

# View the result
print(level_stats)

# Accuracy Models 
# Verify structure
str(filtered_data)

# GLMM for accuracy with Stress as a fixed effect
glmm_accuracy_1 <- glmer(accuracy ~ stress + (1 | id) + (1 | sentence_id), 
                         data = filtered_data, 
                         family = binomial)

# Summary of the model
summary(glmm_accuracy_1)

# GLMM for accuracy with Syllabic Structure as a fixed effect
glmm_accuracy_2 <- glmer(accuracy ~ syllabic_structure + (1 | id) + (1 | sentence_id), 
                         data = filtered_data, 
                         family = binomial)

# Summary of the model
summary(glmm_accuracy_2)

# GLMM for accuracy with LexTale Score (Proficiency) as a fixed effect
glmm_accuracy_3 <- glmer(accuracy ~ scaled_lex_score + (1 | id) + (1 | sentence_id), 
                         data = filtered_data, 
                         family = binomial)

# Summary of the model
summary(glmm_accuracy_3)

# GLMM for accuracy with Language Use as a fixed effect
glmm_accuracy_4 <- glmer(accuracy ~ log_lang_use + (1 | id) + (1 | sentence_id), 
                         data = filtered_data, 
                         family = binomial)

# Summary of the model
summary(glmm_accuracy_4)

str(filtered_data)

# GLMM for accuracy with Working Memory Score as a fixed effect
glmm_accuracy_5 <- glmer(accuracy ~ scaled_wm_score + (1 | id) + (1 | sentence_id), 
                         data = filtered_data, 
                         family = binomial)

# Summary of the model
summary(glmm_accuracy_5)

# GLMM for accuracy with WM group and stress_syllable interaction
glmm_accuracy_6 <- glmer(
  accuracy ~ wm_group * stress_syllable + 
    (1 | id) + (1 | sentence_id), 
  data = filtered_data, 
  family = binomial
)

# Summary of the model
summary(glmm_accuracy_6)


## GAMMS
# GAMM for accuracy with Level as a smooth term
gamm_accuracy_7 <- gam(
  accuracy ~ s(level) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

# Summary of the model
summary(gamm_accuracy_7)

# Visualize the smooth term for level
plot(gamm_accuracy_7, pages = 1)

# GAMM for accuracy with separate smooths for Level by Stress
gamm_accuracy_8 <- gam(
  accuracy ~ s(level, by = stress) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

# Summary of the model
summary(gamm_accuracy_8)

# Visualize smooth terms for Level by Stress
plot(gamm_accuracy_8, pages = 1)

# GAMM for accuracy with separate smooths for Level by Syllabic Structure
gamm_accuracy_9 <- gam(
  accuracy ~ s(level, by = syllabic_structure) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

# Summary of the model
summary(gamm_accuracy_9)

# Visualize smooth terms for Level by Syllabic Structure
plot(gamm_accuracy_9, pages = 1)

# GAMM for accuracy with Level by Proficiency
gamm_accuracy_10 <- gam(
  accuracy ~ s(level, by = scaled_lex_score) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

# Summary of the model
summary(gamm_accuracy_10)

# Visualize smooth terms for Level by Proficiency
plot(gamm_accuracy_10, pages = 1)

# GAMM for accuracy with Level by Language Use
gamm_accuracy_11 <- gam(
  accuracy ~ s(level, by = log_lang_use) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

# Summary of the model
summary(gamm_accuracy_11)

# Visualize smooth terms for Level by Language Use
plot(gamm_accuracy_11, pages = 1)


str(filtered_data)
# GAMM for accuracy with Level by WM
gamm_accuracy_12 <- gam(
  accuracy ~ s(level, by = scaled_wm_score) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = filtered_data, 
  family = binomial, 
  method = "REML"
)

summary(gamm_accuracy_12)

vis.gam(gamm_accuracy_12, view = c("level", "scaled_wm_score"), theta = 120, phi = 30, 
        n.grid = 50, ticktype = "detailed", color = "topo", lwd = 0.5,
        main = "Interaction of Level and WM on Accuracy")

vis.gam(gamm_accuracy_12, view = c("level", "scaled_wm_score"), theta = 145, phi = 15, 
        n.grid = 50, ticktype = "detailed", color = "topo", lwd = 0.5,
        main = "Interaction of Level and WM on Accuracy")

# GAMM for accuracy with Level by WM Group and Stress/Syllabic Structure
accuracy_gam <- gam(
  accuracy ~ s(level, by = interaction(wm_group, stress_syllable), k = 10) + 
    s(id, bs = "re") + 
    s(sentence_id, bs = "re"),
  data = filtered_data,
  family = binomial,
  method = "REML"
)

# Summarize the model
summary(accuracy_gam)

# Generate predictions, excluding random effects
preds_interaction <- predict_gam(
  accuracy_gam, 
  length_out = 12, 
  exclude_terms = c("s(id)", "s(sentence_id)")
)

str(preds_interaction)

ggplot(preds_interaction, aes(x = level, y = fit, color = wm_group, fill = wm_group)) +
  geom_line(size = 1) +  # Smooth lines
  geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.2, show.legend = FALSE) +  # Confidence ribbon
  facet_wrap(~ stress_syllable, ncol = 2, labeller = labeller(stress_syllable = c(
    "oxytone_CV" = "Oxytone CV",
    "paroxytone_CV" = "Paroxytone CV",
    "oxytone_CVC" = "Oxytone CVC",
    "paroxytone_CVC" = "Paroxytone CVC"
  ))) +
  scale_color_manual(values = c("high_WM" = "#0072B2", "low_WM" = "#D55E00"), name = "WM Group") +
  scale_fill_manual(values = c("high_WM" = "#0072B2", "low_WM" = "#D55E00"), name = "WM Group") +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +  # Ensure x-axis levels are 1-12
  labs(
    x = "Level",
    y = "Predicted Accuracy",
    title = "Smooths of Level by Stress-Syllable Type and WM Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )

# Compute Difference Smooths
diff_smooths <- preds_interaction %>%
  pivot_wider(names_from = wm_group, values_from = c(fit, se.fit)) %>%
  mutate(
    diff_fit = fit_high_WM - fit_low_WM,  # Difference in fits
    diff_se = sqrt(se.fit_high_WM^2 + se.fit_low_WM^2),  # Combined standard errors
    significant = ifelse(  # Identify significance
      (diff_fit - diff_se > 0) | (diff_fit + diff_se < 0), 
      TRUE, 
      FALSE
    )
  ) %>%
  dplyr::select(level, stress_syllable, diff_fit, diff_se, significant)

# Plot Difference Smooths with Significance Highlighted
ggplot(diff_smooths, aes(x = level, y = diff_fit)) +
  geom_line(size = 1, color = "blue") +  # Difference smooth line
  geom_ribbon(aes(ymin = diff_fit - diff_se, ymax = diff_fit + diff_se, fill = significant), alpha = 0.2) +  # Confidence interval
  facet_wrap(~ stress_syllable, ncol = 2, labeller = labeller(stress_syllable = c(
    "oxytone_CV" = "Oxytone CV",
    "paroxytone_CV" = "Paroxytone CV",
    "oxytone_CVC" = "Oxytone CVC",
    "paroxytone_CVC" = "Paroxytone CVC"
  ))) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "white"),  # Change color scheme
    name = "Significance",
    labels = c("Not significant", "Significant")
  ) +
  labs(
    x = "Level",
    y = "Difference in Predicted Accuracy (High WM - Low WM)",
    title = "Difference Smooths of Level by Stress-Syllable Type (Significance Highlighted)"
  ) +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )

# WM heatmap
lmer_model <- lmer(
  accuracy ~ wm_score_z + stress_syllable + level +
    (1 | id) + (1 | sentence_id),
  data = filtered_data
)

summary(lmer_model)

# Generate a range of wm_score_z values
wm_score_range <- seq(
  min(filtered_data$wm_score_z, na.rm = TRUE),  # Minimum wm_score_z
  max(filtered_data$wm_score_z, na.rm = TRUE),  # Maximum wm_score_z
  length.out = 50                              # Create 50 points
)

# Create the prediction grid
prediction_grid <- expand.grid(
  wm_score_z = wm_score_range,                      # Range of wm_score_z
  level = seq(min(filtered_data$level),             # Numeric range for levels
              max(filtered_data$level), 
              length.out = 12),
  stress_syllable = levels(filtered_data$stress_syllable)  # Stress syllable levels
)

# Add predicted accuracy values to the grid
prediction_grid$predicted_accuracy <- predict(
  lmer_model, 
  newdata = prediction_grid, 
  re.form = NA  # Exclude random effects
)


# Update the prediction grid to have integer levels
prediction_grid$level <- round(prediction_grid$level)

# Re-plot with corrected levels
ggplot(prediction_grid, aes(x = wm_score_z, y = level, fill = predicted_accuracy)) +
  geom_tile() +
  facet_wrap(~ stress_syllable, nrow = 1) +
  scale_y_continuous(breaks = seq(1, 12, 1)) +  # Ensure y-axis uses discrete levels
  scale_fill_viridis_c(option = "plasma", name = "Predicted Accuracy") +  # Color scale
  labs(
    title = "Predicted Accuracy by WM Score, Level, and Stress Syllable",
    x = "Working Memory (z-scored)",
    y = "Game Level"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# Three-way interaction 

str(filtered_data)

# Fit the model
glmer_accuracy_model_int <- glmer(
  accuracy ~ wm_score_z * stress_syllable * level +
    (1 | id) + (1 | sentence_id),
  data = filtered_data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

# EMMs for wm_score_z and stress_syllable
em_wm_stress <- emmeans(glmer_accuracy_model_int, pairwise ~ wm_score_z | stress_syllable)
summary(em_wm_stress)

# EMMs for stress_syllable and level
em_stress_level <- emmeans(glmer_accuracy_model_int, pairwise ~ stress_syllable | level)
summary(em_stress_level)

# EMMs for wm_score_z, stress_syllable, and level
em_three_way <- emmeans(glmer_accuracy_model_int, ~ wm_score_z * stress_syllable * level)
summary(em_three_way)

# Pairwise comparisons within the three-way interaction
em_three_way_pairwise <- emmeans(glmer_accuracy_model_int, pairwise ~ wm_score_z | stress_syllable * level)
summary(em_three_way_pairwise)

# Summarize the model
summary(glmer_accuracy_model_int)

# RT Models
# Filter for accurate responses
rt_analysis_data <- filtered_data %>%
  filter(accuracy == 1)  # Retain rows where accuracy is 1 (correct responses)

# Remove 0 RTs
rt_analysis_data <- rt_analysis_data %>%
  filter(r_ts_ms > 0) %>%  # Remove zero or negative RTs
  mutate(log_rt_ms = log(r_ts_ms))  # Recalculate log-transformed RT

# Verify structure
str(rt_analysis_data)

# GLMM for RT with Stress as a fixed effect
glmm_rt_1 <- lmer(
  log_rt_ms ~ stress + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_1)

# GLMM for RT with Syllabic Structure as a fixed effect
glmm_rt_2 <- lmer(
  log_rt_ms ~ syllabic_structure + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_2)

# GLMM for RT with LexTale Score as a fixed effect
glmm_rt_3 <- lmer(
  log_rt_ms ~ scaled_lex_score + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_3)

# GLMM for RT with Language Use as a fixed effect
glmm_rt_4 <- lmer(
  log_rt_ms ~ log_lang_use + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_4)


# GLMM for RT with Working Memory Score as a fixed effect
glmm_rt_5 <- lmer(
  log_rt_ms ~ scaled_wm_score + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_5)


# GLMM for RT with WM group and stress/syllable interaction
glmm_rt_6 <- lmer(
  log_rt_ms ~ wm_group * stress_syllable + (1 | id) + (1 | sentence_id), 
  data = rt_analysis_data
)

# Summary of the model
summary(glmm_rt_6)


# GAMM for RT with Level as a smooth term
gamm_rt_7 <- gam(
  log_rt_ms ~ s(level) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_7)

# Visualize the smooth term for level
plot(gamm_rt_7, pages = 1)


# GAMM for RT with separate smooths for Level by Stress
gamm_rt_8 <- gam(
  log_rt_ms ~ s(level, by = stress) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_8)

# Visualize smooth terms for Level by Stress
plot(gamm_rt_8, pages = 1)


# GAMM for RT with separate smooths for Level by Syllabic Structure
gamm_rt_9 <- gam(
  log_rt_ms ~ s(level, by = syllabic_structure) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_9)

# Visualize smooth terms for Level by Syllabic Structure
plot(gamm_rt_9, pages = 1)



# GAMM for RT with Level by Proficiency
gamm_rt_10 <- gam(
  log_rt_ms ~ s(level, by = scaled_lex_score) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_10)

# Visualize smooth terms for Level by Proficiency
plot(gamm_rt_10, pages = 1)

str(rt_analysis_data)

# GAMM for RT with Level by Language Use
gamm_rt_11 <- gam(
  log_rt_ms ~ s(level, by = log_lang_use) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_11)

# Visualize smooth terms for Level by Language Use
plot(gamm_rt_11, pages = 1)

# GAM for RT with Level by WM 
gamm_rt_12 <- gam(
  log_rt_ms ~ s(level, by = scaled_wm_score) + 
    s(id, bs = "re") + s(sentence_id, bs = "re"), 
  data = rt_analysis_data, 
  method = "REML"
)

# Summary of the model
summary(gamm_rt_12)

str(rt_analysis_data)

# Visualize smooth terms for Level by WM
plot(gamm_rt_12, pages = 1)

# 3D visualization of the interaction between Level and WM
vis.gam(gamm_rt_12, view = c("level", "scaled_wm_score"), theta = 120, phi = 30, 
        n.grid = 50, ticktype = "detailed", color = "topo", lwd = 0.5,
        main = "Interaction of Level and Scaled WM on Predicted log RT")

vis.gam(gamm_rt_12, view = c("level", "scaled_wm_score"), theta = 145, phi = 35, 
        n.grid = 50, ticktype = "detailed", color = "topo", lwd = 0.5,
        main = "Interaction of Level and Scaled WM on Predicted log RT")


# GAMM for RT with Level by WM Group and Stress/Syllabic Structure
rt_gam <- gam(
  log_rt_ms ~ s(level, by = interaction(wm_group, stress_syllable), k = 10) + 
    s(id, bs = "re") + 
    s(sentence_id, bs = "re"),
  data = rt_analysis_data,
  method = "REML"
)

# Summarize the model
summary(rt_gam)

# Generate predictions, excluding random effects
rt_preds_interaction <- predict_gam(
  rt_gam, 
  length_out = 12, 
  exclude_terms = c("s(id)", "s(sentence_id)")
)

# Check the structure of the predictions
str(rt_preds_interaction)

ggplot(rt_preds_interaction, aes(x = level, y = fit, color = wm_group, fill = wm_group)) +
  geom_line(size = 1) +  # Smooth lines
  geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.2, show.legend = FALSE) +  # Confidence ribbon
  facet_wrap(~ stress_syllable, ncol = 2, labeller = labeller(stress_syllable = c(
    "oxytone_CV" = "Oxytone CV",
    "paroxytone_CV" = "Paroxytone CV",
    "oxytone_CVC" = "Oxytone CVC",
    "paroxytone_CVC" = "Paroxytone CVC"
  ))) +
  scale_color_manual(values = c("high_WM" = "#0072B2", "low_WM" = "#D55E00"), name = "WM Group") +
  scale_fill_manual(values = c("high_WM" = "#0072B2", "low_WM" = "#D55E00"), name = "WM Group") +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +
  labs(
    x = "Level",
    y = "Predicted Log RT",
    title = "Smooths of Level by Stress-Syllable Type and WM Group (RT Data)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )

# Compute Difference Smooths
rt_diff_smooths <- rt_preds_interaction %>%
  pivot_wider(names_from = wm_group, values_from = c(fit, se.fit)) %>%
  mutate(
    diff_fit = fit_high_WM - fit_low_WM,  # Difference in fits
    diff_se = sqrt(se.fit_high_WM^2 + se.fit_low_WM^2),  # Combined standard errors
    significant = ifelse(  # Identify significance
      (diff_fit - diff_se > 0) | (diff_fit + diff_se < 0), 
      TRUE, 
      FALSE
    )
  ) %>%
  dplyr::select(level, stress_syllable, diff_fit, diff_se, significant)


ggplot(rt_diff_smooths, aes(x = level, y = diff_fit)) +
  geom_line(size = 1, color = "blue") +  # Difference smooth line
  geom_ribbon(aes(ymin = diff_fit - diff_se, ymax = diff_fit + diff_se, fill = significant), alpha = 0.2) +  # Confidence interval
  facet_wrap(~ stress_syllable, ncol = 2, labeller = labeller(stress_syllable = c(
    "oxytone_CV" = "Oxytone CV",
    "paroxytone_CV" = "Paroxytone CV",
    "oxytone_CVC" = "Oxytone CVC",
    "paroxytone_CVC" = "Paroxytone CVC"
  ))) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "white"), 
    name = "Significance",
    labels = c("Not significant", "Significant")
  ) +
  labs(
    x = "Level",
    y = "Difference in Predicted Log RT (High WM - Low WM)",
    title = "Difference Smooths of Level by Stress-Syllable Type (RT Data)"
  ) +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
  )

# Heat map
# Fit the linear mixed-effects model
lmer_rt_model <- lmer(
  log_rt_ms ~ wm_score_z + stress_syllable + level +
    (1 | id) + (1 | sentence_id),
  data = rt_analysis_data
)

# Summarize the model to inspect results
summary(lmer_rt_model)

# Fit the model with a three-way interaction
lmer_rt_model_int <- lmer(
  log_rt_ms ~ wm_score_z * stress_syllable * level +
    (1 | id) + (1 | sentence_id),
  data = rt_analysis_data
)

# Summarize the model to inspect coefficients
summary(lmer_rt_model_int)

# Generate a range for wm_score_z
wm_score_range <- seq(
  min(rt_analysis_data$wm_score_z, na.rm = TRUE), 
  max(rt_analysis_data$wm_score_z, na.rm = TRUE), 
  length.out = 50
)

# Create the prediction grid
rt_prediction_grid <- expand.grid(
  wm_score_z = wm_score_range,                      # Working memory scores
  level = seq(1, 12, 1),                            # Game levels from 1 to 12
  stress_syllable = levels(rt_analysis_data$stress_syllable)  # Stress syllable types
)

# Add predicted log reaction times to the grid
rt_prediction_grid$predicted_log_rt <- predict(
  lmer_rt_model, 
  newdata = rt_prediction_grid, 
  re.form = NA  # Exclude random effects
)


ggplot(rt_prediction_grid, aes(x = wm_score_z, y = level, fill = predicted_log_rt)) +
  geom_tile() +
  facet_wrap(~ stress_syllable, nrow = 1) +  # Separate panels for stress syllable types
  scale_y_continuous(breaks = seq(1, 12, 1)) +  # Set y-axis to levels 1–12
  scale_fill_viridis_c(option = "plasma", name = "Predicted Log RT") +
  labs(
    title = "Predicted Log Reaction Times by WM Score, Level, and Stress Syllable",
    x = "Working Memory (z-scored)",
    y = "Game Level"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Facet labels
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggplot(rt_prediction_grid, aes(x = wm_score_z, y = level, fill = predicted_rt)) +
  geom_tile() +
  facet_wrap(~ stress_syllable, nrow = 1) +  # Separate panels for stress syllable types
  scale_y_continuous(breaks = seq(1, 12, 1)) +  # Set y-axis to levels 1–12
  scale_fill_viridis_c(option = "plasma", name = "Predicted RT (ms)") +
  labs(
    title = "Predicted Reaction Times by WM Score, Level, and Stress Syllable",
    x = "Working Memory (z-scored)",
    y = "Game Level"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Facet labels
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

