# ==============================================================================
# MEC STUDY: INFERENTIAL ANALYSIS SCRIPT
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)     
library(emmeans) 
library(car) # for Type III ANOVA
library(performance) # for checking model assumptions
library(see)


# ------------------------------------------------------------------------------
# 2. LOAD & PREP DATA
# ------------------------------------------------------------------------------
# Import the cleaned, log-IQR filtered dataset
mec_data <- read.csv("project2/cleaned_data_FULL_iqr_NEW.csv")

# quick summary of data
head(mec_data)
table(mec_data$systemMemorySize)
mec_data$GPU_Brand <- factor(mec_data$GPU_Brand)
table(mec_data$GPU_Brand)

# Ensure factors are set and force "non_swiftshader" (Hardware/GPU) as the Reference Level
# This ensures coefficients calculate the "Penalty" of switching TO Swiftshader.
mec_data <- mec_data %>%
  mutate(
    Is_Swiftshader = relevel(factor(Is_Swiftshader), ref = "non_swiftshader"),
    GameType = factor(GameType),
    pid = as.character(pid)
  )

# ------------------------------------------------------------------------------
# 3. T-TESTS (RAW DIFFERENCES)
# ------------------------------------------------------------------------------
print("================ T-TESTS ================")

# A. Overall Trial-Level T-Test (Log Scale)
# Note: This aggregates across all games. The LMER will handle game-specific variance.
print("--- Overall Trial-Level T-Test (log RT) ---")
t_test_trial <- t.test(log(RT) ~ Is_Swiftshader, data = mec_data)
print(t_test_trial)

# B. Participant-Level Paired T-Test (Mean RT in ms)
# Aggregate data to find average RT for each participant by Device Spec
participant_means <- mec_data %>%
  group_by(pid, Is_Swiftshader) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Is_Swiftshader, values_from = mean_RT) %>%
  filter(!is.na(swiftshader) & !is.na(non_swiftshader)) # Ensure they have both conditions

print("--- Participant-Level Paired T-Test (Raw RT) ---")
t_test_paired <- t.test(participant_means$swiftshader, participant_means$non_swiftshader, paired = TRUE)
print(t_test_paired)


# ------------------------------------------------------------------------------
# ANOVA
# ------------------------------------------------------------------------------
print("================ ANOVA ANALYSES ================")

# Fit Model 1 (ANOVA)
interaction_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = mec_data)
Anova(interaction_model, type = "III")

# Check for model fit
# Calculate R-squared for your lmer model
r2(interaction_model)

# 1. Take a random sample of indices
set.seed(123) # Always set a seed for reproducibility
n_sample <- 5000
sample_idx <- sample(1:nrow(mec_data), n_sample)

# 2. Extract residuals and fitted values for the sample
sample_fitted <- fitted(interaction_model)[sample_idx]
sample_residuals <- resid(interaction_model)[sample_idx]

# 3. Now, you can use standard, fast plots
# A. Residuals vs Fitted
plot(sample_fitted, sample_residuals, 
     main="Residuals vs Fitted (Subsampled)",
     xlab="Fitted", ylab="Residuals", pch=20, col=rgb(0,0,0,0.2))
abline(h=0, col="red", lwd=2)

# B. Q-Q Plot for Normality
qqnorm(sample_residuals)
qqline(sample_residuals, col="red", lwd=2)

shapiro.test(sample_residuals)

# ------------------------------------------------------------------------------
# 4. LINEAR MIXED-EFFECTS MODELS (LMER) WITH GAMETYPE
# ------------------------------------------------------------------------------
print("================ LMER ANALYSES ================")

# A. Main Effects Model
# Predict log(RT) based on Device and GameType, accounting for random intercepts by participant
print("--- LMER: Main Effects (Device + GameType) ---")
base_model <- lmer(log(RT) ~ Is_Swiftshader + GameType + (1 | pid), data = mec_data)
print(summary(base_model))

# Calculate confidence intervals for the main effect of Swiftshader
print("--- Confidence Interval for Swiftshader Penalty ---")
print(confint(base_model, parm = "Is_Swiftshaderswiftshader", method = "Wald"))


# B. Interaction Model
# Predict log(RT) to see if the Swiftshader penalty changes depending on the GameType
print("--- LMER: Interaction (Device * GameType) ---")
interaction_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = mec_data)
print(summary(interaction_model))
confint(interaction_model, method = "Wald")

# C. Model with FPS CV as a predictor
# calculate the the CV for FPS for each row
mec_data1 <- mec_data %>%
  mutate(
    FPS_sd = sqrt(FPS_var),                    
    FPS_CV = (FPS_sd / FPS_mean) * 100         # Calculate CV as a percentage
  )

# Linear Mixed-Effects Model (unscaled, less interpretable)
cv_model_unscaled <- lmer(log(RT) ~ Is_Swiftshader + FPS_CV + (1 | pid), data = mec_data1)
summary(cv_model_unscaled)

# Standardizing FPS_CV
mec_data1$FPS_CV_scaled <- scale(mec_data1$FPS_CV)


# Keep only rows where both FPS_CV and log(RT) are present
df_clean <- mec_data1[!is.na(mec_data1$FPS_CV) & !is.na(mec_data1$RT), ]

# Models for checking comparing model fits (For ANOVA)
# Base model
m0 <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = df_clean, REML = FALSE)

# Add fixed effect only
m1 <- lmer(log(RT) ~ Is_Swiftshader * GameType + FPS_CV_scaled + (1 | pid), data = df_clean, REML = FALSE)

# Add random slope
m2 <- lmer(log(RT) ~ Is_Swiftshader * GameType + FPS_CV_scaled + (1 + FPS_CV_scaled | pid), data = df_clean, REML = FALSE)

anova(m0, m1)
anova(m1, m2)

# Final model
final_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + FPS_CV_scaled + (1 + FPS_CV_scaled | pid), data = df_clean)
summary(final_model)


# ------------------------------------------------------------------------------
# 5. ESTIMATED MARGINAL MEANS (EMMEANS)
# ------------------------------------------------------------------------------
print("================ LMER-ADJUSTED MEANS & POST-HOC ================")

# Calculate Estimated Marginal Means for the interaction model
# type = "response" back-transforms from the log scale to actual milliseconds
lmer_emmeans <- emmeans(interaction_model, ~ Is_Swiftshader | GameType, type = "response")

print("--- LMER-Adjusted Means by Game (ms) ---")
print(lmer_emmeans)

# Convert emmeans to a dataframe
df_means <- as.data.frame(lmer_emmeans)
write.csv(df_means, "model_adjusted_means.csv")

# Post-Hoc Pairwise Comparisons
# This compares swiftshader vs non_swiftshader WITHIN each specific GameType
print("--- Post-Hoc Comparisons: Swiftshader Penalty per Game ---")
post_hoc_results <- pairs(lmer_emmeans)
print(post_hoc_results)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 6. ANALYSIS ON BRT DATA
# ------------------------------------------------------------------------------

# filter mec data to only have GameType = BRT
mec_brt <- mec_data %>%
  filter(GameType == "BRT")

# for consort
nrow(mec_brt) # 26089
length(unique(mec_brt$pid)) #266
table(mec_brt$Is_Swiftshader)

# ------------------------------------------------------------------------------
# 3. T-TESTS (RAW DIFFERENCES)
# ------------------------------------------------------------------------------
print("================ T-TESTS ================")

# A. Overall Trial-Level T-Test (Log Scale)
# Note: This aggregates across all games. The LMER will handle game-specific variance.
print("--- Overall Trial-Level T-Test (log RT) ---")
t_test_trial <- t.test(log(RT) ~ Is_Swiftshader, data = mec_brt)
print(t_test_trial)

# B. Participant-Level Paired T-Test (Mean RT in ms)
# Aggregate data to find average RT for each participant by Device Spec
participant_means <- mec_brt %>%
  group_by(pid, Is_Swiftshader) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Is_Swiftshader, values_from = mean_RT) %>%
  filter(!is.na(swiftshader) & !is.na(non_swiftshader)) # Ensure they have both conditions

print("--- Participant-Level Paired T-Test (Raw RT) ---")
t_test_paired <- t.test(participant_means$swiftshader, participant_means$non_swiftshader, paired = TRUE)
print(t_test_paired)


# ------------------------------------------------------------------------------
# ANOVA
# ------------------------------------------------------------------------------
print("================ ANOVA ANALYSES ================")

# Fit Model 1 (ANOVA)
brt_model <- lmer(log(RT) ~ Is_Swiftshader + (1 | pid), data = mec_brt)
Anova(brt_model, type = "III")

# Check for model fit
# Calculate R-squared for your lmer model
r2(brt_model)

# ------------------------------------------------------------------------------
# 4. LINEAR MIXED-EFFECTS MODELS (LMER) WITH GAMETYPE
# ------------------------------------------------------------------------------
print("================ LMER ANALYSES ================")

# A. Main Effects Model
# Predict log(RT) based on Device and GameType, accounting for random intercepts by participant
print("--- LMER: Main Effects (Device + GameType) ---")
base_model <- lmer(log(RT) ~ Is_Swiftshader + (1 | pid), data = mec_brt)
print(summary(base_model))

# Calculate confidence intervals for the main effect of Swiftshader
print("--- Confidence Interval for Swiftshader Penalty ---")
print(confint(base_model, parm = "Is_Swiftshaderswiftshader", method = "Wald"))


# C. Model with FPS CV as a predictor
# calculate the the CV for FPS for each row
mec_brt1 <- mec_brt %>%
  mutate(
    FPS_sd = sqrt(FPS_var),                    
    FPS_CV = (FPS_sd / FPS_mean) * 100         # Calculate CV as a percentage
  )

# Standardizing FPS_CV
mec_brt1$FPS_CV_scaled <- scale(mec_brt1$FPS_CV)

# Keep only rows where both FPS_CV and log(RT) are present
df_clean <- mec_brt1[!is.na(mec_brt1$FPS_CV) & !is.na(mec_brt1$RT), ]

# Models for checking comparing model fits (For ANOVA)
# Base model
m0 <- lmer(log(RT) ~ Is_Swiftshader + (1 | pid), data = df_clean, REML = FALSE)

# Add fixed effect only
m1 <- lmer(log(RT) ~ Is_Swiftshader  + FPS_CV_scaled + (1 | pid), data = df_clean, REML = FALSE)

# Add random slope
m2 <- lmer(log(RT) ~ Is_Swiftshader + FPS_CV_scaled + (1 + FPS_CV_scaled | pid), data = df_clean, REML = FALSE)

anova(m0, m1)
anova(m1, m2)

# Final model
final_model <- lmer(log(RT) ~ Is_Swiftshader + FPS_CV_scaled + (1 + FPS_CV_scaled | pid), data = df_clean)
summary(final_model)


# ------------------------------------------------------------------------------
# 5. ESTIMATED MARGINAL MEANS (EMMEANS)
# ------------------------------------------------------------------------------
print("================ LMER-ADJUSTED MEANS & POST-HOC ================")

# Calculate Estimated Marginal Means for the interaction model
# type = "response" back-transforms from the log scale to actual milliseconds
lmer_emmeans <- emmeans(brt_model, ~ Is_Swiftshader, type = "response")

print("--- LMER-Adjusted Means by Game (ms) ---")
print(lmer_emmeans)


# Post-Hoc Pairwise Comparisons
# This compares swiftshader vs non_swiftshader WITHIN each specific GameType
print("--- Post-Hoc Comparisons: Swiftshader Penalty per Game ---")
post_hoc_results <- pairs(lmer_emmeans)
print(post_hoc_results)

