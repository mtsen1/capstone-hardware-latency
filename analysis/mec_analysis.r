# ==============================================================================
# MEC STUDY: INFERENTIAL ANALYSIS SCRIPT
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)     # Adds p-values to lme4 output
library(emmeans)      # For estimated marginal means and post-hoc tests

# ------------------------------------------------------------------------------
# 2. LOAD & PREP DATA
# ------------------------------------------------------------------------------
# Import the cleaned, log-IQR filtered dataset
analysis_data <- read.csv("project2/cleaned_data_FULL_iqr_NEW.csv")

# Ensure factors are set and force "non_swiftshader" (Hardware/GPU) as the Reference Level
# This ensures coefficients calculate the "Penalty" of switching TO Swiftshader.
analysis_data <- analysis_data %>%
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
t_test_trial <- t.test(log(RT) ~ Is_Swiftshader, data = analysis_data)
print(t_test_trial)

# B. Participant-Level Paired T-Test (Mean RT in ms)
# Aggregate data to find average RT for each participant by Device Spec
participant_means <- analysis_data %>%
  group_by(pid, Is_Swiftshader) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Is_Swiftshader, values_from = mean_RT) %>%
  filter(!is.na(swiftshader) & !is.na(non_swiftshader)) # Ensure they have both conditions

print("--- Participant-Level Paired T-Test (Raw RT) ---")
t_test_paired <- t.test(participant_means$swiftshader, participant_means$non_swiftshader, paired = TRUE)
print(t_test_paired)


# ------------------------------------------------------------------------------
# 4. LINEAR MIXED-EFFECTS MODELS (LMER) WITH GAMETYPE
# ------------------------------------------------------------------------------
print("================ LMER ANALYSES ================")

# A. Main Effects Model
# Predict log(RT) based on Device and GameType, accounting for random intercepts by participant
print("--- LMER: Main Effects (Device + GameType) ---")
base_model <- lmer(log(RT) ~ Is_Swiftshader + GameType + (1 | pid), data = analysis_data)
print(summary(base_model))

# Calculate confidence intervals for the main effect of Swiftshader
print("--- Confidence Interval for Swiftshader Penalty ---")
print(confint(base_model, parm = "Is_Swiftshaderswiftshader", method = "Wald"))


# B. Interaction Model
# Predict log(RT) to see if the Swiftshader penalty changes depending on the GameType
print("--- LMER: Interaction (Device * GameType) ---")
interaction_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = analysis_data)
print(summary(interaction_model))


# ------------------------------------------------------------------------------
# 5. ESTIMATED MARGINAL MEANS (EMMEANS)
# ------------------------------------------------------------------------------
print("================ LMER-ADJUSTED MEANS & POST-HOC ================")

# Calculate Estimated Marginal Means for the interaction model
# type = "response" back-transforms from the log scale to actual milliseconds
lmer_emmeans <- emmeans(interaction_model, ~ Is_Swiftshader | GameType, type = "response")

print("--- LMER-Adjusted Means by Game (ms) ---")
print(lmer_emmeans)

# Post-Hoc Pairwise Comparisons
# This compares swiftshader vs non_swiftshader WITHIN each specific GameType
print("--- Post-Hoc Comparisons: Swiftshader Penalty per Game ---")
post_hoc_results <- pairs(lmer_emmeans)
print(post_hoc_results)
