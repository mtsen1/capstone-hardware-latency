# ==============================================================================
# PROLIFIC STUDY: INFERENTIAL ANALYSIS SCRIPT
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)     # Adds p-values to lme4 output
library(mediation)    # For causal mediation analysis
library(emmeans)      # For estimated marginal means and post-hoc tests
library(performance)  # For checking model assumptions

# ------------------------------------------------------------------------------
# 2. LOAD & PREP DATA
# ------------------------------------------------------------------------------
# Import the cleaned, log-IQR filtered dataset
analysis_data <- read.csv("project2/prolific_data/prolific_cleaned_iqr_NEW.csv")

# Ensure factors are set and force GPU as the Reference Level (Baseline)
# This ensures all coefficients calculate the "Penalty" of switching TO CPU[cite: 60, 63].
analysis_data <- analysis_data %>%
  mutate(
    Device_Spec = relevel(factor(Device_Spec), ref = "GPU"),
    pid = as.character(pid),
    
    # Recalculate Trial-Level Coefficient of Variation (CV) for mediation/thresholding [cite: 64, 99]
    CV = (sqrt(FPS_var) / FPS_mean) * 100
  )

# ------------------------------------------------------------------------------
# 3. T-TESTS (RAW DIFFERENCES)
# ------------------------------------------------------------------------------
print("================ T-TESTS ================")

# A. Trial-Level T-Test (Log Scale) [cite: 46]
print("--- Trial-Level T-Test (log RT) ---")
t_test_trial <- t.test(log(RT) ~ Device_Spec, data = analysis_data)
print(t_test_trial)

# B. Participant-Level Paired T-Test (Mean RT in ms) [cite: 59]
# Aggregate data to find average RT for each participant by Device_Spec [cite: 45]
participant_means <- analysis_data %>%
  group_by(pid, Device_Spec) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Device_Spec, values_from = mean_RT) %>%
  filter(!is.na(CPU) & !is.na(GPU)) # Ensure they have both conditions [cite: 46]

print("--- Participant-Level Paired T-Test (Raw RT) ---")
t_test_paired <- t.test(participant_means$CPU, participant_means$GPU, paired = TRUE)
print(t_test_paired)


# ------------------------------------------------------------------------------
# 4. BASE LINEAR MIXED-EFFECTS MODEL (LMER)
# ------------------------------------------------------------------------------
print("================ BASE LMER ================")

# Predict log(RT) based on Device Spec, accounting for random intercepts by participant
base_model <- lmer(log(RT) ~ Device_Spec + (1 | pid), data = analysis_data)

print(summary(base_model))
print(confint(base_model, parm = "Device_SpecCPU", method = "Wald"))

# --- Estimated Marginal Means & Post-Hoc ---
print("--- LMER-Adjusted Group Means (ms) ---")
# type = "response" back-transforms the log(RT) to standard reaction time (ms)
base_emmeans <- emmeans(base_model, ~ Device_Spec, type = "response")
print(base_emmeans)

print("--- Post-Hoc Pairwise Comparison ---")
# Compares CPU vs. GPU directly based on the model estimates
base_post_hoc <- pairs(base_emmeans)
print(base_post_hoc)

# ------------------------------------------------------------------------------
# 5. MEDIATION ANALYSIS: Does Jitter (CV) explain the CPU penalty?
# ------------------------------------------------------------------------------
print("================ MEDIATION (CV) ================")

# Model M: Predict the Mediator (CV) from the Treatment (Device_Spec) [cite: 64]
med_fit <- lmer(CV ~ Device_Spec + (1 | pid), data = analysis_data)

# Model Y: Predict the Outcome (log RT) from both Treatment and Mediator [cite: 64]
out_fit <- lmer(log(RT) ~ Device_Spec + CV + (1 | pid), data = analysis_data)

# Convert to standard lmerMod format for the mediation package [cite: 64]
med_fit_mod <- as(med_fit, "lmerMod")
out_fit_mod <- as(out_fit, "lmerMod")

# Run the Mediation Simulation (Set sims = 100 for speed, 1000 for final publication) [cite: 64, 65]
mediation_results <- mediation::mediate(
  model.m = med_fit_mod, 
  model.y = out_fit_mod, 
  treat = "Device_Spec", 
  mediator = "CV",
  sims = 100 
)

print(summary(mediation_results))


# ------------------------------------------------------------------------------
# 6. CV THRESHOLDING & SALVAGE LMER
# ------------------------------------------------------------------------------
print("================ THRESHOLD SALVAGE MODEL ================")

# Define the CV Cutoff (e.g., 15% as the theoretical breaking point) [cite: 79]
cv_threshold <- 10

# Group the data into GPU (Reference), Valid CPU, and Invalid CPU [cite: 99]
threshold_data <- analysis_data %>%
  mutate(
    Group = case_when(
      Device_Spec == "GPU" ~ "1. GPU (Reference)",
      Device_Spec == "CPU" & CV <= cv_threshold ~ "2. CPU (Valid)",
      Device_Spec == "CPU" & CV > cv_threshold ~ "3. CPU (Invalid)"
    )
  ) %>%
  filter(!is.na(Group)) 

# Force reference level for the new Group variable [cite: 100]
threshold_data$Group <- factor(threshold_data$Group, 
                               levels = c("1. GPU (Reference)", 
                                          "2. CPU (Valid)", 
                                          "3. CPU (Invalid)"))

print("--- Sample Sizes per Group ---")
print(table(threshold_data$Group))

# Run the Salvage Model [cite: 100]
mod_salvage <- lmer(log(RT) ~ Group + (1 | pid), data = threshold_data)
print(summary(mod_salvage))

# Calculate Estimated Marginal Means (EMMs) back-transformed to milliseconds [cite: 122]
lmer_emmeans <- emmeans(mod_salvage, ~ Group, type = "response")

print("--- LMER-Adjusted Group Means (ms) ---")
print(lmer_emmeans)

# Post-Hoc Pairwise Comparisons (Tukey adjusted) [cite: 122, 123]
print("--- Post-Hoc Pairwise Comparisons ---")
post_hoc_results <- pairs(lmer_emmeans, adjust = "tukey")
print(post_hoc_results)