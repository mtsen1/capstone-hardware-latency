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
library(car)
library(tibble)
library(purrr)
library(knitr)


# ------------------------------------------------------------------------------
# 2. LOAD & PREP DATA
# ------------------------------------------------------------------------------
# Import the cleaned, log-IQR filtered dataset
prolific_dat <- read.csv("project2/prolific_data/prolific_cleaned_iqr_NEW.csv")

# Ensure factors are set and force GPU as the Reference Level (Baseline)
# This ensures all coefficients calculate the "Penalty" of switching TO CPU[cite: 60, 63].
prolific_dat <- prolific_dat %>%
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
t_test_trial <- t.test(log(RT) ~ Device_Spec, data = prolific_dat)
print(t_test_trial)

# B. Participant-Level Paired T-Test (Mean RT in ms) [cite: 59]
# Aggregate data to find average RT for each participant by Device_Spec [cite: 45]
participant_means <- prolific_dat %>%
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
base_model <- lmer(log(RT) ~ Device_Spec + (1 | pid), data = prolific_dat)

print(summary(base_model))
print(confint(base_model, parm = "Device_SpecCPU", method = "Wald"))

# r^2 value
r2(base_model)

Anova(base_model, type = "III")

# --- Estimated Marginal Means & Post-Hoc ---
print("--- LMER-Adjusted Group Means (ms) ---")
# type = "response" back-transforms the log(RT) to standard reaction time (ms)
base_emmeans <- emmeans(base_model, ~ Device_Spec, type = "response")
print(base_emmeans)

print("--- Post-Hoc Pairwise Comparison ---")
# Compares CPU vs. GPU directly based on the model estimates
base_post_hoc <- pairs(base_emmeans)
print(base_post_hoc)

# model assumptions
r2(base_model)
check_model(base_model)

prolific_dat$CV_scaled <- scale(prolific_dat$CV)

# Does Device type affect FPS CV
fps_model <- lmer(CV ~ Device_Spec + (1 | pid), data = prolific_dat)
summary(fps_model)

# Models for checking comparing model fits (For ANOVA)
# base model
m0 <- lmer(log(RT) ~ Device_Spec + (1 | pid), data = prolific_dat, REML = FALSE)

# model with FPS CV as a fixed effect
m1 <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 | pid),
           data = prolific_dat, REML = FALSE)

# model with FPS CV as a random effect
m2 <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid),
           data = prolific_dat, REML = FALSE)

anova(m0, m1)
anova(m1, m2)


# Final model
final_model <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid), data = prolific_dat)
summary(final_model)
r2(final_model)
check_model(final_model)
confint(final_model)


# checking how much the render effect is explained by jitter
beta0 <- fixef(base_model)["Device_SpecCPU"]
beta1 <- fixef(final_model)["Device_SpecCPU"]

percent_reduction <- (beta0 - beta1) / beta0 * 100
percent_reduction


# ------------------------------------------------------------------------------
# 5. MEDIATION ANALYSIS: Does Jitter (CV) explain the CPU penalty?
# ------------------------------------------------------------------------------
# ---- EDITED MEDIATION
# -------------------------------------------------------------------------
# Cluster bootstrap mediation-style pipeline for Swiftshader -> CV -> log(RT)
# Updated to align with the mixed-effects models:
#   Mediator:  CV_scaled ~ Device_Spec + (1 | pid)
#   Outcome:   log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid)
# -------------------------------------------------------------------------
set.seed(123)

# ----------------------------
# 1) Prepare analysis dataset
# ----------------------------
analysis_boot <- prolific_dat %>%
  mutate(
    Y = log(RT),
    X = ifelse(Device_Spec == "CPU", 1, 0),   # CPU = 1, GPU = 0
    M = as.numeric(CV_scaled),                # use already-scaled CV
    ID = as.factor(pid)
  ) %>%
  filter(!is.na(Y), !is.na(X), !is.na(M), !is.na(ID))

# Make sure X is interpreted as a 0/1 numeric predictor
analysis_boot$X <- as.numeric(analysis_boot$X)

# Unique participant IDs
uid <- unique(analysis_boot$ID)
n_subjects <- length(uid)

# ----------------------------
# 2) Helper function: one cluster bootstrap sample
#    IMPORTANT: duplicated participants get unique bootstrap IDs
# ----------------------------
make_boot_sample <- function(dat, ids_sampled) {
  map_dfr(seq_along(ids_sampled), function(i) {
    this_id <- ids_sampled[i]
    dat %>%
      filter(ID == this_id) %>%
      mutate(
        boot_pid = factor(paste0("boot_", i))  # unique grouping factor per resampled cluster
      )
  })
}

# ----------------------------
# 3) Storage for bootstrap estimates
# ----------------------------
nboots <- 100

tEffect  <- rep(NA_real_, nboots)  # total effect: X -> Y
XtoM     <- rep(NA_real_, nboots)  # path a: X -> M
MtoY     <- rep(NA_real_, nboots)  # path b: M -> Y
direct   <- rep(NA_real_, nboots)  # direct effect c'
indirect <- rep(NA_real_, nboots)  # a*b

# ----------------------------
# 4) Bootstrap loop
# ----------------------------
cat("Starting cluster bootstrap...\n")

b <- 1
while (b <= nboots) {
  sample_ids <- sample(uid, n_subjects, replace = TRUE)
  subdata <- make_boot_sample(analysis_boot, sample_ids)

  res <- tryCatch({
    # Total effect model: X -> Y
    fit_total <- lmer(
      Y ~ X + (1 | boot_pid),
      data = subdata,
      REML = FALSE
    )

    # Mediator model: X -> M
    fit_med <- lmer(
      M ~ X + (1 | boot_pid),
      data = subdata,
      REML = FALSE
    )

    # Outcome model: M + X -> Y
    # Updated to include random slope for mediator
    fit_out <- lmer(
      Y ~ M + X + (1 + M | boot_pid),
      data = subdata,
      REML = FALSE
    )

    a <- fixef(fit_med)[["X"]]
    b_path <- fixef(fit_out)[["M"]]
    c_total <- fixef(fit_total)[["X"]]
    c_prime <- fixef(fit_out)[["X"]]
    ab <- a * b_path

    c(
      total = c_total,
      a = a,
      b = b_path,
      direct = c_prime,
      indirect = ab
    )
  }, error = function(e) {
    NULL
  })

  if (!is.null(res)) {
    tEffect[b]  <- res[["total"]]
    XtoM[b]     <- res[["a"]]
    MtoY[b]     <- res[["b"]]
    direct[b]   <- res[["direct"]]
    indirect[b] <- res[["indirect"]]
    b <- b + 1
  }

  if (b %% 100 == 0) {
    cat("Bootstrap iteration:", b - 1, "\n")
  }
}

# ----------------------------
# 5) Fit models on full data
# ----------------------------
fit_total_full <- lmer(
  Y ~ X + (1 | ID),
  data = analysis_boot,
  REML = FALSE
)

fit_med_full <- lmer(
  M ~ X + (1 | ID),
  data = analysis_boot,
  REML = FALSE
)

fit_out_full <- lmer(
  Y ~ M + X + (1 + M | ID),
  data = analysis_boot,
  REML = FALSE
)

# ----------------------------
# 6) Build results table
# ----------------------------
get_ci <- function(x) {
  quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
}

results_df <- data.frame(
  Path = c(
    "Total Effect (X -> Y)",
    "Path a (Device -> CV)",
    "Path b (CV -> RT)",
    "Direct Effect (c')",
    "Indirect Effect (a*b)"
  ),
  Estimate = c(
    fixef(fit_total_full)[["X"]],
    fixef(fit_med_full)[["X"]],
    fixef(fit_out_full)[["M"]],
    fixef(fit_out_full)[["X"]],
    fixef(fit_med_full)[["X"]] * fixef(fit_out_full)[["M"]]
  ),
  Lower_95CI = c(
    get_ci(tEffect)[1],
    get_ci(XtoM)[1],
    get_ci(MtoY)[1],
    get_ci(direct)[1],
    get_ci(indirect)[1]
  ),
  Upper_95CI = c(
    get_ci(tEffect)[2],
    get_ci(XtoM)[2],
    get_ci(MtoY)[2],
    get_ci(direct)[2],
    get_ci(indirect)[2]
  )
)

results_df$Significant <- ifelse(
  results_df$Lower_95CI * results_df$Upper_95CI > 0,
  "Yes",
  "No"
)

# ----------------------------
# 7) Print results
# ----------------------------
print(results_df)
kable(results_df, digits = 4, caption = "Cluster Bootstrap Mediation Results")

# ----------------------------
# 8) Optional: percent mediated
# ----------------------------
total_effect <- fixef(fit_total_full)[["X"]]
indirect_effect <- fixef(fit_med_full)[["X"]] * fixef(fit_out_full)[["M"]]
percent_mediated <- (indirect_effect / total_effect) * 100

cat("\nPercent mediated:", round(percent_mediated, 2), "%\n")

### test
# Calculate Path 'a' (Fixed effect from fit_med)
path_a <- fixef(fit_med_full)[["X"]]

# Extract individual 'b' paths (Fixed effect + Random deviation)
random_slopes <- ranef(fit_out_full)$ID
individual_b <- fixef(fit_out_full)[["M"]] + random_slopes$M

# Calculate individual Indirect Effects
individual_ab <- path_a * individual_b

# Check the distribution
summary(individual_ab)
hist(individual_ab, main="Distribution of Indirect Effects (a*b)")


# ------------------------------------------------------------------------------
# 6. CV THRESHOLDING & SALVAGE LMER
# ------------------------------------------------------------------------------
print("================ THRESHOLD SALVAGE MODEL ================")

# Define the CV Cutoff (e.g., 15% as the theoretical breaking point) [cite: 79]
cv_threshold <- 10

# Group the data into GPU (Reference), Valid CPU, and Invalid CPU [cite: 99]
threshold_data <- prolific_dat %>%
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


