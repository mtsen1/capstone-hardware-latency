library(ggplot2)
library(readxl)
library(tableone)
library(gridExtra)
library(patchwork)
library(tidyverse)


# MEC --------------------------------------------------------------------------
# read excel file
plot_data <- read_excel("project2/mec_lmer_posthoc.xlsx")

# Assuming 'pivot_df_reset' is the clean dataframe from our previous step
# We need to make sure the game types are ordered by penalty size for better readability
plot_data$`Game Type`  <- reorder(pivot_df_reset$`Game Type` , pivot_df_reset$`Penalty (ms)`)

library(tidyverse)

# Assuming your dataframe is named 'plot_data'
plot_data_clean <- plot_data %>%
  # 1. Remove the brackets [ ] and split the numbers by the comma
  mutate(
    CI_clean = gsub("\\[|\\]", "", `95% CI (LCL, UCL)`),
    LCL = as.numeric(str_split_i(CI_clean, ", ", 1)),
    UCL = as.numeric(str_split_i(CI_clean, ", ", 2))
  ) %>%
  # 2. Reorder Game Type by the size of the penalty (makes the plot easier to read)
  mutate(`Game Type` = reorder(`Game Type`, `Penalty (ms)`))

# Check the first few rows to ensure LCL and UCL are now numeric
head(plot_data_clean)

ggplot(plot_data_clean, aes(x = `Penalty (ms)`, y = `Game Type`)) +
  # Add the horizontal error bars (the CI)
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.2, color = "#2c3e50") +
  # Add the point estimate (the Penalty)
  geom_point(size = 3, color = "#3498db") +
  # Add a vertical dashed line at 0 (the "null" line)
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # Labels and Theme
  labs(
    title = "Hardware Rendering Latency Penalty by Task",
    subtitle = "Points represent mean delay (ms); Error bars represent 95% Confidence Intervals",
    x = "Latency Penalty (ms)",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold"))

# model assumption checks

# Create a dataframe for plotting
plot_df <- data.frame(
  Fitted = fitted(interaction_model),
  Residuals = residuals(interaction_model)
)

ggplot(plot_df, aes(x = Fitted, y = Residuals)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c() + # Nice color scale
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted (Hexagonal Binning)",
       subtitle = "Colors indicate point density",
       x = "Model Fitted Values",
       y = "Residuals") +
  theme_minimal()


# mec + prolific model results plot
# 1. Prepare MEC Data
mec_df <- pivot_df_reset %>%
  mutate(Study = "MEC Study") %>%
  select(Study, `Game Type` = GameType, Penalty, LCL, UCL)

# 2. Prepare Prolific/Validation Data
# Create a small dataframe for your validation result
val_df <- data.frame(
  Study = "Validation Study",
  `Game Type` = "Overall (GPU/CPU)",
  Penalty = 107,     # The penalty we calculated
  LCL = 99,          # Add your actual LCL
  UCL = 115          # Add your actual UCL
)

# 3. Combine them
# 1. Prepare MEC Data
mec_df <- plot_data_clean %>%
  mutate(Study = "MEC Study") %>%
  dplyr::select(Study, GameType = `Game Type`, Penalty = `Penalty (ms)`, LCL, UCL)

# 2. Prepare Prolific/Validation Data
# Create a small dataframe for your validation result
val_df <- data.frame(
  Study = "Validation Study",
  GameType = "BRT",
  Penalty = 107,     # The penalty we calculated
  LCL = 99,          # Add your actual LCL
  UCL = 115          # Add your actual UCL
)

# 3. Combine them
combined_df <- rbind(mec_df, val_df)
library(ggplot2)

ggplot(combined_df, aes(x = Penalty, y = GameType)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # This creates the two stacked panels
  facet_grid(Study ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Hardware Rendering Latency Penalty",
    subtitle = "Comparing MEC Interaction Model vs. Validation Study",
    x = "Latency Penalty (ms)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  )


  # mec model (with fps cv)
  
  library(lme4)
  library(dplyr)
  library(tibble)
  
  # Extract random effects
  re <- ranef(m_fix)[["pid"]]
  
  # Convert to dataframe
  df_slopes <- as.data.frame(re) |>
    rownames_to_column("pid")
  
  # Add fixed effect
  fixed_slope <- fixef(m_fix)["FPS_CV_scaled"]
  
  df_slopes <- df_slopes |>
    mutate(total_slope = fixed_slope + FPS_CV_scaled)


  ggplot(df_slopes, aes(x = total_slope)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = fixed_slope, linetype = "dashed") +
    labs(
      x = "Effect of FPS variability on log(RT)",
      y = "Number of participants",
      title = "Distribution of participant-specific FPS effects"
    ) +
    theme_minimal(base_size = 12)


# same plot as above but density
  ggplot(df_slopes, aes(x = total_slope)) +
    geom_density() +
    geom_vline(xintercept = fixed_slope, linetype = "dashed") +
    labs(
      x = "Effect of FPS variability on log(RT)",
      y = "Density",
      title = "Distribution of participant-specific FPS effects"
    ) +
    theme_minimal(base_size = 12)


# VALIDATION
# is the fps cv random slope justified?
library(ggplot2)
library(dplyr)

prolific_dat <- read.csv("project2/prolific_data/prolific_cleaned_iqr_NEW.csv")
prolific_dat <- prolific_dat %>%
  mutate(
    Device_Spec = relevel(factor(Device_Spec), ref = "GPU"),
    pid = as.character(pid),
    
    # Recalculate Trial-Level Coefficient of Variation (CV) for mediation/thresholding [cite: 64, 99]
    CV = (sqrt(FPS_var) / FPS_mean) * 100
  )
prolific_dat$CV_scaled <- scale(prolific_dat$CV)

# Create the Spaghetti Plot
ggplot(prolific_dat, aes(x = CV_scaled, y = RT)) +
  
  # 1. The "Spaghetti": Plot a thin, transparent line for EVERY single participant
  geom_smooth(aes(group = pid), method = "lm", se = FALSE, 
              color = "#007DCE", alpha = 0.3, linewidth = 0.5) +
  
  # 2. The "Fixed Effect": Plot the thick, overall average line on top
  geom_smooth(method = "lm", se = FALSE, 
              color = "black") +
  
  # 3. Clean up the aesthetics for your poster
  coord_cartesian(ylim = c(200, 800)) + # Adjust based on your log/raw RT scale
  labs(
    title = "Individual Differences in Hardware Penalty",
    x = "Frame Stutter (FPS CV Scaled)",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal() 
  

### 
ggplot(prolific_dat, aes(x = CV_scaled, y = log(RT))) +
  
  # Add raw data points with very high transparency so they don't overwhelm the lines
  geom_point(alpha = 0.05, color = "gray50", size = 0.5) +
  
  # Add individual linear trends per participant
  geom_smooth(aes(group = pid), method = "lm", se = FALSE, 
              color = "#007DCE", alpha = 0.3, linewidth = 0.6) +
  
  # Add the overall average trend
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 2) +
  
  labs(
    title = "The 'Double Jeopardy' of Hardware Latency",
    x = "Frame Stutter (Standard Deviations from Mean)",
    y = "Log Reaction Time (ms)"
  ) +
  theme_minimal(base_size = 18)

## maybe delete later
random_effects <- ranef(path_b)$pid %>%
  rownames_to_column("pid")
# 1. Identify the Top 5 and Bottom 5 participants based on their slopes
vulnerable <- random_effects %>%
  slice_max(CV_scaled, n = 5) %>%
  mutate(category = "Vulnerable Population (Top 5 Slopes)")

resilient <- random_effects %>%
  slice_min(CV_scaled, n = 5) %>%
  mutate(category = "Resilient Population (Bottom 5 Slopes)")

# filter pids to only pids in resilient
resilient_dat <- prolific_dat %>%
  filter(pid %in% resilient$pid)
View(resilient_dat)
vul_dat <- prolific_dat %>%
  filter(pid %in% vulnerable$pid)
View(vul_dat)

# Combine them into one 'extreme_groups' dataframe
extreme_groups <- bind_rows(vulnerable, resilient)

# 2. Filter the original data for these 10 participants
plot_data <- prolific_dat %>%
  filter(pid %in% extreme_groups$pid) %>%
  left_join(extreme_groups, by = "pid")

# 3. Create the Grouped Spaghetti Plot
ggplot(plot_data, aes(x = CV_scaled.x, y = log(RT), color = category)) +
  
  # Raw data points (very faint)
  geom_point(alpha = 0.2, size = 1) +
  
  # Individual regression lines for each of the 10 participants
  geom_smooth(aes(group = pid), method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.6) +
  
  # "Average" lines for each group to show the clear difference in trajectory
  geom_smooth(aes(group = category), method = "lm", se = FALSE, linewidth = 2.5) +
  
  scale_color_manual(values = c("#2ca02c", "#d62728")) + # Standard Green and Red
  labs(
    title = "Heterogeneity of Hardware Sensitivity",
    subtitle = "Comparing the Top 5 and Bottom 5 participants by Stutter Sensitivity",
    x = "Frame Stutter (CV Scaled)",
    y = "Log Reaction Time",
    color = "Participant Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", plot.title = element_text(face="bold"))


# follow up to above plot
# 1. Extract the random effects
ranef_data <- ranef(m3)$pid %>%
  as.data.frame() %>%
  tibble::rownames_to_column("pid")

# 2. Plot Intercept vs. CV_scaled Slope
ggplot(ranef_data, aes(x = `(Intercept)`, y = CV_scaled)) +
  geom_point(color = "#024059", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#D95252", se = FALSE) +
  labs(
    title = "Correlation of Random Effects",
    subtitle = "Confirming the r = 0.82 relationship",
    x = "Random Intercept (Baseline Slowness)",
    y = "Random Slope (Sensitivity to Stutter)"
  ) +
  theme_minimal(base_size = 14)


# Find the Top 5 Slowest (Highest Intercepts)
slowest_pids <- ranef_data %>% 
  arrange(desc(`(Intercept)`)) %>% 
  slice(1:5) %>% 
  pull(pid)

# Find the Top 5 Most Vulnerable (Highest Slopes)
vulnerable_pids <- ranef_data %>% 
  arrange(desc(CV_scaled)) %>% 
  slice(1:5) %>% 
  pull(pid)

# See how many people are in BOTH lists
intersect(slowest_pids, vulnerable_pids)







library(ggplot2)
library(ggrepel) # For labeling specific outliers

# Note: Backticks are needed because (Intercept) has parentheses
ggplot(random_effects, aes(x = `(Intercept)`, y = CV_scaled)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  # Label the 5 most influential points (highest slopes)
  geom_text_repel(data = head(arrange(random_effects, desc(CV_scaled)), 5),
                  aes(label = pid), size = 3) +
  labs(title = "Intercept-Slope Correlation Check",
       subtitle = "Is the 0.82 correlation driven by specific outliers?",
       x = "Baseline Slowness (Random Intercept)",
       y = "Lag Sensitivity (Random Slope)") +
  theme_minimal()

# 1. Original Correlation
original_cor <- cor(random_effects$`(Intercept)`, random_effects$CV_scaled)

# 2. Correlation without the Top 5 Vulnerable participants
top_5_ids <- extreme_groups %>% 
  filter(category == "Vulnerable Population (Top 5 Slopes)") %>% 
  pull(pid)

reduced_cor <- random_effects %>%
  filter(!pid %in% top_5_ids) %>%
  summarize(correlation = cor(`(Intercept)`, CV_scaled)) %>%
  pull(correlation)

cat("Original Correlation:", round(original_cor, 3), "\n")
cat("Correlation after dropping Top 5:", round(reduced_cor, 3), "\n")


# Loop through and leave one out
jackknife_results <- sapply(1:nrow(random_effects), function(i) {
  temp_data <- random_effects[-i, ]
  cor(temp_data$`(Intercept)`, temp_data$CV_scaled)
})

# Create a dataframe for results
influence_df <- data.frame(
  pid = random_effects$pid,
  cor_if_removed = jackknife_results
) %>%
  mutate(influence = original_cor - cor_if_removed)

# See which PIDs have the most influence
influence_df %>% arrange(desc(abs(influence))) %>% head(10)


# Corrected Join: Using group_by() instead of the non-existent group_select()
validation_check <- random_effects %>%
  left_join(
    prolific_dat %>% 
      group_by(pid) %>% 
      summarize(max_cv = max(CV_scaled, na.rm = TRUE)),
    by = "pid"
  )

# Use backticks for the specific column name created by ranef()
cor(validation_check$`(Intercept)`, validation_check$max_cv, use = "complete.obs")

# with differnces
# Create a Delta Table
delta_analysis <- prolific_dat %>%
  group_by(pid, Device_Spec) %>%
  summarize(
    mean_RT = mean(RT, na.rm = TRUE),
    mean_CV = mean(CV_scaled, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Device_Spec, 
    values_from = c(mean_RT, mean_CV)
  ) %>%
  mutate(
    delta_RT = mean_RT_CPU - mean_RT_GPU, # The 'Hardware Tax'
    delta_CV = mean_CV_CPU - mean_CV_GPU  # The 'Instability Tax'
  )

# Join with your random effects to see how 'trait' slowness plays in
delta_analysis <- delta_analysis %>%
  left_join(random_effects, by = "pid")

# The Big Correlation
cor(delta_analysis$delta_CV, delta_analysis$delta_RT, use = "complete.obs")

# age

age <- read.csv("project2/prolific_data/qualtrics.csv")

library(dplyr)

prolific_dat_age <- prolific_dat %>%
  left_join(
    age %>% dplyr::select(Q59, Age),
    by = c("pid" = "Q59")
  )

  library(dplyr)

  # Map your specific age range strings to numbers
  # Replace the strings below with exactly what appears in your 'age' column
  validation_check <- validation_check %>%
    left_join(prolific_dat_age %>% dplyr::select(pid, Age) %>% distinct(), by = "pid") %>%
    mutate(age_rank = as.numeric(as.factor(Age))) 
  
  # Check the mapping to make sure it's in the right order (youngest = 1)
  table(validation_check$age, validation_check$age_rank)

# 1. Biological check: Does Age = Slower Intercept?
cor_age_intercept <- cor(validation_check$age_rank, validation_check$`(Intercept)`, use="complete.obs")

# 2. Digital Divide check: Does Age = Messier Hardware?
cor_age_cv <- cor(validation_check$age_rank, validation_check$max_cv, use="complete.obs")

cat("Correlation (Age vs. Baseline Slowness):", round(cor_age_intercept, 3), "\n")
cat("Correlation (Age vs. Hardware Instability):", round(cor_age_cv, 3), "\n")

# delete
# 1. Create a clean, observed baseline for each person
baseline_data <- prolific_dat %>%
  filter(Device_Spec == "GPU") %>%
  group_by(pid) %>%
  summarize(Baseline_RT = mean(RT, na.rm = TRUE))

# 2. Join it back to the main data
analysis_dat <- prolific_dat %>%
  left_join(baseline_data, by = "pid") %>%
  mutate(Baseline_RT_centered = scale(Baseline_RT, scale = FALSE))

# 3. The "Fei-Approved" Model: Interaction between Baseline and CV
# This tests: "Does your baseline speed (Predictor 1) change the effect of lag (Predictor 2) on your RT (Outcome)?"
fit_interaction <- lmer(log(RT) ~ Device_Spec + CV_scaled * Baseline_RT_centered + (1 + CV_scaled | pid), 
                        data = analysis_dat)

summary(fit_interaction)

####################################################################################
## IN ORDER OF PAPER ####################3

# ==========================================
# TABLE 1: RESTRUCTURED FOR DUAL-LEVEL DATA
# ==========================================
hist(mec_brt$age, main = "Distribution of Age", xlab = "Age")
hist(prolific_dat_age$Age, main = "Distribution of Age", xlab = "Age")

# ------------------------------------------
# 1. IMPORT & MERGE DATASETS
# ------------------------------------------
# Import Prolific demographics
prolific_demo1 <- read.csv("project2/prolific_data/prolific_demographic_export_v1.csv")
prolific_demo2 <- read.csv("project2/prolific_data/prolific_demographic_export_v2.csv")
prolific_demo3 <- read.csv("project2/prolific_data/prolific_demographic_export_v3.csv")

# Merge demographics
prolific_demo <- rbind(prolific_demo1, prolific_demo2, prolific_demo3)

# Merge Age into the Prolific trial dataset
prolific_dat_age <- prolific_dat %>%
  left_join(
    prolific_demo %>% dplyr::select(Participant.id, Age),
    by = c("pid" = "Participant.id")
  )
prolific_dat_age$Age <- as.numeric(prolific_dat_age$Age)

# ------------------------------------------
# 2. STANDARDIZE THE COLUMNS AT TRIAL-LEVEL
# ------------------------------------------
# Validation Trial (Prolific) Raw Trial-Level Data
prolific_trials <- prolific_dat_age %>%
  select(
    pid, 
    age = Age, 
    os_full = OS.Version, 
    browser_full = Device.Model, 
    gpu_full = GraphicsDeviceName, 
    renderer = Device_Spec
  ) %>%
  mutate(
    renderer = ifelse(renderer == "GPU", "Hardware", "Software"),
    os_type = case_when(
      str_detect(os_full, "Windows") ~ "Windows",
      str_detect(os_full, "Mac|iOS|OS X") ~ "Apple",
      TRUE ~ "Other"
    ),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel",
      str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA",
      str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual",
      TRUE ~ "Other/Unknown"
    )
  )

# Parent Study (MEC - using mec_brt dataset) Raw Trial-Level Data
mec_trials <- mec_brt %>%
  select(
    pid, 
    age, 
    os_full = OSVersion, 
    browser_full = deviceModel, 
    gpu_full = graphicsDeviceName, 
    renderer = Is_Swiftshader
  ) %>%
  mutate(
    renderer = ifelse(renderer == "non_swiftshader", "Hardware", "Software"),
    os_type = case_when(
      str_detect(os_full, "Windows") ~ "Windows",
      str_detect(os_full, "Mac|iOS|OS X") ~ "Apple",
      TRUE ~ "Other"
    ),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel",
      str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA",
      str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual",
      TRUE ~ "Other/Unknown"
    )
  )

# ------------------------------------------
# 3. SPLIT AND ISOLATE BY DATA LEVEL
# ------------------------------------------

### --- LEVEL A: PARTICIPANT-LEVEL TABLE (AGE ONLY) ---
mec_pt <- mec_trials %>% 
  group_by(pid) %>% 
  summarize(age = first(age), .groups = "drop") %>% 
  mutate(study = "Parent Study (ACE)")

prolific_pt <- prolific_trials %>% 
  group_by(pid) %>% 
  summarize(age = first(age), .groups = "drop") %>% 
  mutate(study = "Validation Trial")

combined_pt <- bind_rows(mec_pt, prolific_pt)

tab1_age <- CreateTableOne(
  vars = "age", 
  strata = "study", 
  data = combined_pt
)

### --- LEVEL B: TRIAL-LEVEL TABLE (TECHNICAL PARADATA) ---
mec_trial_final <- mec_trials %>% 
  select(renderer, os_type, gpu_vendor) %>% 
  mutate(study = "Parent Study (ACE)")

prolific_trial_final <- prolific_trials %>% 
  select(renderer, os_type, gpu_vendor) %>% 
  mutate(study = "Validation Trial")

combined_trials <- bind_rows(mec_trial_final, prolific_trial_final)

tab1_tech <- CreateTableOne(
  vars = c("renderer", "os_type", "gpu_vendor"), 
  strata = "study", 
  data = combined_trials, 
  factorVars = c("renderer", "os_type", "gpu_vendor")
)

# ------------------------------------------
# 4. PRINT AND EXPORT THE SEPARATE TABLES (FIXED)
# ------------------------------------------
cat("\n=== PARTICIPANT LEVEL DEMOGRAPHICS (Age) ===\n")
print(tab1_age, nonnormal = "age", showAllLevels = TRUE)

cat("\n=== TRIAL LEVEL PARADATA (Renderer, OS, GPU Vendor) ===\n")
print(tab1_tech, showAllLevels = TRUE)

# Convert matrices for exporting—both MUST have showAllLevels = TRUE
matrix_age <- print(tab1_age, nonnormal = "age", showAllLevels = TRUE, printToggle = FALSE)
matrix_tech <- print(tab1_tech, showAllLevels = TRUE, printToggle = FALSE)

# Combine them sequentially now that column dimensions match
final_table_one <- rbind(matrix_age, matrix_tech)

# Clean up the row names to remove duplicate "p" or "n" rows if necessary
print(final_table_one, quote = FALSE)

write.csv(final_table_one, file = "project2/table_1_updated5_29.csv", row.names = TRUE)

## TABLE ONE OPTION 2 ------------------------------------------------------------
library(tidyverse)
library(tableone)

# --- 1. DATA IMPORT & INITIAL MERGE (Prolific) ---
prolific_demo1 <- read.csv("project2/prolific_data/prolific_demographic_export_v1.csv")
prolific_demo2 <- read.csv("project2/prolific_data/prolific_demographic_export_v2.csv")
prolific_demo3 <- read.csv("project2/prolific_data/prolific_demographic_export_v3.csv")

prolific_demo <- rbind(prolific_demo1, prolific_demo2, prolific_demo3)

# Merge Age with main dataset
prolific_dat_age <- prolific_dat %>%
  left_join(
    prolific_demo %>% dplyr::select(Participant.id, Age),
    by = c("pid" = "Participant.id")
  )
prolific_dat_age$Age <- as.numeric(prolific_dat_age$Age)


# --- 2. CLEANING & STANDARDIZATION ---

# Process Prolific Dataset (Validation Trial)
prolific_clean <- prolific_dat_age %>%
  select(
    pid, 
    age = Age, 
    os_full = OS.Version, 
    browser_full = Device.Model, 
    gpu_full = GraphicsDeviceName, 
    renderer = Device_Spec
  ) %>%
  mutate(
    renderer = ifelse(renderer == "GPU", "Hardware", "Software"),
    os_type = case_when(
      str_detect(os_full, "Windows") ~ "Windows",
      str_detect(os_full, "Mac|iOS|OS X") ~ "Apple",
      TRUE ~ "Other"
    ),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel",
      str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA",
      str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual",
      TRUE ~ "Other/Unknown"
    )
  )

# Process MEC Dataset (Parent Study)
mec_clean <- mec_data %>%
  select(
    pid, 
    Session, # Included for session-level grouping
    age, 
    os_full = OSVersion, 
    browser_full = deviceModel, 
    gpu_full = graphicsDeviceName, 
    renderer = Is_Swiftshader
  ) %>%
  mutate(
    renderer = ifelse(renderer == "non_swiftshader", "Hardware", "Software"),
    os_type = case_when(
      str_detect(os_full, "Windows") ~ "Windows",
      str_detect(os_full, "Mac|iOS|OS X") ~ "Apple",
      TRUE ~ "Other"
    ),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel",
      str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA",
      str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual",
      TRUE ~ "Other/Unknown"
    )
  )


# --- 3. FINAL AGGREGATION ---

# MEC: Group by pid AND Session to capture device switches between Pre/Post
mec_final <- mec_clean %>%
  group_by(pid, Session) %>%
  summarize(
    age = first(age),
    renderer = names(which.max(table(renderer))),
    os_type = names(which.max(table(os_type))),
    gpu_vendor = names(which.max(table(gpu_vendor))),
    .groups = "drop"
  ) %>%
  mutate(study = "Parent Study (ACE)")

# Prolific: Keep within-subjects renderer switch (N = 2 per participant)
prolific_final <- prolific_clean %>%
  distinct(pid, renderer, .keep_all = TRUE) %>%
  mutate(study = "Validation Trial")


# --- 4. CREATE TABLE ONE ---

combined_table <- bind_rows(mec_final, prolific_final)

# Define variables for the table
vars_to_summarize <- c("age", "renderer", "os_type", "gpu_vendor")
categorical_vars <- c("renderer", "os_type", "gpu_vendor")

tab1 <- CreateTableOne(
  vars = vars_to_summarize,
  strata = "study",
  data = combined_table,
  factorVars = categorical_vars
)

# Print and format for export
tab1_formatted <- print(tab1, 
                       showAllLevels = TRUE, 
                       quote = FALSE, 
                       noSpaces = TRUE, 
                       printToggle = FALSE)

# Display result
print(tab1_formatted)

# 1. Convert the tableone object to a matrix
tab1_matrix <- print(tab1_formatted, 
  showAllLevels = TRUE, 
  quote = FALSE, 
  noSpaces = TRUE, 
  printToggle = FALSE)

# 2. Export to CSV
write.csv(tab1_matrix, file = "Table1_Hardware_Analysis.csv")


library(dplyr)

# Parent Study (MEC) Trial Distribution
mec_trial_dist <- mec_data %>%
  count(Is_Swiftshader) %>%
  mutate(percent = n / sum(n) * 100)

# Validation Trial (Prolific) Trial Distribution
prolific_trial_dist <- prolific_dat %>%
  count(Device_Spec) %>%
  mutate(percent = n / sum(n) * 100)

print("--- MEC Trial Distribution ---")
print(mec_trial_dist)

print("--- Prolific Trial Distribution ---")
print(prolific_trial_dist)


### Table of FPS ----
# Define a function to get performance metrics
get_perf_stats <- function(df, renderer_col) {
  df %>%
    group_by(!!sym(renderer_col)) %>%
    summarise(
      mean_fps = mean(FPS_mean, na.rm = TRUE),
      sd_fps = sd(FPS_mean, na.rm = TRUE),
      # Calculating average variance to show 'jitter'
      avg_jitter = mean(sqrt(FPS_var), na.rm = TRUE), 
      n_trials = n()
    )
}

# Generate stats
mec_perf <- get_perf_stats(mec_data, "Is_Swiftshader")
prolific_perf <- get_perf_stats(prolific_dat, "Device_Spec")

# Print for manual table entry
print(mec_perf)
print(prolific_perf)



# RESULTS ----
library(ggplot2)
library(dplyr)
library(patchwork) # For combining panels cleanly

# JMIR Compliant Theme Settings
jmir_theme <- theme_classic(base_size = 12, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

# Color Palette (High contrast, color-blind friendly)
render_colors <- c("Hardware" = "#2c7bb6", "Software" = "#d7191c")

# -------------------------------------------------------------------------
# PANEL A: MEC Task-Specific Latency 
# Replace with your actual emmeans/predict data frame if available
# -------------------------------------------------------------------------
mec_plot_data <- mec_data %>%
  group_by(GameType, Is_Swiftshader) %>%
  summarise(
    Mean_RT = mean(RT), 
    SD_RT = sd(RT),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(
    SE = SD_RT / sqrt(N),
    Lower_CI = Mean_RT - (1.96 * SE),
    Upper_CI = Mean_RT + (1.96 * SE),
    Renderer = ifelse(Is_Swiftshader == "non_swiftshader", "Hardware", "Software")
  )

# Apply the renaming mapping to your final dataset
mec_plot_data <- mec_plot_data %>%
  mutate(
    GameType = case_when(
      GameType == "ISHIHARA"      ~ "Color Blindness Test",
      GameType == "COLOR_PICKING"  ~ "Color Swatch",
      GameType == "TASK_SWITCH_V2" ~ "Sun & Moon",
      GameType == "mAID"           ~ "AID",
      GameType == "BOXED"          ~ "Boxed",
      GameType == "FILTER"         ~ "Filter",
      GameType == "STROOP"         ~ "Color Tricker",
      GameType == "TNT"            ~ "Triangle Trace",
      GameType == "FLANKER"        ~ "Flanker",
      GameType == "SAAT_SUSTAINED" ~ "Venus UFO",
      GameType == "SAAT_IMPULSIVE" ~ "Mars UFO",
      GameType == "ADP"            ~ "Face Switch",
      GameType == "SPATIAL_CUEING" ~ "Compass",
      GameType == "BRT"            ~ "Basic Response Time",
      GameType == "Tova"           ~ "TOVA",
      TRUE                         ~ GameType # Keeps any unmatched categories as-is
    )
  )

panel_a <- ggplot(mec_plot_data, aes(x = Mean_RT, y = reorder(GameType, Mean_RT), color = Renderer)) +
    # 1. Draw the points FIRST (on the bottom layer)
    geom_point(size = 3) +
    
    # 2. Draw the black error bars SECOND (on the top layer, over the dots)
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.3, size = 0.8, alpha = 0.7, color = "black") +
    
    scale_color_manual(values = render_colors) +
    labs(
      x = "Response Time (ms)",
      y = "Assessment Task Type"
    ) +
    jmir_theme

ggsave("Figure1_Main_Findings.jpeg", plot = panel_a, width = 7.5, height = 5, dpi = 300)


# -------------------------------------------------------------------------
# PANEL B: Validation Study (Prolific Within-Subjects Shift)
# -------------------------------------------------------------------------
prolific_plot_data <- prolific_dat %>%
  mutate(Renderer = ifelse(Device_Spec == "GPU", "Hardware", "Software"))

panel_b <- ggplot(prolific_plot_data, aes(x = Renderer, y = RT, fill = Renderer)) +
  geom_violin(alpha = 0.4, color = NA, trim = TRUE) +
  geom_boxplot(width = 0.2, color = "#2b2b2b", outlier.shape = NA, alpha = 0.7, lwd = 0.7) +
  scale_fill_manual(values = render_colors) +
  labs(
    title = "B. Validation Study (Basic Response Time Task)",
    x = "Rendering Condition",
    y = "Reaction Time (ms)"
  ) +
  jmir_theme +
  theme(legend.position = "none") # Redundant legend removed

# Combine into a single manuscript-ready figure using patchwork
main_figure <- panel_a / panel_b + plot_layout(heights = c(2, 1))
ggsave("Figure1_Main_Findings.tiff", plot = main_figure, width = 8, height = 10, dpi = 300, compression = "lzw")
ggsave("Figure1_Main_Findings.jpeg", plot = main_figure, width = 8, height = 10, dpi = 300)

# option 2
mec_brt_plot_data <- mec_brt %>%
  mutate(Renderer = ifelse(Is_Swiftshader == "non_swiftshader", "Hardware", "Software"))

panel_a <- ggplot(mec_brt_plot_data, aes(x = Renderer, y = RT, fill = Renderer)) +
  geom_violin(alpha = 0.4, color = NA, trim = TRUE) +
  geom_boxplot(width = 0.2, color = "#2b2b2b", outlier.shape = NA, alpha = 0.7, lwd = 0.7) +
  scale_fill_manual(values = render_colors) +
  labs(
    title = "A. MEC Study (Basic Response Time Task)",
    x = "Rendering Condition",
    y = "Reaction Time (ms)"
  ) +
  jmir_theme +
  theme(legend.position = "none") # Redundant legend removed

# Combine into a single manuscript-ready figure using patchwork
main_figure <- panel_a / panel_b 
ggsave("Figure1_Main_Findings.tiff", plot = main_figure, width = 8, height = 10, dpi = 300, compression = "lzw")
ggsave("Figure1_Main_Findings2.jpeg", plot = main_figure, width = 8, height = 10, dpi = 300)
# -------------------------------------------------------------------------
# PANEL A: FPS CV Distribution Shift
# -------------------------------------------------------------------------
panel_cv_a <- ggplot(prolific_plot_data, aes(x = Renderer, y = CV, fill = Renderer)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.1, alpha = 0.8) +
  scale_fill_manual(values = render_colors) +
  labs(
    title = "A. Frame Instability by Renderer Type",
    x = "Rendering Condition",
    y = "FPS Coefficient of Variation (CV %)"
  ) +
  jmir_theme +
  theme(legend.position = "none")

# -------------------------------------------------------------------------
# PANEL B: Spaghetti Plot of Random Slopes
# -------------------------------------------------------------------------
# Extract random effects and fixed effects from your lmer model to plot precisely:
# For draft purposes, we use a random sample of 30 participants to keep it scannable
sample_pids <- sample(unique(prolific_plot_data$pid), 74, replace = FALSE)

spaghetti_data <- prolific_plot_data %>%
  filter(pid %in% sample_pids) %>%
  mutate(log_RT = log(RT))

panel_cv_b <- ggplot(spaghetti_data, aes(x = CV_scaled, y = log_RT, group = pid)) +
  # Individual patient random slopes (faint, thin lines)
  geom_smooth(method = "lm", se = FALSE, color = "grey60", size = 0.4, alpha = 0.5) +
  # Population-level fixed effect (thick, bold flat line)
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "#2b2b2b", size = 1.5) +
  labs(
    title = "B. Individual Slopes of FPS CV on log(RT)",
    x = "FPS Coefficient of Variation (Scaled)",
    y = "log(Reaction Time)"
  ) +
  jmir_theme

# Combine and export mechanism figures
mechanism_figure <- (panel_cv_a + panel_cv_b) + plot_layout(ncol = 2)
ggsave("Figure2_Mechanism_FPS_CV.tiff", plot = mechanism_figure, width = 10, height = 5, dpi = 300, compression = "lzw")
ggsave("Figure2_Mechanism_FPS_CV.jpeg", plot = mechanism_figure, width = 10, height = 5, dpi = 300)

## ver 2
###
library(marginaleffects)

# 1. Generate predictions holding Device_Spec at its reference level (or averaged)
# to isolate the pure effect of CV_scaled as calculated by your model
pred_fixed <- predictions(
  final_model, 
  newdata = datagrid(
    CV_scaled = seq(min(spaghetti_data$CV_scaled), max(spaghetti_data$CV_scaled), length.out = 100), 
    Device_Spec = "GPU"
  ),
  re.form = NA
)

panel_cv_b <- ggplot(spaghetti_data, aes(x = CV_scaled, y = log_RT)) +
  # Individual patient random slopes (faint, thin lines)
  geom_smooth(aes(group = pid), method = "lm", se = FALSE, color = "grey60", size = 0.4, alpha = 0.5) +
  
  # TRUE population-level fixed effect from your model output (adjusted flat line)
  # geom_line(data = pred_fixed, aes(x = CV_scaled, y = estimate), color = "#2b2b2b", size = 1.5) +
  
  labs(
    title = "B. Individual Slopes of FPS CV on log(RT)",
    x = "FPS Coefficient of Variation (Scaled)",
    y = "log(Reaction Time)"
  ) +
  jmir_theme

# 1. Ensure you have the clean, flat population prediction vector
# 1. Generate predictions for BOTH GPU and CPU from your original model
pred_parallel <- predictions(
  final_model, # Your original model: log(RT) ~ Device_Spec + CV_scaled + ...
  newdata = datagrid(
    CV_scaled = seq(min(spaghetti_data$CV_scaled), max(spaghetti_data$CV_scaled), length.out = 100),
    Device_Spec = c("GPU", "CPU") # This forces separate predictions for each condition
  ),
  re.form = NA
)

# 2. Plot the scatter points, grey random slopes, and TWO parallel population averages
panel_cv_v2 <- ggplot(spaghetti_data) +
  # Raw data points
  geom_point(aes(x = CV_scaled, y = log_RT, color = Device_Spec), alpha = 0.25, size = 1) +
  
  # Individual patient random slopes (faint, thin grey lines)
  #geom_smooth(aes(x = CV_scaled, y = log_RT, group = pid), method = "lm", se = FALSE, color = "grey75", size = 0.3, alpha = 0.4) +
  
  # Two parallel population lines (one for GPU, one for CPU)
  geom_line(data = pred_parallel, aes(x = CV_scaled, y = estimate, color = Device_Spec), size = 1.5) +
  
  # Apply your exact color palette to BOTH points and lines
  scale_color_manual(
    values = c("GPU" = "#2c7bb6", "CPU" = "#d7191c"), 
    labels = c("GPU" = "Hardware-Accelerated", "CPU" = "Software-Based")
  ) +
  
  labs(
    title = "B. Baseline Latency Adjustments",
    x = "FPS Coefficient of Variation (Scaled)", # Updated label to match 0-125 percentage scale
    y = "log(Reaction Time)",
    color = "Rendering Architecture"
  ) +
  jmir_theme +
  theme(legend.position = "bottom")
###

final_combined_figure <- panel_cv_a + panel_cv_b + panel_cv_v2 + 
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom")

# Preview the figure
final_combined_figure

# Save a high-res version for your manuscript
ggsave("figure4_final_3panel.png", plot = final_combined_figure, width = 15, height = 5, dpi = 300)

# MODEL ASSUMPTION CHECKS ----
# 1. Extract diagnostics from best-fitting models
base_model <- lmer(log(RT) ~ Device_Spec + (1 | pid), data = prolific_dat)
final_model <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid), data = prolific_dat)
interaction_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = mec_data)
fps_model <- lmer(CV_scaled ~ Device_Spec + (1 | pid), data = prolific_dat)
brt_model <- lmer(log(RT) ~ Is_Swiftshader + (1 | pid), data = mec_brt)


diag_data <- data.frame(
  Fitted    = fitted(base_model),
  Residuals = residuals(base_model, type = "pearson") # Standardized/Pearson residuals
)
diag_data <- data.frame(
  Fitted    = fitted(final_model),
  Residuals = residuals(final_model, type = "pearson") # Standardized/Pearson residuals
)
diag_data <- data.frame(
  Fitted    = fitted(interaction_model),
  Residuals = residuals(interaction_model, type = "pearson") # Standardized/Pearson residuals
)
diag_data <- data.frame(
  Fitted    = fitted(fps_model),
  Residuals = residuals(fps_model, type = "pearson") # Standardized/Pearson residuals
)
diag_data <- data.frame(
  Fitted    = fitted(brt_model),
  Residuals = residuals(brt_model, type = "pearson") # Standardized/Pearson residuals
)

# 2. Plot A: Residuals vs. Fitted Values (Homoscedasticity)
plot_homosc <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "#2c3e50", size = 1) +  # Transparent points for dense data
  geom_hline(yintercept = 0, color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "#2980b9", se = FALSE, linewidth = 0.8, method.args = list(degree = 1)) +
  labs(
    title = "A. Residuals vs. Fitted Values",
    x = "Fitted Values (log-ms)",
    y = "Standardized Residuals"
  ) +
  theme_classic(base_size = 11) + # Standard academic text sizes
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid = element_blank()
  )

# 3. Plot B: Residual Q-Q Plot (Normality)
plot_norm <- ggplot(diag_data, aes(sample = Residuals)) +
  geom_qq(alpha = 0.3, color = "#2c3e50", size = 1) +
  geom_qq_line(color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "B. Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    panel.grid = element_blank()
  )

# 4. Combine into a single multipanel figure
diagnostic_panel <- grid.arrange(plot_homosc, plot_norm, ncol = 2)

# 5. Export as a high-resolution TIFF or PNG for JMIR submission
# JMIR prefers 300-600 DPI for figures.
ggsave(
  filename = "Multimedia_Appendix_X5_Diagnostics.png", 
  plot = diagnostic_panel,
  width = 10, 
  height = 4.5, 
  dpi = 300
)

# 1. Extract residuals and fitted values into a data frame
mec_diag <- data.frame(
  pid       = mec_data$pid,       # or your MEC dataset ID column
  Task      = mec_data$GameType,      # include task type for stratification
  Fitted    = fitted(interaction_model),
  Residuals = residuals(interaction_model, type = "pearson")
)

# 2. Downsample to a statistically representative subset (e.g., 10,000 total trials)
# Grouping by task ensures the entire behavioral battery is equally represented
set.seed(42) # For reproducibility
mec_subsample <- mec_diag %>%
  group_by(Task) %>%
  sample_n(size = min(n(), 2500)) %>% # Pulls up to 2,500 random trials per task type
  ungroup()

# 3. Plot exactly as you did before using the downsampled dataset
plot_homosc <- ggplot(mec_subsample, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.2, color = "#2c3e50", size = 0.8) + # Higher alpha since less data
  geom_hline(yintercept = 0, color = "#e74c3c", linetype = "dashed") +
  geom_smooth(method = "loess", color = "#2980b9", se = FALSE, linewidth = 0.8) +
  labs(title = "A. Residuals vs. Fitted Values (MEC Subset)", x = "Fitted Values", y = "Standardized Residuals") +
  theme_classic()

plot_norm <- ggplot(mec_subsample, aes(sample = Residuals)) +
  geom_qq(alpha = 0.2, color = "#2c3e50", size = 0.8) +
  geom_qq_line(color = "#e74c3c", linetype = "dashed") +
  labs(title = "B. Normal Q-Q Plot (MEC Subset)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_classic()

mec_diagnostic_panel <- grid.arrange(plot_homosc, plot_norm, ncol = 2)

ggsave(
  filename = "Multimedia_Appendix_X4_Diagnostics.png", 
  plot = mec_diagnostic_panel,
  width = 10, 
  height = 4.5, 
  dpi = 300
)


# =========================================================================
# 3. GENERATE POPULATION MODEL PREDICTIONS 
# =========================================================================
# Reconstruct final model to extract unconfounded parallel slopes
final_model <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid), 
                    data = prolific_plot_data)

pred_parallel <- predictions(
  final_model,
  newdata = datagrid(
    CV_scaled = seq(min(spaghetti_data$CV_scaled), max(spaghetti_data$CV_scaled), length.out = 100),
    Device_Spec = c("GPU", "CPU")
  ),
  re.form = NA
)

# =========================================================================
# 4. PANEL GENERATION
# =========================================================================

# PANEL A: Left-hand Boxplot
panel_cv_a <- ggplot(prolific_plot_data, aes(x = Renderer, y = CV, fill = Renderer)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.1, alpha = 0.8) +
  scale_fill_manual(values = render_colors) +
  labs(
    title = "A. Frame Instability by Renderer Type",
    x = "Rendering Condition",
    y = "FPS Coefficient of Variation (CV %)"
  ) +
  jmir_theme +
  theme(legend.position = "none")

# PANEL B: Right-hand Baseline Latency Adjustments (Parallel Model Slopes)
panel_cv_v2 <- ggplot(spaghetti_data) +
  # Raw data points matching your high transparency look
  geom_point(aes(x = CV_scaled, y = log_RT, color = Device_Spec), alpha = 0.25, size = 1) +
  
  # Dual parallel population lines from final_model
  geom_line(data = pred_parallel, aes(x = CV_scaled, y = estimate, color = Device_Spec), size = 1.5) +
  
  # Dynamic scales mapping matching image_bdab23.png
  scale_color_manual(
    values = c("GPU" = "#2c7bb6", "CPU" = "#d7191c"), 
    labels = c("GPU" = "Hardware-Accelerated", "CPU" = "Software-Based")
  ) +
  labs(
    title = "B. Baseline Latency Adjustments",
    x = "FPS Coefficient of Variation (Scaled)", 
    y = "log(Response Time)",
    color = "Rendering Architecture"
  ) +
  jmir_theme +
  theme(legend.position = "bottom")

# =========================================================================
# 5. COMPILE AND EXPORT (As rendered in image_bdab23.png)
# =========================================================================
# Patchwork row layout combining panel A and panel C (renamed B), skipping spaghetti panel
final_2panel_figure <- panel_cv_a + panel_cv_v2 + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")

# Display in your IDE window
final_2panel_figure

# Save final high-res copy for manuscript submission
ggsave("figure2_final_2panel.png", plot = final_2panel_figure, width = 11, height = 5, dpi = 300)

