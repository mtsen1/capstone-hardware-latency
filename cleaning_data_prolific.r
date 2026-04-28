# ==========================================
# 1. SETUP & LIBRARIES
# ==========================================
library(tidyverse)
library(dplyr)
library(stringr)

# ==========================================
# 2. IMPORT & TAG HARDWARE GROUPS
# ==========================================
# Load and filter CPU (SwiftShader/Basic)
cpu <- read.csv("project2/prolific_data/BRT_cpu.csv") %>%
  filter(str_detect(Graphics.Device.Name, "Basic|SwiftShader")) %>%
  mutate(
    Device_Spec = "CPU",
    pid = Participant.Id %>% str_trim() %>% str_remove("^vulkanon_")
  )

# Load and filter GPU (Hardware Accelerated)
gpu <- read.csv("project2/prolific_data/BRT_gpu.csv") %>%
  filter(!str_detect(Graphics.Device.Name, "Basic|SwiftShader")) %>%
  mutate(
    Device_Spec = "GPU",
    pid = Participant.Id %>% str_trim() %>% str_remove("^vulkanoff_")
  )

# ==========================================
# 3. INTERSECT & MERGE
# ==========================================
# Keep only participants who completed BOTH conditions
complete_pids <- intersect(cpu$pid, gpu$pid)

dat_merged <- bind_rows(cpu, gpu) %>%
  filter(pid %in% complete_pids) %>%
  rename(
    GraphicsDeviceName = Graphics.Device.Name,
    RT = Response.Time,
    FPS_mean = FPS.Mean,
    FPS_var = FPS.Variance,
    Condition = Condition
  )

# ==========================================
# 4. ACCURACY & THROTTLING FILTERS
# ==========================================
dat_valid <- dat_merged %>%
  # Keep only actual trials (no practice) and correct responses
  filter(Response.Window != -1, Correct.Button == 1) %>%
  
  # Calculate CV and remove browser-throttled trials
  mutate(CV = (sqrt(FPS_var) / FPS_mean) * 100) %>%
  filter(!is.na(CV)) %>%
  mutate(Is_Throttled = ifelse(FPS_mean < 10 & CV < 5, TRUE, FALSE)) %>%
  filter(Is_Throttled == FALSE) %>%
  dplyr::select(-Is_Throttled)

# ==========================================
# 5. ALIGNED LOG-IQR OUTLIER REMOVAL
# ==========================================
prolific_cleaned_iqr <- dat_valid %>%
  group_by(Device_Spec) %>%
  mutate(
    # 1. Safely calculate log(RT) ignoring non-positive values
    log_rt = suppressWarnings(log(ifelse(RT > 0, RT, NA))),
    
    # 2. Calculate Q3 and IQR on the LOG scale per device
    Q3_log = quantile(log_rt, 0.75, na.rm = TRUE),
    IQR_log = IQR(log_rt, na.rm = TRUE),
    
    # 3. Calculate Upper Limit on the LOG scale
    log_upper_limit = Q3_log + (1.5 * IQR_log),
    
    # 4. Exponentiate back to standard milliseconds for filtering
    upper_limit_ms = exp(log_upper_limit)
  ) %>%
  ungroup() %>%
  
  # 5. Filter bounds: Floor is exactly 200ms, Ceiling is the log-adjusted IQR limit
  filter(RT >= 200 & RT <= upper_limit_ms) %>%
  
  # 6. Clean up temporary math columns
  dplyr::select(-log_rt, -Q3_log, -IQR_log, -log_upper_limit, -upper_limit_ms)

# ==========================================
# 6. VERIFICATION SUMMARY
# ==========================================
attrition_summary <- dat_valid %>%
  group_by(Device_Spec) %>%
  summarise(
    Total_Valid_Trials = n(),
    
    # Recalculate inline for the summary table
    q3_log = quantile(log(RT[RT > 0]), 0.75, na.rm = TRUE),
    iqr_log = IQR(log(RT[RT > 0]), na.rm = TRUE),
    upper_limit_ms = exp(q3_log + (1.5 * iqr_log)),
    
    Kept_Trials = sum(RT >= 200 & RT <= upper_limit_ms, na.rm = TRUE),
    Lost_Trials = Total_Valid_Trials - Kept_Trials,
    Percent_Lost = round((Lost_Trials / Total_Valid_Trials) * 100, 2),
    .groups = "drop"
  )

print("--- Data Loss from 200ms Floor and Log-IQR Ceiling ---")
print(attrition_summary)

# Save the final aligned dataset
write.csv(prolific_cleaned_iqr, "project2/prolific_data/prolific_cleaned_iqr_NEW.csv", row.names = FALSE)
