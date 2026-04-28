# ==========================================
# 1. SETUP & LIBRARIES
# ==========================================
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

# ==========================================
# 2. DEFINE CLEANING FUNCTION
# ==========================================
target_cols <- c(
  "GameType", "pid", "age", "key", "OSVersion", 
  "graphicsDeviceName", "clientTimeZone", "timeGameplayedUtc", "systemMemorySize", "deviceModel",
  "result_details:fpsMean", "result_details:fpsVariance", "result_details:Condition",
  "result_details:trialNumber", "result_details:responseTime", "result_details:feedbackResult"
)

process_dataset_safely <- function(df) {
  df %>%
    filter(tolower(key) == "real") %>%
    {
      if ("result_details.feedbackResult" %in% names(.)) {
        filter(., tolower(result_details.feedbackResult) == "green")
      } else { . }
    } %>%
    mutate(
      `result_details:fpsMean` = if("result_details.fpsMean" %in% names(.)) result_details.fpsMean else NA,
      `result_details:fpsVariance` = if("result_details.fpsVariance" %in% names(.)) result_details.fpsVariance else NA,
      `result_details:trialNumber` = if("result_details.trialNumber" %in% names(.)) result_details.trialNumber else NA,
      `result_details:Condition` = coalesce(
        if("result_details.Condition" %in% names(.)) result_details.Condition else NA,
        if("result_details.Mode" %in% names(.)) result_details.Mode else NA
      ),
      `result_details:responseTime` = coalesce(
        if("result_details.responseTime" %in% names(.)) result_details.responseTime else NA,
        if("result_details.RT" %in% names(.)) result_details.RT else NA,
        if("result_details.ResponseSpeed" %in% names(.)) result_details.ResponseSpeed else NA
      ),
      `result_details:feedbackResult` = if("result_details.feedbackResult" %in% names(.)) result_details.feedbackResult else NA
    ) %>%
    dplyr::select(any_of(target_cols))
}

# ==========================================
# 3. LOAD & MERGE RAW DATA
# ==========================================
# Load core datasets
full_data <- read.csv("MEC/data-raw/raw_combined_all_cols.csv")
pids <- read.csv("pids.csv") 
aid <- read.csv("MEC/data-raw/other/combined_outputs/combined_mAID.csv")
tova <- read.csv("MEC/data-raw/other/combined_outputs/combined_Tova.csv")

# Join PIDs to ACE data
full_data1 <- full_data

# Process and combine
cleaned_ace <- process_dataset_safely(full_data1)
cleaned_aid <- process_dataset_safely(aid)
cleaned_tova <- process_dataset_safely(tova)

final_combined_data <- bind_rows(cleaned_ace, cleaned_aid, cleaned_tova)

# combine with pids to add group
final_combined_data <- final_combined_data %>% left_join(pids, by = "pid")

# ==========================================
# 4. PRE-PROCESSING (Sessions, Age, NAs)
# ==========================================
final_combined_data2 <- final_combined_data %>%
  mutate(timeGameplayedUtc = ymd_hms(timeGameplayedUtc)) %>%
  arrange(pid, GameType, timeGameplayedUtc) %>%  
  group_by(pid, GameType) %>% 
  mutate(
    days_from_first = as.numeric(difftime(timeGameplayedUtc, min(timeGameplayedUtc), units = "days")),
    cutoff_time = ifelse(max(days_from_first) >= 7, timeGameplayedUtc[which(days_from_first >= 7)[1]], NA),
    Session = case_when(
      is.na(cutoff_time) ~ "Pre",
      timeGameplayedUtc < cutoff_time ~ "Pre",
      TRUE ~ "Post"
    )
  ) %>%
  dplyr::select(-days_from_first, -cutoff_time) %>%
  ungroup()

# Handle missing data (-1 to NA) and fill ages
final_combined_data2[final_combined_data2 == -1] <- NA

raw_data <- final_combined_data2 %>%
  group_by(pid) %>%
  fill(age, .direction = "downup") %>%
  ungroup()

# ==========================================
# 5. CLEANING HARDWARE & RT METRICS
# ==========================================
raw_data_prepped <- raw_data %>%
  # Remove missing RTs (using backticks for the colon)
  filter(!is.na(`result_details:responseTime`)) %>%
  mutate(
    pid = as.character(pid),
    systemMemorySize = as.character(systemMemorySize),
    
    # GPU logic
    GPU_Brand = case_when(
      str_detect(tolower(graphicsDeviceName), "google") ~ "Google",
      str_detect(tolower(graphicsDeviceName), "intel") ~ "Intel",
      str_detect(tolower(graphicsDeviceName), "amd") ~ "AMD",
      str_detect(tolower(graphicsDeviceName), "apple") ~ "Apple",
      str_detect(tolower(graphicsDeviceName), "nvidia") ~ "Nvidia",
      TRUE ~ "Other"
    ),
    Is_Swiftshader = ifelse(GPU_Brand == "Google", "swiftshader", "non_swiftshader"),
    
    # Convert RT to ms for specific games
    RT = case_when(
      GameType %in% c("Tova", "mAID") ~ `result_details:responseTime` * 1000,
      TRUE ~ `result_details:responseTime`
    )
  ) %>%
  rename(
    FPS_mean = `result_details:fpsMean`,
    FPS_var = `result_details:fpsVariance`,
    Condition = `result_details:Condition`,
    Trial_Num = `result_details:trialNumber`,
    Feedback = `result_details:feedbackResult`
  ) %>%
  dplyr::select(-`result_details:responseTime`) # Drop old RT column

# ==========================================
# 6. IQR OUTLIER REMOVAL (LOG-TRANSFORMED)
# ==========================================
ignore_apps <- c("REVERSE_SPATIAL_SPAN", "SPATIAL_SPAN")

final_dataset_CLEANED_iqr <- raw_data_prepped %>%
  filter(!GameType %in% ignore_apps) %>%
  group_by(GameType, Is_Swiftshader) %>% 
  mutate(
    # 1. Safely calculate log(RT) to maintain row length (ignoring non-positive RTs)
    log_rt = suppressWarnings(log(ifelse(RT > 0, RT, NA))),
    
    # 2. Calculate Q3 and IQR on the LOG scale
    Q3_log = quantile(log_rt, 0.75, na.rm = TRUE),
    IQR_log = IQR(log_rt, na.rm = TRUE),
    
    # 3. Calculate Upper Limit on the LOG scale
    log_upper_limit = Q3_log + (1.5 * IQR_log),
    
    # 4. Exponentiate back to standard milliseconds for filtering
    upper_limit_ms = exp(log_upper_limit)
  ) %>%
  ungroup() %>%
  
  # 5. Filter bounds: Floor is now 200ms, Ceiling is the log-adjusted IQR limit
  filter(RT >= 200 & RT <= upper_limit_ms) %>%
  
  # 6. Clean up temporary math columns
  dplyr::select(-log_rt, -Q3_log, -IQR_log, -log_upper_limit, -upper_limit_ms)

# ==========================================
# 7. DATA LOSS ANALYSIS
# ==========================================
loss_analysis_iqr <- raw_data_prepped %>%
  filter(!GameType %in% ignore_apps) %>%
  group_by(GameType, Is_Swiftshader) %>%
  summarise(
    Total_Rows = n(),
    
    # Calculate q3 and iqr directly inline so it only returns a single value per group
    q3_log = quantile(log(RT[RT > 0]), 0.75, na.rm = TRUE),
    iqr_log = IQR(log(RT[RT > 0]), na.rm = TRUE),
    upper_limit_ms = exp(q3_log + (1.5 * iqr_log)),
    
    # Count rows kept (Floor is 200ms)
    Kept_Rows = sum(RT >= 200 & RT <= upper_limit_ms, na.rm = TRUE),
    Lost_Rows = Total_Rows - Kept_Rows,
    Percent_Lost = round((Lost_Rows / Total_Rows) * 100, 2),
    .groups = "drop" 
  ) %>%
  dplyr::select(GameType, Is_Swiftshader, Total_Rows, Lost_Rows, Percent_Lost) %>%
  arrange(desc(Percent_Lost))

print(loss_analysis_iqr, n = 40)
# ==========================================
# 8. EXPORT CLEANED DATA
# ==========================================
write.csv(final_dataset_CLEANED_iqr, "project2/cleaned_data_FULL_iqr_NEW.csv", row.names = FALSE)
print("Data cleaned and saved successfully!")




