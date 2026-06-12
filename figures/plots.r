# =========================================================================
# LIBRARIES & INITIAL CONFIGURATION
# =========================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(tableone)
library(patchwork)
library(gridExtra)
library(marginaleffects)
library(lme4)

# Global JMIR Compliant Theme Settings [cite: 43]
jmir_theme <- theme_classic(base_size = 12, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

render_colors <- c("Hardware" = "#2c7bb6", "Software" = "#d7191c")

# =========================================================================
# DATA LOADING & UNIFICATION
# =========================================================================
# 1. Study 1 (MEC Parent Dataset) [cite: 1, 44]
mec_data <- read_excel("project2/mec_lmer_posthoc.xlsx") 

# 2. Study 2 (Prolific Validation Dataset) [cite: 11]
prolific_dat <- read.csv("project2/prolific_data/prolific_cleaned_iqr_NEW.csv")

# 3. Demographics & Age Merging [cite: 28, 35]
prolific_demo1 <- read.csv("project2/prolific_data/prolific_demographic_export_v1.csv")
prolific_demo2 <- read.csv("project2/prolific_data/prolific_demographic_export_v2.csv")
prolific_demo3 <- read.csv("project2/prolific_data/prolific_demographic_export_v3.csv")
prolific_demo  <- rbind(prolific_demo1, prolific_demo2, prolific_demo3)

prolific_dat_age <- prolific_dat %>%
  left_join(prolific_demo %>% dplyr::select(Participant.id, Age), by = c("pid" = "Participant.id")) %>%
  mutate(
    Age = as.numeric(Age),
    Device_Spec = relevel(factor(Device_Spec), ref = "GPU"),
    pid = as.character(pid),
    CV = (sqrt(FPS_var) / FPS_mean) * 100, # Trial-level Coefficient of Variation [cite: 11]
    Renderer = ifelse(Device_Spec == "GPU", "Hardware", "Software")
  )
prolific_dat_age$CV_scaled <- scale(prolific_dat_age$CV)

# Isolate Basic Response Time (BRT) subset from Study 1 for unified data modeling [cite: 30, 290]
mec_brt <- mec_data %>% filter(GameType == "BRT")

# =========================================================================
# MANUSCRIPT TABLE 1: Baseline Demographics & Technical Paradata [cite: 259]
# =========================================================================
# Standardize Study 2 Profile [cite: 35]
prolific_clean <- prolific_dat_age %>%
  select(pid, age = Age, os_full = OS.Version, gpu_full = GraphicsDeviceName, renderer = Renderer) %>%
  mutate(
    os_type = case_when(str_detect(os_full, "Windows") ~ "Windows", str_detect(os_full, "Mac|iOS|OS X") ~ "Apple", TRUE ~ "Other"),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel", str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA", str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual", TRUE ~ "Other/Unknown"
    )
  )

# Standardize Study 1 Profile [cite: 31, 37]
mec_clean <- mec_data %>%
  select(pid, Session, age, os_full = OSVersion, gpu_full = graphicsDeviceName, renderer = Is_Swiftshader) %>%
  mutate(
    renderer = ifelse(renderer == "non_swiftshader", "Hardware", "Software"),
    os_type = case_when(str_detect(os_full, "Windows") ~ "Windows", str_detect(os_full, "Mac|iOS|OS X") ~ "Apple", TRUE ~ "Other"),
    gpu_vendor = case_when(
      str_detect(gpu_full, "Intel") ~ "Intel", str_detect(gpu_full, "Apple") ~ "Apple",
      str_detect(gpu_full, "NVIDIA|GeForce") ~ "NVIDIA", str_detect(gpu_full, "AMD|Radeon") ~ "AMD",
      str_detect(gpu_full, "Microsoft Basic Render Driver|SwiftShader") ~ "Software/Virtual", TRUE ~ "Other/Unknown"
    )
  )

# Collapse to Participant-Session metrics to match reporting levels [cite: 38, 39]
mec_final <- mec_clean %>%
  group_by(pid, Session) %>%
  summarize(
    age = first(age), renderer = names(which.max(table(renderer))),
    os_type = names(which.max(table(os_type))), gpu_vendor = names(which.max(table(gpu_vendor))), .groups = "drop"
  ) %>%
  mutate(study = "Study 1 (Parent Study)")

prolific_final <- prolific_clean %>%
  distinct(pid, renderer, .keep_all = TRUE) %>%
  mutate(study = "Study 2 (Validation Trial)")

combined_table <- bind_rows(mec_final, prolific_final)

tab1 <- CreateTableOne(
  vars = c("age", "renderer", "os_type", "gpu_vendor"), strata = "study",
  data = combined_table, factorVars = c("renderer", "os_type", "gpu_vendor")
)

tab1_matrix <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) [cite: 40]
write.csv(tab1_matrix, file = "Table1_Hardware_Analysis.csv") [cite: 41]

# =========================================================================
# MANUSCRIPT FIGURE 3: Main Empirical BRT Distributions [cite: 344]
# =========================================================================
# Panel A: Study 1 BRT Task [cite: 51, 345]
mec_brt_plot_data <- mec_brt %>%
  mutate(Renderer = ifelse(Is_Swiftshader == "non_swiftshader", "Hardware", "Software"))

panel_3a <- ggplot(mec_brt_plot_data, aes(x = Renderer, y = RT, fill = Renderer)) +
  geom_violin(alpha = 0.4, color = NA, trim = TRUE) + [cite: 49]
  geom_boxplot(width = 0.2, color = "#2b2b2b", outlier.shape = NA, alpha = 0.7, lwd = 0.7) +
  scale_fill_manual(values = render_colors) +
  labs(x = "Rendering Condition", y = "Reaction Time (ms)") +
  jmir_theme +
  theme(legend.position = "none")

# Panel B: Study 2 BRT Task [cite: 50, 352]
panel_3b <- ggplot(prolific_dat_age, aes(x = Renderer, y = RT, fill = Renderer)) +
  geom_violin(alpha = 0.4, color = NA, trim = TRUE) + [cite: 49]
  geom_boxplot(width = 0.2, color = "#2b2b2b", outlier.shape = NA, alpha = 0.7, lwd = 0.7) +
  scale_fill_manual(values = render_colors) +
  labs(x = "Rendering Condition", y = "Reaction Time (ms)") +
  jmir_theme +
  theme(legend.position = "none")

figure_3 <- panel_3a / panel_3b + plot_layout(ncol = 1) [cite: 52]
ggsave("Figure3_BRT_Comparisons.png", plot = figure_3, width = 8, height = 10, dpi = 300)

# =========================================================================
# MANUSCRIPT FIGURE 4: Mechanistic Path & Structural Instability [cite: 362]
# =========================================================================
# Reconstruct best-fitting random-slopes model to derive population estimates [cite: 69]
final_model <- lmer(log(RT) ~ Device_Spec + CV_scaled + (1 + CV_scaled | pid), data = prolific_dat_age)

set.seed(42)
sample_pids <- sample(unique(prolific_dat_age$pid), 74, replace = FALSE) [cite: 53]
spaghetti_data <- prolific_dat_age %>% filter(pid %in% sample_pids) %>% mutate(log_RT = log(RT))

pred_parallel <- predictions(
  final_model,
  newdata = datagrid(
    CV_scaled = seq(min(spaghetti_data$CV_scaled), max(spaghetti_data$CV_scaled), length.out = 100),
    Device_Spec = c("GPU", "CPU") [cite: 58]
  ),
  re.form = NA
)

# Panel A: Instability Boxplot [cite: 53, 366]
panel_4a <- ggplot(prolific_dat_age, aes(x = Renderer, y = CV, fill = Renderer)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.1, alpha = 0.8) + [cite: 70]
  scale_fill_manual(values = render_colors) +
  labs(x = "Rendering Condition", y = "FPS Coefficient of Variation (CV %)") +
  jmir_theme +
  theme(legend.position = "none")

# Panel B: Parallel Latency Overheads [cite: 60, 367]
panel_4b <- ggplot(spaghetti_data) +
  geom_point(aes(x = CV_scaled, y = log_RT, color = Device_Spec), alpha = 0.25, size = 1) + [cite: 72]
  geom_line(data = pred_parallel, aes(x = CV_scaled, y = estimate, color = Device_Spec), size = 1.5) + [cite: 59]
  scale_color_manual(
    values = c("GPU" = "#2c7bb6", "CPU" = "#d7191c"), 
    labels = c("GPU" = "Hardware-Accelerated", "CPU" = "Software-Based")
  ) +
  labs(x = "FPS Coefficient of Variation (Scaled)", y = "log(Reaction Time)", color = "Rendering Architecture") +
  jmir_theme +
  theme(legend.position = "bottom")

figure_4 <- panel_4a + panel_4b + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom") [cite: 73]
ggsave("Figure4_Mechanism_FPS_CV.png", plot = figure_4, width = 11, height = 5, dpi = 300)

# =========================================================================
# SUPPLEMENTARY APPENDIX 3 & 4: Residual Diagnostics [cite: 131, 157]
# =========================================================================
# Generate and plot model diagnostics (Pearson Residuals) [cite: 62, 63]
generate_diagnostic_plots <- function(model_object, dataset_title) {
  diag_data <- data.frame(
    Fitted    = fitted(model_object),
    Residuals = residuals(model_object, type = "pearson")
  )
  
  # Downsample for dense clusters if processing parent study [cite: 66, 67]
  if (nrow(diag_data) > 30000) {
    set.seed(42)
    diag_data <- diag_data %>% sample_n(10000)
  }
  
  p_homosc <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.3, color = "#2c3e50", size = 1) + [cite: 63]
    geom_hline(yintercept = 0, color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
    geom_smooth(method = "loess", color = "#2980b9", se = FALSE, linewidth = 0.8, method.args = list(degree = 1)) + [cite: 63]
    labs(title = paste("A. Residuals vs. Fitted Values (", dataset_title, ")", sep=""), x = "Fitted Values (log-ms)", y = "Standardized Residuals") + [cite: 64, 68]
    theme_classic(base_size = 11) + theme(plot.title = element_text(face = "bold"))
    
  p_norm <- ggplot(diag_data, aes(sample = Residuals)) +
    geom_qq(alpha = 0.3, color = "#2c3e50", size = 1) + [cite: 65]
    geom_qq_line(color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
    labs(title = paste("B. Normal Q-Q Plot (", dataset_title, ")", sep=""), x = "Theoretical Quantiles", y = "Sample Quantiles") + [cite: 65]
    theme_classic(base_size = 11) + theme(plot.title = element_text(face = "bold"))
    
  return(arrangeGrob(p_homosc, p_norm, ncol = 2)) [cite: 65]
}

# Base Model for Study 1 Evaluation [cite: 61, 62]
interaction_model <- lmer(log(RT) ~ Is_Swiftshader * GameType + (1 | pid), data = mec_data)
s1_diag <- generate_diagnostic_plots(interaction_model, "Study 1")
ggsave("Multimedia_Appendix_3_Diagnostics_S1.png", plot = s1_diag, width = 10, height = 4.5, dpi = 300)

# Base Model for Study 2 Evaluation [cite: 61, 62]
s2_diag <- generate_diagnostic_plots(final_model, "Study 2")
ggsave("Multimedia_Appendix_4_Diagnostics_S2.png", plot = s2_diag, width = 10, height = 4.5, dpi = 300)

# =========================================================================
# SUPPLEMENTARY APPENDIX 7: Figure S6 Task Latencies Battery [cite: 221]
# =========================================================================
# Clean, scale, and format task list metrics [cite: 44]
mec_plot_data <- mec_data %>%
  group_by(GameType, Is_Swiftshader) %>%
  summarise(Mean_RT = mean(RT), SD_RT = sd(RT), N = n(), .groups = "drop") %>%
  mutate(
    SE = SD_RT / sqrt(N), Lower_CI = Mean_RT - (1.96 * SE), Upper_CI = Mean_RT + (1.96 * SE),
    Renderer = ifelse(Is_Swiftshader == "non_swiftshader", "Hardware", "Software"),
    GameType = case_when(
      GameType == "ISHIHARA"       ~ "Color Blindness Test", GameType == "COLOR_PICKING"  ~ "Color Swatch",
      GameType == "TASK_SWITCH_V2" ~ "Sun & Moon",          GameType == "mAID"           ~ "AID",
      GameType == "BOXED"          ~ "Boxed",               GameType == "FILTER"         ~ "Filter",
      GameType == "STROOP"         ~ "Color Tricker",       GameType == "TNT"            ~ "Triangle Trace",
      GameType == "FLANKER"        ~ "Flanker",             GameType == "SAAT_SUSTAINED" ~ "Venus UFO",
      GameType == "SAAT_IMPULSIVE" ~ "Mars UFO",            GameType == "ADP"            ~ "Face Switch",
      GameType == "SPATIAL_CUEING" ~ "Compass",             GameType == "BRT"            ~ "Basic Response Time",
      GameType == "Tova"           ~ "TOVA",                TRUE                         ~ GameType [cite: 45, 46, 47]
    )
  )

figure_s6 <- ggplot(mec_plot_data, aes(x = Mean_RT, y = reorder(GameType, Mean_RT), color = Renderer)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.3, size = 0.8, alpha = 0.7, color = "black") + [cite: 48]
  scale_color_manual(values = render_colors) +
  labs(x = "Response Time (ms)", y = "Assessment Task Type") +
  jmir_theme

# Strips local figure titles to maintain clean layouts for submission parameters
ggsave("Multimedia_Appendix_7_Figure_S6.png", plot = figure_s6, width = 7.5, height = 5, dpi = 300)
