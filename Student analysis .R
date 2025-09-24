# ============================================
# student_analysis.R: Student Performance Based On Sleep VS Study
# ============================================

# ---- 0) Packages ----
req_pkgs <- c("tidyverse", "broom", "patchwork")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
invisible(lapply(req_pkgs, library, character.only = TRUE))


# ---- 1) Paths ---- 


data_path <- "student_exam_scores 3.csv"
out_dir <- "figs"
if (!dir.exists(out_dir)) dir.create(out_dir)

#----- 2) Loading and preparing data -----



if (!file.exists(data_path)) {
  stop(paste("File not found:", data_path, "\nPut your CSV file beside this script or update data_path."))
}
raw_data <- readr::read_csv(data_path, show_col_types = FALSE)

needed_cols <- c("hours_studied", "sleep_hours", "attendance_percent", "previous_scores", "exam_score")
if (!all(needed_cols %in% names(raw_data))) {
  stop("Missing required columns. Need: hours_studied, sleep_hours, attendance_percent, previous_scores, exam_score")
}

student_data <- raw_data %>%
  select(hours_studied, sleep_hours, attendance_percent, previous_scores, exam_score) %>%
  mutate(
    hours_studied = suppressWarnings(as.numeric(hours_studied)),
    sleep_hours = suppressWarnings(as.numeric(sleep_hours)),
    attendance_percent = suppressWarnings(as.numeric(attendance_percent)),
    previous_scores = suppressWarnings(as.numeric(previous_scores)),
    exam_score = suppressWarnings(as.numeric(exam_score))
  ) %>%
  
  filter(!is.na(hours_studied), !is.na(sleep_hours), !is.na(exam_score)) %>%
  mutate(
    sleep_group = dplyr::case_when(
      sleep_hours >= 8 ~ "High Sleep (8+ hrs)",
      sleep_hours >= 6 ~ "Medium Sleep (6-8 hrs)",
      TRUE ~ "Low Sleep (<6 hrs)"
      
    ),
    sleep_group = factor(sleep_group, levels = c("Low Sleep (<6 hrs)", "Medium Sleep (6-8 hrs)", "High Sleep (8+ hrs)")),
    study_group = dplyr::case_when(
      hours_studied >= 8 ~ "High Study (8+ hrs)",
      hours_studied >= 4 ~ "Medium Study (4-8 hrs)",
      TRUE ~ "Low Study (<4 hrs)"
    ),
    study_group = factor(study_group, levels = c("Low Study (<4 hrs)", "Medium Study (4-8 hrs)", "High Study (8+ hrs)"))
  )

cat("Rows after cleaning:", nrow(student_data), "\n")
suppressMessages(print(glimpse(student_data)))

#----- 3) Descriptive Statistics -----



summary_stats <- student_data %>%
  summarise(
    n = n(),
    # Sleep statistics
    sleep_mean = mean(sleep_hours), sleep_sd = sd(sleep_hours),
    sleep_min = min(sleep_hours), sleep_q1 = quantile(sleep_hours, 0.25),
    sleep_med = median(sleep_hours), sleep_q3 = quantile(sleep_hours, 0.75), 
    sleep_max = max(sleep_hours),
    
    # Study hours statistics  
    study_mean = mean(hours_studied), study_sd = sd(hours_studied),
    study_min = min(hours_studied), study_q1 = quantile(hours_studied, 0.25),
    study_med = median(hours_studied), study_q3 = quantile(hours_studied, 0.75),
    study_max = max(hours_studied),
    
    # Exam score statistics
    exam_mean = mean(exam_score), exam_sd = sd(exam_score),
    exam_min = min(exam_score), exam_q1 = quantile(exam_score, 0.25),
    exam_med = median(exam_score), exam_q3 = quantile(exam_score, 0.75),
    exam_max = max(exam_score)
  )

cat("\nSummary statistics:\n")
print(dplyr::mutate(summary_stats, dplyr::across(where(is.numeric), ~round(.x, 2))))
readr::write_csv(summary_stats, file.path(out_dir, "summary_stats.csv"))

# Group-based statistics for report
sleep_group_stats <- student_data %>%
  group_by(sleep_group) %>%
  summarise(
    count = n(),
    avg_exam_score = mean(exam_score),
    sd_exam_score = sd(exam_score),
    .groups = 'drop'
  )

study_group_stats <- student_data %>%
  group_by(study_group) %>%
  summarise(
    count = n(),
    avg_exam_score = mean(exam_score),
    sd_exam_score = sd(exam_score),
    .groups = 'drop'
  )

cat("\nExam scores by sleep groups:\n")
print(dplyr::mutate(sleep_group_stats, dplyr::across(where(is.numeric), ~round(.x, 2))))

cat("\nExam scores by study groups:\n")
print(dplyr::mutate(study_group_stats, dplyr::across(where(is.numeric), ~round(.x, 2))))

readr::write_csv(sleep_group_stats, file.path(out_dir, "sleep_group_stats.csv"))
readr::write_csv(study_group_stats, file.path(out_dir, "study_group_stats.csv"))

# ----- 4) Plots for Analysis ----



# Plot 1: Distribution of exam scores 
p_hist_exam <- ggplot(student_data, aes(x = exam_score)) +
  geom_histogram(bins = 20, alpha = 0.8, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Exam Scores", 
       x = "Exam Score", y = "Count") +
  theme_minimal()

# Plot 2: Grades gotten by sleep groups 
p_box_sleep <- ggplot(student_data, aes(x = sleep_group, y = exam_score, fill = sleep_group)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Exam Scores by Sleep Groups", 
       x = "Sleep Group", y = "Exam Score") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Grades gotten by study groups
p_box_study <- ggplot(student_data, aes(x = study_group, y = exam_score, fill = study_group)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Exam Scores by Study Groups", 
       x = "Study Group", y = "Exam Score") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Scatter plot: Sleep vs Exam Score
p_scatter_sleep <- ggplot(student_data, aes(x = sleep_hours, y = exam_score)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Exam Score vs Sleep Hours", 
       x = "Sleep Hours", y = "Exam Score") +
  theme_minimal()

# Plot 5: Scatter plot: Study vs Exam Score  
p_scatter_study <- ggplot(student_data, aes(x = hours_studied, y = exam_score)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Exam Score vs Study Hours", 
       x = "Study Hours", y = "Exam Score") +
  theme_minimal()


ggsave(file.path(out_dir, "exam_score_distribution.png"), p_hist_exam, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "exam_scores_by_sleep_groups.png"), p_box_sleep, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "exam_scores_by_study_groups.png"), p_box_study, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "sleep_vs_exam_scatter.png"), p_scatter_sleep, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir, "study_vs_exam_scatter.png"), p_scatter_study, width = 8, height = 6, dpi = 300)


# ---- 5) Statistical Testing ----


# Test 1: Comparing exam scores between high sleep vs low sleep groups

high_sleep_scores <- student_data %>% filter(sleep_group == "High Sleep (8+ hrs)") %>% pull(exam_score)
low_sleep_scores <- student_data %>% filter(sleep_group == "Low Sleep (<6 hrs)") %>% pull(exam_score)

if (length(high_sleep_scores) > 2 && length(low_sleep_scores) > 2) {
  sleep_t_test <- t.test(high_sleep_scores, low_sleep_scores, alternative = "two.sided", var.equal = FALSE)
  cat("\nT-test: High Sleep vs Low Sleep Groups\n")
  cat("High Sleep Group - Mean:", round(mean(high_sleep_scores), 2), "SD:", round(sd(high_sleep_scores), 2), "n =", length(high_sleep_scores), "\n")
  cat("Low Sleep Group - Mean:", round(mean(low_sleep_scores), 2), "SD:", round(sd(low_sleep_scores), 2), "n =", length(low_sleep_scores), "\n")
  print(sleep_t_test)
} else {
  cat("\nT-test skipped for sleep groups (insufficient data).\n")
}

# Test 2: Comparing exam scores between high study vs low study groups  

high_study_scores <- student_data %>% filter(study_group == "High Study (8+ hrs)") %>% pull(exam_score)
low_study_scores <- student_data %>% filter(study_group == "Low Study (<4 hrs)") %>% pull(exam_score)

if (length(high_study_scores) > 2 && length(low_study_scores) > 2) {
  study_t_test <- t.test(high_study_scores, low_study_scores, alternative = "two.sided", var.equal = FALSE)
  cat("\nT-test: High Study vs Low Study Groups\n")
  cat("High Study Group - Mean:", round(mean(high_study_scores), 2), "SD:", round(sd(high_study_scores), 2), "n =", length(high_study_scores), "\n")
  cat("Low Study Group - Mean:", round(mean(low_study_scores), 2), "SD:", round(sd(low_study_scores), 2), "n =", length(low_study_scores), "\n")
  print(study_t_test)
} else {
  cat("\nT-test skipped for study groups (insufficient data).\n")
}

# ---- 6) Regression Analysis: exam_score ~ sleep_hours + hours_studied ----


model <- lm(exam_score ~ sleep_hours + hours_studied + attendance_percent + previous_scores, data = student_data)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("REGRESSION MODEL RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
print(summary(model))


model_coefs <- broom::tidy(model, conf.int = TRUE)
model_performance <- broom::glance(model)


model_coefs_rounded <- model_coefs %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))

model_performance_rounded <- model_performance %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))

cat("\nModel Coefficients (with 95% Confidence Intervals):\n")
print(model_coefs_rounded)

cat("\nModel Performance Metrics:\n")
print(model_performance_rounded)


readr::write_csv(model_coefs, file.path(out_dir, "regression_coefficients.csv"))
readr::write_csv(model_performance, file.path(out_dir, "regression_performance.csv"))

#Answers to the research question
sleep_coef <- model_coefs$estimate[model_coefs$term == "sleep_hours"]
study_coef <- model_coefs$estimate[model_coefs$term == "hours_studied"]
sleep_pvalue <- model_coefs$p.value[model_coefs$term == "sleep_hours"]
study_pvalue <- model_coefs$p.value[model_coefs$term == "hours_studied"]

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("RESEARCH QUESTION ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Sleep Hours Impact: Each additional hour of sleep increases exam score by", round(sleep_coef, 2), "points\n")
cat("Study Hours Impact: Each additional hour of study increases exam score by", round(study_coef, 2), "points\n")
cat("Sleep Hours p-value:", round(sleep_pvalue, 4), ifelse(sleep_pvalue < 0.05, "(SIGNIFICANT)", "(NOT SIGNIFICANT)"), "\n")
cat("Study Hours p-value:", round(study_pvalue, 4), ifelse(study_pvalue < 0.05, "(SIGNIFICANT)", "(NOT SIGNIFICANT)"), "\n")

if (abs(sleep_coef) > abs(study_coef) && sleep_pvalue < 0.05) {
  cat("\nCONCLUSION: Sleep hours have a STRONGER significant impact on exam performance than study hours.\n")
} else if (abs(study_coef) > abs(sleep_coef) && study_pvalue < 0.05) {
  cat("\nCONCLUSION: Study hours have a STRONGER significant impact on exam performance than sleep hours.\n")
} else {
  cat("\nCONCLUSION: Both factors should be examined further. Check significance levels and effect sizes.\n")
}

# ---- 7) Model Diagnostics ----


# Creating diagnostic plots to validate model assumptions
# Residuals vs Fitted Values plot
png(file.path(out_dir, "diagnostic_residuals_vs_fitted.png"), width = 900, height = 600, res = 120)
plot(model$fitted.values, resid(model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = "red")
dev.off()

# Normal Q-Q plot for residuals
png(file.path(out_dir, "diagnostic_qqplot.png"), width = 900, height = 600, res = 120)
qqnorm(resid(model), main = "Normal Q-Q Plot of Residuals")
qqline(resid(model), col = "red")
dev.off()


print(p_hist_exam)
print(p_box_sleep) 
print(p_box_study)
print(p_scatter_sleep)
print(p_scatter_study)
