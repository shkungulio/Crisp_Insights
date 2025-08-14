# diabetes_visualizations.R
# ---------------------------------------------
# Visualizing Diabetes_binary relationships
# Dataset: diabetes_data.csv
# Author: <your name>
# Date: Sys.Date()
#
# This script produces a set of exploratory visualizations with
# reference to the target variable `Diabetes_binary`.
# ---------------------------------------------

# ---- Setup ----
# Uncomment if you need to install packages
 install.packages(c("tidyverse", "ggplot2", "patchwork"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
})

# ---- Load data ----
# Set your working directory so the CSV path resolves correctly.
# setwd("path/to/your/project")
df <- read.csv("diabetes_data.csv", stringsAsFactors = FALSE)

# ---- Light cleaning / typing ----
# Ensure target is a factor with readable labels
if (!"Diabetes_binary" %in% names(df)) {
  stop("Column 'Diabetes_binary' not found. Check your file.")
}
df <- df |>
  mutate(
    Diabetes_binary = factor(Diabetes_binary,
                             levels = c(0, 1),
                             labels = c("Non-diabetic", "Diabetic"))
  )

# Try to coerce some known categorical columns to factor if present
cat_cols <- intersect(
  c("HighBP","HighChol","CholCheck","Smoker","Stroke",
    "HeartDiseaseorAttack","PhysActivity","Fruits","Veggies","HvyAlcoholConsump",
    "AnyHealthcare","NoDocbcCost","DiffWalk","Sex","GenHlth",
    "Education","Income","Age_Bucket","BMI_Category","Activity_Level",
    "Fruit_Intake","Veggie_Intake"),
  names(df)
)

for (cc in cat_cols) {
  if (!is.factor(df[[cc]])) df[[cc]] <- as.factor(df[[cc]])
}

# Identify numeric columns (excluding the target encoded as factor)
num_cols <- names(df)[sapply(df, is.numeric)]
num_cols <- setdiff(num_cols, c("Diabetes_binary"))

# ---- 1) Class balance ----
p_balance <- df |>
  ggplot(aes(x = Diabetes_binary, fill = Diabetes_binary)) +
  geom_bar(aes(y = after_stat(count / sum(count))), width = 0.6, color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Class balance for Diabetes_binary",
       x = NULL, y = "Share of respondents") +
  guides(fill = "none") +
  theme_minimal(base_size = 12)

# ---- 2) Helper: numeric vs target (density + boxplot) ----
plot_num_by_target <- function(data, num_var, target = "Diabetes_binary") {
  stopifnot(num_var %in% names(data), target %in% names(data))
  p1 <- ggplot(data, aes(x = .data[[num_var]], fill = .data[[target]])) +
    geom_density(alpha = 0.35) +
    labs(title = paste0(num_var, " distribution by ", target),
         x = num_var, y = "Density", fill = target) +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(data, aes(x = .data[[target]], y = .data[[num_var]], fill = .data[[target]])) +
    geom_boxplot(outlier.alpha = 0.25) +
    labs(title = paste0(num_var, " by ", target),
         x = NULL, y = num_var, fill = target) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5))
  
  p1 / p2
}

# Example numeric plots (if columns exist)
plots_numeric <- list()
for (v in intersect(num_cols, c("BMI","MentHlth","PhysHlth","Age","Risk_Score"))) {
  plots_numeric[[v]] <- plot_num_by_target(df, v)
}

# ---- 3) Helper: categorical vs target (normalized bars) ----
plot_cat_by_target <- function(data, cat_var, target = "Diabetes_binary", top_n = NA) {
  stopifnot(cat_var %in% names(data), target %in% names(data))
  d <- data |>
    mutate(across(all_of(c(cat_var, target)), as.factor))
  
  if (!is.na(top_n)) {
    keep_levels <- d |>
      count(.data[[cat_var]], sort = TRUE) |>
      slice_head(n = top_n) |>
      pull(1) |> as.character()
    d <- d |> filter(.data[[cat_var]] %in% keep_levels)
  }
  
  ggplot(d, aes(x = .data[[cat_var]], fill = .data[[target]])) +
    geom_bar(position = "fill", color = "white") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0(cat_var, " vs ", target),
         x = cat_var, y = "Share within category", fill = target) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Example categorical plots
cat_vars_to_plot <- intersect(c("HighBP","HighChol","Smoker","Stroke",
                                "HeartDiseaseorAttack","PhysActivity","Sex",
                                "GenHlth","Education","Income","Age_Bucket",
                                "BMI_Category","Activity_Level"), names(df))

plots_categorical <- list()
for (v in cat_vars_to_plot) {
  plots_categorical[[v]] <- plot_cat_by_target(df, v)
}

# ---- 4) Age bucket by target (ordered, if present) ----
if ("Age_Bucket" %in% names(df)) {
  # Attempt to order age buckets logically if formatted like "18–34", "35–54", "55+"
  age_levels <- c("18–34","35–54","55+")
  if (all(df$Age_Bucket %in% age_levels)) {
    df$Age_Bucket <- factor(df$Age_Bucket, levels = age_levels, ordered = TRUE)
  }
  p_age <- plot_cat_by_target(df, "Age_Bucket")
} else {
  p_age <- NULL
}

# ---- 5) Correlation of numeric features with target (point-biserial) ----
# Convert target to numeric 0/1 for correlation
df_corr <- df
df_corr$target01 <- as.numeric(df_corr$Diabetes_binary) - 1

num_for_cor <- setdiff(num_cols, "target01")
if (length(num_for_cor) > 0) {
  cor_vals <- sapply(num_for_cor, function(v) {
    suppressWarnings(cor(df_corr[[v]], df_corr$target01, use = "complete.obs"))
  })
  cor_df <- tibble(feature = names(cor_vals), correlation = as.numeric(cor_vals)) |>
    arrange(desc(abs(correlation)))
  
  p_cor <- ggplot(cor_df, aes(x = reorder(feature, correlation), y = correlation)) +
    geom_col() +
    coord_flip() +
    labs(title = "Correlation with Diabetes_binary (0/1)",
         x = NULL, y = "Pearson correlation") +
    theme_minimal(base_size = 12)
} else {
  p_cor <- NULL
}

# ---- 6) Assemble a quick dashboard (example) ----
# This part arranges a few key plots on one page for a quick look.
dashboard <- p_balance
if (!is.null(p_age)) dashboard <- dashboard / p_age
if (!is.null(plots_numeric[["BMI"]])) dashboard <- dashboard / plots_numeric[["BMI"]]
if (!is.null(p_cor)) dashboard <- dashboard / p_cor

# ---- 7) Print / Save ----
print(p_balance)
if (!is.null(p_age)) print(p_age)
for (nm in names(plots_numeric)) print(plots_numeric[[nm]])
for (nm in names(plots_categorical)) print(plots_categorical[[nm]])
if (!is.null(p_cor)) print(p_cor)
print(dashboard)

# Optionally save to files (uncomment to save)
 ggsave("plot_class_balance.png", p_balance, width = 6, height = 4, dpi = 300)
 if (!is.null(p_age)) ggsave("plot_age_bucket.png", p_age, width = 7, height = 4, dpi = 300)
 if (!is.null(p_cor)) ggsave("plot_target_correlations.png", p_cor, width = 7, height = 5, dpi = 300)

# Save the dashboard
 ggsave("dashboard_overview.png", dashboard, width = 8.5, height = 11, dpi = 300)
