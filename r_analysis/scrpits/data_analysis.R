# === Load packages ===
install.packages("effectsize")
library(effectsize)    # For eta-squared
library(tidyverse)
library(ggplot2)
library(patchwork)     # Combine multiple plots
library(reshape2)      # For heatmap
install.packages("pheatmap")
library(pheatmap)
library(knitr)
library(kableExtra)
install.packages("gt")
library(gt)

# === Load and trim data ===
df_use <- read.csv('/Users/cp/Documents/GitHub/pump-spring2025/r_analysis/clean_LIWC_data/df_use.csv')
df_use_drop <- df_use[1:(nrow(df_use) - 194), ]

# === LIWC Variables to Analyze ===
variables <- c("ppron", "i", "we", "you", 
               "cogproc", "emo_pos", "emo_neg", 
               "prosocial", "affiliation", "work")

# === ANOVA: F, p, eta-squared ===
anova_summary <- map_dfr(variables, function(var) {
  model <- aov(as.formula(paste(var, "~ school")), data = df_use_drop)
  summary_out <- summary(model)[[1]]
  eta <- eta_squared(model, partial = FALSE)
  
  tibble(
    Variable = var,
    F_stat = round(summary_out$`F value`[1], 3),
    p_value = signif(summary_out$`Pr(>F)`[1], 5),
    eta_squared = round(eta$Eta2[1], 3)
  )
})

print("ANOVA Summary:")
print(anova_summary)

# === Group means by school ===
school_means <- df_use_drop %>%
  select(school, all_of(variables)) %>%
  group_by(school) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3)))

print("Means by School:")
print(school_means)

# === Post-Hoc: Tukey HSD for significant variables only ===
tukey_results <- list()

for (var in variables) {
  p_val <- anova_summary$p_value[anova_summary$Variable == var]
  if (p_val < 0.05) {
    model <- aov(as.formula(paste(var, "~ school")), data = df_use_drop)
    tukey_results[[var]] <- TukeyHSD(model)
    
    print(paste0("===== Tukey HSD results for ", var, " ====="))
    print(tukey_results[[var]])
  }
}

# === Bar Plot of Eta-Squared Values ===
ggplot(anova_summary, aes(x = reorder(Variable, eta_squared), y = eta_squared)) +
  geom_col(fill = "#bf5700") +
  coord_flip() +
  labs(
    title = "Effect Sizes by LIWC Variable (η²)",
    x = "LIWC Variable",
    y = "Eta Squared"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)  # ✅ Center the title
  )

# === HEATMAP of means ===
# Prepare matrix
heatmap_matrix <- school_means %>%
  column_to_rownames("school") %>%
  as.matrix()

desired_order <- c("ppron", "i", "we", "you", "cogproc",
                   "emo_pos", "emo_neg", "prosocial", "affiliation", "work")
heatmap_matrix <- heatmap_matrix[, desired_order]

# Create the clean heatmap
pheatmap(
  mat = heatmap_matrix,
  scale = "column",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("white", "#bf5700"))(100), #steelblue
  fontsize_row = 12,
  fontsize_col = 10,
  angle_col = 45,
  main = "Average LIWC Scores by School",
  border_color = NA
)

# === BOX PLOTS ===
plot_liwc_box <- function(var, x_label = FALSE, y_label = FALSE) {
  ggplot(df_use_drop, aes(x = school, y = .data[[var]])) +
    geom_boxplot(
      fill = "#bf5700",
      outlier.size = 1,
      outlier.alpha = 0.55
    ) +
    labs(
      title = NULL,
      x = if (x_label) "School" else NULL,
      y = if (y_label) paste(var, "score") else NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 12, face = "bold")
    )
}

# 2x2 Boxplot Grid: Top variables
p1 <- plot_liwc_box("i", y_label = TRUE)
p2 <- plot_liwc_box("work", y_label = TRUE)
p3 <- plot_liwc_box("emo_neg", y_label = TRUE, x_label = TRUE)
p4 <- plot_liwc_box("emo_pos", y_label = TRUE, x_label = TRUE)

(p1 | p2) / (p3 | p4) +
  plot_annotation(title = "LIWC Category Differences Across Schools")