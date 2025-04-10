# === Load packages ===
library(tidyverse)
library(ggplot2)
library(patchwork)  # for combining plots
library(reshape2)   # for heatmap

# === Load and clean data ===
df_use <- read.csv('/Users/cp/Documents/GitHub/pump-spring2025/r_analysis/clean_LIWC_data/df_use.csv')
df_use_drop <- df_use[1:(nrow(df_use) - 194), ]

# === LIWC Variables to Analyze ===
variables <- c("ppron", "i", "we", "you", 
               "cogproc", "emo_pos", "emo_neg", 
               "prosocial", "affiliation", "work")

# === ANOVA: F-stat and p-values ===
anova_summary <- map_dfr(variables, function(var) {
  model <- aov(as.formula(paste(var, "~ school")), data = df_use_drop)
  out <- summary(model)[[1]]
  tibble(
    Variable = var,
    F_stat = round(out$`F value`[1], 3),
    p_value = signif(out$`Pr(>F)`[1], 5)
  )
})

# p val and f stat
print("ANOVA Summary:")
print(anova_summary)

# === Group means by school ===
school_means <- df_use_drop %>%
  select(all_of(c("school", variables))) %>%
  group_by(school) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3)))

print("Means by School:")
print(school_means)

# === Post-Hoc: Tukey HSD for significant variables ===
tukey_results <- list()

for (var in variables) {
  if (anova_summary$p_value[anova_summary$Variable == var] < 0.05) {
    model <- aov(as.formula(paste(var, "~ school")), data = df_use_drop)
    tukey_results[[var]] <- TukeyHSD(model)
    print(paste0("===== Tukey HSD results for ", var, " ====="))
    print(tukey_results[[var]])
  }
}

# === Visualization Functions ===

# HEATMAP
heatmap_data <- as.data.frame(school_means)
rownames(heatmap_data) <- heatmap_data$school
heatmap_data$school <- NULL
heatmap_matrix <- as.matrix(heatmap_data)

heatmap(heatmap_matrix, Colv = NA, scale = "column",
        col = colorRampPalette(c("white", "steelblue"))(100),
        main = "LIWC Score Heatmap by School")


# MAKING BOX PLOTS
plot_liwc_box <- function(var, x_label = FALSE, y_label = FALSE) {
  ggplot(df_use_drop, aes(x = school, y = .data[[var]])) +
    geom_boxplot(
      fill = "#91c9f7",
      outlier.size = 1, # dot size
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


# Top left
p1 <- plot_liwc_box("i", x_label = FALSE, y_label = TRUE)

# Top right
p2 <- plot_liwc_box("work", x_label = FALSE, y_label = TRUE)

# Bottom left
p3 <- plot_liwc_box("emo_neg", x_label = FALSE, y_label = TRUE)

# Bottom right
p4 <- plot_liwc_box("emo_pos", x_label = FALSE, y_label = TRUE)

# Combine using patchwork
boxplot_grid <- (p1 | p2) / (p3 | p4)
boxplot_grid + plot_annotation(title = "LIWC Category Differences Across Schools")