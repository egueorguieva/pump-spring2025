# === Load Required Packages ===
library(tidyverse)     # Includes dplyr, tidyr, ggplot2
library(car)           # For Type III ANOVA
library(effectsize)    # For partial eta squared
library(ggpubr)        # For tables
library(RColorBrewer)  # for color themes

# === Load and Prepare Data ===
df <- read.csv('/Users/cp/Documents/GitHub/pump-spring2025/eg-analysis/subreddit-LIWC.csv')
df$school <- as.factor(df$school)

# Rename school labels for clarity in plots
levels(df$school) <- c("BYU", "Harvard", "Oregon State", "UT Austin", "Williams College")

# === LIWC Variable Groups ===
variables <- c("i", "we", "you", "shehe", "emo_pos", "emo_anx", "emo_sad", "emo_anger")
emotion_words <- c("emo_pos", "emo_anx", "emo_sad", "emo_anger")
function_words <- c("i", "we", "you", "shehe")

# === ANCOVA: Effect of School on Each LIWC Variable, Controlling for Word Count ===
ancova_results <- map_dfr(variables, function(var) {
  model <- lm(as.formula(paste(var, "~ school + WC")), data = df)
  anova_out <- Anova(model, type = "III")
  eta_out <- eta_squared(model, partial = TRUE)
  
  tibble(
    Variable = var,
    F_stat = round(anova_out["school", "F value"], 3),
    p_value = signif(anova_out["school", "Pr(>F)"], 5),
    partial_eta2 = round(eta_out$Eta2_partial[eta_out$Parameter == "school"], 3)
  )
})

print("ANCOVA Summary:")
print(ancova_results)

# === Group Means by School ===
school_means <- df %>%
  select(school, all_of(variables)) %>%
  group_by(school) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3)))

print("Means by School:")
print(school_means)

# === Post-Hoc: Tukey HSD for Significant Variables Only ===
tukey_results <- list()

for (var in variables) {
  p_val <- ancova_results$p_value[ancova_results$Variable == var]
  if (p_val < 0.05) {
    model <- aov(as.formula(paste(var, "~ school + WC")), data = df)
    tukey_out <- suppressWarnings(TukeyHSD(model, "school"))
    tukey_results[[var]] <- tukey_out
    print(paste0("===== Tukey HSD for ", var, " ====="))
    print(tukey_out)
  }
}

# === custom color pallet 
custom_blues <- c("#1E90FF","#4682B4", "#00BFFF","#5F9EA0","#4169E1")

# === Bar Plot: Emotion Words ===
df %>%
  pivot_longer(cols = all_of(emotion_words), names_to = "emotion_words", values_to = "value") %>%
  ggplot(aes(x = school, y = value, fill = school)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge", width = 0.2) +
  facet_wrap(~ emotion_words, scales = "free_y") +
  labs(title = "Mean LIWC Emotion Word Scores by University",
       x = "University", y = "Mean Score") +
  theme_minimal() +
  scale_fill_manual(values = custom_blues) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# === Bar Plot: Function Words ===
df %>%
  pivot_longer(cols = all_of(function_words), names_to = "function_words", values_to = "value") %>%
  ggplot(aes(x = school, y = value, fill = school)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge", width = 0.2) +
  facet_wrap(~ function_words, scales = "free_y") +
  labs(title = "Mean LIWC Emotion Word Scores by University",
       x = "University", y = "Mean Score") +
  theme_minimal() +
  scale_fill_manual(values = custom_blues) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
# === Table of Effect Sizes for Function and Emotion Words ===

# Table 1: Function Words
function_effects <- ancova_results %>%
  filter(Variable %in% function_words) %>%
  select(Variable,partial_eta2)

# Table 2: Emotion Words
emotion_effects <- ancova_results %>%
  filter(Variable %in% emotion_words) %>%
  select(Variable,partial_eta2)

# View tables
print("Function Word Effect Sizes:")
print(function_effects)

print("Emotion Word Effect Sizes:")
print(emotion_effects)

# Stylized Table: Function Words
ggtexttable(
  function_effects,
  rows = NULL,
  theme = ttheme(
    colnames.style = colnames_style(color = "white", fill = "#156082", face = "bold", size = 14),
    tbody.style = tbody_style(color = "black", fill = "#f6f6f6", size = 12)
  )
)

# Stylized Table: Emotion Words
ggtexttable(
  emotion_effects,
  rows = NULL,
  theme = ttheme(
    colnames.style = colnames_style(color = "white", fill = "#156082", face = "bold", size = 14),
    tbody.style = tbody_style(color = "black", fill = "#f6f6f6", size = 12)
  )
)