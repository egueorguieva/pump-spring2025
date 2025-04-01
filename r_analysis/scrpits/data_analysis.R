library(tidyverse)

df_use <- read.csv('/Users/cp/Documents/GitHub/pump-spring2025/r_analysis/clean_LIWC_data/df_use.csv')

df_use_drop <- df_use[1:(nrow(df_use) - 194), ]

variables <- c("emo_neg", "emo_pos", "tone_neg", "tone_pos", "socrefs", "i", "we", "you", "AllPunc")

# Empty lists to store p-values and means for now
p_values <- list()
means <- list()

# Loop 1: Calculate p-values from ANOVA
for (var in variables) {
  
  # Run ANOVA for the current variable (using the 'school' column as the grouping factor)
  anova_result <- aov(as.formula(paste(var, "~ school")), data = df_use_drop)
  
  # Extract p-value from ANOVA result (this tests for overall differences between schools)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Store p-value in the p_values list
  p_values[[var]] <- p_value
  
  # Print the p-value for the current variable
  print(paste("p-value for", var, ":", p_value))
}

# Loop 2: Calculate means
for (var in variables) {
  
  # Calculate the mean for the current variable
  mean_value <- mean(df_use_drop[[var]])
  
  # Store mean in the means list
  means[[var]] <- mean_value
  
  # Print the mean for the current variable
  print(paste("Mean for", var, ":", mean_value))
}
