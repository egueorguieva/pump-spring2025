library(tidyverse)

df = read.csv('/Users/cp/Documents/GitHub/pump-spring2025/r_analysis/clean_LIWC_data/all _clean_LIWC_Analysis.csv')

df_cleaned <- df %>%
  filter(AllPunc <= 50,  # Remove posts with more than 50% punctuation
         Dic >= 50,      # Keep posts with at least 50% dictionary words
         WC >= 25)       # Keep posts with at least 25 words

# Saves the dataset
write.csv(df_cleaned, "~/Downloads/df_cleaned.csv", row.names = FALSE)