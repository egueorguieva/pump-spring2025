library(gridExtra)
library(grid)
library(tidyverse)
install.packages("pheatmap")
library(pheatmap)
install.packages("ggpubr")  # Run this once
library(ggpubr) 

# TABLE OF LIWC CATEGORIES
liwc_table <- data.frame(
  "LIWC Category" = c("ppron (Personal pronouns)", 
                      "i (1st person singular)", 
                      "we (1st person plural)", 
                      "you (2nd person)", 
                      "cog_proc (Cognitive processes)",
                      "emo_pos (Positive emotion)", 
                      "emo_neg (Negative emotion)", 
                      "prosocial (Prosocial behavior)", 
                      "affiliation", 
                      "work"),
  "Example Words" = c(
    "I, you, my, me",
    "I, me, my, myself",
    "we, our, us, lets",
    "you, your, u, yourself",
    "but, not, if, or, know",
    "good, love, happy, hope",
    "bad, hate, hurt, tired",
    "care, help, thank, please",
    "we, our, us, help",
    "work, school, working, class"
  ),
  check.names = FALSE
)

ggtexttable(
  liwc_table,
  rows = NULL,  # no row numbers
  theme = ttheme(
    colnames.style = colnames_style(color = "white", fill = "#bf5700", face = "bold", size = 14),
    tbody.style = tbody_style(color = "black", fill = "white", size = 12)
  )
)