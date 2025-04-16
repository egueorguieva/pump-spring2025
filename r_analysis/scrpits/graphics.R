# === Load Required Package ===
library(ggpubr)  # For ggtexttable()

# === LIWC Category Table ===
liwc_table <- data.frame(
  "LIWC Category" = c(
    "i (1st person singular)", 
    "we (1st person plural)", 
    "you (2nd person)", 
    "shehe (3rd person singular)",
    "emo_pos (Positive emotion)", 
    "emo_anx (Anxiety)", 
    "emo_anger (Anger)", 
    "emo_sad (Sadness)"
  ),
  "Example Words" = c(
    "I, me, my, myself",
    "we, our, us, lets",
    "you, your, u, yourself",
    "he, she, her, his",
    "good, love, happy, hope",
    "worry, fear, afraid, nervous",
    "hate, mad, angry, frustr*",
    ":(, sad, disappoint*, cry"
  ),
  check.names = FALSE
)

# === Display Stylized Table ===
ggtexttable(
  liwc_table,
  rows = NULL,
  theme = ttheme(
    colnames.style = colnames_style(color = "white", fill = "steelblue", face = "bold", size = 14),
    tbody.style = tbody_style(color = "black", fill = "#f6f6f6", size = 12)
  )
)