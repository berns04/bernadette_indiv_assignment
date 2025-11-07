# (1) Start clean (optional but recommended)
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# (2) Load data
bdhs_cl <- read.csv("Data/02-result.csv")

# (3) Create the binary underweight variable FROM SCRATCH
bdhs_cl <- bdhs_cl %>%
  mutate(
    # Underweight if z-score < -2, else Not Underweight
    o_binary_uw = ifelse(o_weight_age_zscore < -2,
                         "Underweight",
                         "Not Underweight"),
    # clean any accidental spaces
    o_binary_uw = trimws(o_binary_uw),
    # make it a factor (we'll order levels here)
    o_binary_uw = factor(o_binary_uw,
                         levels = c("Not Underweight", "Underweight")),
    # ensure continuous vars are numeric
    m_age         = as.numeric(m_age),
    m_first_birth = as.numeric(m_first_birth),
    m_BMI         = as.numeric(m_BMI)
  )

# Quick check: THIS MUST show real counts now, not all NA
table(bdhs_cl$o_binary_uw, useNA = "ifany")
# (4) Long format for the 3 variables
violin_data <- bdhs_cl %>%
  select(o_binary_uw, m_age, m_first_birth, m_BMI) %>%
  pivot_longer(
    cols = c(m_age, m_first_birth, m_BMI),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c("m_age", "m_first_birth", "m_BMI"),
      labels = c("Maternal Age", "Age at First Birth", "Maternal BMI")
    )
  )

# (5) Plot: red vs forest green, 3 panels
ggplot(violin_data, aes(x = o_binary_uw, y = Value, fill = o_binary_uw)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point",
               shape = 21, size = 2.5, fill = "black") +
  scale_fill_manual(
    values = c("Not Underweight" = "red",
               "Underweight"     = "forestgreen"),
    name = "Underweight"
  ) +
  facet_wrap(~Variable, nrow = 1, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  ) +
  labs(
    x = "Underweight (Yes/No)",
    y = "",
    title = "Maternal Characteristics by Underweight Status"
  )
