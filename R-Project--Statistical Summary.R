library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Dataset Summary
str(linkedin_cleaned)
View(linkedin_cleaned)
colSums(is.na(linkedin_cleaned))


# ------------------------------

# Statistical Summary of ‘applies’
linkedin_cleaned %>%
  summarise(
    Q1 = quantile(applies, 0.25, na.rm = TRUE),
    Median = median(applies, na.rm = TRUE),
    Q3 = quantile(applies, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(applies, na.rm = TRUE),
    Max = max(applies, na.rm = TRUE),
    Mean = mean(applies, na.rm = TRUE),
    Count = n()
  ) %>%
  print()

# ------------------------------

# Statistical Summary of ‘min_salary’
linkedin_cleaned %>%
  summarise(
    Q1 = quantile(min_salary, 0.25, na.rm = TRUE),
    Median = median(min_salary, na.rm = TRUE),
    Q3 = quantile(min_salary, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Min = min(min_salary, na.rm = TRUE),
    Max = max(min_salary, na.rm = TRUE),
    Mean = mean(min_salary, na.rm = TRUE),
    Count = n()
  ) %>%
  print()

# ------------------------------