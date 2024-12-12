library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Dataset Summary
str(linkedin_cleaned)
View(linkedin_cleaned)
colSums(is.na(linkedin_cleaned))

#----------------------------------------

# Question: When was the Job Post Listed?
linkedin_cleaned %>%
  group_by(original_listed_time) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = original_listed_time, y = count)) +
  geom_line() +
  labs(
    x = "Date Listed",
    y = "Number of Listings",
    title = "Number of Listings by Date"
  ) +
  theme_minimal()


# ------------------------------