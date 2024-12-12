library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Dataset Summary
str(linkedin_cleaned)
View(linkedin_cleaned)
colSums(is.na(linkedin_cleaned))

#----------------------------------------

# Question: What is the Relationship between Number of Views and Number of Applies Disaggregated by Experience Level
linkedin_cleaned %>%
  filter(!is.na(views), !is.na(applies), !is.na(formatted_experience_level)) %>%  # Remove NAs
  ggplot(aes(x = views, y = applies)) +
  geom_point(aes(color = formatted_experience_level), alpha = 0.6) +
  geom_smooth(aes(color = formatted_experience_level), method = "lm", se = FALSE) + 
  scale_color_manual(
    values = c(
      "Associate" = "red",
      "Director" = "blue",
      "Entry level" = "green",
      "Executive" = "purple",
      "Internship" = "orange",
      "Mid-Senior level" = "black"
    )
  ) + 
  labs(
    title = "Views vs Applies Disaggregated by Experience Level",
    x = "Number of Views",
    y = "Number of Applies",
    color = "Experience Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"  # Move the legend to the top for better visibility
  )

# ------------------------------