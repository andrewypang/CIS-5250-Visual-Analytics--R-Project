library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Dataset Summary
str(linkedin_cleaned)
View(linkedin_cleaned)
colSums(is.na(linkedin_cleaned))

#----------------------------------------

# Question: What Is The Salary Difference Between Remote and In-Office Jobs?
# Calculate IQR and bounds
q1 <- quantile(linkedin_cleaned$normalized_salary, 0.25, na.rm = TRUE)
q3 <- quantile(linkedin_cleaned$normalized_salary, 0.75, na.rm = TRUE)
iqr <- q3 - q1

upper_bound <- q3 + 3 * iqr
lower_bound <- q1 - 3 * iqr

# CHART
linkedin_cleaned %>%
  filter(normalized_salary >= lower_bound & normalized_salary <= upper_bound) %>%
  ggplot(aes(x = as.factor(remote_allowed), y = normalized_salary)) +
  geom_boxplot() +
  scale_x_discrete(
    name = "In-Office/Remote Status",
    labels = c("0" = "In-Office", "1" = "Remote")
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Remote Allowed",
    y = "Normalized Salary",
    title = "Normalized Salary by Remote Allowed"
  )

# TABLE
linkedin_cleaned %>%
  filter(normalized_salary >= lower_bound & normalized_salary <= upper_bound) %>%
  group_by(remote_allowed) %>%
  summarise(
    Q1 = quantile(normalized_salary, 0.25, na.rm = TRUE),
    Median = median(normalized_salary, na.rm = TRUE),
    Q3 = quantile(normalized_salary, 0.75, na.rm = TRUE),
    IQR = IQR(normalized_salary, na.rm = TRUE),  
    Count = n() 
  )

# ------------------------------