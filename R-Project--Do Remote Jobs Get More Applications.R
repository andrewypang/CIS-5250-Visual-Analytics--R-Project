library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Dataset Summary
str(linkedin_cleaned)
View(linkedin_cleaned)
colSums(is.na(linkedin_cleaned))

#----------------------------------------

# Question: Do Remote Jobs Get More Applications?
# ------------------------------

linkedin_cleaned %>%
  filter(!is.na(apply_rate)) %>%
  filter(views >= 10) %>%
  group_by(remote_allowed) %>%
  ggplot(aes(x=as.factor(remote_allowed),y=apply_rate)) +
  geom_boxplot() +
  scale_x_discrete(
    name = "In-Office/Remote Status",  # Legend title
    labels = c("0" = "In-Office", "1" = "Remote")  # Custom labels
  ) +
  labs(
    title = "Do Remote Jobs Get More Applications?",
    y = "Apply Rate"
  )


linkedin_cleaned %>%
  filter(!is.na(apply_rate)) %>%
  filter(views >= 10) %>%
  group_by(remote_allowed) %>%
  summarise(
    q1 = quantile(apply_rate, 0.25, na.rm = TRUE),
    Median = median(apply_rate, na.rm = TRUE),
    q3 = quantile(apply_rate, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    Count = n()
  ) %>%
  arrange(desc(Count)) %>%
  print()
