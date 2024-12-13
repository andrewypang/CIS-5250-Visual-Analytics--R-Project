library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings--cleaned.csv")


# Given a city name, the following script will return the average
# minimum salary (Q1, Median, Q3, Mean), as well as the
# number of job postings for that city.
# Please note that this script only searches within cities in California.
# ----------------------------------------------------------------------------

citySalary <- function(name){
  
  if(name %in% linkedin_cleaned$location) {
    
    result <- linkedin %>%
      group_by(location) %>%
      filter(location==name) %>%
      summarise(
        Q1 = quantile(min_salary, 0.25, na.rm = TRUE),
        Median = median(min_salary, na.rm = TRUE),
        Q3 = quantile(min_salary, 0.75, na.rm = TRUE),
        Mean = mean(min_salary, na.rm = TRUE),
        NumberOfJobPostings = n()
      )
    return(result)
    
  } else {
    print("City Not Found.")
  }
  
}
