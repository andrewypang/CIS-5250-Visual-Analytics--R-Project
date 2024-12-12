library(tidyverse)
library(scales)

linkedin <- read.csv("data/postings.csv")

View(linkedin)

colnames(linkedin)
str(linkedin)
summary(linkedin)

# Dataclean 1: filter to California only
linkedin_cleaned <- linkedin %>%
  filter(grepl('CA|California|^United States$', location))

# Dataclean 1: Validate
unique(linkedin_cleaned$location)

# Dataclean 2: Convert NA to 0 in column remote_allowed 
linkedin_cleaned$remote_allowed[is.na(linkedin_cleaned$remote_allowed)] <- 0
linkedin_cleaned$remote_allowed <- as.factor(linkedin_cleaned$remote_allowed)

# Dataclean 2: Validate 
View(linkedin_cleaned)

# Dataclean 3: convert Blanks to NAs
linkedin_cleaned$currency[linkedin_cleaned$currency == ""] <- NA
linkedin_cleaned$company_name[linkedin_cleaned$company_name == ""] <- NA
linkedin_cleaned$pay_period[linkedin_cleaned$pay_period == ""] <- NA
linkedin_cleaned$application_url[linkedin_cleaned$application_url == ""] <- NA
linkedin_cleaned$formatted_experience_level[linkedin_cleaned$formatted_experience_level == ""] <- NA
linkedin_cleaned$skills_desc[linkedin_cleaned$skills_desc == ""] <- NA
linkedin_cleaned$posting_domain[linkedin_cleaned$posting_domain == ""] <- NA
linkedin_cleaned$compensation_type[linkedin_cleaned$compensation_type == ""] <- NA

# Dataclean 3: Validate 
View(linkedin_cleaned)

# Dataclean 4: convert original_listed_time, expiry, closed_time, listed_time to timestamps
linkedin_cleaned <- linkedin_cleaned %>%
  mutate(
    # Check if the values are likely in milliseconds (values greater than 10^9)
    original_listed_time = ifelse(original_listed_time > 1e9, original_listed_time / 1000, original_listed_time),
    expiry = ifelse(expiry > 1e9, expiry / 1000, expiry),
    closed_time = ifelse(closed_time > 1e9, closed_time / 1000, closed_time),
    listed_time = ifelse(listed_time > 1e9, listed_time / 1000, listed_time),
    
    # Convert timestamps to POSIXct
    original_listed_time = as.POSIXct(original_listed_time, origin = "1970-01-01", tz = "UTC"),
    expiry = as.POSIXct(expiry, origin = "1970-01-01", tz = "UTC"),
    closed_time = as.POSIXct(closed_time, origin = "1970-01-01", tz = "UTC"),
    listed_time = as.POSIXct(listed_time, origin = "1970-01-01", tz = "UTC")
  )

# Dataclean 4: Validate
View(linkedin_cleaned)


# Data Cleaning 5: Adding new columns: 1) post_duration, 2) apply_rate
# Create column 1) post_duration
linkedin_cleaned <- linkedin_cleaned %>%
  mutate(
    # Calculate the time difference between expiry and original_listed_time
    post_duration = round(as.numeric(difftime(expiry, original_listed_time, units = "days")))
  )

# Create column 2) apply_rate
linkedin_cleaned <- linkedin_cleaned %>%
  mutate(
    # Calculate the rate from applies and views
    apply_rate = ifelse(!is.na(applies) & !is.na(views) & views != 0,
                        round(as.numeric(applies) / as.numeric(views), 2),
                        NA)
  )

# Data Cleaning 5: Validate
View(linkedin_cleaned)

# Validate if Blanks became NAs
colSums(is.na(linkedin_cleaned))

# Export to CSV
write.csv(linkedin_cleaned, "data/postings--cleaned.csv", row.names = FALSE)
    