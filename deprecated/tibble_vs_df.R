library(tidyverse)
library(tibble)
library(lubridate)
library(sentimentr)

# Sample data in tibble format
social_media_posts <- tibble(
  user_id = c(1, 2, 1, 3),
  post = c("Love this new phone", "Not happy with the service", "Enjoying the holidays", "Feeling sad today"),
  timestamp = ymd_hms(c("2023-01-01 12:00:00", "2023-01-02 15:30:00", "2023-01-03 09:00:00", "2023-01-04 20:20:00")),
  likes = c(150, 20, 200, 50)
)

# Sentiment analysis
sentiment_scores <- social_media_posts %>%
  mutate(sentiment = sentiment_by(post)$sentiment)

# Summarising sentiment scores by user
summary_by_user <- sentiment_scores %>%
  group_by(user_id) %>%
  summarise(average_sentiment = mean(sentiment))

# Display the summarised data
print(summary_by_user)
