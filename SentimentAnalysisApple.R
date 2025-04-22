# LOAD REQUIRED PACKAGES
install.packages(c("tidytext", "dplyr", "ggplot2", "wordcloud", "syuzhet"))
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(syuzhet)

# CREATE SAMPLE DATA
apple_reviews <- tibble(
  text = c(
    "I love my new iPhone! The camera is amazing.",
    "Macbook battery life is terrible.",
    "AirPods are worth every penny.",
    "iPad software updates make it slower.",
    "Apple customer service is the best!",
    "The new iOS has too many bugs.",
    "Apple products are overpriced but reliable.",
    "My Apple Watch saved my life with heart rate monitoring."
  ),
  product = c("iPhone", "Macbook", "AirPods", "iPad", "Service", "iOS", "General", "Watch")
)

# VIEW THE DATA
View(apple_reviews)  # Opens a spreadsheet-like view

# SENTIMENT ANALYSIS USING BING LEXICON
# Tokenize and remove stop words
tidy_reviews <- apple_reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Get sentiment scores
bing_sentiment <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# VIEW SENTIMENT WORDS
View(bing_sentiment)

# PLOT POSITIVE/NEGATIVE WORDS
bing_plot <- bing_sentiment %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>%  # Top 5 words per sentiment
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Frequency", y = "Word",
       title = "Words Contributing to Positive/Negative Sentiment")

# DISPLAY THE PLOT
print(bing_plot)

# MORE DETAILED SENTIMENT ANALYSIS (SYUZHET)
sentiment_scores <- get_nrc_sentiment(apple_reviews$text)

# COMBINE WITH ORIGINAL DATA
apple_with_sentiment <- cbind(apple_reviews, sentiment_scores)

# VIEW THE ENHANCED DATA
View(apple_with_sentiment)

# PLOT SENTIMENT BY PRODUCT
product_sentiment <- apple_with_sentiment %>%
  group_by(product) %>%
  summarise(positive = mean(positive),
            negative = mean(negative),
            net_sentiment = positive - negative)

sentiment_plot <- ggplot(product_sentiment, 
                         aes(x = reorder(product, net_sentiment), 
                             y = net_sentiment, 
                             fill = product)) +
  geom_col() +
  coord_flip() +
  labs(title = "Net Sentiment by Apple Product",
       x = "Product",
       y = "Sentiment (Positive - Negative)") +
  theme_minimal()

print(sentiment_plot)

# CREATE WORD CLOUD
wordcloud_data <- apple_reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

wordcloud(words = wordcloud_data$word,
          freq = wordcloud_data$n,
          max.words = 50,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


write.csv(apple_with_sentiment, "apple_sentiment_results.csv")