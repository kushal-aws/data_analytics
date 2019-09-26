library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(readr)
library(ggplot2)



yelp <-
  fromJSON("/Users/kushalwalia/Desktop/yelp_academic_dataset_business.json")

yelp <-
  stream_in(file(
    "/Users/kushalwalia/Desktop/yelp_academic_dataset_business.json"
  ))

str(yelp)

head(yelp)
yelp_flat <- flatten(yelp)

str(yelp_flat)



yelp_tbl <- as_data_frame(yelp_flat)

yelp_tbl


#removing categories with restaurants
yelp_categories <-
  yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>% filter(str_detect(categories, "Restaurant")) %>%
  mutate(categories = strsplit(as.character(categories), ",")) %>% unnest(categories)  %>%  mutate_if(is.character, str_trim) %>% filter(categories != "Restaurants") %>%
  select(name, state, categories)

yelp_categories

#unnests categories and creates multiple rows for each restaurants with broken down categories.
yelp_restaurant_types <-
  yelp_categories %>% count(yelp_categories$categories) %>% arrange(desc(n))

yelp_restaurant_types

#restaurant types by state

yelp_restaurant_types_byState <-
  yelp_categories %>% count(state, categories)  %>% group_by(state) %>% filter(n > 10) %>% top_n(5, n) %>% arrange(desc(state, n))

yelp_restaurant_types_byState

write.csv(yelp_categories, file = "/Users/kushalwalia/Desktop/categories_restaurants.csv")

#Viz.

ratings_and_users <- yelp_tbl %>% group_by(stars) %>% count()

ggplot(data=ratings_and_users, aes(x=cut(stars,c(0,1,2,3,4,5)), y=n,fill=cut(stars, c(0,1,2,3,4,5)))) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Spectral") + labs(y = "Users", x = "Stars") + theme(legend.position="none") +
  scale_x_discrete(labels=c("1","2","3","4","5")) 

yelp_flat %>% filter(yelp_flat$attributes.DogsAllowed=="TRUE") %>% summarise(count=n())  %>% group_by(yelp_flat$city) %>% arrange(desc(count)) 


#word cloud



# we're reading only 200,000 in this example
# you can try it with the full dataset too, it's just a little slower to process!
infile <- "/Users/kushalwalia/Downloads/yelp_academic_dataset_review.json"
review_lines <- read_lines(infile, n_max = 200000, progress = FALSE)


# Each line is a JSON object- the fastest way to process is to combine into a
# single JSON string and use fromJSON and flatten
reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]")

reviews <- fromJSON(reviews_combined) %>%
  flatten() %>%
  tbl_df()
reviews
library(tidytext)

review_words <- reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

review_words

#we use the AFINN lexicon to do our sentiment analysis and word cloud

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN

reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment

theme_set(theme_bw())

ggplot(reviews_sentiment, aes(stars, sentiment, group = stars)) +
  geom_boxplot() +
  ylab("Average sentiment score")


review_words_counted <- review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

review_words_counted


word_summaries <- review_words_counted %>% group_by(word) %>%  summarize( reviews = n(),
                                                                     uses = sum(n),
                                                                     average_stars = mean(stars))
word_summaries

word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200)

word_summaries_filtered


word_summaries_filtered %>%
  arrange(desc(average_stars))

word_summaries_filtered %>%
  arrange(average_stars)


ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Stars")
