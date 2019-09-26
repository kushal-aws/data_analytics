library(jsonlite)
yelp <- fromJSON("/Users/kushalwalia/Desktop/yelp_academic_dataset_business.json")

yelp <- stream_in(file("/Users/kushalwalia/Desktop/yelp_academic_dataset_business.json"))

str(yelp)

yelp_flat <- flatten(yelp)

str(yelp_flat)

library(tibble)

yelp_tbl <- as_data_frame(yelp_flat)

yelp_tbl

library(dplyr)
yelp_tbl %>% mutate(categories = as.character(categories)) %>% select(categories)

yelp_tbl %>%
  select(-starts_with("hours"), -starts_with("attribute"))


library(stringr)


# to select only rows with the work restaurant in categories.
yelp_count_restaurants <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant"))

#Just to check whether the categories have the word restaurant in it.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  mutate(categories = as.character(categories)) %>% select(categories)


library(tidyr)

#unnests categories and creates multiple rows for each restaurants with
yelp_categories <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>% filter(str_detect(categories, "Restaurant")) %>%
mutate(categories=strsplit(as.character(categories), ",")) %>%
unnest(categories) %>% select(name, categories)

#unnests categories and creates multiple rows for each restaurants with broken down categories.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>% mutate(categories=strsplit(as.character(categories), ",")) %>%
  unnest(categories) %>%
  select(name, categories) %>% count(categories)

#unnests categories and creates multiple rows for each restaurants with broken down categories. Arranges desc.
yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>% mutate(categories=strsplit(as.character(categories), ",")) %>%
  unnest(categories) %>%
  select(name, categories) %>%
  count(categories) %>%
  arrange(desc(n))

#removes seperate rows with the work restaurant in them.
yelp_restaurant_count_types <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>% mutate(categories=strsplit(as.character(categories), ",")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  select(name, categories) %>%
  count(categories) %>%
  arrange(desc(n))

#restaurant type by state
yelp_restaurant_bystate <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>% mutate(categories=strsplit(as.character(categories), ",")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  arrange(desc(n))

yelp_restaurant_bystate <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>% mutate(categories=strsplit(as.character(categories), ",")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  group_by(state) %>%
  top_n(5, n)


yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  group_by(state) %>%
  top_n(1, n)


 yelp_test <- yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  filter(n > 10) %>%
  group_by(state) %>%
  top_n(5, n)

 yelp_tbl %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
   filter(str_detect(categories, "Restaurant")) %>%
   unnest(categories) %>%
   filter(categories != "Restaurants") %>%
   count(state, categories) %>%
   group_by(state) %>%
   top_n(1, n)

write.csv(yelp_flat, file = "/Users/kushalwalia/Desktop/business_final.csv")


#Viz.

ratings_and_users <- yelp_tbl %>% group_by(stars) %>% count()

ggplot(data=ratings_and_users, aes(x=cut(stars,c(0,0.9,1.9,2.9,3.9,5)), y=n,fill=cut(stars, c(0,0.9,1.9,2.9,3.9,5)))) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "RdBu") + labs(y = "Users", x = "Stars") + theme(legend.position="none") +
  scale_x_discrete(labels=c("1","2","3","4","5"))

yelp_flat %>% filter(yelp_flat$attributes.DogsAllowed=="TRUE") %>% summarise(count=n())  %>% group_by(yelp_flat$city) %>% arrange(desc(count))
