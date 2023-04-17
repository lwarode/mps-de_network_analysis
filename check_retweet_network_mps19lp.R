library(tidyverse)
tweet_df_final <- readRDS("data/tweet_df_final_2.rds")

# tweet_df_final_2 <- tweet_df_final %>% 
#   select(
#     tw_id, author_id, id, referenced_tweets, date, politician_id, party = faction, full_name, starts_with("tweet_type")
#   )

saveRDS(object = tweet_df_final_2, file = "tweet_df_final_2.rds")

tweet_df_final %>% colnames()
# tweet_df_final_2 %>% colnames()


tweet_df_final %>% 
  select(author_id, tw_id) %>% 
  distinct()

tweet_df_final %>% 
  select(author_id) %>% 
  distinct()

tweet_df_final %>% 
  select(tw_id) %>% 
  distinct()

tweet_df_final %>%
  select(tw_id, tweet_id = id) %>%
  group_by(tw_id) %>% 
  count()

# author_ids <- tweet_df_final$author_id
# author_ids %>% unique %>% length

# referenced_tweets_list <- tweet_df_final$referenced_tweets
# referenced_tweets %>% unlist %>% 
# tweet_types <- map(1:length(referenced_tweets_list) , ~ referenced_tweets_list[[.x]][["type"]])
# tweet_types %>% unlist()
# tweet_ref_id <- map(1:length(referenced_tweets_list) , ~ referenced_tweets_list[[.x]][["id"]])

tweet_df_final %>% 
  filter(id == "1049376782862995456") %>% pull(referenced_tweets)

tweet_df_final %>% 
  filter(id == "1266003012230750213") %>% pull(referenced_tweets)

tweet_df_final %>% 
  filter(id == "1266002195364880386") %>% pull(tw_id)






tweet_df_final$id


retweet_df <- tweet_df_final %>% 
  select(referenced_tweets, tweet_type_character, tweet_type_final, id) %>% 
  mutate(
    # type of referenced tweet
    ref_tweet_types = unlist(map(1:length(referenced_tweets) , ~ referenced_tweets[[.x]][["type"]])),
    # id of referenced tweet (NOT id of user)
    ref_tweet_id = unlist(map(1:length(referenced_tweets) , ~ referenced_tweets[[.x]][["id"]]))
  )  

retweet_df$ref_tweet_id %>% class()

# author id of retweets
author_id_ref_tweet <- retweet_df %>% 
  filter(ref_tweet_types == "retweeted") %>% 
  mutate(ref_tweet_id = unlist(ref_tweet_id)) %>% 
  left_join(
    tweet_df_final %>% select(id, author_id, tw_id), 
    by = c("ref_tweet_id" = "id")
  ) %>% 
  filter(!is.na(author_id), !is.na(tw_id)) %>% 
  select(id, ref_tweet_id, author_id, author_id, tw_id)
 
# final df of retweets within with author information
retweet_within_df <- retweet_df %>% 
  filter(ref_tweet_types == "retweeted") %>% 
  filter(ref_tweet_id %in% tweet_ids) %>% 
  left_join(tweet_df_final %>% select(id, author_id, tw_id)) %>% 
  # directedness: from (orginal tweet) -> to (retweeted tweet) 
  left_join(author_id_ref_tweet, by = "id", suffix = c("_to", "_from"))

# check -- works
tweet_df_final %>% filter(id == "1440944083581669376") %>% select(tw_id, id, text)


retweet_within_df %>% 
  group_by(tw_id_from) %>% 
  count() %>% 
  arrange(desc(n))

library(igraph)
library(tidygraph)

# graph <- igraph::graph_from_data_frame(retweet_within_df)

graph <- retweet_within_df %>% 
  select(to = tw_id_to, from = tw_id_from) %>% 
  igraph::graph_from_data_frame(directed = T)

graph
plot(graph)

tidy_graph <- retweet_within_df %>% 
  select(to = tw_id_to, from = tw_id_from) %>% 
  tidygraph::as_tbl_graph()

tidy_graph
library(ggraph)
tidy_graph %>% 
  ggplot

retweet_within_df %>% 
  group_by(tw_id_from) %>%
  count() %>%
  ungroup() %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  left_join(party_user, by = c("tw_id_from" = "tw_id")) %>%
  ggplot(aes(x = n, y = reorder(tw_id_from, n), fill = party)) + 
  geom_col() +
  scale_fill_manual(values = party_colors)

retweet_within_df_dist <- retweet_within_df %>% 
  distinct(tw_id_from, tw_id_to, .keep_all = T)

tidy_graph_2 <- retweet_within_df_dist %>% 
  select(tw_id_from, tw_id_to) %>% 
  slice(1:100) %>% 
  as_tbl_graph(directed = T)

plot(tidy_graph_2)


  
ggraph(tidy_graph_2) + theme_graph()

network_all <- ggraph(tidy_graph) + 
  geom_edge_link() +
  geom_node_point()

network_all

network_party <- ggraph(tidy_graph) + 
  geom_edge_link(aes(alpha = 0.5)) +
  geom_node_point(aes(alpha = 0.5))


  retweet_within_df %>% mutate(ref_tweet_id = unlist(.))




# TODO write csv with subset that can be used for assignment 2 (and 3)
  

# user_id %>% filter(tweet_ref_id != "NULL" & tweet_types != "NULL")

# tweet_df_final %>% slice(7) %>% pull(tweet_type_list) %>% is.null()

# user_id

od_tw_info <- read_csv("data_processed/od_tw_merge_final.csv")

# 
retweets_within_df %>% 
  

# 
# tweet_ref_df <- data.frame(
#   tweet_type = tweet_types %>% unlist(),
#   tweet_ref_id = tweet_ref_id %>% unlist()
# )
# 
# tweet_ref_df %>% 
#   filter(tweet_type == "retweeted") %>% 
#   filter(tweet_ref_id %in% tweet_ids) 

# number of tweets that are retweets within network


retweets_within <- retweets_within_df %>% 
  pull(tweet_ref_id)

retweets_within



# tweet_type_list = map(
#   1:nrow(.), ~ tweet_df[["referenced_tweets"]][[.x]][["type"]] 
# )
#   