library(tidyverse)
library(tidygraph)

tweet_df_final <- readRDS("data/tweet_df_final_2.rds")
od_tw_info <- read_csv("data/od_tw_merge_final.csv")

retweet_df <- tweet_df_final %>% 
  select(referenced_tweets, tweet_type_character, tweet_type_final, id) %>% 
  mutate(
    # type of referenced tweet
    ref_tweet_types = map(1:length(referenced_tweets),
                          ~ referenced_tweets[[.x]][["type"]]),
    # id of referenced tweet (NOT id of user)
    ref_tweet_id = map(1:length(referenced_tweets),
                       ~ referenced_tweets[[.x]][["id"]])
  ) 

author_id_ref_tweet <- retweet_df %>% 
  filter(ref_tweet_types == "retweeted") %>% 
  mutate(ref_tweet_id = unlist(ref_tweet_id)) %>% 
  left_join(
    tweet_df_final %>% select(id, author_id, tw_id), 
    by = c("ref_tweet_id" = "id")
  ) %>% 
  filter(!is.na(author_id), !is.na(tw_id)) %>% 
  select(id, ref_tweet_id, author_id, author_id, tw_id)

# vector with IDs from Tweets that are from authors within the dataset
tweet_ids <- tweet_df_final$id

retweet_within_df <- retweet_df %>% 
  filter(ref_tweet_types == "retweeted") %>% 
  filter(ref_tweet_id %in% tweet_ids) %>% 
  left_join(tweet_df_final %>% select(id, author_id, tw_id, date)) %>% 
  # directedness: from (orginal tweet) -> to (retweeted tweet) 
  left_join(author_id_ref_tweet, by = "id", suffix = c("_to", "_from")) |> 
  # party affiliation
  ## from
  left_join(od_tw_info |> select(twitter_name, party_from = faction), 
            by = c("tw_id_from" = "twitter_name")) |> 
  ## to
  left_join(od_tw_info |> select(twitter_name, party_to = faction), 
            by = c("tw_id_to" = "twitter_name"))

# check
retweet_within_df |> 
  filter(tw_id_from == "c_lindner") |> 
  pull(party_from) |> 
  unique()

retweet_within_df |> 
  filter(tw_id_from == "c_lindner") |> 
  pull(party_to) 

write_csv(retweet_within_df, "data/retweet_within_df.csv")
write_rds(retweet_within_df, "data/retweet_within_df.rds")



# retweet_network <- retweet_within_df %>% 
#   select(from = tw_id_from, to = tw_id_to) %>% 
#   as_tbl_graph(directed = TRUE) %>% 
#   left_join(party_user, by = c("name" = "tw_id")) 
# 
# # get corresponding row ID (from ID) 
# row_id_from <- retweet_network %>% 
#   as_tibble() %>%
#   mutate(id_from = row_number())
# 
# # update retweet network with edge information: name and party of retweeted users (from)
# retweet_network <- retweet_network %>% 
#   activate(edges) %>% 
#   left_join(row_id_from, by = c("from" = "id_from")) %>% 
#   rename(party_from = party, name_from = name) %>% 
#   activate(nodes)

# retweet_network