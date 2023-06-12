library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)


# Prerequisites -----------------------------------------------------------

party_colors <- c("#138BD8", "#000000", "#FFEE0A", "#529222", "#AE1862", "#E30019")
names(party_colors) <- c("AfD", "CDU/CSU", "FDP", "Grüne", "DIE LINKE.", "SPD")

theme_graph_custom <- function() {
  font <- "Corbel"
  # base theme
  theme_graph() %+replace%
    theme(
      text = element_text(family = font),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.box.background = element_rect(
        colour = "black",
        fill = "white",
        linetype = "solid"
      ),
      # faceting
      strip.background = element_blank(),
      strip.text = element_text(color = "black"),
    )
}

theme_custom <- function() {
  font <- "Corbel"
  # base theme
  theme_bw() %+replace%
    theme(
      text = element_text(family = font),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.box.background = element_rect(
        colour = "black",
        fill = "white",
        linetype = "solid"
      ),
      # grid lines
      panel.grid.major = element_line(color = "grey60", size = 0.2),
      panel.grid.minor = element_line(color = "grey80", size = 0.1),
      # faceting
      strip.background = element_blank(),
      strip.text = element_text(color = "black"),
    )
}

# function that stores environment object based on relative sub-folder path
store_object <- function(obj_name, rel_path, file_type = ".rds") {
  obj_name_string <- deparse(substitute(obj_name))
  object_file <- paste0(obj_name_string, file_type)
  file_name <- here::here(rel_path, object_file)
  # return(file_name)
  if (file_type == ".rds") {
    saveRDS(obj_name, file_name)
  }
}

retweet_within_df <- read_rds("data/retweet_within_df.rds")

mdb_profiles_lp19_list <- read_rds("data/mdb_profiles_lp19_list.rds")

mdb_profiles_lp19_list_df <- mdb_profiles_lp19_list |> 
  bind_rows() |> 
  mutate(tw_id = str_to_lower(username))




edge_information <- retweet_within_df |> 
  select(date) |> 
  mutate(edge_id = row_number())

retweet_network <- retweet_within_df %>% 
  select(from = tw_id_from, to = tw_id_to) %>%
  as_tbl_graph(directed = TRUE) |> 
  left_join(node_information, by = c("name" = "tw_id_from")) |> 
  activate(edges) |> 
  mutate(edge_id = row_number()) |> 
  left_join(edge_information) |> 
  activate(nodes) |> 
  rename(party = party_from)

row_id_from <- retweet_network %>% 
  as_tibble() %>%
  mutate(id_from = row_number())

retweet_network <- retweet_network |> 
  activate(edges) %>% 
  left_join(row_id_from, by = c("from" = "id_from")) %>% 
  rename(party_from = party, name_from = name) %>% 
  activate(nodes)

retweet_network  

plot_all_auto <- retweet_network %>% 
  filter(!party %in% c("Fraktionslos", NA)) %>% 
  ggraph(layout = "auto") + 
  geom_edge_link(alpha = 0.0082) +
  geom_node_point(aes(color = party))  +
  scale_color_manual(values = party_colors) + 
  theme_graph_custom() 

class(retweet_network)

ggsave(
  filename = "figures/plot_all_auto.png",
  plot = plot_all_auto,
  width = 10,
  height = (10/16*9)
)

plot_all_circular <- retweet_network %>% 
  filter(!party %in% c("Fraktionslos", NA)) %>% 
  ggraph(layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(color = party_from), alpha = 0.015) +
  scale_edge_color_manual(values = party_colors) + 
  coord_fixed() +
  theme_graph_custom() +
  theme(legend.position = "none")

ggsave(
  filename = "figures/plot_all_circular.png",
  plot = plot_all_circular,
  width = 7,
  height = 7
)


# Community Detection -----------------------------------------------------

set.seed(42)
group_infomap_retweet_network <- retweet_network %>% 
  mutate(
    group_infomap = group_infomap(trials = 1000)
  )

group_louvain_retweet_network <- retweet_network %>% 
  mutate(
    group_louvain = group_louvain()
  )

group_infomap_retweet_network |> 
  as.data.frame() |> 
  group_by(party, group_infomap) |> 
  count() |> 
  arrange(desc(n))

# Association Centrality Measures and Profile Metrics ---------------------

# Profile Metrics according to January 2023

# df with centrality measures
centrality_retweet_network <- retweet_network |> 
  activate(nodes) |> 
  mutate(
    `In-degree` = centrality_degree(mode = "in"),
    `Out-degree` = centrality_degree(mode = "out"),
    Closeness = centrality_closeness(),
    Betweenness = centrality_betweenness(),
    Eigen = centrality_eigen(),
    PageRank = centrality_pagerank()
  ) 
  
df_centrality_retweet_network <- centrality_retweet_network |> 
  as.data.frame() |> 
  left_join(mdb_profiles_lp19_list_df, by = c("name" = "tw_id"))

map(
  df_centrality_retweet_network |> 
    select(in_degree:PR),
  ~ cor.test(
    .x,
    df_centrality_retweet_network$public_metrics$followers_count
  )
)

# logged followers count
map(
  df_centrality_retweet_network |> 
    select(in_degree:PR),
  ~ cor.test(
    .x,
    log(df_centrality_retweet_network$public_metrics$followers_count)
  )
)

df_centrality_retweet_network_long <- df_centrality_retweet_network |> 
  pivot_longer(
    cols = `In-degree`:PageRank,
    names_to = "measure",
    values_to = "value"
  )

df_centrality_retweet_network_long |> 
  filter(!party %in% c("Fraktionslos", NA)) %>%
  ggplot(aes(x = public_metrics$followers_count, y = value)) +
  facet_wrap(~ party + measure, scales = "free") +
  scale_color_manual(values = party_colors) +
  geom_point(aes(color = party, alpha = 0.35)) +
  ggpubr::stat_cor() +
  theme_custom() +
  scale_x_log10() +
  scale_y_log10()


  
options(scipen=10000)

pl_centrality_measures <- df_centrality_retweet_network_long |> 
  filter(value != 1) |> 
  filter(!party %in% c("Fraktionslos", NA)) %>%
  ggplot(aes(x = public_metrics$followers_count, y = value)) +
  facet_wrap(~ measure, scales = "free") +
  scale_color_manual(values = party_colors) +
  scale_x_continuous(labels = scales::label_comma()) +
  geom_point(alpha = 0.5) +
  ggpubr::stat_cor(p.accuracy = 0.001) +
  theme_custom() +
  labs(x = "N Followers", y = "Centrality Value")
  # scale_x_log10() +
  # scale_y_log10()

pl_centrality_measures

ggsave(
  "figures/pl_centrality_measures.png",
  plot = pl_centrality_measures,
  width = 10,
  height = (10/16*9)
)

pl_centrality_measures_log <- df_centrality_retweet_network_long |> 
  filter(value != 1) |> 
  filter(!party %in% c("Fraktionslos", NA)) %>%
  ggplot(aes(x = public_metrics$followers_count, y = value)) +
  facet_wrap(~ measure, scales = "free") +
  scale_color_manual(values = party_colors) +
  geom_point(alpha = 0.5) +
  ggpubr::stat_cor(p.accuracy = 0.001) +
  theme_custom() +
  labs(x = "N Followers", y = "Centrality Value") +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10()

pl_centrality_measures_log

ggsave(
  "figures/pl_centrality_measures_log.png",
  plot = pl_centrality_measures_log,
  width = 10,
  height = (10/16*9)
)

pl_centrality_measures_log_party <- df_centrality_retweet_network_long |> 
  filter(value != 1) |>
  filter(measure %in% c("Betweenness", "Closeness", "Out-degree")) |> 
  filter(!party %in% c("Fraktionslos", NA)) %>%
  ggplot(aes(x = public_metrics$followers_count, y = value)) +
  facet_wrap(~ measure + party, scales = "free", ncol = 6) +
  scale_color_manual(values = party_colors) +
  geom_point(alpha = 0.5, aes(color = party)) +
  ggpubr::stat_cor(p.accuracy = 0.001) +
  theme_custom() +
  labs(x = "N Followers", y = "Centrality Value") +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10() +
  theme(legend.position = "none")

pl_centrality_measures_log_party

ggsave(
  "figures/pl_centrality_measures_log_party.png",
  plot = pl_centrality_measures_log_party,
  width = 14,
  height = (14/16*9)
)


# Modelling ---------------------------------------------------------------

# Goal: Calculating Party Congruence per found cluster

# Infomap
imc <- cluster_infomap(retweet_network, nb.trials = 10)
membership(imc)
communities(imc)
plot(imc, retweet_network)

?cluster_infomap

rep(communities(cluster_infomap(retweet_network, nb.trials = 10)), 10)

fun_mean_party_congruence <- function(n_trials = 10) {
  
  df_infomap <- retweet_network |> 
    filter(!party %in% c("Fraktionslos", NA)) %>%
    mutate(
      im1 = group_infomap(trials = n_trials),
      # im2 = group_infomap()
    ) |> 
    as.data.frame() |> 
    group_by(im1, party) |> 
    count() |> 
    group_by(im1) |> 
    mutate(party_freq = n / sum(n))
  
  mean_all <- df_infomap |> 
    filter(n == max(n)) |> 
    pull(party_freq) |> 
    mean()
  
  mean_party <- df_infomap |> 
    filter(n == max(n)) |> 
    group_by(party) |> 
    summarise(mean = mean(party_freq)) |> 
    ungroup()
  
  df <- tibble(
    mean_all = mean_all,
    party = mean_party$party,
    mean_party = mean_party$mean,
    n_trials = n_trials
  )
  
}


# From all predicted communities, what is the average share of the largest party within that community?
# Mean = mean of all values
# Measure = party congruence

df_test <- fun_mean_party_congruence()

df_infomap |> 
  filter(n == max(n)) |> 
  group_by(party) |> 
  summarise(mean = mean(party_freq))

df_infomap |> distinct(im1)

infomap_run_100_500_50 <- map(
  seq(100, 500, 50),
  ~ rerun(50, fun_mean_party_congruence(n_trials = .x))
)
5
store_object(infomap_run_100_500_50, "data")

df_infomap_run_5_100_50 <- readRDS("~/Documents/MDS/Semester_4/Applied_Network_Analysis/mps-de_network_analysis/data/df_infomap_run_5_100_50.rds")



df_infomap_run_5_100_50 |> 
  pull(mean_all) |> 
  mean()


# TODO replace with seq(5, 500, 10)

# df_infomap_run_5_100_100 <- infomap_run_5_100_100 |> 
#   bind_rows() 
# 
# store_object(df_infomap_run_5_100_100, "data")
# 

df_infomap_run_5_100_50 |> 
  distinct(mean_all, n_trials, .keep_all = T) |> 
  pull(mean_all) |> 
  mean()

pl_infomap_run_5_100_50 <- df_infomap_run_5_100_50 |>
  distinct(mean_all, n_trials, .keep_all = T) |>
  ggplot(aes(x = factor(n_trials), y = mean_all)) +
  geom_boxplot() +
  geom_point(alpha = 0.35) +
  theme_custom() +
  labs(x = "N Trials", y = "Party Congruence (Mean)")  +
  scale_y_continuous(labels = scales::percent)

pl_infomap_run_5_100_50

ggsave(
  file = "figures/pl_infomap_run_5_100_50.png",
  plot = pl_infomap_run_5_100_50,
  width = 6,
  height = 6
)


df_infomap_run_5_100_50 |> 
  group_by(party) |> 
  summarise(mean_party = mean(mean_party)) |> 
  arrange(desc(mean_party))


pl_infomap_run_5_100_50_party <- df_infomap_run_5_100_50 |>
  ggplot(aes(x = factor(n_trials), y = mean_party, color = party)) +
  facet_wrap(~ party) +
  scale_color_manual(values = party_colors) +
  geom_boxplot() +
  geom_point(alpha = 0.35) +
  theme_custom() +
  theme(legend.position = "none") +
  labs(x = "N Trials", y = "Party Congruence (Mean)")  +
  scale_y_continuous(labels = scales::percent)


ggsave(
  file = "figures/pl_infomap_run_5_100_50_party.png",
  plot = pl_infomap_run_5_100_50_party,
  width = 10,
  height = 8
)
  

pl_infomap_run_5_100_50_party

seq(5, 100, 10) |> length()



# HRG Link Prediction -----------------------------------------------------
predict_network <- readRDS("~/Documents/MDS/Semester_4/Applied_Network_Analysis/mps-de_network_analysis/data/predict_network.rds")


pred_df <- function(pred_obj) {
  from_df <- retweet_network |> 
    activate(edges) |>
    # select(from, name_from) |> 
    distinct(from, name_from) |> 
    as.data.frame() |> 
    select(from, name_from)
  
  node_information <- retweet_network |> 
    activate(nodes) |> 
    select(name, party_from = party) |> 
    # filter(!party_from %in% c("Fraktionslos", NA)) |> 
    as.data.frame() |> 
    distinct()
  
  row_id_from <- retweet_network %>% 
    as_tibble() %>%
    mutate(id_from = row_number())
  
  tibble(
    edges_from = pred_obj$edges[, 1],
    edges_to = pred_obj$edges[, 2],
    prob = pred_obj$prob
  ) |> 
    left_join(from_df, by = c("edges_from" = "from")) |> 
    left_join(node_information, by = c("name_from" = "name")) |> 
    left_join(row_id_from |> select(name_to = name, party_to = party, row_id = id_from), by = c("edges_to" = "row_id"))
}

df_predict_network <- pred_df(predict_network)


df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |>
  group_by(party_from, party_to) |> 
  summarise(prob_mean = mean(prob), prob_median = median(prob), prob_sd = sd(prob), n = n()) |> 
  # filter(party_from == party_to) |> 
  arrange(desc(prob_mean))

# mean
pl_predict_network_mean_party <- df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |>
  group_by(party_from, party_to) |> 
  summarise(prob_mean = mean(prob), prob_median = median(prob), prob_sd = sd(prob), n = n()) |> 
  ggplot(aes(x = party_to, y = prob_mean, fill = party_to)) +
  geom_col() +
  facet_wrap(~ party_from) +
  theme_custom() +
  theme(legend.position = "none") +
  labs(y = "Mean Probability of Link (Retweet)", x = "") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = party_colors) 

pl_predict_network_mean_party 

ggsave(
  filename = "figures/pl_predict_network_mean_party.png",
  pl_predict_network_mean_party,
  width = 12,
  height = (12*9/16)
)

# median
df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |>
  group_by(party_from, party_to) |> 
  summarise(prob_mean = mean(prob), prob_median = median(prob), prob_sd = sd(prob), n = n()) |> 
  ggplot(aes(x = party_to, y = prob_median, fill = party_to)) +
  geom_col() +
  facet_wrap(~ party_from) +
  theme_custom() +
  scale_fill_manual(values = party_colors) 

pl_predict_network_density_party <- df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  ggplot(aes(x = prob, y = party_to, fill = party_to))  +
  ggridges::geom_density_ridges() +
  facet_wrap(~ party_from) +
  scale_fill_manual(values = party_colors) +
  theme_custom() +
  theme(legend.position = "none") +
  labs(x = "Probability of Link (Retweet)", y = "") +
  scale_x_continuous(labels = scales::label_percent())

pl_predict_network_density_party

ggsave(
  filename = "figures/pl_predict_network_density_party.png",
  pl_predict_network_density_party,
  width = 12,
  height = (12*9/16)
)
  


df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  filter(prob > 0.5) |>
  group_by(party_from, party_to) |> 
  summarise(n = n())

df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  # filter(prob > 0.5) |>
  group_by(party_from, party_to) |> 
  summarise(n = n())

# test
df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  filter(prob > 0.5) |>
  filter(party_from != party_to) 

df_predict_network |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  filter(party_from != party_to) |> 
  arrange(desc(prob))


# Temporal Structure, ML 80/20 --------------------------------------------



# train_80_predict_links <- predict_edges(train_80_retweet_network)
# 
# store_object(train_80_predict_links, "data")
# 
# test_20_predict_links <- predict_edges(test_20_retweet_network)
# 
# store_object(test_20_predict_links, "data")
# 
# train_80_predict_links

train_80_predict_links <- readRDS("~/Documents/MDS/Semester_4/Applied_Network_Analysis/mps-de_network_analysis/data/train_80_predict_links.rds")
test_20_predict_links <- readRDS("~/Documents/MDS/Semester_4/Applied_Network_Analysis/mps-de_network_analysis/data/test_20_predict_links.rds")



df_train_80_predict_links <- pred_df(train_80_predict_links) |> 
  mutate(
    prob_gr_0.5 = ifelse(prob > 0.5, 1, 0),
    edge_id = paste(edges_from, edges_to, sep =  "_")
  )
df_test_20_predict_links <- pred_df(test_20_predict_links) |> 
  mutate(
    prob_gr_0.5 = ifelse(prob > 0.5, 1, 0),
    edge_id = paste(edges_from, edges_to, sep = "_")
  )


# TODO share of retweets within party

# share of parties from retweeted tweets by party 
retweet_within_df |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  group_by(party_from) |> 
  count(party_to) |> 
  mutate(freq = n / sum(n)) %>%
  print(n = nrow(.))

retweet_within_df |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  group_by(party_from) |> 
  count(party_to) |> 
  mutate(freq = n / sum(n)) %>% 
  filter(party_from == party_to) |> 
  arrange(desc(freq))

retweet_within_df |> 
  filter(!party_from %in% c("Fraktionslos", NA),
         !party_to %in% c("Fraktionslos", NA)) |> 
  group_by(party_to) |> 
  count(party_from) |> 
  mutate(freq = n / sum(n)) %>%
  print(n = nrow(.))
  
