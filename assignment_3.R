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

retweet_within_df <- read_rds("data/retweet_within_df.rds")

mdb_profiles_lp19_list <- read_rds("data/mdb_profiles_lp19_list.rds")

mdb_profiles_lp19_list_df <- mdb_profiles_lp19_list |> 
  bind_rows() |> 
  mutate(tw_id = str_to_lower(username))


node_information <- retweet_within_df |> 
  select(tw_id_from, party_from) |> 
  distinct()

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

fun_mean_party_congruence <- function() {
  
  df_infomap <- retweet_network |> 
    filter(!party %in% c("Fraktionslos", NA)) %>%
    mutate(
      im1 = group_infomap(trials = 10),
      # im2 = group_infomap()
    ) |> 
    as.data.frame() |> 
    group_by(im1, party) |> 
    count() |> 
    group_by(im1) |> 
    mutate(party_freq = n / sum(n))
  
  df_infomap |> 
    filter(n == max(n)) |> 
    pull(party_freq) |> 
    mean()
}

fun_mean_party_congruence()

df_infomap |> 
  filter(n == max(n)) |> 
  group_by(party) |> 
  summarise(mean = mean(party_freq))

df_infomap |> distinct(im1)

?group_infomap

mean(replicate(1000, fun_mean_party_congruence()))
