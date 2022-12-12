library(adventdrob)
library(tidyverse)
library(tidygraph)

advent_input(12, year = 2022, parse = T) -> input

# Part 1
g <- input %>% grid_graph(x, mutual = TRUE, directed = TRUE)

nodes <- as_tibble(g) %>% mutate(elevation = case_when(value == "S" ~ 1L,
                                                    value == "E" ~ 26L,
                                                    TRUE ~ match(value, letters)))
filt <- g %>% activate("edges") %>%
  mutate(from_val = nodes$elevation[from],
         to_val = nodes$elevation[to]) %>%
  filter(to_val <= from_val + 1) %>%
  activate("nodes") %>%
  mutate(distance = node_distance_to(which(nodes$value=="E"), mode="out"))

filt %>% filter(value=="S")

# Part 2
filt |> filter(value=="a") |> arrange(distance)
