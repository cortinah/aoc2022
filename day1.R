library(adventdrob)
library(tidyverse)

input <- advent_input(1, parse = T)

# part 1
input |> mutate(group=cumsum(is.na(x))) |> count(group, wt = x) |> arrange(-n)

# part 2
input |> mutate(group=cumsum(is.na(x))) |> count(group, wt = x) |>
  arrange(-n) |> head(3) |> summarize(sum(n))
