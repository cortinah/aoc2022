library(adventdrob)
library(tidyverse)

input <- advent_input(5, year = 2022, parse = F)

# part 1

moves <- input |> filter(str_detect(x, "move")) |> 
  extract(x, c("num", "from", "to"), "move (\\d+) from (\\d+) to (\\d+)",
          convert = TRUE)

crates <- input |> filter(!str_detect(x, "move")) |>
  grid_tidy(x) |>
  filter(str_detect(value, "[A-Z]")) |>
  arrange(desc(row)) |>
  mutate(crate=match(col, unique(sort(col)))) |>
  select(crate,value) |>
  group_by(crate) |>
  summarize(crates=list(value)) |>
  deframe()

for(i in 1:nrow(moves)) {
  
  r <- moves[i, ]
  to_move = rev(tail(crates[[r$from]], r$num))
  crates[[r$from]] <- head(crates[[r$from]], -r$num)
  crates[[r$to]] <- c(crates[[r$to]], to_move)
  }

# part2

for(i in 1:nrow(moves)) {
  
  r <- moves[i, ]
  to_move = tail(crates[[r$from]], r$num)
  crates[[r$from]] <- head(crates[[r$from]], -r$num)
  crates[[r$to]] <- c(crates[[r$to]], to_move)
}
