library(adventdrob)
library(tidyverse)

input <- advent_input(7, year = 2022, parse = F)

cd <- function(path, dir) {
  
  if (!is.na(dir)) {
    if (dir == "..") {
      return(head(path, -1))
    }
    return(c(path, paste0(tail(path, 1), "/", dir)))
  }
  return(path)
}

# part 1

dir_sizes <- input |>
  extract(x, "cd_dir", "cd (.*)", remove = FALSE) |>
  mutate(path = c(accumulate(cd_dir,cd))) |>
  unnest(path) |>
  filter(str_detect(x, "\\d")) |>
  separate(x, c("size", "file"), sep =" ", convert=TRUE) |>
  group_by(path) |>
  summarise(size=sum(size))

dir_sizes |>
  filter(size < 100000)|> summarise(sum(size))

# part 2
dir_sizes |>
  filter(size > (41412830+ 30000000 - 70000000)) |>
  summarize(min(size))
