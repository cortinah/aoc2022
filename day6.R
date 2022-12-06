library(adventdrob)
library(tidyverse)

input <- as.character(advent_input(6, year = 2022, parse = F))
input <- str_split(input,pattern = '')[[1]]

# part 1
for (i in 4:length(input)) {
  if (length(unique(input[(i-3):i]))==4) {cat(i);return(i)}
}

# part 2
for (i in 14:length(input)) {
  if (length(unique(input[(i-13):i]))==14) {cat(i);return(i)}
}
