library(adventdrob)
library(tidyverse)

input <- advent_input(4, year = 2022, parse = T)

# part 1
input |> separate(col = x,into = c('a','b'),sep = ",") -> input
input |> separate(col = a,into = c('a1','a2'),sep = "-") -> input
input |> separate(col = b,into = c('b1','b2'),sep = "-") -> input
input |> mutate_all(as.numeric) -> input


input |> mutate(inc1 = ((a1 >= b1) & (a2 <= b2)) ) -> input
input |> mutate(inc2 = ((b1 >= a1) & (b2 <= a2)) ) -> input
input |> mutate(inc = inc1 | inc2) -> input

input |> summarize(ans=sum(inc))

# part 2

input |> mutate(inc = ((a1 <= b2) & (a2 >= b1))) -> input

input |> summarize(ans=sum(inc))

