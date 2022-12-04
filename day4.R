library(adventdrob)
library(tidyverse)

input <- advent_input(4, year = 2022, parse = T)

# part 1
input |> extract(col=x, into=c("a1","a2","b1","b2"), regex="(\\d+)-(\\d+),(\\d+)-(\\d+)", convert = T) -> input


input |> mutate(inc = ((a1 >= b1) & (a2 <= b2)) | ((b1 >= a1) & (b2 <= a2)) ) -> input
input |> summarize(ans=sum(inc)) 

# part 2

input |> mutate(inc = ((a1 <= b2) & (a2 >= b1))) -> input
input |> summarize(ans=sum(inc))

