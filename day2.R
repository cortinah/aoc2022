library(adventdrob)
library(tidyverse)

input <- advent_input(2, parse = F)

# part 1
input <- separate(input, col = x, into=c("a", "b"))

input <- input |> mutate(playera=ifelse(a=="A", "rock", ifelse(a=="B", "paper", "scissors")))
input <- input |> mutate(playerb=ifelse(b=="X", "rock", ifelse(b=="Y", "paper", "scissors")))

input <- input |> mutate(winner=ifelse(playera==playerb, 3, 0))
input <- input |> mutate(winner=ifelse(playera=="paper" & playerb=="scissors", 6, winner))
input <- input |> mutate(winner=ifelse(playera=="scissors" & playerb=="rock", 6, winner))
input <- input |> mutate(winner=ifelse(playera=="rock" & playerb=="paper", 6, winner))
input <- input |> mutate(point=ifelse(playerb=="rock", 1, ifelse(playerb=="paper", 2, 3)))
input <- input |> mutate(total=winner+point)

input |> summarize(total_all=sum(total))
          
# part 2
input <- advent_input(2, parse = F)
input <- separate(input, col = x, into=c("a", "outcome"))
input <- input |> mutate(playera=ifelse(a=="A", "rock", ifelse(a=="B","paper","scissors")))

input <- input |> mutate(playerb=ifelse(outcome=="Z" & playera=="rock", "paper", NA))
input <- input |> mutate(playerb=ifelse(outcome=="Z" & playera=="scissors", "rock", playerb))
input <- input |> mutate(playerb=ifelse(outcome=="Z" & playera=="paper", "scissors", playerb))

input <- input |> mutate(playerb=ifelse(outcome=="X" & playera=="rock", "scissors",playerb))
input <- input |> mutate(playerb=ifelse(outcome=="X" & playera=="scissors", "paper", playerb))
input <- input |> mutate(playerb=ifelse(outcome=="X" & playera=="paper", "rock", playerb))

input <- input |> mutate(playerb=ifelse(outcome=="Y" & playera=="rock", "rock",playerb))
input <- input |> mutate(playerb=ifelse(outcome=="Y" & playera=="scissors", "scissors", playerb))
input <- input |> mutate(playerb=ifelse(outcome=="Y" & playera=="paper", "paper", playerb))

input <- input |> mutate(winner=ifelse(playera==playerb, 3, 0))
input <- input |> mutate(winner=ifelse(playera=="paper" & playerb=="scissors", 6, winner))
input <- input |> mutate(winner=ifelse(playera=="scissors" & playerb=="rock", 6, winner))
input <- input |> mutate(winner=ifelse(playera=="rock" & playerb=="paper", 6, winner))
input <- input |> mutate(point=ifelse(playerb=="rock", 1, ifelse(playerb=="paper", 2, 3)))
input <- input |> mutate(total=winner+point)

input |> summarize(total_all=sum(total))
