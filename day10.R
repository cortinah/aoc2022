library(adventdrob)
library(tidyverse)

advent_input(10, year = 2022, parse = F) |> separate(x,c("opp","val")," ") |> mutate(n=row_number()) -> input

# Part 1
input |> mutate(quant=ifelse(opp=="addx", 2, 1)) -> input
as.data.frame(lapply(input, rep, input$quant)) |> arrange(n) |> mutate(cycle=row_number()) -> input
input |> mutate(dup=duplicated(input[,c("opp","n")])) -> input

input |> mutate(val=ifelse(input$dup==FALSE,0,input$val)) -> input
input |> mutate(val=ifelse(is.na(input$val),0,input$val)) -> input
  
input |> mutate(vallag=lag(input$val)) -> input
input[1,"vallag"] <- 0

input |> mutate(x=1+cumsum(vallag)) |> mutate(signal=cycle*x) -> input

input |> filter(cycle %in% c(20, 60, 100, 140, 180, 220)) |> summarize(sum(signal))

# Part 2

input |> select(x) |> mutate(cycle=row_number()) |> select(cycle, x) -> input
input |> mutate(spritel=x-1, spritem=x, spriter=x+1) |> select(-x) -> input
input |> mutate(drawing=rep(0:39,6)) -> input

input |> mutate(match= (drawing==spritel) | (drawing==spritem) | (drawing==spriter) ) -> input
input |> mutate(pixel= ifelse(match==T, "#", " ") ) -> input

screen = matrix(input$pixel, ncol=40, nrow=6, byrow = T)                
                