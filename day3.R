library(adventdrob)
library(tidyverse)
library(vecsets)

input <- advent_input(3, parse = F)

# part 1
input |> mutate(l=str_length(x)) -> input

input |> mutate(a=substr(x,1,l/2)) |> mutate(b=substr(x,(l/2)+1,l)) -> input

intersect <- function(a,b) {
  i<-vintersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]], multiple = F) }

input |> mutate(c=map2_chr(a,b,intersect)) -> input

lets <- c(letters, LETTERS)
input |> mutate(p=map_int(c,~which(lets==.x))) -> input
  
input |> summarize(sum(p))  
  
# part 2
input <- advent_input(3, parse = F)

input |> mutate(group=rep(c("a","b","c"),100),obs=sort(rep(1:100,3)) ) -> input

pivot_wider(input, names_from = group, values_from = x, id_cols = obs) -> input

intersect <- function(a,b) {
  i<-vintersect(strsplit(as.character(a), "")[[1]], strsplit(as.character(b), "")[[1]],multiple = F)
  paste(unlist(i),collapse="")}

input |> mutate(d=map2_chr(a,b,intersect)) -> input
input |> mutate(d=map2_chr(c,d,intersect)) -> input

input |> mutate(p=map_int(d,~which(lets==.x))) -> input

input |> summarize(sum(p))  

