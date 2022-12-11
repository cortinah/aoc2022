library(adventdrob)
library(tidyverse)

input <- advent_input(9, year = 2022, parse = F) |> separate(x, c("d","q")," ") |> mutate(n=row_number())
input <- as.data.frame(lapply(input, rep, input$q)) |> arrange(n) |> select(d)
input <- input |> mutate(hx=NA, hy=NA, tx=NA, ty=NA)
input <- rbind(c(0,0,0,0,0),input)

# Part 1

for (i in 2:nrow(input)) {
  
  if (input[i,"d"]=="U") {input[i,"hy"] <- input[i-1,"hy"]+1; input[i,"hx"] <- input[i-1,"hx"]}
  if (input[i,"d"]=="D") {input[i,"hy"] <- input[i-1,"hy"]-1; input[i,"hx"] <- input[i-1,"hx"]}
  if (input[i,"d"]=="R") {input[i,"hx"] <- input[i-1,"hx"]+1; input[i,"hy"] <- input[i-1,"hy"]}
  if (input[i,"d"]=="L") {input[i,"hx"] <- input[i-1,"hx"]-1; input[i,"hy"] <- input[i-1,"hy"]}
 }

for (i in 2:nrow(input)) {
  
  deltax <- input[i,"hx"] - input[i-1,"tx"]
  deltay <- input[i,"hy"] - input[i-1,"ty"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1)) {input[i,"tx"]<-input[i-1,"tx"]; input[i,"ty"]<-input[i-1,"ty"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"tx"]<-input[i-1,"tx"]+sign(deltax); input[i,"ty"]<-input[i-1,"ty"]+sign(deltay)}
     }

input |> group_by(tx,ty) |> count()

# Part 2
## 123456789
## abcdefgit

input <- advent_input(9, year = 2022, parse = F) |> separate(x, c("d","q")," ") |> mutate(n=row_number())
input <- as.data.frame(lapply(input, rep, input$q)) |> arrange(n) |> select(d)
input <- input |> mutate(hx=NA, hy=NA, ax=NA, ay=NA, bx=NA,by=NA, cx=NA,cy=NA, dx=NA, dy=NA,
                         ex=NA, ey=NA, fx=NA, fy=NA, gx=NA, gy=NA, ix=NA, iy=NA, tx=NA, ty=NA)
input <- rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), input)

for (i in 2:nrow(input)) {
  
  if (input[i,"d"]=="U") {input[i,"hy"] <- input[i-1,"hy"]+1; input[i,"hx"] <- input[i-1,"hx"]}
  if (input[i,"d"]=="D") {input[i,"hy"] <- input[i-1,"hy"]-1; input[i,"hx"] <- input[i-1,"hx"]}
  if (input[i,"d"]=="R") {input[i,"hx"] <- input[i-1,"hx"]+1; input[i,"hy"] <- input[i-1,"hy"]}
  if (input[i,"d"]=="L") {input[i,"hx"] <- input[i-1,"hx"]-1; input[i,"hy"] <- input[i-1,"hy"]}
}

for (i in 2:nrow(input)) {
  
  #a
  deltax <- input[i,"hx"] - input[i-1,"ax"]
  deltay <- input[i,"hy"] - input[i-1,"ay"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1)) {input[i,"ax"]<-input[i-1,"ax"]; input[i,"ay"]<-input[i-1,"ay"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"ax"]<-input[i-1,"ax"]+sign(deltax); input[i,"ay"]<-input[i-1,"ay"]+sign(deltay)}

  #b
  deltax <- input[i,"ax"] - input[i-1,"bx"]
  deltay <- input[i,"ay"] - input[i-1,"by"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1)) {input[i,"bx"]<-input[i-1,"bx"]; input[i,"by"]<-input[i-1,"by"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"bx"]<-input[i-1,"bx"]+sign(deltax); input[i,"by"]<-input[i-1,"by"]+sign(deltay)}
  
  #c
  deltax <- input[i,"bx"] - input[i-1,"cx"]
  deltay <- input[i,"by"] - input[i-1,"cy"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1)) {input[i,"cx"]<-input[i-1,"cx"]; input[i,"cy"]<-input[i-1,"cy"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"cx"]<-input[i-1,"cx"]+sign(deltax); input[i,"cy"]<-input[i-1,"cy"]+sign(deltay)}

  #d
  deltax <- input[i,"cx"] - input[i-1,"dx"]
  deltay <- input[i,"cy"] - input[i-1,"dy"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1)) {input[i,"dx"]<-input[i-1,"dx"]; input[i,"dy"]<-input[i-1,"dy"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"dx"]<-input[i-1,"dx"]+sign(deltax); input[i,"dy"]<-input[i-1,"dy"]+sign(deltay)}
  
  #e
  deltax <- input[i,"dx"] - input[i-1,"ex"]
  deltay <- input[i,"dy"] - input[i-1,"ey"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1))  {input[i,"ex"]<-input[i-1,"ex"];              input[i,"ey"]<-input[i-1,"ey"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"ex"]<-input[i-1,"ex"]+sign(deltax); input[i,"ey"]<-input[i-1,"ey"]+sign(deltay)}
  
  #f
  deltax <- input[i,"ex"] - input[i-1,"fx"]
  deltay <- input[i,"ey"] - input[i-1,"fy"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1))  {input[i,"fx"]<-input[i-1,"fx"];              input[i,"fy"]<-input[i-1,"fy"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"fx"]<-input[i-1,"fx"]+sign(deltax); input[i,"fy"]<-input[i-1,"fy"]+sign(deltay)}
  
  #g
  deltax <- input[i,"fx"] - input[i-1,"gx"]
  deltay <- input[i,"fy"] - input[i-1,"gy"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1))  {input[i,"gx"]<-input[i-1,"gx"];              input[i,"gy"]<-input[i-1,"gy"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"gx"]<-input[i-1,"gx"]+sign(deltax); input[i,"gy"]<-input[i-1,"gy"]+sign(deltay)}
  
  #i
  deltax <- input[i,"gx"] - input[i-1,"ix"]
  deltay <- input[i,"gy"] - input[i-1,"iy"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1))  {input[i,"ix"]<-input[i-1,"ix"];              input[i,"iy"]<-input[i-1,"iy"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"ix"]<-input[i-1,"ix"]+sign(deltax); input[i,"iy"]<-input[i-1,"iy"]+sign(deltay)}
  
  #t
  deltax <- input[i,"ix"] - input[i-1,"tx"]
  deltay <- input[i,"iy"] - input[i-1,"ty"]
  if ((abs(deltax)<=1) | (abs(deltay)<=1))  {input[i,"tx"]<-input[i-1,"tx"];              input[i,"ty"]<-input[i-1,"ty"]}
  if ((abs(deltax)>=2) | (abs(deltay)>=2) ) {input[i,"tx"]<-input[i-1,"tx"]+sign(deltax); input[i,"ty"]<-input[i-1,"ty"]+sign(deltay)}
  }

input |> group_by(tx,ty) |> count()
