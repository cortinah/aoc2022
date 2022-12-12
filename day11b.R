## Part 2

acm = 11*17*5*13*19*2*3*7

pass <- function(item, add=0, mult=1, divisible, iftrue, iffalse) {
  operation = ((item+add)*mult) %% acm
  div_test = operation %% divisible
  if(div_test==0) return(c(iftrue, operation))
  return(c(iffalse, operation)) }

monkey0 <- function(item) {
  pass(item, add=0, mult=13, divisible = 11, 4, 7) }
monkey0list <- c(98, 97, 98, 55, 56, 72)

monkey1 <- function(item) {
  pass(item, add=4, mult=1, divisible = 17, 2, 6) }
monkey1list <- c(73, 99, 55, 54, 88, 50, 55)

monkey2 <- function(item) {
  pass(item, add=0, mult=11, divisible = 5, 6, 5) }
monkey2list <- c(67, 98)

monkey3 <- function(item) {
  pass(item, add=8, mult=1, divisible = 13, 1, 2) }
monkey3list <- c(82, 91, 92, 53, 99)

monkey4 <- function(item) {
  pass(item, add=0, mult=item, divisible = 19, 3, 1) }
monkey4list <- c(52, 62, 94, 96, 52, 87, 53, 60)

monkey5 <- function(item) {
  pass(item, add=5, mult=1, divisible = 2, 7, 0) }
monkey5list <- c(94, 80, 84, 79)

monkey6 <- function(item) {
  pass(item, add=1, mult=1, divisible = 3, 0, 5) }
monkey6list <- c(89)

monkey7 <- function(item) {
  pass(item, add=3, mult=1, divisible = 7, 4, 3) }
monkey7list <- c(70, 59, 63)

items <- list(monkey0list, monkey1list, monkey2list, monkey3list, monkey4list, monkey5list, monkey6list, monkey7list)
rm(monkey0list, monkey1list, monkey2list, monkey3list, monkey4list, monkey5list, monkey6list, monkey7list)
monkeyops <- rep(0, 8)

monkeyfun <- function(monkeynum, items) {
  if(monkeynum==1) return(monkey0(items))
  if(monkeynum==2) return(monkey1(items))
  if(monkeynum==3) return(monkey2(items))
  if(monkeynum==4) return(monkey3(items))
  if(monkeynum==5) return(monkey4(items))
  if(monkeynum==6) return(monkey5(items))
  if(monkeynum==7) return(monkey6(items))
  if(monkeynum==8) return(monkey7(items))
}  

for (rounds in 1:10000) {
  for (monkeynum in 1:8) {
  
    while (length(items[[monkeynum]])!=0) {
      out <- monkeyfun(monkeynum, items[[monkeynum]][1])
      items[[monkeynum]] <- tail(items[[monkeynum]], -1)
      tomonkey <- out[1]+1; value <- out[2]
      items[[tomonkey]] <- c(items[[tomonkey]], value)    
      monkeyops[monkeynum] <- monkeyops[monkeynum]+1
    }
  }
}

prod(sort(monkeyops)[7:8])

