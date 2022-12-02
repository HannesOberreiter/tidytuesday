source("partials/setup.R")
source("functions/functions.R")

# A for Rock, B for Paper, and C for Scissors
# X for Rock, Y for Paper, and Z for Scissors.
# shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
# outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)

shapes = tibble(me = c("X", "Y", "Z"), values = c(1,2,3), translated = c("A", "B", "C"))

# Part 1
readr::read_lines("aoc/2022/02/data.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("op", 'me'), sep = " ") %>% 
  left_join(shapes) %>% 
  mutate(
    outcome = case_when(
      op == translated ~ 3,
      op == "C" & translated == "A" ~ 6,
      op == "C" & translated == "B" ~ 0,
      op == "B" & translated == "C" ~ 6,
      op == "B" & translated == "A" ~ 0,
      op == "A" & translated == "B" ~ 6,
      op == "A" & translated == "C" ~ 0,
    ),
    total = outcome + values
  ) %>% 
  select(op, translated, values, outcome, total) %>% 
  pull(total) %>%  sum
    
# Part 2
#  X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win

shapes = tibble(me = c("X", "Y", "Z"), values = c(1,2,3), translated = c("A", "B", "C"))

y_draw <- function(input){
  return(
    case_when(
      input == "A" ~ 1 + 3,
      input == "B" ~ 2 + 3,
      input == "C" ~ 3 + 3
    )
  )
}
x_loose <- function(input){
  return(
    case_when(
      input == "A" ~ 3,
      input == "B" ~ 1,
      input == "C" ~ 2
    )
  )
}
z_win <- function(input){
  return(
    case_when(
      input == "A" ~ 2 + 6,
      input == "B" ~ 3  + 6,
      input == "C" ~ 1 + 6
    )
  )
}

readr::read_lines("aoc/2022/02/data.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("op", 'me'), sep = " ") %>% 
  mutate(
    outcome = case_when(
      me == "X" ~ x_loose(op),
      me == "Y" ~ y_draw(op),
      me == "Z" ~ z_win(op),
      
    )
  ) %>%  pull(outcome) %>%  sum
  