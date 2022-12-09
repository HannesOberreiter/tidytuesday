source("partials/setup.R")
source("functions/functions.R")
day <- "02"

# A for Rock, B for Paper, and C for Scissors
# X for Rock, Y for Paper, and Z for Scissors.
# shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
# outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)

shapes = tibble(me = c("X", "Y", "Z"), values = c(1,2,3), translated = c("A", "B", "C"))

# Part 1
result <- readr::read_lines(glue("aoc/2022/{day}.txt")) %>% 
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
  select(op, translated, values, outcome, total)

result %>%
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

result2 <- readr::read_lines("aoc/2022/02/data.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("op", 'me'), sep = " ") %>% 
  mutate(
    outcome = case_when(
      me == "X" ~ x_loose(op),
      me == "Y" ~ y_draw(op),
      me == "Z" ~ z_win(op),
      
    )
  )

result2 %>%  pull(outcome) %>%  sum

r <- result %>% count(outcome) %>% 
  mutate(
    outcome = case_when(
      outcome == 0 ~ 'Loose',
      outcome == 3 ~ 'Draw',
      outcome == 6 ~ 'Win'
    ),
    round = 'Part 1'
  ) %>% 

r2 <- result2 %>% count(me) %>% 
  mutate(
    outcome = case_when(
      me == 'X' ~ 'Loose',
      me == 'Y' ~ 'Draw',
      me == 'Z' ~ 'Win'
    ),
    round = 'Part 2'
  ) %>% 
  select(-me)

bind_rows(r, r2) %>% 
  ungroup() %>% 
  mutate(
    percent = round(n / nrow(result) * 100, 1),
    outcome = forcats::fct_relevel(outcome, c('Loose', 'Draw', 'Win'))
  ) %>% 
  ggplot(aes(x = outcome, y = n, fill = round)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_text(
      aes(y = n + 25, label=paste(percent,"%")),
      position = position_dodge(.9),
      ) + 
  ggplot2::scale_fill_manual(
    values = colorBlindBlack8,
    name = "Part"
  ) +
  ggplot2::labs(
    title = "Distribution of outcomes from the Rock Paper Scissors strategy game",
    subtitle = glue::glue("Part 1 score: {result %>% pull(total) %>%  sum %>%  format(big.mark = ',')}; Part 2 score: {result2 %>%  pull(outcome) %>% sum %>% format(big.mark = ',')}"),
    caption = "AoC 2022 - Day 2",
    x = '',
    y = "Count [#]"
  ) +
  theme_classic()

