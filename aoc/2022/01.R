source("partials/setup.R")
source("functions/functions.R")
day <- "01"

elve <- 1
elve_counter <- function(){
  elve <<- elve + 1
  return(elve)  
}
# Part 1
result <- readr::read_lines(glue("aoc/2022/{day}.txt")) %>% 
  as_tibble %>% 
  rowwise() %>% 
  mutate(
    elve = ifelse(value == "", elve_counter(), .env$elve)
  ) %>% 
  group_by(elve) %>% 
  summarise(
    calories = sum(as.integer(value), na.rm = TRUE)
  ) %>% 
  arrange(desc(calories))
  
# Part 2
sum(result[1:3,2])

result[1:3,2][[1]]

ggplot(result, aes(calories)) +
  geom_histogram() +
  scale_y_continuous(
    breaks = scales::breaks_pretty()
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty()
  ) +
  ggplot2::labs(
    title = "Distribution of the Calories each Elf is carrying",
    subtitle = glue::glue("The max calories carried by one elve is {result[1,2][[1]]}."),
    caption = "AoC 2022 - Day 1",
    x = 'Calories [kcals]',
    y = "Count [#]"
  ) +
  theme_classic()

