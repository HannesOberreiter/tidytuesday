source("partials/setup.R")
source("functions/functions.R")

elve <- 1
elve_counter <- function(){
  elve <<- elve + 1
  return(elve)  
}
# Part 1
result <- readr::read_lines("aoc/2022/01/test.txt") %>% 
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

