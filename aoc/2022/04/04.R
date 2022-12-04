source("partials/setup.R")
source("functions/functions.R")

day <- "04"

readr::read_lines(glue("aoc/2022/{day}/{day}.txt")) %>% 
  as_tibble() %>% 
  separate(value, into = c("f_1", "f_2", "s_1", "s_2"), sep ="[-,]", convert = TRUE) %>%
  rowwise() %>% 
  mutate(
    f = list(c(f_1:f_2)),
    s = list(c(s_1:s_2)),
    i = list(intersect(unlist(f), unlist(s))),
    l = length(i),
    total_overlap = length(f) == l | length(s) == l
  ) %>% 
  # Part 1
  filter(total_overlap)
  # Part 2
  # filter(l >= 1)
  
