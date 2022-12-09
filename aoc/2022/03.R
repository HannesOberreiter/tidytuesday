source("partials/setup.R")
source("functions/functions.R")
day <- "03"

# A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, 

# Lowercase item types a through z have priorities 1 through 26.
# Uppercase item types A through Z have priorities 27 through 52.

items <- tibble(item = c(letters, LETTERS), prio = c(1:52))

readr::read_lines(glue("aoc/2022/{day}.txt")) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(
      first = stringr::str_sub(value, 1, stringr::str_length(value)/2),
      second = stringr::str_sub(value, stringr::str_length(value)/2 + 1, stringr::str_length(value)),
      same = intersect(unlist(strsplit(first, "")), unlist(strsplit(second, "")))
  ) %>% 
  ungroup() %>% 
  separate_rows(same, sep = "") %>% 
  filter(same != "") %>% 
  left_join(items, by = c("same" = "item")) %>% 
  pull(prio) %>% sum()

# Part 2
readr::read_lines("aoc/2022/03/data.txt") %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  mutate(
    group = cumsum(lag(rowid, default = FALSE) %% 3 == 0)
  ) %>% 
  group_by(group) %>% 
  summarise(
    same = intersect(
        intersect(
          unlist(strsplit(value[1], "")), 
          unlist(strsplit(value[2], ""))
        ),
        unlist(strsplit(value[3], ""))
        )
  ) %>% 
  left_join(items, by = c("same" = "item")) %>% 
  pull(prio) %>% sum()
