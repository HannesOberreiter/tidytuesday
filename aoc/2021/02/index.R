source("partials/setup.R")
source("functions/functions.R")

# Part 1 ----
readr::read_delim("aoc/2021/02/data.csv", delim = " ", col_names = c("direction", "value")) %>%
    mutate(
        g = ifelse(direction == "forward", "horizontal", "vertical"),
        value = ifelse(direction == "up", value * -1, value)
    ) %>%
    group_by(g) %>%
    summarise(
        value = sum(value)
    ) %>%
    pull(value) %>%
    prod()

# Part 2 ----
readr::read_delim("aoc/2021/02/data.csv", delim = " ", col_names = c("direction", "value")) %>%
    mutate(
        value = ifelse(direction == "up", value * -1, value),
        aim = ifelse(direction == "forward", 0, value) %>% cumsum(),
        depth = ifelse(direction == "forward", value * aim, value)
    ) %>%
    filter(direction == "forward") %>%
    summarise(
        position = sum(value),
        depth = sum(depth)
    ) %>%
    as.numeric() %>%
    prod()