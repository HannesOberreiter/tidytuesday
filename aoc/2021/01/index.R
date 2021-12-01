source("partials/setup.R")
source("functions/functions.R")

# Part 1 ----
readr::read_lines("aoc/2021/01/data.csv") %>%
    as_tibble() %>%
    glimpse() %>%
    mutate(
        value = as.numeric(value),
        increase = ifelse(lag(value) < value, 1, 0)
    ) %>%
    glimpse() %>%
    summarise(
        result = sum(increase, na.rm = TRUE)
    )

# Part 2 ----
readr::read_lines("aoc/2021/01/data.csv") %>%
    as_tibble() %>%
    mutate(
        value = as.numeric(value),
        window = value + lead(value) + lead(value, 2),
        increase = ifelse(lag(window) < window, 1, 0)
    ) %>%
    glimpse() %>%
    summarise(
        result = sum(increase, na.rm = TRUE)
    )