source("partials/setup.R")
source("functions/functions.R")
library(ggtext)
library(showtext)
library(lubridate)
font_add_google("Caveat", "custom")
showtext_auto()

raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv")

drought_names <- c(
    "no drought" = "None",
    "abnormally dry" = "D0",
    "moderate drought" = "D1",
    "severe drought" = "D2",
    "extreme drought" = "D3",
    "exceptional drought" = "D4"
)

raw %>%
    mutate(
        date = map_date %>% lubridate::ymd(.),
        year = year(date),
        drought_lvl = as.factor(drought_lvl),
        drought_lvl = forcats::fct_recode(drought_lvl, !!!drought_names),
        drough_int = as.integer(drought_lvl),
    ) %>%
    glimpse() %>%
    ggplot(aes(x = date, y = pop_pct, fill = drought_lvl)) +
    ggplot2::geom_col()


group_by(date, state_abb) %>%
    mutate(
        total = sum(pop_pct)
    ) %>%
    ungroup() %>%
    glimpse() %>%
    group_by(date, state_abb, drough_int, drought_lvl) %>%
    summarise(
        p = sum(pop_pct) / first(total)
    ) %>%
    ungroup() %>%
    glimpse()