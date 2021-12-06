source("partials/setup.R")
source("functions/functions.R")

data <- readr::read_lines("aoc/2021/06/data.csv") %>%
    str_split(",", simplify = TRUE) %>%
    as.integer()

# Part 1
history <- list()
current <- data
days <- 80
for (day in 1:days) {
    new <- rep(8, length(current[current == 0]))
    current <- current - 1
    current[current == -1] <- 0
    current <- c(current, new)
    history[[day]] <- current
    current <- history[[day]]
}
length(history[[days]])

# Part 2
fish <- as.numeric(rep(0, 9)) # integer gives integer overflow :/
names(fish) <- 0:8
fish[names(table(data))] <- table(data)
days <- 256
for (day in 1:days) {
    spawn <- fish[1]
    fish[1:8] <- fish[2:9]
    fish[9] <- spawn
    fish[7] <- fish[7] + spawn
}
format(sum(fish), scientific = FALSE)


# Plotting History
fish <- as.numeric(rep(0, 9)) # integer gives integer overflow :/
names(fish) <- 0:8
fish[names(table(data))] <- table(data)
history <- list()
days <- 256
for (day in 1:days) {
    spawn <- fish[1]
    fish[1:8] <- fish[2:9]
    fish[9] <- spawn
    fish[7] <- fish[7] + spawn
    history[[day]] <- fish
}
format(sum(fish), scientific = FALSE)

library(gganimate)
df <- bind_rows(history) %>%
    mutate(daysum = rowSums(.)) %>%
    rowid_to_column("day") %>%
    pivot_longer(-c("day", "daysum")) %>%
    mutate(
        day_string = glue::glue("Day: {day} || Laternfish: {daysum}"),
        day_string = forcats::fct_reorder(day_string, day)
    ) %>%
    ggplot(aes(x = name, y = value)) +
    geom_col() +
    scale_y_log10(
        # trans = scales::pseudo_log_trans(),
        breaks = scales::log_breaks(),
        labels = scales::label_number_si()
    ) +
    labs(
        title = "AoC - Day 6: Lanternfish",
        subtitle = "Day: {frame_time}",
        x = "Spawning Counter",
        y = "Count [#]",
        caption = "Advent of Code 2021, Hannes Oberreiter"
    ) +
    theme_classic() +
    theme(
        panel.grid.major.y = element_line(linetype = "dashed"),
        axis.title.y = element_text(margin = margin(l = 50, r = -50)),
        plot.caption = element_text(hjust = 1, size = 5)
    ) +
    transition_time(day) +
    ease_aes("linear") +
    enter_grow()

anim_save("output/figs/aoc06.gif", animate(df, fps = 8))
