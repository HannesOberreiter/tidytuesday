tibble(
    deg = seq(min(1), max(360), length.out = length(names) + 1) %>% tail(-1),
    name = names
) %>%
    # Convert to Coordinates
    mutate(
        theta = 2 * pi * deg / 360,
        x = 5 * sin(theta),
        y = 5 * cos(theta)
    ) %>%
    glimpse()