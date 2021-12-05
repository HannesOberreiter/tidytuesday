source("partials/setup.R")

# Load Data
vents <- readr::read_delim(
    file = "aoc/2021/05/data.csv",
    col_names = c("first", "second"),
    col_types = "cc",
    delim = " -> "
) %>%
    separate(first, sep = ",", into = c("x1", "y1"), convert = TRUE) %>%
    separate(second, sep = ",", into = c("x2", "y2"), convert = TRUE)
# add one because our matrix starts with 1 and not 0
vents <- vents + 1
vent_map <- matrix(0, ncol = max(c(vents$x1, vents$x2)), nrow = max(c(vents$y1, vents$y2)))
# Part 1
vents_filtered <- vents %>%
    filter(x1 == x2 | y1 == y2)
for (i in 1:nrow(vents_filtered)) {
    vent <- vents_filtered[i, ]
    vent_map[c(vent$y1:vent$y2), c(vent$x1:vent$x2)] <-
        vent_map[c(vent$y1:vent$y2), c(vent$x1:vent$x2)] + 1
}
length(vent_map[vent_map > 1])

# Part 2
vents_diag <- vents %>%
    filter(x1 != x2 & y1 != y2)

for (i in 1:nrow(vents_diag)) {
    vent <- vents_diag[i, ]
    diag_matrix <- matrix(FALSE, nrow = nrow(vent_map), ncol = ncol(vent_map))
    diag(diag_matrix[c(vent$y1:vent$y2), c(vent$x1:vent$x2)]) <- TRUE
    vent_map[diag_matrix] <- vent_map[diag_matrix] + 1
}
length(vent_map[vent_map > 1])


# Plot it
vent_map %>%
    as_tibble() %>%
    rownames_to_column("id") %>%
    pivot_longer(-c(id), values_to = "counts") %>%
    ggplot(aes(x = name, y = id, fill = counts)) +
    geom_raster() +
    ggplot2::scale_fill_viridis_c(option = "inferno") +
    theme_void() +
    ggplot2::coord_equal()