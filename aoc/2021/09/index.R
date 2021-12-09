source("partials/setup.R")
source("functions/functions.R")

# Load File
data <- scan("aoc/2021/09/data.csv", what = character()) %>%
    map(~ as.numeric(strsplit(as.character(.x), "") %>% unlist()))
data <- matrix(data %>% unlist(), ncol = length(data[[1]]), nrow = length(data), byrow = TRUE)

# Part 1
checkOutside <- function(r, c, m) {
    x <- tryCatch(m[r, c], error = function(x) logical())
    ifelse(length(x) > 0, x, 9)
}
checkAroundMe <- function(r, c, m) {
    up <- checkOutside(r, c + 1, m)
    down <- checkOutside(r, c - 1, m)
    left <- checkOutside(r - 1, c, m)
    right <- checkOutside(r + 1, c, m)
    center <- m[r, c]
    center < up & center < down & center < left & center < right
}
counter <- 1
results <- list()
for (r in 1:nrow(data)) {
    for (c in 1:ncol(data)) {
        if (checkAroundMe(r, c, data)) {
            results[[counter]] <- data[r, c]
            counter <- counter + 1
        }
    }
}
result <- results %>% as.numeric()
sum(result + 1)


# Part 2
checkBasin <- function(positions, m, visited) {
    if (length(positions) == 0) {
        return(visited)
    }
    new_positions <- list()
    counter <- 1
    for (i in 1:length(positions)) {
        p <- positions[[i]]
        if (checkOutside(p[1], p[2], m) != 9) {
            if (visited[p[1], p[2]] == FALSE) {
                visited[p[1], p[2]] <- TRUE
                new_positions[[counter]] <- c(p[1], p[2] + 1)
                counter <- counter + 1
                new_positions[[counter]] <- c(p[1], p[2] - 1)
                counter <- counter + 1
                new_positions[[counter]] <- c(p[1] - 1, p[2])
                counter <- counter + 1
                new_positions[[counter]] <- c(p[1] + 1, p[2])
                counter <- counter + 1
            }
        }
    }
    checkBasin(new_positions, m, visited)
}

results <- list()
counter <- 1
for (r in 1:nrow(data)) {
    for (c in 1:ncol(data)) {
        if (checkAroundMe(r, c, data)) {
            positions <- list()
            positions[[1]] <- c(r, c + 1)
            positions[[2]] <- c(r, c - 1)
            positions[[3]] <- c(r - 1, c)
            positions[[4]] <- c(r + 1, c)
            visited <- data > 10
            visited[r, c] <- TRUE
            v <- checkBasin(positions, data, visited)
            results[[counter]] <- v
            counter <- counter + 1
        }
    }
}
x <- map(results, sum) %>%
    as.numeric() %>%
    sort(decreasing = TRUE)
prod(x[1:3])


# Plotting fun
top <- map(results, sum) %>%
    as.numeric() %>%
    sort(decreasing = TRUE) %>%
    .[1:5]
vec <- map(results, sum) %>%
    as.numeric()
top_ma <- results[which(vec %in% top, arr.ind = TRUE)]
top_basins <- Reduce("+", lapply(top_ma, function(x) x == TRUE)) %>%
    as_tibble() %>%
    rowid_to_column("id") %>%
    pivot_longer(-c(id), values_to = "Depth") %>%
    mutate(name = str_remove(name, "V") %>% as.numeric())

background <- data %>%
    as_tibble() %>%
    rowid_to_column("id") %>%
    pivot_longer(-c(id), values_to = "Depth") %>%
    mutate(name = str_remove(name, "V") %>% as.numeric())

p <- background %>%
    ggplot(aes(x = name, y = id, fill = Depth)) +
    geom_raster(show.legend = FALSE) +
    geom_raster(data = top_basins, aes(x = name, y = id, alpha = Depth), fill = "#e2731e", show.legend = FALSE, inherit.aes = FALSE) +
    ggplot2::scale_fill_continuous(limits = c(0, 8), na.value = "#0072B2") +
    # ggplot2::scale_fill_viridis_c(option = "cividis", direction = 0) +
    scale_alpha(range = c(0, 0.3)) +
    theme_void() +
    ggplot2::coord_equal() +
    labs(
        title = "<b>AoC - Day 9: Smoke Basin, <span style='color:#e2731e'>Top 5 biggest basins.</span></b>",
        subtitle = "",
        caption = "Advent of Code 2021, Hannes Oberreiter  "
    ) +
    ggplot2::theme(
        panel.background = element_rect(fill = "#0072B2"),
        # plot.subtitle = ggtext::element_markdown(margin = margin(t = 10, b = -20, r = 50), hjust = 1),
        plot.title = ggtext::element_markdown(hjust = 0.5, color = "white", margin = margin(t = 30, b = -40)),
        plot.caption = element_text(color = "white", margin = margin(t = -15, b = 15), hjust = 0.5)
    )

fSaveImages("aoc09", p, w = 10, h = 9.5)
