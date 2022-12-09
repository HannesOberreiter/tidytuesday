source("partials/setup.R")
source("functions/functions.R")

day <- "04"

result <- readr::read_lines(glue("aoc/2022/{day}.txt")) %>% 
  as_tibble() %>% 
  separate(value, into = c("f_1", "f_2", "s_1", "s_2"), sep ="[-,]", convert = TRUE) %>%
  rowwise() %>% 
  mutate(
    f = list(c(f_1:f_2)),
    s = list(c(s_1:s_2)),
    i = list(intersect(unlist(f), unlist(s))),
    l = length(i),
    total_overlap = length(f) == l | length(s) == l
  )
# Part 1
result %>%  filter(total_overlap)
# Part 2
result %>% filter(l >= 1)

f <- result %>%  pull(f)
s <- result %>%  pull(s)
t <- c(f, s)
plot <- list()
for(i in 1:length(t)){
  plot[[i]] <- tibble(values = t[[i]])
}

bind_rows(plot) %>% 
  count(values) %>% 
  mutate(
    g = floor(values/10),
    f = as.character(values),
    f = substr(f, nchar(f), nchar(f)),
  ) %>%  View()

bind_rows(plot) %>% 
  count(values) %>% 
  mutate(
    g = floor(values/10) %>%  as.character(),
    f = as.character(values),
    f = substr(f, nchar(f), nchar(f)),
  ) %>%
  ggplot(aes(g, f, fill = n)) +
  geom_tile() +
  ggplot2::scale_fill_viridis_c(
    option = "inferno", direction = -1, name = "Cleaned [#]"
    ) +
  ggplot2::coord_fixed() +
  ggplot2::labs(
    title = "Heatmap of how often sections are cleaned",
    subtitle = "Camp and section corresponding to IDs eg., Camp 2, Section 4 is ID 24.",
    caption = "AoC 2022 - Day 4",
    x = 'Camp',
    y = "Section"
  ) +
  theme_classic()
