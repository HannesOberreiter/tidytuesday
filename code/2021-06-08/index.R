source("partials/setup.R")
source("functions/functions.R")
library(ggwordcloud)
library(ggtext)
library(showtext)
font_add_google("Source Sans Pro", "sourcesans")
## Automatically use showtext to render text for future devices
showtext_auto()
## Tell showtext the resolution of the device,
## only needed for bitmap graphics. Default is 96
showtext_opts(dpi = 96)

fishing <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv")
# stocked <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv")

# Cleanup ----
fishing_clean <- fishing %>%
    tidyr::separate_rows(species, sep = " and ") %>%
    mutate(
        species = stringr::str_to_title(species) %>%
            # Hardcoded why not
            stringr::str_replace("Chubs", "Chub") %>%
            stringr::str_replace("Crappies", "Crappie") %>%
            stringr::str_replace("Bullheads", "Bullhead") %>%
            stringr::str_replace("Amercian Eel", "American Eel")
    )

# Distinct Observed ----
distinct_years <- fishing_clean %>%
    drop_na(grand_total) %>%
    filter(grand_total > 0) %>%
    distinct(year, lake, species) %>%
    group_by(year, species) %>%
    summarise(
        lakes_n = n_distinct(lake),
        lake = str_c(lake, collapse = ",")
    ) %>%
    filter(lakes_n == 1)

distinct_all <- fishing_clean %>%
    drop_na(grand_total) %>%
    filter(grand_total > 0 & between(year, 2012, 2015)) %>%
    distinct(lake, species) %>%
    group_by(species) %>%
    summarise(
        lakes_n = n_distinct(lake),
        lake = str_c(lake, collapse = ",")
    ) %>%
    filter(lakes_n == 1)

# Wordcloud ----
wordcloud <- fishing_clean %>%
    drop_na(grand_total) %>%
    filter(grand_total > 0) %>%
    group_by(lake) %>%
    mutate(grand_total_lake = sum(grand_total)) %>%
    group_by(lake, species) %>%
    summarise(
        grand_total_percent = sum(grand_total) * 100 / grand_total_lake[[1]]
    )

wordcloud %>%
    ggplot(aes(label = species, size = grand_total_percent, color = grand_total_percent)) +
    ggwordcloud::geom_text_wordcloud() +
    theme_bw() +
    facet_wrap(~lake)

# Maps ----
library(sf)
library(rnaturalearth)

lake_names <- unique(wordcloud$lake)
lakes <- ne_download(scale = 50, type = "lakes", category = "physical", returnclass = "sf")

great_lakes <- lakes %>%
    mutate(
        lake = stringr::str_remove(name, "Lake ")
    ) %>%
    filter(
        lake %in% lake_names
    )
centers <- great_lakes$geometry %>%
    sf::st_centroid() %>%
    st_coordinates() %>%
    as_tibble()
great_lakes <- dplyr::bind_cols(great_lakes, centers)
great_words <- great_lakes %>%
    left_join(wordcloud)

p <- great_words %>%
    ggplot2::ggplot(aes(x = X, y = Y, color = lake)) +
    # Dirty helper for legend
    geom_point() +
    ggplot2::geom_sf(color = "black", fill = "#6699CC") +
    ggwordcloud::geom_text_wordcloud(
        aes(label = species, size = grand_total_percent),
        grid_size = 1
    ) +
    ggplot2::scale_size(range = c(2, 10)) +
    ggplot2::scale_colour_manual(values = c("#882255", "#CC79A7", "#D55E00", "#009E73", "#585858")) +
    ggplot2::coord_sf(clip = "off") +
    xlab("") +
    ylab("") +
    ggplot2::labs(
        title = "Great Lakes - Relative Observation of Species 1915-2015",
        subtitle = "Wordcloud size corresponds to the relative number of grand total observed species.",
        caption = "Source: Great Lakes Fishery Commission | Map: NaturalEarthData | Graphics by: Hannes Oberreiter | #TidyTuesday ",
        color = ""
    ) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(
            color = "grey10", size = 25, face = "bold",
            hjust = 0,
            margin = margin(t = 0)
        ),
        plot.subtitle = ggtext::element_markdown(
            color = "grey30", size = 15,
            lineheight = 1.35,
            hjust = 0,
            margin = margin(t = 10, b = 0)
        ),
        plot.title.position = "plot",
        strip.background = element_rect(
            fill = "white",
            colour = "black", size = rel(2)
        ),
        plot.caption = element_text(
            color = "grey30", size = 8,
            lineheight = 1.2, hjust = 0.5,
            margin = margin(t = 4, b = 0)
        ),
        legend.position = c(0.7, 0.85),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "grey30", size = 15)
    ) +
    guides(colour = guide_legend(ncol = 2, nrow = 3, byrow = TRUE, override.aes = list(size = 10)))

fSaveImages("2021-06-08", p, w = 14, h = 10)