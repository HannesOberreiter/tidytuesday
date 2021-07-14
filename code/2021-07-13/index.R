source("partials/setup.R")
source("functions/functions.R")
library(ggtext)
library(showtext)
font_add_google("Caveat", "custom")
## Automatically use showtext to render text for future devices
showtext_auto()
## Tell showtext the resolution of the device,
## only needed for bitmap graphics. Default is 96
showtext_opts(dpi = 96)

scoobydoo <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv")


# Looking at Correlation between Snacks and Caught
raw_data <- scoobydoo %>%
    select(c(tidyr::starts_with("snack_"), tidyr::starts_with("caught_"))) %>%
    mutate(
        across(everything(), as.logical),
    ) %>%
    # double pivot to get matrix
    pivot_longer(
        tidyr::starts_with("snack_"),
        names_to = "Snack_Character",
        values_to = "Snack",
    ) %>%
    pivot_longer(
        tidyr::starts_with("caught_"),
        names_to = "Caught_Character",
        values_to = "Caught"
    ) %>%
    # Don't want this categories
    filter(!(Caught_Character %in% c("caught_not", "caught_other"))) %>%
    # Drop Rows with missing data
    filter(!if_any(c("Snack", "Caught"), is.na)) %>%
    count(Snack_Character, Caught_Character, Snack, Caught) %>%
    filter(Snack != FALSE) %>%
    # Arrange that we can later use last and first
    # first is not caught, last is caught
    arrange(Caught) %>%
    # Text Cleanup
    mutate(
        across(Snack_Character:Caught_Character, stringr::str_remove_all, pattern = "snack_|caught_"),
        across(Snack_Character:Caught_Character, stringr::str_to_title)
    )

# Summarise the Data for Plotting
data <- raw_data %>%
    group_by(Snack_Character, Caught_Character) %>%
    summarise(
        # first is not caught, last is caught
        ratio = round(last(n) / first(n) * 100),
        label = glue::glue("({last(n)} / {first(n)}) <br/> **{ratio}%**"),
        snacks = sum(n),
        label_color = ifelse(ratio > 90, "white", "black")
    ) %>%
    ungroup() %>%
    glimpse()

# Creating some Custom Axis Text Labels
axis_labels <- unique(data$Caught_Character) # Extract Axis Names
axis_label_x <- data %>%
    group_by(Snack_Character) %>%
    summarise(
        snacks = sum(snacks),
        label = glue::glue("**{Snack_Character[[1]]}**<br/>(Snacks = {snacks})")
    ) %>%
    glimpse() %>%
    pull(label)
names(axis_label_x) <- axis_labels

labels <- c(
    setosa = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/Iris_setosa.JPG/180px-Iris_setosa.JPG'
    width='100' /><br>*I. setosa*",
    virginica = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Iris_virginica_-_NRCS.jpg/320px-Iris_virginica_-_NRCS.jpg'
    width='100' /><br>*I. virginica*",
    versicolor = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/20140427Iris_versicolor1.jpg/320px-20140427Iris_versicolor1.jpg'
    width='100' /><br>*I. versicolor*"
)

# Plotting
p <- data %>%
    ggplot(aes(Snack_Character, Caught_Character, fill = ratio, label = label)) +
    ylab("caught by (success / failed)") +
    xlab("snack eaten by") +
    geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1, guide = "none") +
    ggplot2::scale_color_identity() +
    ggtext::geom_richtext(
        aes(color = label_color),
        size = 8,
        family = "custom",
        fill = NA, label.color = NA, # remove background and outline
    ) +
    coord_fixed() +
    ggplot2::scale_x_discrete(
        labels = axis_label_x
    ) +
    ggplot2::labs(
        title = "Scooby Doo Episodes: Do snacks influence the capture success?",
        subtitle = "Characters who got to eat snacks in the episode and the ratio of success for the individual characters in comparison.\n Not accounted for possible characters combination(s) of snacks per episode, removed unsuccessful hunt and if caught by other than main character.",
        caption = "Source: plummye (kaggle.com) | Graphics by: Hannes Oberreiter | #TidyTuesday "
    ) +
    ggplot2::theme(
        text = element_text(family = "custom"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_markdown(size = 15, face = "bold", margin = margin(r = -5)),
        axis.text.x = element_markdown(size = 15, face = "bold", margin = margin(t = -5)),
        axis.title.x = element_text(size = 20, margin = margin(t = 10)),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(
            color = "grey10", size = 20, face = "bold",
            hjust = 0,
            margin = margin(t = 0)
        ),
        plot.title.position = "plot",
        plot.subtitle = element_text(
            color = "grey30", size = 12,
            lineheight = 1.2,
            hjust = 0,
            margin = margin(t = 10, b = 0)
        ),
        plot.caption = element_text(
            color = "grey30", size = 8,
            lineheight = 1.2, hjust = 1,
            margin = margin(t = 10, b = 0)
        ),
    )

fSaveImages("2021-07-13", p, w = 10, h = 10)