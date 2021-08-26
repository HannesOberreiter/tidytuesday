source("partials/setup.R")
source("functions/functions.R")
library(ggtext)
library(showtext)
font_add_google("Roboto", "custom")
## Automatically use showtext to render text for future devices
showtext_auto()

raw_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv")
raw_data %>% glimpse()

birth_data <- raw_data %>%
    # drop NAs
    drop_na(dam_age_at_concep_y, sire_age_at_concep_y, litter_size) %>%
    # take distinct for each id, otherwise we would get multiple birth events
    dplyr::distinct(dlc_id, .keep_all = TRUE) %>%
    # control field this should be unique if we count size_damn and litter_size
    mutate(
        sire_dam = paste(sire_id, dam_id),
        sire_dam_dob = paste(sire_dam, dob)
    ) %>%
    # count(sire_dam, litter_size, sort = TRUE)
    mutate(
        age_difference = sire_age_at_concep_y - dam_age_at_concep_y,
        parent_difference = case_when(
            age_difference == 0 ~ "Same Age",
            age_difference > 0 ~ "Dam Older",
            age_difference < 0 ~ "Sire Older"
        )
    )

# most same partners
top_same_partners <- birth_data %>%
    distinct(sire_dam_dob, .keep_all = TRUE) %>%
    group_by(sire_dam) %>%
    summarise(
        successfull_mating = n(),
        name = paste(dam_name[[1]], sire_name[[1]], sep = " & ") %>% stringr::str_to_title(),
        offspring = sum(litter_size)
    ) %>%
    dplyr::slice_max(successfull_mating, n = 5) %>%
    glimpse()

# most offsprings
top_males <- birth_data %>%
    distinct(sire_dam_dob, .keep_all = TRUE) %>%
    group_by(sire_name) %>%
    summarise(offsprings = sum(litter_size), sire_name = sire_name[[1]] %>% stringr::str_to_title()) %>%
    dplyr::slice_max(offsprings, n = 5) %>%
    glimpse()
top_females <- birth_data %>%
    distinct(sire_dam_dob, .keep_all = TRUE) %>%
    group_by(dam_name) %>%
    summarise(offsprings = sum(litter_size), dam_name = dam_name[[1]] %>% stringr::str_to_title()) %>%
    dplyr::slice_max(offsprings, n = 5) %>%
    glimpse()


# Litter Size to Parents Age Difference?
p_age_diff <- birth_data %>%
    add_count(parent_difference) %>%
    group_by(parent_difference, litter_size) %>%
    summarise(
        nn = n(),
        d = nn / n[[1]] * 100,
        label_color = ifelse(d > 40, "white", "black"),
        label_d = paste0(round(d), "%")
    ) %>%
    ggplot(aes(x = parent_difference, y = litter_size, fill = d)) +
    geom_tile() +
    coord_fixed() +
    ylab("Litter Size") +
    xlab("Parents Age Difference") +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1, name = "Frequency [%]") +
    ggplot2::scale_color_identity() +
    ggtext::geom_richtext(
        aes(label = label_d, color = label_color),
        size = 8,
        fill = NA, label.color = NA, # remove background and outline
    ) +
    theme_classic() +
    theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = ggplot2::element_text(size = 15, hjust = 6, color = "black"),
        axis.text.x = ggplot2::element_text(size = 15, vjust = 6, color = "black"),
        axis.title.y = ggplot2::element_text(size = 15, color = "grey30"),
        axis.title.x = ggplot2::element_text(size = 15, vjust = 6, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
    )
fSaveImages("2021-08-24-age_diff", p_age_diff, w = 6, h = 6)

# Ratio Litter Size and Females and Males
p_ratio <- birth_data %>%
    filter(sex != "ND") %>%
    mutate(
        sex = ifelse(sex == "M", "Male", "Female")
    ) %>%
    add_count(litter_size) %>%
    group_by(litter_size, sex) %>%
    summarise(
        nn = n(),
        percent = nn / n[[1]] * 100,
        percent_label = paste0(round(percent), "%")
    ) %>%
    glimpse() %>%
    ggplot(aes(x = as.factor(litter_size), y = percent, fill = sex)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent_label), position = position_stack(vjust = 0.5)) +
    ggplot2::scale_fill_manual("Sex", values = c("#56B4E9", "#CC79A7"), aesthetics = "fill") +
    xlab("Litter Size") +
    ylab("") +
    theme_classic() +
    theme(
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = ggplot2::element_text(size = 15, vjust = 5, color = "black"),
        axis.title.x = ggplot2::element_text(size = 15, vjust = 6, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
    )

fSaveImages("2021-08-24-pratio", p_ratio, w = 5, h = 5)

# Hate Tables inside plots :(
tt <- gridExtra::ttheme_minimal(
    core = list(fg_params = list(hjust = 1, x = 1)),
    colhead = list(fg_params = list(hjust = 1, x = 1))
)
t1 <- gridExtra::tableGrob(top_males, theme = tt, rows = NULL, cols = c("Sire", "Offsprings"))
t2 <- gridExtra::tableGrob(top_females, theme = tt, rows = NULL, cols = c("Dam", "Offsprings"))
t3 <- gridExtra::tableGrob(top_same_partners[c("name", "successfull_mating")], theme = tt, rows = NULL, cols = c("Dam and Sire", "Matings"))
t1 <- gtable::gtable_add_grob(t1,
    grobs = grid::rectGrob(gp = grid::gpar(fill = "#56B4E9", lwd = 2, alpha = 0.3)),
    t = 2, b = nrow(t1), l = 1, r = ncol(t1)
)
t2 <- gtable::gtable_add_grob(t2,
    grobs = grid::rectGrob(gp = grid::gpar(fill = "#CC79A7", lwd = 2, alpha = 0.3)),
    t = 2, b = nrow(t2), l = 1, r = ncol(t2)
)
t3 <- gtable::gtable_add_grob(t3,
    grobs = grid::rectGrob(gp = grid::gpar(fill = "#E69F00", lwd = 2, alpha = 0.3)),
    t = 2, b = nrow(t3), l = 1, r = ncol(t3)
)
haligned <- gridExtra::gtable_combine(t1, t2, along = 1)


# Final Plot assembly
p <- (p_age_diff - p_ratio / (patchwork::wrap_elements(panel = haligned) / patchwork::wrap_elements(panel = t3))) +
    plot_annotation(
        title = "Lemurs litter distribution and mating stats",
        subtitle = "(A) Looking at how litter size depends on age difference between parents. (B) Female to Male ratio for the given litter sizes.\n(C) Top five most offsprings for dam and sire. (D) Partners with most mating successes.",
        caption = "Source: The Duke Lemur Center, cleaned by  Jesse Mostipak | Graphics by: Hannes Oberreiter | #TidyTuesday ",
        tag_levels = "A",
        theme = theme(
            text = element_text(family = "custom"),
            plot.subtitle = element_text(
                color = "grey30", size = 12,
                lineheight = 1.2,
                hjust = 0,
                margin = margin(t = 5, b = 0)
            ),
            plot.title = element_text(
                color = "grey10", size = 20, face = "bold",
                hjust = 0,
                margin = margin(t = 0)
            ),
            plot.caption = element_text(
                color = "grey30", size = 8,
                lineheight = 1.2, hjust = 1,
                margin = margin(t = 10, b = 0)
            ),
        )
    )

fSaveImages("2021-08-24", p, w = 12, h = 9)