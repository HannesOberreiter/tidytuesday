source("partials/setup.R")
source("functions/functions.R")

require(jsonlite)
library(rvest)
library(broom)

# Rather slow api, be friendly and only fetch if data is old or not available inside data folder
# returns raw json result
fetchData <- function(descriptor = "livestock-beehives",
                      years = c(1961:2019)) {
    temp <- list()
    for (i in seq_along(years) - 1) {
        url <- glue::glue("https://bee-hub.eu/api/v1/data?descriptor={descriptor}&year={i}&resolution=0")
        print(url)
        df <- jsonlite::fromJSON(url)
        temp[[i + 1]] <- df
    }
    return(temp)
}

# Extract nested Popup from Json
# returns a tibble
scrapePopup <- function(df) {
    map(df, ~ enframe(unlist(.x$data$popup))) %>%
        bind_rows() %>%
        select(value)
}

# Small helper to reduce code redudancy
extractHelper <- function(input, selector = "p") {
    rvest::read_html(input) %>% rvest::html_elements(selector)
}

# Using the HTML Popup we extract the important Data for us
extractData <- function(df) {
    df %>%
        rowwise() %>%
        mutate(
            year = extractHelper(value) %>% html_text() %>% nth(1),
            country = extractHelper(value, "h5") %>% html_text() %>% nth(1),
            colonies = extractHelper(value) %>% html_text() %>% nth(2),
            # Owner
            owner = extractHelper(value, ".popup-owners") %>% html_children() %>% html_attr("title") %>% list(),
            owner_href = extractHelper(value, ".popup-owners") %>% html_children() %>% html_attr("href") %>% list()
        ) %>%
        mutate(
            year = stringr::str_extract(year, "\\d+") %>% as.integer(),
            colonies = stringr::str_remove_all(colonies, ",") %>% stringr::str_extract("\\d+") %>% as.integer()
        ) %>%
        as_tibble()
}

df <- fetchData(descriptor = "livestock-beehives", years = c(1961:1962))
df_popup <- scrapePopup(df) %>% glimpse()

extractData(df_popup) %>% glimpse()

# Save our List
# saveRDS(data_list, "data/ColonyNumbers_2017_07_15.rds")
# x <- readRDS("data/ColonyNumbers_2017_07_15.rds")

# Some Cleanup
data <- df %>%
    # Nederland has zero colonies after 1987
    filter(colonies != 0) %>%
    # Sverige 1988 seems to be wrong
    filter(!(country == "Sverige" & year == 1988)) %>%
    # Austria is double
    distinct(year, country, colonies) %>%
    as_tibble()


data %>%
    group_by(country) %>%
    mutate(
        from_first = first(colonies) / colonies
    ) %>%
    ungroup() %>%
    ggplot(aes(year, from_first, color = country)) +
    # geom_line() +
    geom_point() +
    stat_smooth(aes(group = 1)) +
    guides() +
    ggplot2::scale_x_continuous(
        minor_breaks = years,
    ) +
    ggplot2::scale_y_continuous(
        breaks = seq(0, 5, 0.25),
        labels = scales::label_percent()
    ) +
    theme_light() +
    theme(legend.position = "bottom") -> p


fSaveImages("Playing", p, w = 10, h = 10)

# Linear Model

lm_data <- data %>%
    group_by(country) %>%
    nest() %>%
    mutate(
        lm = map(data, ~ lm(year ~ colonies, data = .)),
        glance = map(lm, glance)
    ) %>%
    unnest(glance) %>%
    unnest(data) %>%
    glimpse()

unique(lm_data$country)

data %>%
    filter(country == "Ελλαδα") %>%
    ggplot(aes(year, colonies)) +
    geom_point()

lm_data %>%
    filter(adj.r.squared > 0.5)

lm_data %>%
    group_by(country) %>%
    mutate(
        from_first = first(colonies) / colonies,
        color = ifelse(adj.r.squared > 0.5, "R^2 > 0.5", "R^2 <= 0.5")
    ) %>%
    ungroup() %>%
    ggplot(aes(year, colonies, color = color, shape = country, group = country)) +
    geom_line() +
    geom_point() +
    ggplot2::scale_shape_manual(
        values = 1:length(unique(lm_data$country))
    ) +
    # ggplot2::scale_color_identity("R Squared") +
    ggplot2::scale_x_continuous(
        minor_breaks = years,
    ) +
    ggplot2::scale_y_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
    ) +
    theme_light() +
    theme(legend.position = "bottom") -> p

fSaveImages("Playing", p, w = 10, h = 10)



# Winter Mortality
# Save our List
# saveRDS(dfLoss, "data/ColonyLosses_2017_07_15.rds")
# dfLoss <- readRDS("data/ColonyLosses_2017_07_15.rds")

dfLoss <- fetchData(descriptor = "winter-mortality-aggregate", years = c(1:10))
dfLossPop <- scrapePopup(dfLoss)

dfLossBub <- extractData(dfLossPop) %>% glimpse()
dfLossBub <- dfLossBub %>%
    rowwise() %>%
    mutate(
        year = value %>% extractHelper() %>% html_text() %>% nth(4),
        year = stringr::str_extract(year, "\\d+") %>% as.integer(),
    ) %>%
    as_tibble()


dfLossTooltip <- map(dfLoss, ~ enframe(unlist(.x$data$tooltip))) %>%
    bind_rows()

dfLossTooltip <- dfLossTooltip %>%
    mutate(
        loss_rate = stringr::str_extract(value, "[0-9,\\.]+") %>% as.double(),
        country = stringr::str_extract(value, "^(.*?):") %>% str_remove_all(":"),
    )

df_comb <- bind_cols(dfLossTooltip %>% select(loss_rate, country), dfLossBub %>% select(year, owner, owner_href))

df_comb %>%
    mutate(
        owner_c = map_chr(owner, paste0, collapse = ",")
    ) %>%
    ggplot(aes(year, loss_rate, fill = country)) +
    geom_bar(stat = "identity", position = position_dodge())

df_comb %>%
    unnest(owner) %>%
    pull(owner) %>%
    unique()


fao <- readr::read_csv("data/fao.csv")
fao %>% glimpse()

fao %>%
    filter(Item == "Beehives", Area == "Austria") %>%
    left_join(

        data %>%
            filter(country == "Österreich"),
        by = c("Year" = "year")
    ) %>%
    filter(Value != colonies) %>%
    select(Year, Area, FAO = Value, EFSA = colonies) %>%
    mutate(
        Difference = FAO - EFSA
    )


fao %>%
    filter(Item == "Beehives", Area == "Austria")


fao <- readr::read_csv("data/fao.csv")


html <- function(x) structure(x, class = "advr_html")
print.advr_html <- function(x, ...) {
    out <- paste0("<HTML> ", x)
    cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}
html(c("1", "2"))
list2

# The main difference with list(...) is that list2(...) enables
# the `!!!` syntax to splice lists:
x <- list(2, 3)
numeric(1, !!!x, 4)

# As well as unquoting of names:
nm <- "yup!"
numeric(!!nm := 1)

library(rlang)
x <- function() {
    new_function(NULL, NULL, caller_env())
}
y <- function() {
    new_function(NULL, NULL)
}
a <- x()
b <- y()
env_print(a)
env_print(b)


x <- function() {
    c <- 2
    new_function(NULL, expr(print(c)), caller_env())
}
y <- function() {
    c <- 2
    new_function(NULL, expr(print(c)))
}
a <- x()
b <- y()
c <- 3
a()
b()

f <- function(x) x + 3
g <- new_function(alist(x = ), quote(x + 3))

# The components of the functions are identical
identical(formals(f), formals(g))
identical(body(f), body(g))
identical(environment(f), environment(g))

environment(a)
environment(b)

# But the functions are not identical because f has src code reference
identical(f, g)

attr(f, "srcref") <- NULL
# Now they are:
stopifnot(identical(f, g))