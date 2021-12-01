source("partials/setup.R")
source("functions/functions.R")

# libary for json
# install.packages("tidyjson")
# require(tidyjson)
require(jsonlite)
library(rvest)
library(broom)

data_list <- list()

# API awaits ids from 1961:2019
years <- c(1961:2019)

# Starting by zero
for (i in seq_along(years) - 1) {
    url <- glue::glue("https://bee-hub.eu/api/v1/data?descriptor=livestock-beehives&year={i}&resolution=0")
    # fetch json
    df <- jsonlite::fromJSON(url)
    # extract data nesting of interest
    data_raw <- enframe(unlist(df$data$popup)) %>% glimpse()
    data_clean <- data_raw %>%
        rowwise() %>%
        mutate(
            year = read_html(value) %>% html_elements("p") %>% html_text() %>% nth(1),
            country = read_html(value) %>% html_element("h5") %>% html_text(),
            colonies = read_html(value) %>% html_elements("p") %>% html_text() %>% nth(2)
        ) %>%
        select(year, country, colonies) %>%
        mutate(
            year = stringr::str_extract(year, "\\d+") %>% as.integer(),
            colonies = stringr::str_remove_all(colonies, ",") %>% stringr::str_extract("\\d+") %>% as.integer()
        )
    data_list[[i + 1]] <- data_clean
}

# Save our List
# saveRDS(data_list, "ColonieNumbers.rds")
# x <- readRDS("ColonieNumbers.rds")

# to tibble
df <- bind_rows(data_list)

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



library(lubridate)
library(tidyverse)

df <- data.frame(
    date = as.Date(c("2021-04-19", "2021-04-20", "2021-04-21", "2021-04-22", "2021-04-23", "2021-04-24", "2021-04-26")),
    arrival_time = ymd_hms(c(
        "2021-04-19 19:03:00", "2021-04-20 19:50:00", "2021-04-21 20:04:00", "2021-04-22 20:52:00", "2021-04-23 21:06:00",
        "2021-04-24 21:22:00", "2021-04-27 01:47:00"
    )),
    departure_time = ymd_hms(c(
        "2021-04-20 06:00:00", "2021-04-21 05:47:00", "2021-04-22 06:23:00", "2021-04-23 05:56:00",
        "2021-04-24 04:59:00", "2021-04-25 06:32:00", "2021-04-27 06:40:00"
    ))
)

df %>%
    mutate(
        # Extract hms to plot all on same day, conver tto datetime otherwise we need to use scale_x_time
        arrival = hms::as_hms(arrival_time) %>% lubridate::as_datetime(),
        # calculate difference between cols and with the result the duration
        diff = lubridate::interval(arrival_time, departure_time),
        dur = lubridate::as.duration(diff),
        # workaround if arrival is after midnight
        arrival = if_else(day(arrival_time) > day(date), arrival + days(1), arrival),
    ) %>%
    glimpse() %>%
    # some descriptive statistics
    group_by() %>%
    mutate(
        m_arrival = mean(arrival),
        m_departure = mean(arrival + dur)
    ) %>%
    ungroup() %>%
    glimpse() %>%
    ggplot(aes(x = arrival, y = date, group = date)) +
    geom_vline(aes(xintercept = m_arrival), color = "red") +
    geom_vline(aes(xintercept = m_departure), color = "red") +
    geom_vline(aes(xintercept = lubridate::as_datetime("1970-01-02 00:00:00")), size = 2, color = "gray") +
    # using point range as I think it looks nicer
    # important xmax is now arrival plus the duration
    geom_pointrange(aes(xmin = arrival, xmax = arrival + dur)) +
    geom_text(aes(label = dur, x = arrival + hours(2)), nudge_y = 0.3) +
    scale_y_date(date_breaks = "1 day", date_labels = "%d %b") +
    scale_x_datetime(
        date_breaks = "1 hour",
        date_labels = "%H"
    ) +
    labs(
        x = "Time at Site",
        y = "Date"
    )



tribble(
    ~group, ~question, ~n_answer,
    1, "more", 55,
    1, "less", 20,
    1, "medium", 17,
    2, "yes", 5,
    2, "no", 10,
    3, "more", 45,
    3, "less", 26,
    3, "medium", 57,
    4, "unknown question", 60,
) %>%
    mutate(
        label = case_when(
            question %in% c("more", "less", "medium") ~ "A",
            question %in% c("yes", "no") ~ "B",
            TRUE ~ "C"
        )
    )

label_df <- tribble(
    ~label, ~question,
    "A", "more",
    "A", "less",
    "A", "medium",
    "B", "yes",
    "B", "no",
)

tribble(
    ~group, ~question, ~n_answer,
    1, "more", 55,
    1, "less", 20,
    1, "medium", 17,
    2, "yes", 5,
    2, "no", 10,
    3, "more", 45,
    3, "less", 26,
    3, "medium", 57,
    4, "unknown question", 60,
) %>%
    left_join(label_df)



tribble(
    ~group, ~question, ~n_answer,
    1, "more", 55,
    1, "less", 20,
    1, "medium", 17,
    2, "yes", 5,
    2, "no", 10,
    3, "more", 45,
    3, "less", 26,
    3, "medium", 57,
    4, "unknown question", 60,
) %>%
    group_by(group) %>%
    mutate(
        # ordering for safety
        alternatives = stringr::str_sort(question) %>% stringr::str_c(., collapse = ",")
    ) %>%
    ungroup() %>%
    mutate(
        # convert to factor and use integer, you could also relabel the factor levels or group by alternatives
        label = forcats::as_factor(alternatives) %>% as.integer(.)
    )


library(Rcpp)
Rcpp::sourceCpp("./playground.cpp")
foundC(letters, c("a", "m", "z"))

mpg$model

library(tidyverse)
library(here)

fp <- "data/AllVAERSDataCSVS/"
files <- dir(fp, pattern = "^[0-9]*VAERSDATA.csv")
data <- files %>%
    map(function(x) read_csv(file.path(fp, x))) %>%
    reduce(rbind)
files <- dir(fp, pattern = "^[0-9]*VAERSVAX.csv")
vax <- files %>%
    map(function(x) read_csv(file.path(fp, x))) %>%
    reduce(rbind)

died <- data %>% filter(DIED == "Y")
died %>%
    left_join(vax) %>%
    mutate(
        year_vax = lubridate::as_date(RECVDATE, format = "%d/%m/%Y") %>% lubridate::year(),
        covid_vax = if_else(VAX_TYPE == "COVID19", "COVID19", "OTHER_VAX")
    ) %>%
    count(year_vax, covid_vax) %>%
    ggplot(aes(x = year_vax, y = n, fill = covid_vax)) +
    xlab("Year") +
    ylab("Vaccine recipient died [#]") +
    scale_fill_discrete(name = "Vaccination Type") +
    geom_col() +
    theme_bw()

?strptime

library(tidyverse)
library(tidytext)
library(jsonlite)
library(fuzzyjoin)
# Comments
request <- httr::GET(
    "https://www.reddit.com/r/Austria/comments/p2uhuq/wie_viel_verdient_ihr_so.json",
    httr::add_headers(`User-agent` = "hoberreiter@gmail.com")
)
response <- httr::content(request, as = "text", encoding = "UTF-8")
df <- fromJSON(response, flatten = TRUE) %>%
    tibble()
df <- df$data.children[[2]]
# Jobs
request <- httr::GET(
    "https://www.xing.com/campus/api/v1/job_titles?fields=name%2Cslug&locale=de&per_page=1000&sort=slug",
    httr::add_headers(`User-agent` = "hoberreiter@gmail.com")
)
(response <- httr::content(request, as = "text", encoding = "UTF-8"))
jobs <- fromJSON(response, flatten = TRUE) %>%
    tibble()
# cleanup
jobs <- jobs[2, ] %>% unnest(cols = c(.))

fuzzy_join()
df %>%
    select(data.body) %>%
    drop_na() %>%
    tidytext::unnest_tokens(word, `data.body`) %>%
    glimpse() %>%
    fuzzyjoin::stringdist_left_join(jobs, by = c("word" = "name"), max_dist = 1) %>%
    drop_na(name)


??stringdist

# Calculates Coordinates for plotting
# of a network in circle
fCircleCoordinates <- function(names) {
    source("fCircleCoordinates.R", local = TRUE)
}

# Simple Testcase
typeof(fCircleCoordinates(tibble())) == "list"
nrow(fCircleCoordinates(tibble("test"))) == 1



mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)
x <- runif(1e5)
bench::mark(
    mean1(x),
    mean2(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]


f <- function() {
    profvis::pause(0.1)
    g()
    h()
}
g <- function() {
    profvis::pause(0.1)
    h()
}
h <- function() {
    profvis::pause(0.1)
}

profvis::profvis(f())

gctorture2(0)
f <- function() {
    return(NULL)
}
profvis::profvis(f(), torture = 10)

f <- function(n = 1e5) {
    x <- rep(1, n)
    rm(x)
}
profvis::profvis(f())
profvis::profvis(f(), torture = 10)



library(tidyverse)
x <- data.frame(letter_col = c("a", "a", "b", "b"))

f <- function(x, which_to_keep = "All") {
    x %>% filter(
        if (which_to_keep == "All") TRUE else letter_col == which_to_keep
    )
}

f(x, "All")
f(x, "a")
f(x, "Not present")

which_to_keep <- "a"
x <- if (which_to_keep == "All") .

x %>%
    filter(
        # the infamous dot (.) would also work in this example as TRUE condition
        letter_col == if (which_to_keep == "All") letter_col else which_to_keep
    )

library(tidyverse)
x <- data.frame(letter_col = c("a", "a", "b", "b"))

f <- function(x, which_to_keep = "All") {
    x %>% filter(
        if (which_to_keep == "All") TRUE else letter_col == which_to_keep
    )
}

f(x, "All")
f(x, "a")
f(x, "Not present")


f2 <- function(x, which_to_keep = "All") {
    x %>% filter(
        # the infamous dot (.) would also work as TRUE condition
        letter_col == if (which_to_keep == "All") letter_col else which_to_keep
    )
}

f3 <- function(x, which_to_keep = "All") {
    x %>% filter(
        letter_col == which_to_keep | which_to_keep == "All"
    )
}

f2(x, "All")
f2(x, "b")
f3(x, "All")
f3(x, "b")