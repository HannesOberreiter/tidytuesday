source("partials/setup.R")
source("functions/functions.R")

# Part 1
fHelper <- function(x, t = "min") {
    if (t == "min") {
        r <- names(table(x))[which.min(table(x))]
    } else {
        r <- names(table(x))[which.max(table(x))]
    }
    as.integer(r)
}
toBinary <- function(x) {
    as.character(x) %>%
        paste0(collapse = "") %>%
        strtoi(base = 2)
}
data <- readr::read_lines("aoc/2021/03/data.csv") %>%
    as_tibble() %>%
    separate(value, sep = "", into = c(as.character(1:13)), convert = TRUE) %>%
    select_if(function(x) any(!is.na(x)))

gamma <- data %>%
    summarise(
        across(everything(), ~ fHelper(.x, "max")),
    ) %>%
    toBinary()

epsilon <- data %>%
    summarise(
        across(everything(), ~ fHelper(.x)),
    ) %>%
    toBinary()

gamma * epsilon

# Part 2
fHelper2 <- function(x, t = "min") {
    zero <- sum(x == 0)
    one <- sum(x == 1)
    if (t == "min") {
        binary <- 0
        if (one < zero) {
            binary <- 1
        }
    } else {
        binary <- 1
        if (zero > one) {
            binary <- 0
        }
    }
    binary
}

fRec <- function(x, pos = 1, t = "max") {
    if (nrow(x) == 1) {
        return(x)
    }
    r <- x[, pos] %>%
        unlist() %>%
        fHelper2(., t = t)
    fRec(x[x[, pos] == r, ], pos + 1, t = t)
}

oxygen <- fRec(data, t = "max") %>%
    toBinary()

co2 <- fRec(data, t = "min") %>%
    toBinary()


oxygen * co2