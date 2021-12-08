source("partials/setup.R")
source("functions/functions.R")

# Load File
data <- read_lines("aoc/2021/08/test.csv") %>%
    as_tibble() %>%
    separate(value, into = c("input", "result"), sep = " [|] ")

# Part 1
# Only interested in results column
results <- data %>% pull(result)
known <- tibble(
    num = c(1L, 4L, 7L, 8L),
    dig = c(2L, 4L, 3L, 7L)
)
digits <- results %>%
    stringr::str_split(pattern = " ", simplify = TRUE) %>%
    stringr::str_length()
sum(digits %in% known$dig)

# Part 2
# Sorting helper
# https://stackoverflow.com/questions/5904797/how-to-sort-letters-in-a-string
fSorter <- function(x) vapply(x, function(xi) paste(sort(strsplit(xi, NULL)[[1]]), collapse = ""), "")


# Some Dirty Global Helper
# so I dont leave tidy pipe :)
known_named <- as.character()
fNamer <- function(x) {
    known_named <<- x$input
    names(known_named) <<- x$num
    x
}
helper <- as.character()
fHelper <- function(x) {
    helper <<- known_named["8"] %>%
        stringr::str_remove_all(glue::glue("[{known_named['3']}]")) %>%
        stringr::str_remove_all(glue::glue("[{known_named['4']}]"))
    x
}

final_solution <- 0
for (i in 1:nrow(data)) {
    results <- data[i, ] %>%
        pull(result) %>%
        str_split(pattern = " ", simplify = TRUE) %>%
        as.character() %>%
        fSorter(.)
    inputs <- data[i, ] %>%
        pull(input) %>%
        str_split(pattern = " ", simplify = TRUE) %>%
        as.character() %>%
        fSorter(.)

    ws <- tibble(
        input = inputs,
        length = stringr::str_length(input)
    ) %>%
        left_join(known, by = c("length" = "dig")) %>%
        fNamer() %>%
        mutate(
            num = case_when(
                !is.na(num) ~ as.integer(num),
                length == 5 & stringr::str_count(input, glue::glue('[{known_named["1"]}]')) == 2 ~ 3L,
                TRUE ~ NA_integer_
            )
        ) %>%
        fNamer() %>%
        fHelper() %>%
        mutate(
            num = case_when(
                !is.na(num) ~ as.integer(num),
                length == 5 & stringr::str_count(input, glue::glue("[{helper}]")) == 0 ~ 5L,
                length == 5 & stringr::str_count(input, glue::glue("[{helper}]")) == 1 ~ 2L,
                length == 6 & stringr::str_count(input, glue::glue('[{known_named["3"]}]')) == 5 ~ 9L,
                TRUE ~ NA_integer_
            )
        ) %>%
        fNamer() %>%
        mutate(
            num = case_when(
                !is.na(num) ~ as.integer(num),
                length == 6 & stringr::str_count(input, glue::glue('[{known_named["5"]}{helper}]')) == 6 ~ 6L,
                TRUE ~ 0L
            )
        )

    results_solution <- tibble(
        results = results
    ) %>%
        left_join(ws, by = c("results" = "input")) %>%
        pull(num) %>%
        as.character() %>%
        stringr::str_c(collapse = "") %>%
        as.numeric()
    final_solution <- final_solution + results_solution
}
final_solution
