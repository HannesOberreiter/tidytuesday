source("partials/setup.R")

numbers_data <- readr::read_lines("aoc/2021/04/data.csv", n_max = 1)
boards_data <- readr::read_lines("aoc/2021/04/data.csv", skip = 2)
# Tidy Data
numbers <- numbers_data %>%
    str_split(",", simplify = FALSE) %>%
    unlist() %>%
    as.numeric()
boards_data <- boards_data %>%
    str_split(" ") %>%
    unlist() %>%
    as.numeric()
boards_data <- boards_data[!is.na(boards_data)]
# Create Boards
boards <- list()
boards_dummy <- list()
b <- 1
e <- 25
for (i in 1:(length(boards_data) / 25)) {
    boards[[i]] <- matrix(boards_data[b:e], nrow = 5, ncol = 5)
    boards_dummy[[i]] <- matrix(FALSE, nrow = 5, ncol = 5)
    b <- e + 1
    e <- e + 25
}
# Part 1
# Start looping logic
breaking <- FALSE
for (i in 1:length(numbers)) {
    number <- numbers[i]
    # loop over boards
    for (j in 1:length(boards)) {
        # find in matrix
        found <- which(boards[[j]] == number, arr.ind = TRUE)
        # update dummy
        boards_dummy[[j]][found] <- TRUE
        if (5 %in% rowSums(boards_dummy[[j]])) {
            print(glue::glue("Board {j} is the winner, last number {number}"))
            print(sum(boards[[j]][!boards_dummy[[j]]]) * number)
            breaking <- TRUE
            break
        }
        if (5 %in% colSums(boards_dummy[[j]])) {
            print(glue::glue("Board {j} is the winner, last number {number}"))
            print(sum(boards[[j]][!boards_dummy[[j]]]) * number)
            breaking <- TRUE
            break
        }
    }
    if (breaking) {
        break
    }
}

# Part 2
# Start looping logic
last_winner_board <- matrix()
last_dummy_board <- matrix()
last_winner_number <- 0L
for (i in 1:length(numbers)) {
    number <- numbers[i]
    # loop over boards
    for (j in 1:length(boards)) {
        # find in matrix
        found <- which(boards[[j]] == number, arr.ind = TRUE)
        # update dummy
        boards_dummy[[j]][found] <- TRUE
        if (5 %in% rowSums(boards_dummy[[j]])) {
            print(glue::glue("Board {j} is the winner, last number {number}"))
            print(sum(boards[[j]][!boards_dummy[[j]]]) * number)
            last_winner_board <- boards[[j]]
            boards[[j]] <- matrix(1000L)
            last_dummy_board <- boards_dummy[[j]]
            boards_dummy[[j]] <- matrix(FALSE)
            last_winner_number <- number
        }
        if (5 %in% colSums(boards_dummy[[j]])) {
            print(glue::glue("Board {j} is the winner, last number {number}"))
            print(sum(boards[[j]][!boards_dummy[[j]]]) * number)
            last_winner_board <- boards[[j]]
            boards[[j]] <- matrix(1000L)
            last_dummy_board <- boards_dummy[[j]]
            boards_dummy[[j]] <- matrix(FALSE)
            last_winner_number <- number
        }
    }
}