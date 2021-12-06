source("partials/setup.R")
source("functions/functions.R")

data <- readr::read_lines("aoc/2021/06/data.csv") %>%
    str_split(",", simplify = TRUE) %>%
    as.integer()

# Part 1
history <- list()
current <- data
days <- 80
for (day in 1:days) {
    new <- rep(8, length(current[current == 0]))
    current <- current - 1
    current[current == -1] <- 0
    current <- c(current, new)
    history[[day]] <- current
    current <- history[[day]]
}
length(history[[days]])

# Part 2
fish <- as.numeric(rep(0, 9))
names(fish) <- 0:8
fish[names(table(data))] <- table(data)
days <- 256
for (day in 1:days) {
    spawn <- fish[1]
    fish[1:8] <- fish[2:9]
    fish[9] <- spawn
    fish[7] <- fish[7] + spawn
}
format(sum(fish), scientific = FALSE)