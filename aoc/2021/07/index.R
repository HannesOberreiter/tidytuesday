source("partials/setup.R")
source("functions/functions.R")

# Load File
data <- scan("aoc/2021/07/data.csv", what = integer(), sep = ",")
## Create full length Vector to compare
full_range <- c(min(data):max(data))
names(full_range) <- full_range

## Lowest sum is closed number
fClosest <- function(range, data) {
    sum(abs(data - range))
}

# Part 1
map_int(full_range, fClosest, data = data) %>%
    sort() %>%
    head()

# Part 2
fClosest2 <- function(range, data) {
    y <- abs(data - range)
    sum(map_int(y, ~ sum(c(0:.x))))
}
map_int(full_range, fClosest2, data = data) %>%
    sort() %>%
    head()