source("partials/setup.R")
source("functions/functions.R")

day <- "05"

data <- readr::read_lines(glue("aoc/2022/{day}.txt"))

breakpoint <- FALSE
stacks <- list()
commands <- list()
for(i in 1:length(data)){
  if(breakpoint){
    commands <- append(commands, data[[i]])
  } else {
    stacks[i] <- data[[i]]
  }
  if(data[[i]] == "") breakpoint <- TRUE
}

number_of_stacks <- stacks[length(stacks)-1] %>% str_split("  ", simplify = TRUE) %>%  as.integer()
rev_stack <- stacks[1:(length(stacks)-2)] %>% rev()
ordered <- list()
for(i in 1:length(rev_stack)){
  stack <- rev_stack[[i]]
  stack <- stack %>% str_extract_all("(    |\\[.\\])", simplify = TRUE)
  for(j in number_of_stacks){
    if(i == 1){
      ordered[j] <-  stack[j]
    } else {
      ordered[[j]] <-  c(ordered[[j]], stack[j])
    }
  }
}
ordered <- lapply(ordered, function(x) c(" ", x[x != "   "]))
result <- ordered

for(i in 1:length(commands)){
  split_commands <- commands[[i]] %>% str_split("(move | from | to )", simplify = TRUE)
  amount <- split_commands[1,2] %>% as.integer()
  from <- split_commands[1,3] %>% as.integer()
  to <- split_commands[1,4] %>% as.integer()
  
  from_vector <- result[[from]]
  take_vector <- from_vector[(length(from_vector)-(amount-1)):length(from_vector)]
  print(take_vector)
  print(from_vector[1:(length(from_vector)-length(take_vector))])
  # Part 1
  #result[[to]] <- c(result[[to]], rev(take_vector))
  # Part 2
  result[[to]] <- c(result[[to]], take_vector)
  result[[from]] <- from_vector[1:(length(from_vector)-length(take_vector))]
}

r <- sapply(result, function(x){
  x[length(x)] %>%  stringr::str_remove_all("[\\[|\\]]")
})
stringr::str_flatten(r)

# QMJMJDFTV