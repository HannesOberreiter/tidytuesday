source("partials/setup.R")
source("functions/functions.R")

day <- "06"

data <- readr::read_lines(glue("aoc/2022/{day}.txt"))

stream <- str_split(data, "", simplify = TRUE)
buffer <- c(stream[1])
buffer_size <- 14 # 4 part 1
for(i in 1:length(stream)){
  current <- stream[i]
  buffer <- append(buffer[2:buffer_size], current)
  if(length(buffer[!is.na(buffer)]) >= buffer_size){
    if(length(unique(buffer)) == buffer_size){
      print(i)
      break
    }
  }
}
