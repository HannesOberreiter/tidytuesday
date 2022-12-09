source("partials/setup.R")
source("functions/functions.R")

day <- "07"
data <- readr::read_lines(glue("aoc/2022/{day}.txt"))

v <- c('root')
for(i in 1:length(data)){
  command <- data[i]
  if(str_detect(command, '^(dir) ')){
    v <- append(v, command)
  }
}

files <- FALSE
path <- c('root')
result <- list('root' = 0)
for(i in 1:length(data)){
  command <- data[i]

  if(str_detect(command, '(\\$ cd)')){
    files <- FALSE
    change <- str_split(command, " ", simplify = TRUE)
    if(change[3] == '..'){
      path <- path[1:(length(path)-1)]
    } else {
      pname <- str_c(change[3], i, sep = "_")
      path <- append(path, pname)
      result[[pname]] <- 0
    }
  }
  
  if(files){
    if(!str_detect(command, '^(dir)')){
      print(command)
      size <- str_extract_all(command, "\\d+", simplify = TRUE) |>  as.integer()
      for(j in 1:length(path)){
        result[[path[j]]] <- result[[path[j]]] + size
      }
    }
  }
  if(command == "$ ls"){
    files <- TRUE
  }
}
# Remove Root and /
result_w <- result[3:length(result)]

part1 <- sapply(result_w, function(x) x <= 100000) 
reduce(result_w[part1], sum)

# part 2
disk_size <- 70000000
update <- 30000000
free_space <- disk_size - result[[1]]
needed <- update - free_space
part2 <- sapply(result_w, function(x) x >= needed)
sort(unlist(result_w[part2]))
