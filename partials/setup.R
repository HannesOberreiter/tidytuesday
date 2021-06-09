# Description -------------------------------------------------------------
# Loading of Libraries
print("Loading Setup")

# Libraries ---------------------------------------------------------------
libs <- c(
    "tidyverse", "here", "glue", "patchwork", "rlang", "knitr", "lintr", "scales",
    "tidytuesdayR"
)

# Load Libraries with function, install binary if not installed (mac binaries are defined!)
# x = Libraries name as String
fLoadLibs <- function(x) {
    if (!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary")
    library(x, character.only = TRUE)
    return(T)
}

sapply(libs, fLoadLibs)
rm(libs, fLoadLibs)

colorBlindBlack8 <- c(
    "#464343", "#E69F00", "#56B4E9", "#009E73",
    "#CC79A7", "#F0E442", "#0072B2", "#D55E00"
)
