library(dplyr)
library(tidyr)
library(tibble)
library(readxl)

one_one <- read_xlsx("one_one.xlsx")
one_one <- as_tibble(one_one)
one_one <- pivot_longer(one_one, 2:5, names_to = "Crime", values_to = "Values")
saveRDS(one_one, file = "one_one.RDS")

