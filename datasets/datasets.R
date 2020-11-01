library(dplyr)
library(tidyr)
library(tibble)
library(readxl)

# 1.1 ----
one_one_w <- read_xlsx("one_one.xlsx")
one_one_w <- as_tibble(one_one_w)
one_one_l <- pivot_longer(one_one_w, 2:5, names_to = "Crime", values_to = "Value")
save(one_one_w, one_one_l, file = "./app/datasets.rda")

# 1.2 ----