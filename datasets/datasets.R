library(dplyr)
library(tidyr)
library(tibble)
library(readxl)

# 1.1 ----
# Wide
one_one_w <- read_xlsx("./datasets/one_one.xlsx")
one_one_w <- as_tibble(one_one_w)

one_one_l <- pivot_longer(one_one_w, 2:5, names_to = "Crime", values_to = "Value")

# 3_3_3_4 ----
# Wide
three_five_w <- read_xlsx("./datasets/three_five.xlsx")
three_five_w <- as_tibble(three_five_w) 

# Long
three_five_l <- pivot_longer(three_five_w, 2:15, names_to = "Age", values_to = "Value")
three_five_l <- three_five_l %>% mutate(Gender = Age)
three_five_l$Gender <- gsub("_.*", "", three_five_l$Gender)  
three_five_l$Age <- gsub(".*_", "", three_five_l$Age)

# Save ----
save(one_one_w, one_one_l, three_five_w, three_five_l, file = "./app/datasets.rda")