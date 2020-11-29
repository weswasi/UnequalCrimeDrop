library(dplyr)
library(tidyr)
library(readxl)

# 1.1 ----
# Wide
one_one_w <- read_xlsx("./datasets/one_one.xlsx")

one_one_l <- one_one_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value")

# 1.2 ----
# Wide
one_two_w <- read_xlsx("./datasets/one_two.xlsx")

#Long
one_two_l <- one_two_w %>% 
  pivot_longer(!Year, names_to = "Source", values_to = "Value")

# 1.3 ----
# Wide
one_three_w <- read_xlsx("./datasets/one_three.xlsx")

#Long
one_three_l <- one_three_w %>% 
  pivot_longer(!Year, names_to = "Prosecutions", values_to = "Value")

# 1.4
# Wide
one_four_w <- read_xlsx("./datasets/one_four.xlsx")

# Long
one_four_l <- one_four_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value")

#3.2 ----
# Wide
three_two_w <- read_xlsx("./datasets/three_two.xlsx")

# Long
three_two_l <- three_two_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value") %>% 
  mutate(Gender = Crime) %>% 
  select(Year, Gender, Crime, Value)


three_two_l$Gender <- gsub("-.*", "", three_two_l$Gender)  
three_two_l$Crime <- gsub(".*-", "", three_two_l$Crime)

# 3_3_3_4 ----
# Wide
three_three_four_w <- read_xlsx("./datasets/three_three_four.xlsx")

# Long
three_three_four_l <- three_three_four_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Gender = Age) %>% 
  select(Year, Gender, Age, Value)

three_three_four_l$Gender <- gsub(" .*", "", three_three_four_l$Gender)  
three_three_four_l$Age <- gsub(".* ", "", three_three_four_l$Age)

# 3_1 ----
# Wide
three_one_w <- read_xlsx("./datasets/three_one.xlsx")

# Long
three_one_l <- three_one_w %>% 
  pivot_longer(!Year, names_to = "Gender", values_to = "Value")

# 3_5_ab
# Wide
three_five_ad_w <- read_xlsx("./datasets/three_five_ad.xlsx")

# Long
three_five_ad_l <- three_five_ad_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Gender = Age) %>%
  select(Year, Gender, Age, Value)

three_five_ad_l$Gender <- gsub(" .*", "", three_five_ad_l$Gender)  
three_five_ad_l$Age <- gsub(".* ", "", three_five_ad_l$Age)

# 3_5_be
# Wide
three_five_be_w <- read_xlsx("./datasets/three_five_be.xlsx")

# Long
three_five_be_l <- three_five_be_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Gender = Age) %>%
  select(Year, Gender, Age, Value)

three_five_be_l$Gender <- gsub(" .*", "", three_five_be_l$Gender)  
three_five_be_l$Age <- gsub(".* ", "", three_five_be_l$Age)

# 3_5_be
# Wide
three_five_cf_w <- read_xlsx("./datasets/three_five_cf.xlsx")

# Long
three_five_cf_l <- three_five_cf_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Gender = Age) %>%
  select(Year, Gender, Age, Value)

three_five_cf_l$Gender <- gsub(" .*", "", three_five_cf_l$Gender)  
three_five_cf_l$Age <- gsub(".* ", "", three_five_cf_l$Age)

# Save ----
save(one_one_w, 
     one_one_l, 
     one_two_w,
     one_two_l,
     one_three_w,
     one_three_l,
     one_four_w,
     one_four_l,
     three_one_w,
     three_one_l,
     three_two_w,
     three_two_l,
     three_three_four_w, 
     three_three_four_l, 
     three_five_ad_w,
     three_five_ad_l,
     three_five_be_w,
     three_five_be_l,
     three_five_cf_w,
     three_five_cf_l,
     file = "./app/datasets.rda")