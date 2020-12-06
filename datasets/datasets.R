library(dplyr)
library(tidyr)
library(readxl)

# 1_1 ----
# Wide
one_one_w <- read_xlsx("./datasets/one_one.xlsx")

# Long
one_one_l <- one_one_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value")

# 1_2 ----
# Wide
one_two_w <- read_xlsx("./datasets/one_two.xlsx")

# Long
one_two_l <- one_two_w %>% 
  pivot_longer(!Year, names_to = "Source", values_to = "Value")

# 1_3 ----
# Wide
one_three_w <- read_xlsx("./datasets/one_three.xlsx")

# Long
one_three_l <- one_three_w %>% 
  pivot_longer(!Year, names_to = "Prosecutions", values_to = "Value")

# 1_4 ----
# Wide
one_four_w <- read_xlsx("./datasets/one_four.xlsx")

# Long
one_four_l <- one_four_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value")

# 3_1 ----
# Wide
three_one_w <- read_xlsx("./datasets/three_one.xlsx")

# Long
three_one_l <- three_one_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value")

# 3_2 ----
# Wide
three_two_w <- read_xlsx("./datasets/three_two.xlsx")

# Long
three_two_l <- three_two_w %>% 
  pivot_longer(!Year, names_to = "Crime", values_to = "Value") %>% 
  mutate(Sex = Crime) %>% 
  select(Year, Sex, Crime, Value)


three_two_l$Sex <- gsub("-.*", "", three_two_l$Sex)  
three_two_l$Crime <- gsub(".*-", "", three_two_l$Crime)

# 3_3_3_4 ----
# Wide
three_three_four_w <- read_xlsx("./datasets/three_three_four.xlsx")

# Long
three_three_four_l <- three_three_four_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Sex = Age) %>% 
  select(Year, Sex, Age, Value)

three_three_four_l$Sex <- gsub(" .*", "", three_three_four_l$Sex)  
three_three_four_l$Age <- gsub(".* ", "", three_three_four_l$Age)

# 3_5_ab ----
# Wide
three_five_ad_w <- read_xlsx("./datasets/three_five_ad.xlsx")

# Long
three_five_ad_l <- three_five_ad_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Sex = Age) %>%
  select(Year, Sex, Age, Value)

three_five_ad_l$Sex <- gsub(" .*", "", three_five_ad_l$Sex)  
three_five_ad_l$Age <- gsub(".* ", "", three_five_ad_l$Age)

# 3_5_be ----
# Wide
three_five_be_w <- read_xlsx("./datasets/three_five_be.xlsx")

# Long
three_five_be_l <- three_five_be_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Sex = Age) %>%
  select(Year, Sex, Age, Value)

three_five_be_l$Sex <- gsub(" .*", "", three_five_be_l$Sex)  
three_five_be_l$Age <- gsub(".* ", "", three_five_be_l$Age)

# 3_5_cf ----
# Wide
three_five_cf_w <- read_xlsx("./datasets/three_five_cf.xlsx")

# Long
three_five_cf_l <- three_five_cf_w %>% 
  pivot_longer(!Year, names_to = "Age", values_to = "Value") %>% 
  mutate(Sex = Age) %>%
  select(Year, Sex, Age, Value)

three_five_cf_l$Sex <- gsub(" .*", "", three_five_cf_l$Sex)  
three_five_cf_l$Age <- gsub(".* ", "", three_five_cf_l$Age)

# 4_1_4_2 ----
# Wide
four_one_four_two_w <- read_xlsx("./datasets/four_one_four_two.xlsx")

# Long
four_one_four_two_l <- four_one_four_two_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Country = Sex) %>% 
  select(Year, Sex, Country, Value)

four_one_four_two_l$Sex <- gsub("-.*", "", four_one_four_two_l$Sex)   
four_one_four_two_l$Country <- gsub(".*-", "", four_one_four_two_l$Country)

# 4_3_ac ----
# Wide
four_three_ac_w <- read_xlsx("./datasets/four_three_ac.xlsx")

# Long
four_three_ac_l <- four_three_ac_w %>% 
  pivot_longer(!Year, names_to = "Country", values_to = "Value") %>% 
  mutate(Crime = Country) %>% 
  select(Year, Crime, Country, Value)

four_three_ac_l$Country <- gsub("-.*", "", four_three_ac_l$Country)   
four_three_ac_l$Crime <- gsub(".*-", "", four_three_ac_l$Crime)

# 4_3_ac ----
# Wide
four_three_df_w <- read_xlsx("./datasets/four_three_df.xlsx")

# Long
four_three_df_l <- four_three_df_w %>% 
  pivot_longer(!Year, names_to = "Country", values_to = "Value") %>% 
  mutate(Crime = Country) %>% 
  select(Year, Crime, Country, Value)

four_three_df_l$Country <- gsub("-.*", "", four_three_df_l$Country)
four_three_df_l$Crime <- gsub(".*-", "", four_three_df_l$Crime)

# 5_1_ab ----
# Wide
five_one_ab_w <- read_xlsx("./datasets/five_one_ab.xlsx")

five_one_ab_l <- five_one_ab_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Income = Sex) %>% 
  select(Year, Sex, Income, Value)

five_one_ab_l$Sex <- gsub("-.*", "", five_one_ab_l$Sex)
five_one_ab_l$Income <- gsub(".*-", "", five_one_ab_l$Income)

# 5_2_af ----
# Wide
five_two_af_w <- read_xlsx("./datasets/five_two_af.xlsx")

# Long
five_two_af_l <- five_two_af_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Income =  Sex,
         Crime = Sex) %>% 
  select(Year, Sex, Crime, Income, Value)

five_two_af_l$Sex <- gsub("-.*", "", five_two_af_l$Sex)
five_two_af_l$Crime <- gsub(".*-", "", five_two_af_l$Crime)
five_two_af_l$Income <- gsub("^[^-]+-|-.*", "", five_two_af_l$Income)

# 6_1_ab ----
# Wide
six_one_ab_w <- read_xlsx("./datasets/six_one_ab.xlsx")

# Long
six_one_ab_l <- six_one_ab_w %>% 
  pivot_longer(!Age, names_to = "Sex", values_to = "Value") %>% 
  mutate(Birthyear = Sex) %>% 
  select(Age, Sex, Birthyear, Value)

six_one_ab_l$Sex <- gsub("-.*", "", six_one_ab_l$Sex)
six_one_ab_l$Birthyear <- gsub(".*-", "", six_one_ab_l$Birthyear)

# 6_2_ad ----
# Wide
six_two_ad_w <- read_xlsx("./datasets/six_two_ad.xlsx")

# Long
six_two_ad_l <- six_two_ad_w %>% 
  pivot_longer(!Age, names_to = "Sex", values_to = "Value") %>% 
  mutate(Birthyear = Sex) %>% 
  select(Age, Sex, Birthyear, Value)

six_two_ad_l$Sex <- gsub("-.*", "", six_two_ad_l$Sex)
six_two_ad_l$Birthyear <- gsub(".*-", "", six_two_ad_l$Birthyear)

# 6_2_be ----
# Wide
six_two_be_w <- read_xlsx("./datasets/six_two_be.xlsx")

# Long
six_two_be_l <- six_two_be_w %>% 
  pivot_longer(!Age, names_to = "Sex", values_to = "Value") %>% 
  mutate(Birthyear = Sex) %>% 
  select(Age, Sex, Birthyear, Value)

six_two_be_l$Sex <- gsub("-.*", "", six_two_be_l$Sex)
six_two_be_l$Birthyear <- gsub(".*-", "", six_two_be_l$Birthyear)

# 6_2_cf ----
# Wide
six_two_cf_w <- read_xlsx("./datasets/six_two_cf.xlsx")

# Long
six_two_cf_l <- six_two_cf_w %>% 
  pivot_longer(!Age, names_to = "Sex", values_to = "Value") %>% 
  mutate(Birthyear = Sex) %>% 
  select(Age, Sex, Birthyear, Value)

six_two_cf_l$Sex <- gsub("-.*", "", six_two_cf_l$Sex)
six_two_cf_l$Birthyear <- gsub(".*-", "", six_two_cf_l$Birthyear)

# 6_3_ab ----
# Wide
six_three_ab_w <- read_xlsx("./datasets/six_three_ab.xlsx")

# Long
six_three_ab_l <- six_three_ab_w %>% 
  pivot_longer(!Birthyear, names_to = "Sex", values_to = "Value") %>% 
  mutate(ProportionAverage = Sex) %>%
  select(Birthyear, Sex, ProportionAverage, Value)

six_three_ab_l$Sex <- gsub("-.*", "", six_three_ab_l$Sex)
six_three_ab_l$ProportionAverage <- gsub(".*-", "", six_three_ab_l$ProportionAverage)

six_three_ab_l <- six_three_ab_l %>% 
  pivot_wider(names_from = "ProportionAverage", values_from = "Value")

# 6_4_ab ----
# Wide
six_four_ab_w <- read_xlsx("./datasets/six_four_ab.xlsx")

# Long
six_four_ab_l <- six_four_ab_w  %>% 
  pivot_longer(!Birthyear, names_to = "Sex", values_to = "Value") %>% 
  mutate(Percentile = Sex) %>%
  select(Birthyear, Sex, Percentile, Value)

six_four_ab_l$Sex <- gsub("_.*", "", six_four_ab_l$Sex)
six_four_ab_l$Percentile <- gsub(".*_", "", six_four_ab_l$Percentile)

# 7_1_ab ----
# Wide
seven_one_ab_w <- read_xlsx("./datasets/seven_one_ab.xlsx")

# Long
seven_one_ab_l <- seven_one_ab_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Country = Sex) %>% 
  select(Year, Sex, Country, Value)

seven_one_ab_l$Sex <- gsub("-.*", "", seven_one_ab_l$Sex)
seven_one_ab_l$Country <- gsub(".*-", "", seven_one_ab_l$Country)

# 7_1_cd ----
# Wide
seven_one_cd_w <- read_xlsx("./datasets/seven_one_cd.xlsx")

# Long
seven_one_cd_l <- seven_one_cd_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Country = Sex) %>% 
  select(Year, Sex, Country, Value)

seven_one_cd_l$Sex <- gsub("-.*", "", seven_one_cd_l$Sex)
seven_one_cd_l$Country <- gsub(".*-", "", seven_one_cd_l$Country)

# 7_2_ab ----
# Wide
seven_two_ab_w <- read_xlsx("./datasets/seven_two_ab.xlsx")

# Long
seven_two_ab_l <- seven_two_ab_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Income = Sex) %>% 
  select(Year, Sex, Income, Value)

seven_two_ab_l$Sex <- gsub("-.*", "", seven_two_ab_l$Sex)
seven_two_ab_l$Income <- gsub(".*-", "", seven_two_ab_l$Income)

# 7_2_cd ----
# Wide
seven_two_cd_w <- read_xlsx("./datasets/seven_two_cd.xlsx")

# Long
seven_two_cd_l <- seven_two_cd_w %>% 
  pivot_longer(!Year, names_to = "Sex", values_to = "Value") %>% 
  mutate(Income = Sex) %>% 
  select(Year, Sex, Income, Value)

seven_two_cd_l$Sex <- gsub("-.*", "", seven_two_cd_l$Sex)
seven_two_cd_l$Income <- gsub(".*-", "", seven_two_cd_l$Income)

# 7_3_ab ----
# Wide
seven_three_ab_w <- read_xlsx("./datasets/seven_three_ab.xlsx")

# Long
seven_three_ab_l <- seven_three_ab_w %>% 
  pivot_longer(!Year, names_to = "Country", values_to = "Value") %>% 
  mutate(Income = Country) %>% 
  select(Year, Country, Income, Value)

seven_three_ab_l$Country <- gsub("-.*", "", seven_three_ab_l$Country)
seven_three_ab_l$Income <- gsub(".*-", "", seven_three_ab_l$Income)

# 7_4_ab ----
# Wide
seven_four_ab_w <- read_xlsx("./datasets/seven_four_ab.xlsx")

# Long
seven_four_ab_l <- seven_four_ab_w %>% 
  pivot_longer(!Year, names_to = "Prosecution", values_to = "Value") %>% 
  mutate(Country = Prosecution) %>% 
  select(Year, Prosecution, Country, Value)

seven_four_ab_l$Country <- gsub("-.*", "", seven_four_ab_l$Country)
seven_four_ab_l$Prosecution <- gsub(".*-", "", seven_four_ab_l$Prosecution)

# 7_5_ab ----
# Wide
seven_five_ab_w <- read_xlsx("./datasets/seven_five_ab.xlsx")

# Long
seven_five_ab_l <- seven_five_ab_w %>% 
  pivot_longer(!Year, names_to = "Prosecution", values_to = "Value") %>% 
  mutate(Country = Prosecution) %>% 
  select(Year, Prosecution, Country, Value)

seven_five_ab_l$Country <- gsub("-.*", "", seven_five_ab_l$Country)
seven_five_ab_l$Prosecution <- gsub(".*-", "", seven_five_ab_l$Prosecution)

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
     four_one_four_two_w,
     four_one_four_two_l,
     three_three_four_w, 
     three_three_four_l, 
     three_five_ad_w,
     three_five_ad_l,
     three_five_be_w,
     three_five_be_l,
     three_five_cf_w,
     three_five_cf_l,
     four_three_ac_w,
     four_three_ac_l,
     four_three_df_w,
     four_three_df_l,
     five_one_ab_w,
     five_one_ab_l,
     five_two_af_w,
     five_two_af_l,
     six_one_ab_w,
     six_one_ab_l,
     six_two_ad_w,
     six_two_ad_l,
     six_two_be_w,
     six_two_be_l,
     six_two_cf_w,
     six_two_cf_l,
     six_three_ab_w,
     six_three_ab_l,
     six_four_ab_w,
     six_four_ab_l,
     seven_one_ab_w,
     seven_one_ab_l,
     seven_one_cd_w,
     seven_one_cd_l,
     seven_two_ab_w,
     seven_two_ab_l,
     seven_two_cd_w,
     seven_two_cd_l,
     seven_three_ab_w,
     seven_three_ab_l,
     seven_four_ab_w,
     seven_four_ab_l,
     seven_five_ab_w,
     seven_five_ab_l,
     file = "./app/datasets.rda")