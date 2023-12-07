library(tidyverse)
df <- read_csv("datos/Debernardi.csv")
df
df["lnLYVE1"] <- log(df$LYVE1)

summarise(group_by(df, diagnosis), x = mean(LYVE1))

df %>% select(age, sex, diagnosis, LYVE1)
df %>%
  mutate(lnLYVE1 = log(LYVE1)) %>%
  group_by(diagnosis) %>%
  summarise(lnLYVE1_mean = mean(lnLYVE1), lnLYVE1_se = sd(lnLYVE1))
we