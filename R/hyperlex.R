library(tidyverse)

hyperlex_full <- read_table2("data/hyperlex-all.txt") %>%
  janitor::clean_names() %>%
  select(-scores)

hyperlex <- read_csv("data/hyperlex-morphdetaug-manual.csv")
hyperlex %>%
  filter(str_detect(rel, "^hyp")) %>%
  count(word2, sort = TRUE)

# "All food can be daxed"
# "All groups can dax"??
# A club can be daxed