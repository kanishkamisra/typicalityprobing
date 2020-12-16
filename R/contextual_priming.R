library(tidyverse)
library(fs)
library(glue)
library(scales)
library(broom)

arguments <- dir_ls("data/results/premiseconclusion/", regexp = "*.csv") %>%
  map_df(read_csv) %>%
  mutate(
    model = str_remove(model, "(google_)"),
    model = str_remove(model, "\\-generator"),
    model = str_remove(model, "\\-uncased"),
    model = str_replace(model, "large", "l"),
    model = str_replace(model, "medium", "m"),
    model = str_replace(model, "small", "s"),
    model = str_replace(model, "base", "b"),
    model = str_replace(model, "openai\\-", "")
  ) %>%
  group_by(model, category, predicate_id, argument_id) %>% 
  mutate(rank = row_number()) %>%
  ungroup()

correlations <- arguments %>%
  group_by(model, params, category, predicate_id, argument_id) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x) {cor(-x$score, x$rank, method = "kendall") %>% tidy()})
  ) %>%
  select(-data) %>%
  unnest(cor)
