library(tidyverse)
library(fs)
library(glue)
library(broom)

theme_set(theme_bw(base_size = 16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

model_meta <- read_csv("data/meta.csv") %>%
  mutate(
    model = str_remove(model, "(google_)"),
    model = str_remove(model, "\\-generator"),
    model = str_remove(model, "\\-uncased"),
    model = str_replace(model, "large", "l"),
    model = str_replace(model, "medium", "m"),
    model = str_replace(model, "small", "s"),
    model = str_replace(model, "base", "b"),
    model = str_replace(model, "openai\\-", ""),
    model = str_replace(model, "\\-v1", "")
  ) 

levels = c('5gram', 'albert-b', 'albert-l', 'albert-xl', 'albert-xxl', 'distilbert-b', 'bert-b', 'bert-l', 'electra-s', 'electra-b', 'electra-l', 'distilgpt2', 'gpt', 'gpt2', 'gpt2-m', 'gpt2-l', 'gpt2-xl', 'distilroberta-b', 'roberta-b', 'roberta-l')

colors <- c('gray', '#73a2c6','#5d8abd','#4771b2','#2e59a8','#ffca68','#ffc14c','#fdb827','#92338c','#800080','#a35298','#ff9895','#f4777f','#e4576b','#cf3759','#b41648','#93003a','#adcf90','#75af00','#008000')

induction <- dir_ls("data/results/premiseconclusion/", regexp = "*.csv") %>%
  map_df(read_csv) %>%
  mutate(
    model = str_remove(model, "(google_)"),
    model = str_remove(model, "\\-generator"),
    model = str_remove(model, "\\-uncased"),
    model = str_replace(model, "large", "l"),
    model = str_replace(model, "medium", "m"),
    model = str_replace(model, "small", "s"),
    model = str_replace(model, "base", "b"),
    model = str_replace(model, "openai\\-", ""),
    model = str_replace(model, "\\-v1", "")
  ) %>%
  group_by(model, category, predicate_id, argument_id) %>% 
  mutate(rank = row_number()) %>%
  ungroup()

induction %>%
  inner_join(model_meta) %>%
  mutate(model = factor(model, levels = levels)) %>%
  ggplot(aes(score - conclusion_only, fill = color, color = color)) +
  geom_histogram(alpha = 0.75) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_grid(category~family, scales = "free")

induction %>%
  inner_join(model_meta) %>%
  mutate(model = factor(model, levels = levels)) %>%
  ggplot(aes(conclusion_only, score, color = color, fill = color)) +
  geom_point(alpha = 0.03, size = 2) +
  geom_smooth(method = "lm") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(~family, scales = "free")

cor_truth <- induction %>%
  filter(model != "5gram") %>%
  group_by(model, params, category, predicate_id, argument_id) %>%
  mutate(
    standardized_logprob = (score - min(score))/(max(score) - min(score))
  ) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x) {
      cor.test(-x$standardized_logprob, x$rank, method = "kendall") %>%
        tidy()
    })
  ) %>%
  unnest(cor)

cor_truth %>% 
  select(-data) %>% View()

# Human Alignment.

cor_truth %>% 
  ungroup() %>%
  select(-data) %>% 
  group_by(model, params, category) %>% 
  summarize(cor = mean(estimate)) %>%
  ungroup() %>%
  inner_join(model_meta) %>%
  mutate(
    model = factor(model, levels)
  ) %>%
  ggplot(aes(model, cor, fill = color, color = color)) +
  geom_col() +
  scale_y_continuous(limits = c(-0.2, 0.6)) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(~category)


