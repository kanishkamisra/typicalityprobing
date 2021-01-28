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

colors <- c('#595959', '#73a2c6','#5d8abd','#4771b2','#2e59a8','#fee391','#fec44f','#fe9929','#bcbddc', '#6a51a3','#54278f','#ff9895','#f4777f','#e4576b','#cf3759','#b41648','#93003a','#d9f0a3','#78c679','#238443')

sims <- dir_ls("data/results/premiseconclusion/similarities/", regexp = ".csv") %>%
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
  group_by(model, category, predicate_id, argument_id, layer) %>% 
  mutate(rank = row_number()) %>%
  ungroup()

simcor <- sims %>%
  group_by(model, params, layer, category, predicate_id, argument_id) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x) {
      cor.test(-x$similarity, x$rank, method = "kendall") %>%
        tidy()
    })
  ) %>%
  select(-data) %>%
  unnest(cor) 

simcor %>%
  group_by(model, params, layer) %>%
  summarize(
    correlation = mean(estimate)
  ) %>%
  # filter(category == "sport") %>%
  inner_join(model_meta, by = c("model")) %>%
  mutate(
    model = factor(model, levels = levels)
  ) %>%
  ungroup() %>%
  ggplot(aes(layer, correlation, color = color, group = model)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~family) +
  scale_color_identity()
