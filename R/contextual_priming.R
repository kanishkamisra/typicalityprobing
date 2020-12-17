library(tidyverse)
library(fs)
library(glue)
library(scales)
library(broom)

model_meta <- read_csv("data/meta.csv")

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

frequencies <- read_csv("data/frequencies/raw_wiki_counts2.csv")
  
typicality_results <- fs::dir_ls("data/rosch1975/results/rosch1975/", regexp = "*.csv") %>%
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
  left_join(frequencies %>% select(item = word, count)) %>%
  group_by(model, category) %>% 
  mutate(rank = row_number()) %>%
  ungroup()

rm(frequencies)
gc()
# correlations <- arguments %>%
#   group_by(model, params, category, predicate_id, argument_id) %>%
#   nest() %>%
#   mutate(
#     cor = map(data, function(x) {cor.test(-x$score, x$rank, method = "kendall") %>% tidy()})
#   ) %>%
#   select(-data) %>%
#   unnest(cor)

groups <- arguments %>%
  distinct(item, category, predicate_id, argument_id, rank) %>%
  group_by(category, predicate_id, argument_id) %>%
  nest() %>%
  mutate(
    data = map(data, function(x) {
      if(nrow(x) %% 2 != 0) {
        return(x[-nrow(x),])
      }
      else {
        return(x)
      }
    })
  ) %>%
  unnest(data) %>%
  add_count(category, predicate_id, argument_id) %>%
  group_by() %>%
  mutate(
    group = case_when(
      rank <= n/2 ~ "typical",
      TRUE ~ "atypical"
    )
  )

atypical <- groups %>%
  filter(group == "atypical") %>%
  select(category2 = category, item2 = item, pid2 = predicate_id, aid2 = argument_id, rank2 = rank) %>%
  group_by(category2, pid2, aid2) %>%
  nest(item2 = -group_cols())

typical <- groups %>%
  filter(group == "typical") %>%
  select(category1 = category, item1 = item, pid1 = predicate_id, aid1 = argument_id, rank1 = rank) %>%
  group_by(category1, pid1, aid1) %>%
  nest(item1 = -group_cols())

run_comparisons <- function() {
  comparisons <- bind_cols(typical, atypical) %>%
    mutate(
      item1 = map(item1, function(x) {
        rows = sample(nrow(x))
        return(x[rows, ])
      }),
      item2 = map(item2, function(x) {
        rows = sample(nrow(x))
        return(x[rows, ])
      })
    ) %>%
    unnest(c(item1, item2)) %>%
    inner_join(arguments %>% select(item1 = item, category1 = category, score1 = score, pid1 = predicate_id, aid1 = argument_id, rank1 = rank, model, params)) %>%
    inner_join(arguments %>% select(item2 = item, category2 = category, score2 = score, pid2 = predicate_id, aid2 = argument_id, rank2 = rank, model, params)) %>%
    select(-category2, -pid2, -aid2) %>%
    rename(category = category1, pid = pid1, aid = aid1) %>%
    inner_join(typicality_results %>% select(category, model, rank1 = rank, lp1 = logprob, nlp1 = normalized_logprob, freq1 = count)) %>%
    inner_join(typicality_results %>% select(category, model, rank2 = rank, lp2 = logprob, nlp2 = normalized_logprob, freq2 = count))
  return(comparisons)
}


set.seed(1234)
# this will be fun.
replicates <- replicate(100, run_comparisons(), simplify = FALSE)

predict <- function(item1, item2, metric1, metric2) {
  if(is.na(metric1) | is.na(metric2)) {
    return(NA)
  }
  else if(metric1 > metric2){
    return(item1)
  }
  else if (metric2 > metric1) {
    return(item2)
  }
  else {
    return("equal")
  }
}

# replicates[[1]]  %>%
#   group_by(model, category) %>%
#   summarize(acc = mean(agreeability_nlp))

alignments <- replicates %>%
  map(function(x) {
    x %>%
      group_by(model, params, category) %>%
      summarize(
        alignment = mean(score1 > score2)
      )
  }) %>%
  bind_rows(.id = "run") %>%
  group_by(model, params) %>%
  summarize(
    se = 1.96 * plotrix::std.error(alignment), 
    alignment = mean(alignment)
  )

agreeability_setup <- replicates %>%
  map(function(x) {
    x %>%
      ungroup() %>%
      mutate(
        taxonomic_prediction_lp = pmap_chr(list(item1, item2, lp1, lp2), predict),
        taxonomic_prediction_nlp = pmap_chr(list(item1, item2, nlp1, nlp2), predict),
        argument_prediction = pmap_chr(list(item1, item2, score1, score2), predict),
        frequency_prediction = pmap_chr(list(item1, item2, freq1, freq2), predict)
      ) %>%
      # group_by(model, params, category, pid, aid, item1, item2) %>%
      mutate(
        alignment_taxonomic_lp = taxonomic_prediction_lp == item1,
        alignment_taxonomic_nlp = taxonomic_prediction_nlp == item1,
        alignment_frequency = frequency_prediction == item1,
        alignment_argument = argument_prediction == item1,
        agreeability_argument_lp = taxonomic_prediction_lp == argument_prediction,
        agreeability_argument_nlp = taxonomic_prediction_nlp == argument_prediction,
        agreeability_frequency_lp = case_when(
          !is.na(frequency_prediction) ~ taxonomic_prediction_lp == frequency_prediction,
          TRUE ~ NA
        ),
        agreeability_frequency_nlp = case_when(
          !is.na(frequency_prediction) ~ taxonomic_prediction_nlp == frequency_prediction,
          TRUE ~ NA
        )
      ) %>%
      select(model, params, category, item1, item2, pid, aid, alignment_taxonomic_lp, alignment_taxonomic_nlp, alignment_frequency, alignment_argument, agreeability_argument_lp, agreeability_argument_nlp, agreeability_frequency_lp, agreeability_frequency_nlp) %>%
      group_by(model, params, category) %>%
      summarize(
        alignment_taxonomic_lp = mean(alignment_taxonomic_lp),
        alignment_taxonomic_nlp = mean(alignment_taxonomic_nlp),
        alignment_frequency = mean(alignment_frequency, na.rm = TRUE),
        alignment_argument = mean(alignment_argument),
        agreeability_argument_lp = mean(agreeability_argument_lp),
        agreeability_argument_nlp = mean(agreeability_argument_nlp),
        agreeability_frequency_lp = mean(agreeability_frequency_lp, na.rm = TRUE),
        agreeability_frequency_nlp = mean(agreeability_frequency_nlp, na.rm = TRUE)
      ) %>%
      ungroup() 
  }) %>%
  bind_rows(.id = "run")

experiment_results <- agreeability_setup %>%
  # filter(!is.na(alignment_frequency)) %>%
  # group_by(model, params, category) %>%
  group_by(model, params) %>%
  summarize(
    alignment_taxonomic_lp = mean(alignment_taxonomic_lp),
    alignment_taxonomic_nlp = mean(alignment_taxonomic_nlp),
    alignment_frequency = mean(alignment_frequency, na.rm = TRUE),
    alignment_argument = mean(alignment_argument),
    agreeability_argument_lp = mean(agreeability_argument_lp),
    agreeability_argument_nlp = mean(agreeability_argument_nlp),
    agreeability_frequency_lp = mean(agreeability_frequency_lp, na.rm = TRUE),
    agreeability_frequency_nlp = mean(agreeability_frequency_nlp, na.rm = TRUE)
  ) %>%
  ungroup() 

levels = c('albert-b-v1', 'albert-l-v1', 'albert-xl-v1', 'albert-xxl-v1', 'distilbert-b', 'bert-b', 'bert-l', 'electra-s', 'electra-b', 'electra-l', 'distilgpt2', 'gpt', 'gpt2', 'gpt2-m', 'gpt2-l', 'gpt2-xl', 'distilroberta-b', 'roberta-b', 'roberta-l')

experiment_results %>%
  inner_join(model_meta) %>%
  mutate(
    model = factor(model, levels)
  ) %>%
  ggplot(aes(model, agreeability_frequency_nlp, color = color, fill = color)) +
  geom_col(width = 0.9) +
  # facet_wrap(~category) +
  scale_color_identity() +
  scale_fill_identity() +
  # scale_alpha_identity() +
  # geom_hline(aes(yintercept = alignment_frequency))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
