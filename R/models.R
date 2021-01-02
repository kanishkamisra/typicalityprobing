library(tidyverse)
library(widyr)
library(fs)

col_name <- function(x, default = stop("Please supply column name", call. = FALSE))
{
  if (is.character(x))
    return(x)
  if (identical(x, quote(expr = )))
    return(default)
  if (is.name(x))
    return(as.character(x))
  if (is.null(x))
    return(x)
  stop("Invalid column specification", call. = FALSE)
}

multi_scale <- function(tbl, item1, item2, value, k = 2, ...) {
  multi_scale_(tbl,
               col_name(substitute(item1)),
               col_name(substitute(item2)),
               col_name(substitute(value)),
               k = k, ...)
}


multi_scale_ <- function(tbl, item1, item2, value, k = 2) {
  tbl_matrix <- tbl %>%
    spread(item2, col_name(value), fill = 0) %>%
    as.data.frame() %>%
    remove_rownames() %>%
    column_to_rownames("item1") %>%
    as.matrix()
  
  cmdscale(tbl_matrix, k = k) %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    as.tibble()
}

cslb_categories <- read_tsv("data/cslb/concept.dat") %>%
  janitor::clean_names() %>%
  mutate(
    concept = str_replace_all(concept, " ", "_"),
    concept = str_replace(concept, "dates_\\(fruit\\)", "dates"),
    concept = str_replace(concept, "nail_\\(tool\\)", "nail"),
    concept = str_replace(concept, "yo\\-yo", "yoyo")
  )

cslb <- read_tsv("data/cslb/norms.dat") %>%
  mutate(
    concept = str_replace(concept, "coach_\\(vehicle\\)", "coach"),
    concept = str_replace(concept, "castenets", "castanets"),
    concept = str_replace(concept, "catapault", "catapult"),
    concept = str_replace(concept, "organ_\\(musical_instrument\\)", "organ"),
    concept = str_replace(concept, "seal_\\(animal\\)", "seal"),
    concept = str_replace(concept, "pennicillin", "penicillin")
  ) %>%
  inner_join(cslb_categories %>% select(category, concept))

typicality_ratings <- function(category_name = "bird") {
  concept_features <- cslb %>%
    filter(category == category_name) %>%
    filter(`feature type` == "visual perceptual") %>%
    select(concept, feature, pf) %>%
    pivot_wider(names_from = feature, values_from = pf, values_fill = 0) %>%
    column_to_rownames("concept") %>%
    as.matrix() 
  
  weights = colSums(concept_features) 
  
  return(concept_features * weights[col(concept_features)])
  
}

(cslb %>%
  filter(category == "vegetable") %>%
  select(concept, feature, pf) %>%
  pivot_wider(names_from = feature, values_from = pf, values_fill = 0) %>%
  column_to_rownames("concept") %>%
  as.matrix()) * typicality_ratings("vegetable") %>%
  rowSums() %>% sort()

cslb %>%
  filter(category == "land animal") %>%
  select(concept, feature, pf) %>%
  pairwise_dist(concept, feature, pf) %>%
  multi_scale(item1, item2, distance, k = 3) %>%
  # group_by(item) %>%
  mutate(
    distance = sqrt((V1 - mean(V1))^2 + (V2 - mean(V2))^2 + (V3 - mean(V3))^2)
  ) %>%
  select(-V1, -V2, -V3) %>%
  arrange(distance) %>%
  View()

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

features <- dir_ls("data/rosch1975/results/rosch1975_features/", regexp = "*.csv") %>%
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
  )

feature_matrix <- features %>%
  filter(model == "gpt2-xl") %>%
  select(item, category, feature, logprob) %>%
  filter(category == "bird") %>%
  pivot_wider(names_from = feature, values_from = logprob) %>%
  select(-category) %>%
  column_to_rownames("item") %>%
  as.matrix() 
feature_matrix = 100 - feature_matrix

weights <- colSums(feature_matrix)

rowSums(feature_matrix / weights[col(feature_matrix)]) %>% sort()


features %>%
  filter(model == "electra-l") %>%
  select(item, category, feature, logprob) %>%
  filter(category == "sport") %>%
  select(-category) %>%
  pairwise_dist(item, feature, logprob) %>%
  multi_scale(item1, item2, distance, k = 3) %>%
  mutate(
    distance = sqrt((V1 - mean(V1))^2 + (V2 - mean(V2))^2 + (V3 - mean(V3))^2)
  ) %>%
  select(-V1, -V2, -V3) %>%
  arrange(distance) %>%
  View()
