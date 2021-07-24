library(tidyverse)
library(broom)
library(widyr)
library(fs)
library(glue)
library(ggrepel)
library(gt)
library(knitr)
library(kableExtra)

theme_set(theme_bw(base_size = 16))


model_meta <- read_csv(here::here("data/meta.csv")) %>%
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

levels = c('5gram', 'albert-b', 'albert-l', 'albert-xl', 'albert-xxl', 'distilbert-b', 'bert-b', 'bert-l', 'electra-s', 'electra-b', 'electra-l', 'distilroberta-b', 'roberta-b', 'roberta-l', 'distilgpt2', 'gpt', 'gpt2', 'gpt2-m', 'gpt2-l', 'gpt2-xl')

colors <- c('#595959', '#73a2c6','#5d8abd','#4771b2','#2e59a8','#fee391','#fec44f','#fe9929','#bcbddc', '#6a51a3','#54278f', '#d9f0a3','#78c679','#238443', '#ff9895','#f4777f','#e4576b','#cf3759','#b41648','#93003a')

shortlevels = c("5g", "A-b", "A-l", "A-xl", "A-xxl", "dB-b", "B-b", "B-l", "E-s", "E-b", "E-l", "dR-b", "R-b", "R-l", "dGPT2", "GPT", "GPT2", "GPT2-m", "GPT2-l", "GPT2-xl")

typicality_ratings <- dir_ls(here::here("data/rosch1975/results/rosch1975_alternate/"), regexp = "*.csv") %>%
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
  group_by(model, category) %>% 
  mutate(rank = row_number()) %>%
  ungroup() %>%
  inner_join(read_csv(here::here("data/rosch1975/rosch1975_ratings.csv")))

human_ratings <- read_csv(here::here("data/rosch1975/rosch1975_ratings.csv"))

plural = c("checker", "animal", "book", "checker", "crayon", "jack", "marble", "paper doll", "stilt")

induction <- dir_ls(here::here("data/results/premiseconclusion/"), regexp = "*.csv") %>%
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
  ungroup() %>%
  mutate(
    item = case_when(item == "brussel" ~ "brussels sprouts", TRUE ~ item),
    item = case_when(item %in% plural ~ paste0(item, "s"), TRUE ~ item),
    item = case_when(item == "skate" & category == "toy" ~ "skates", TRUE ~ item)
  )

frequencies <- read_csv(here::here("data/frequencies.csv")) %>%
  inner_join(human_ratings)

induction %>%
  filter(model != "5gram") %>%
  group_by(model, params) %>%
  summarize(
    conclusion_only = mean(score > conclusion_only),
    order_sensitivity = mean(shuffled_diff > 0),
    taxonomic_priming = mean(score > control_score)
  ) %>%
  ungroup() %>%
  inner_join(model_meta %>% select(-params)) %>%
  mutate(name = factor(model, levels = levels))

# correlation with human.

# AS Score correction

fit_as_score <- lm(score ~ scale(control_score, scale = FALSE) + scale(shuffled_diff, scale = FALSE), data = induction)
summary(fit_as_score)
residuals(fit_as_score) + coef(fit_as_score)["(Intercept)"]

corrected <- induction %>%
  group_by(model, predicate_id, argument_id, category) %>% mutate(item_id = row_number()) %>% ungroup() %>%
  group_by(model, predicate_id, argument_id, category) %>%
  nest() %>%
  mutate(
    corrected_score = map(data, function(x) {
      fit <- lm(score ~ scale(control_score, scale = FALSE) + scale(shuffled_diff, scale = FALSE), data = x)
      # fit <- lm(score ~ control_score + shuffled_diff, data = x)
      corrected_score = residuals(fit) + coef(fit)["(Intercept)"]
      p_value = glance(fit)$p.value
      tidied <- tidy(fit)
      beta_1 = tidied %>% filter(term == "scale(control_score, scale = FALSE)") %>% pull(estimate)
      # beta_1 = tidied %>% filter(term == "control_score") %>% pull(estimate)
      beta_2 = tidied %>% filter(term == "scale(shuffled_diff, scale = FALSE)") %>% pull(estimate)
      # beta_2 = tidied %>% filter(term == "shuffled_diff") %>% pull(estimate)
      return(
        tibble(item = x$item, item_id = x$item_id, score = x$score, corrected_score = corrected_score, rsq = summary(fit)$r.squared, p = p_value, beta1 = beta_1, beta2 = beta_2)
      )
    })
  ) %>%
  select(-data) %>%
  unnest(corrected_score)

induction_scores <- corrected %>%
  inner_join(typicality_ratings %>% select(model, category, model_score = score, rating) %>% group_by(model, category) %>% mutate(item_id = row_number())) %>%
  # group_by(model, category, predicate_id, argument_id) %>%
  # mutate(
  #   model_score = (model_score - min(model_score))/(max(model_score) - min(model_score)),
  #   score = (score - min(score))/(max(score) - min(score)),
  #   corrected_score = (corrected_score - min(corrected_score))/(max(corrected_score) - min(corrected_score))
  # ) %>%
  # ungroup() %>%
  group_by(model, category, item, item_id) %>%
  summarize(
    model_score = mean(model_score),
    corrected_score = mean(corrected_score),
    score = mean(score),
    rating = mean(rating)
  ) %>% 
  ungroup() %>% 
  group_by(model, category) %>%
  mutate(
    taxonomic = (model_score - min(model_score))/(max(model_score) - min(model_score)),
    score = (score - min(score))/(max(score) - min(score)),
    corrected_score = (corrected_score - min(corrected_score))/(max(corrected_score) - min(corrected_score)),
    rating = (rating - min(rating))/(max(rating) - min(rating))
  ) %>%
  ungroup()

corrected_correlation <- induction_scores %>% 
  group_by(model) %>%
  nest() %>%
  mutate(
    cor = map(data, function(x) {
      pre = cor.test(-x$score, x$rating, method = "spearman") %>% 
        tidy()
      post = cor.test(-x$corrected_score, x$rating, method = "spearman") %>% 
        tidy()
      taxonomic = cor.test(x$corrected_score, x$taxonomic, method = "spearman") %>% 
        tidy()
      return(
        tibble(
          pre_cor = pre$estimate,
          pre_p = pre$p.value,
          post_cor = post$estimate,
          post_p = post$p.value,
          consistency = taxonomic$estimate,
          consistency_p = taxonomic$p.value
        )
      )
    })
  ) %>%
  select(-data) %>%
  unnest(cor) %>%
  inner_join(model_meta) %>% 
  mutate(model = factor(model, levels = levels), short = factor(short, levels = shortlevels)) 

cor.test(corrected_correlation %>% filter(model != '5gram') %>% pull(params) %>% log10(), corrected_correlation %>% filter(model != '5gram') %>% pull(post_cor), method = "spearman")

corrected_correlation %>%
  filter(model != '5gram') %>%
  ggplot(aes(params/1e6, post_cor)) + 
  geom_point(aes(color = color), show.legend = FALSE, size = 3) +
  # geom_text_repel(aes(label = model), size = 5, nudge_x = -0.03, fontface = "bold", show.legend = FALSE) +
  # facet_wrap(~family, nrow = 1, scales = "free_x") +
  # geom_smooth(method = 'lm') +
  # scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_log10() +
  # scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_color_identity() + 
  labs(
    x = "Parameters (in millions)",
    y = "Spearman's Rho"
  )

p1 <- corrected_correlation %>%
  ggplot(aes(short, post_cor, color = color, fill = color)) +
  geom_col() +
  scale_color_identity(aesthetics = c("color", "fill")) +
  annotate("text", x = 2.5, y = 0.55, label = "ALBERT", size = 5.5, color = "#2e59a8", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 6, y = 0.55, label = "BERT", size = 5.5, color = "#fe9929", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 9, y = 0.55, label = "ELECTRA", size = 5.5, color = "#54278f", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 16.5, y = 0.55, label = "GPT/GPT2", size = 5.5, color = "#93003a", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 12, y = 0.55, label = "RoBERTa", size = 5.5, color = "#238443", fontface = "bold", family = "CMU Sans Serif") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.012), breaks = scales::pretty_breaks(6)) +
  labs(
    y = "Spearman's Rho"
  ) +
  theme_bw(base_size = 18, base_family = "CMU Sans Serif") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 20, vjust = 0.8),
    axis.title.x = element_blank(),
    plot.margin = margin(0.1, 0.2, 0.1, 0.1, "cm"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent")
  )

ggsave("paper/inductionspearmantsv.pdf", p1, height = 5, width = 10, device = cairo_pdf, dpi = 300)

classes <- human_ratings %>%
  group_by(category) %>%
  mutate(
    rating = (-rating - min(-rating))/(max(-rating) - min(-rating)),
    class = case_when(
      rating >= median(rating) ~ "high",
      TRUE ~ "low"
    )
  ) %>%
  ungroup()

model_ratings <- induction_scores %>%
  inner_join(classes %>% select(-rating)) %>%
  select(model, item, category, rating = corrected_score, class)

averaged_category_wise <- corrected %>%
  inner_join(typicality_ratings %>% select(model, category, model_score = score, rating) %>% group_by(model, category) %>% mutate(item_id = row_number())) %>%
  group_by(model, category, item, item_id) %>%
  summarize(
    model_score = mean(model_score),
    corrected_score = mean(corrected_score),
    score = mean(score),
    rating = mean(rating)
  ) %>% 
  ungroup() %>%
  select(item, category, score = corrected_score) %>%
  group_by(category, item) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  inner_join(classes %>% select(-rating)) %>%
  group_by(category) %>%
  mutate(
    rating = (score - min(score))/(max(score) - min(score)),
    model = "average"
  ) %>%
  select(-score)


p2 <- bind_rows(
  model_ratings,
  classes %>%
    mutate(model = "human"),
  averaged_category_wise
) %>%
  group_by(model, class, category) %>%
  summarize(
    rating = mean(rating)
  ) %>%
  mutate(
    class = factor(class, levels = c('low', 'high'))
  ) %>%
  mutate(
    color = case_when(
      model == "human" ~ "#a20a0a",
      model == "average" ~ "#0278ae",
      model == "5gram" ~ "black",
      TRUE ~ "black"
    ),
    alpha = case_when(
      model == "human" ~ 1,
      model == "average" ~ 1,
      model == "5gram" ~ 1,
      TRUE ~ 0.1
    ),
    size = case_when(
      model == "human" ~ 1,
      model == "average" ~ 1,
      model == "5gram" ~ 0.75,
      TRUE ~ 1
    ),
    linetype = case_when(
      model == "human" ~ "solid",
      model == "average" ~ "solid",
      model == "5gram" ~ "dotdash",
      TRUE ~ "solid"
    ),
    # category = str_to_upper(category)
  ) %>%
  ggplot(aes(class, rating, group = model, color = color, linetype = linetype, alpha = alpha)) +
  geom_point(size = 1.5) + 
  geom_line(aes(size = size)) + 
  facet_wrap(~category, nrow = 2, scales = "free_x") +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_linetype_identity() +
  scale_size_identity() + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(expand = c(0,0.2)) +
  theme_bw(base_size = 16, base_family = "CMU Sans Serif") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  ) + 
  labs(
    y = "Rating (scaled)"
  )

ggsave("paper/inductioncategorywisetsv.pdf", p2, height = 5, width = 10, device = cairo_pdf)

p3 <- bind_rows(
  model_ratings %>% replace_na(list(rating = 0)) %>% group_by(model, class) %>% summarize(rating = mean(rating)),
  classes %>%
    mutate(model = "human") %>%
    group_by(model, class) %>% summarize(rating = mean(rating)),
  averaged_category_wise %>% group_by(model, class) %>% summarize(rating = mean(rating))
) %>%
  group_by(model, class) %>%
  summarize(
    rating = mean(rating)
  ) %>%
  mutate(
    class = str_replace(class, "low", "Low Typicality"),
    class = str_replace(class, "high", "High Typicality"),
    class = factor(class, levels = c('Low Typicality', 'High Typicality'))
  ) %>%
  mutate(
    color = case_when(
      model == "human" ~ "#a20a0a",
      model == "average" ~ "#0278ae",
      model == "5gram" ~ "black",
      TRUE ~ "black"
    ),
    alpha = case_when(
      model == "human" ~ 1,
      model == "average" ~ 1,
      model == "5gram" ~ 1,
      TRUE ~ 0.1
    ),
    size = case_when(
      model == "human" ~ 1.2,
      model == "average" ~ 1.2,
      model == "5gram" ~ 0.8,
      TRUE ~ 1
    ),
    linetype = case_when(
      model == "human" ~ "solid",
      model == "average" ~ "solid",
      model == "5gram" ~ "dotdash",
      TRUE ~ "solid"
    ),
    # category = str_to_upper(category)
  ) %>%
  ggplot(aes(class, rating, group = model, color = color, linetype = linetype, alpha = alpha)) +
  geom_point(size = 3) + 
  geom_line(aes(size = size)) + 
  annotate("text", label = "Human", x = "High Typicality", y = 0.88, size = 6.5, color = "#a20a0a", family = "CMU Sans Serif", fontface = 'bold') +
  annotate("text", label = "Avg.\nof LMs", x = 0.85, y = 0.4, size = 6.5, color = "#0278ae", family = "CMU Sans Serif", fontface = 'bold') +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_linetype_identity() +
  scale_size_identity() + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(expand = c(0,0.3)) +
  theme_bw(base_size = 20, base_family = "CMU Sans Serif") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  ) + 
  labs(
    y = "Typicality Rating (scaled)"
  )

ggsave("paper/inductionlowvshighavg.pdf", height = 5, width = 6, device = cairo_pdf, dpi = 300)

