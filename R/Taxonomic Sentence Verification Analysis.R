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

human_ratings <- read_csv(here::here("data/rosch1975/rosch1975_ratings.csv"))

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

scale_minmax <- function(x, range = c(0, 1)) {
  scaled = (x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1]
  return(scaled)
}

ngram <- typicality_ratings %>% 
  filter(model == "5gram") %>%
  rename(ngram_score = score) %>%
  select(-stimulus, -model, -params)

tsv_correlations <- typicality_ratings %>%
  # filter(model != "5gram") %>%
  group_by(model, params) %>%
  nest() %>%
  mutate(cor = map(data, function(x) cor.test(x$rating, -x$score, method = "spearman") %>% tidy())) %>%
  unnest(cor) %>%
  select(-data) %>%
  inner_join(model_meta %>% select(-params)) %>%
  mutate(model = factor(model, levels = levels), short = factor(short, levels = shortlevels))

cor.test(tsv_correlations %>% filter(model != '5gram') %>% pull(params) %>% log10(), tsv_correlations %>% filter(model != '5gram') %>% pull(estimate), method = "pearson")

tsv_correlations %>% 
  filter(model != '5gram') %>%
  ggplot(aes(params/1e6, estimate)) + 
  geom_point(aes(color = color), show.legend = FALSE, size = 3) +
  # geom_text_repel(aes(label = model), size = 5, nudge_x = -0.03, fontface = "bold", show.legend = FALSE) +
  # facet_wrap(~family, nrow = 1, scales = "free_x") +
  # geom_smooth(method = 'lm') +
  # scale_y_continuous(limits = c(0, 0.5)) +
  # scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_log10() +
  scale_color_identity() + 
  labs(
    x = "Parameters (in millions)",
    y = "Spearman's Rho"
  )

# 11.5 x 3.5

p <- tsv_correlations %>%
  ggplot(aes(short, estimate, color = color, fill = color)) +
  # geom_point(size = 2) + 
  # geom_line(size = 0.75) +
  geom_col() +
  # geom_text(aes(y = 0.02, label = model), color = "white", angle = 90, hjust = "left", vjust = "center") +
  # facet_wrap(~category, nrow = 2) +
  # scale_color_identity(guide = "legend", name = "Model", aesthetics = c("color", "fill"), labels = levels, breaks = colors) +
  annotate("text", x = 3.5, y = 0.5, label = "ALBERT", size = 5.5, color = "#2e59a8", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 7, y = 0.5, label = "BERT", size = 5.5, color = "#fe9929", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 10, y = 0.5, label = "ELECTRA", size = 5.5, color = "#54278f", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 17.5, y = 0.5, label = "GPT/GPT2", size = 5.5, color = "#93003a", fontface = "bold", family = "CMU Sans Serif") +
  annotate("text", x = 13, y = 0.5, label = "RoBERTa", size = 5.5, color = "#238443", fontface = "bold", family = "CMU Sans Serif") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.012), breaks = scales::pretty_breaks(6)) +
  labs(
    x = "Model",
    y = "Spearman's Rho"
  ) +
  theme_bw(base_size = 18, base_family = "Times") +
  theme(
    legend.position = "top",
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, vjust = 0.8),
    axis.title.x = element_blank(),
    plot.margin = margin(0.1, 0.2, 0.1, 0.1, "cm"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent")
  )

ggsave("paper/spearmantsv.pdf", p, height = 5, width = 10, device = cairo_pdf, dpi = 300)

## 10 x 5 (w x h)

typicality_ratings %>%
  filter(model == "gpt2-xl") %>%
  group_by(category) %>%
  filter(score == max(score))

p <- typicality_ratings %>%
  # filter(model != "5gram") %>%
  group_by(model, params, category) %>%
  nest() %>%
  mutate(cor = map(data, function(x) cor.test(x$rating, -x$score, method = "spearman") %>% tidy())) %>%
  unnest(cor) %>%
  select(-data) %>%
  inner_join(model_meta %>% select(-params)) %>%
  mutate(model = factor(model, levels = levels)) %>%
  ggplot(aes(model, estimate, color = color, fill = color)) +
  geom_col() +
  # geom_text(aes(y = 0.02, label = model), color = "white", angle = 90, hjust = "left", vjust = "center") +
  facet_wrap(~category, nrow = 2) +
  scale_color_identity(guide = "legend", name = "Model", aesthetics = c("color", "fill"), labels = levels, breaks = colors) +
  # scale_fill_identity() +
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  labs(
    x = "Model",
    y = "Spearman's Rho"
  ) +
  theme_bw(base_size = 16, base_family = "CMU Sans Serif") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

ggsave("paper/tsv_categorywise.pdf", p, height = 6, width = 10, device = cairo_pdf)

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

model_ratings <- typicality_ratings %>%
  inner_join(classes %>% select(-rating)) %>%
  select(model, item, category, score, class) %>%
  group_by(model, category) %>%
  mutate(
    rating = (score - min(score))/(max(score) - min(score)),
  ) %>%
  select(-score)

averaged_category_wise <- typicality_ratings %>%
  filter(model != "5gram") %>%
  select(item, category, score) %>%
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
    y = "Typicality Rating (scaled)"
  )

ggsave("paper/categorywisetsv.pdf", p2, height = 5, width = 10, device = cairo_pdf)

# 10 x 5 (wxh)


p3 <- bind_rows(
  model_ratings %>% 
    replace_na(list(rating = 0)) %>% 
    group_by(model, class) %>% 
    summarize(ste = 1.96 * plotrix::std.error(rating), rating = mean(rating)),
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
  annotate("text", label = "5gram", x = "High Typicality", y = 0.36, size = 6.5, family = "CMU Sans Serif", fontface='bold') +
  annotate("text", label = "Human", x = "High Typicality", y = 0.88, size = 6.5, color = "#a20a0a", family = "CMU Sans Serif", fontface='bold') +
  annotate("text", label = "Avg.\nof LMs", x = 0.85, y = 0.55, size = 6.5, color = "#0278ae", family = "CMU Sans Serif", fontface='bold') +
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

ggsave("paper/lowvshighavg.pdf", height = 5, width = 6, device = cairo_pdf, dpi = 300)
## Induction Experiments


## Bilsland

p_tsv <- tsv_correlations %>%
  filter(model != "5gram") %>%
  ggplot(aes(params/1e6, estimate)) +
  geom_point(size = 3, color = "#0081B4") +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), 
                limit = c(10^1, 10^3.5)) +
  theme_bw(base_size = 16, base_family = "Times") +
  theme(
    axis.text = element_text(color = "black"),
  ) +
  labs(
    y = "Correlation",
    x = "Parameters (in millions)"
  )

ggsave("paper/bilsland-example.pdf", p_tsv, height = 3, width = 3.5, dpi = 300, device = cairo_pdf)

bind_rows(
  model_ratings %>% 
    replace_na(list(rating = 0)) %>% 
    group_by(model, class) %>% 
    summarize(ste = 1.96 * plotrix::std.error(rating), rating = mean(rating)),
  classes %>%
    mutate(model = "human") %>%
    group_by(model, class) %>% summarize(rating = mean(rating)),
  averaged_category_wise %>% group_by(model, class) %>% summarize(rating = mean(rating))
) %>%
  filter(model != "5gram") %>%
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
  # annotate("text", label = "5gram", x = "High Typicality", y = 0.36, size = 6.5, family = "CMU Sans Serif", fontface='bold') +
  annotate("text", label = "Human", x = "High Typicality", y = 0.88, size = 6.5, color = "#a20a0a", family = "CMU Sans Serif", fontface='bold') +
  annotate("text", label = "Avg.\nof LMs", x = 0.85, y = 0.55, size = 6.5, color = "#0278ae", family = "CMU Sans Serif", fontface='bold') +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_linetype_identity() +
  scale_size_identity() + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(expand = c(0,0.3)) +
  theme_bw(base_size = 20, base_family = "Times") +
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
