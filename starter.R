# Starter Script - Develop

# todo: obter smiles dos textos
# todo: n de palavras unicas
# todo: n de maiusculas
# todo: % do texto que foi coletado
# todo: usar busca de palavras cm sentimento negativo e retornar trexos com essas frases
# todo: usar a palavra que vem antes e que vem depoois (3 submissoes)
# todo: montar baseline para modelo de ml fazendo uma selecao aleatoria entre as ngrams 
# (incluir regra para quando for neutro e realizar um teste depois para saber se a pena usar o neutro)
# todo: calcular kappa para saber se o vader esta acertando como agnt espera
# todo: comferir se precisa melhorar funcao de limpeza (one grams mais frequentes)

# Script --------------------------------------------------------------------------------------

# packages
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(quanteda)

train_data <- read_csv("data/train.csv")
test_data <- read_csv("data/test.csv")

# failures source: https://www.kaggle.com/c/tweet-sentiment-extraction/discussion/138272
source("functions.R")

# get metadata:
train_metadata <- get_metadata(train_data) # aguarde um pouco..
# test_metadata <- get_metadata(test_data) # aguarde um pouco..



# Rascunho ------------------------------------------------------------------------------------

# Mantenha o rascunho daqui para baixo

# pre process
train_data <-
  train_data %>%
  mutate_at(c("text", "selected_text"), ~ .x %>%
    str_trim() %>%
    str_squish()) # segundo as regras nao faz diferenca

vader::getVader(train_data$text[1])

# Amostrar um tweet que nao seja neutro
non_neutral <-
  train_data %>%
  filter(sentiment != "neutral")

glue::glue("Non-neutral are {round((nrow(non_neutral)*100) / nrow(train_data),2)}% of full dataset")

# Selecionar o maior twitter de uma amostra aleatoria:
ind <- sample(1:2, size = nrow(non_neutral), replace = T, prob = c(.005, .95))
res <- parallel::mclapply(non_neutral$text[ind == 1], ntoken, mc.cores = 4)
ind_max <- which.max(unlist(res))

# selecionado:
non_neutral[ind_max, ]$text
non_neutral[ind_max, ]$selected_text
non_neutral[ind_max, ]$sentiment

vader_compound <- function(x) {
  # normalized, weighted composite score
  # https://github.com/cjhutto/vaderSentiment#about-the-scoring
  if (str_length(x) == 0) {
    NA
  } else {
    tryCatch(
      vader::getVader(x) %>%
          .[names(.) == "compound"] %>%
          {
            case_when(
              . >= 0.05 ~ "positive",
              . <= -0.05 ~ "negative",
              T ~ "neutral"
            )
          },
      error = function(e) NA
    )
  }
}

make_dataset <- function(x) {
  
  text_vader = x$text %>% map_chr(vader_compound)
  
  tibble(
    txt_id = x$textID,
    txt = x$text,
    txt_len = str_split(x$text, pattern = " ", )[[1]] %>% length(),
    text_sentiment = x$sentiment,
    text_vader = text_vader,
    all_ngrams = map(1:txt_len, ~ tau::textcnt(x$text, method = "string", split = " ", n = .x, tolower = FALSE) %>% names()) %>% unlist()
  ) %>%
    mutate(
      ngram_len = all_ngrams %>% map_dbl(~ str_split(.x, pattern = " ", )[[1]] %>% length()),
      ngram_prop = ngram_len / txt_len,
      ngram_vader = all_ngrams %>% map_chr(vader_compound),
      dif_txt_ngram = txt_len - ngram_len,
      dif_prop_txt_ngram = (txt_len - ngram_len) / txt_len,
      dif_ngram_vader = map2_chr(x$text, all_ngrams, ~ str_remove(.x, .y)),
      dif_ngram_vader = map_chr(dif_ngram_vader, vader_compound)
    ) %>%
    rowwise() %>%
    mutate(jaccard = jaccard(x$text, all_ngrams)) %>% # y
    ungroup()
}


# para a amostra
results <- make_dataset(x = non_neutral[ind_max, ])
# para todos nao neutros:
results_nonneutral <- parallel::mclapply(non_neutral, make_dataset, mc.cores = 4)

lexicon::hash_internet_slang



non_neutral$selected_text[ind_max]
