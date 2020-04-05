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
library(parallel)
cl <- makeCluster(detectCores() - 1)


train_data <- read_csv("data/train.csv")
# test_data <- read_csv("data/test.csv")

# failures source: https://www.kaggle.com/c/tweet-sentiment-extraction/discussion/138272
source("functions.R")

# get metadata:
train_metadata <- get_metadata(train_data) # aguarde um pouco..
# test_metadata <- get_metadata(test_data) # aguarde um pouco..



# Rascunho ------------------------------------------------------------------------------------

# Mantenha o rascunho daqui para baixo

# pre process
train_pp <-
  train_data %>%
  mutate_at(c("text", "selected_text"), ~ .x %>%
    str_trim() %>% str_squish()) %>%  # segundo as regras nao faz diferenca
  filter(!is.na(text)) %>%
  filter(sentiment != "neutral") # Amostrar um tweet que nao seja neutro

vader::getVader(train_pp$text[1])

glue::glue("Non-neutral are {round((nrow(train_pp)*100) / nrow(train_data),2)}% of full dataset")

# Selecionar o maior twitter de uma amostra aleatoria:
ind <- sample(1:2, size = nrow(train_pp), replace = T, prob = c(.005, .95))
res <- parallel::mclapply(train_pp$text[ind == 1], ntoken, mc.cores = 4)
ind_max <- which.max(unlist(res))

# selecionado:
train_pp[ind_max, ]$text
train_pp[ind_max, ]$selected_text
train_pp[ind_max, ]$sentiment

# para o texto amostrado
results <- make_dataset(train_pp = train_pp[ind_max, ])

# para todos nao neutros:

# Create a cluster with 1 fewer cores than are available. Adjust as necessary

nested_train_pp <- 
  train_pp[1:50,] %>% 
  group_by(textID) %>% 
  tidyr::nest() 

parsed <- parLapply(cl, nested_train_pp$data, 
               function(.x){
                 source("functions.R")
                 # make_dataset(.x)
                 tryCatch(make_dataset(.x), error = function(e){print(e);print(.x); NA})
               })
stopCluster()

# Combinar resultados em um unico data.frame
parsed %>% 
  map_dfr(~as_tibble(as.data.frame(.x))) %>% View()

