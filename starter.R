# Starter Script - Develop

# todo: obter smiles dos textos
# todo: comferir se precisa melhorar funcao de limpeza (one grams mais frequentes)

# todo: montar baseline para modelo de ml fazendo uma selecao aleatoria entre as ngrams (esperar parse)
# todo: transformar emoticons em palavras antes de aplicar o vader
# (incluir regra para quando for neutro e realizar um teste depois para saber se a pena usar o neutro)
# todo: calcular kappa para saber se o vader esta acertando como agnt espera

# Script --------------------------------------------------------------------------------------

# packages
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(quanteda)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores()-1, outfile="")
registerDoParallel(cl)


train_data <- read_csv("data/train.csv")
# test_data <- read_csv("data/test.csv")

# failures source: https://www.kaggle.com/c/tweet-sentiment-extraction/discussion/138272
source("functions.R")

# get metadata:
train_metadata <- get_metadata(train_data) # aguarde um pouco..
# test_metadata <- get_metadata(test_data) # aguarde um pouco..

# pre process 1
train_pp <-
  train_data %>%
  mutate_at(c("text", "selected_text"), ~ .x %>%
    str_trim() %>% str_squish()) %>%  # segundo as regras nao faz diferenca
  filter(!is.na(text)) %>%
  filter(sentiment != "neutral") # Amostrar um tweet que nao seja neutro

glue::glue("Non-neutral are {round((nrow(train_pp)*100) / nrow(train_data),2)}% of full dataset")

nested_train_pp <- 
  train_pp %>% 
  # .[1:1000,] %>% 
  group_by(textID) %>% 
  tidyr::nest() 

t0 <- Sys.time()
train_parsed <-  
  foreach(x=nested_train_pp$data,
          y=nested_train_pp$textID,
          n=1:length(nested_train_pp$textID),
          # .export = ls(),
          # .packages = c(dplyr, purrr, stringr),
          # .errorhandling = c("pass"),
          .combine = rbind,
          .inorder = F
  ) %dopar% {
    tryCatch({
      cat(paste0("Parsing tweet ",n,": ", x$text," (", x$sentiment, ") \n"))
      make_dataset(x, y)
    }, error = function(e){print(paste0("Error in tweet: ", y, "\n", 
                                  "caused the error: '", e, "'\n")); NA})
  
  }
Sys.time() - t0
stopCluster(cl)

saveRDS(train_parsed, "data/train_parsed.rds")
train_parsed <- readRDS("data/train_parsed.rds")


# Rascunho ------------------------------------------------------------------------------------

# Mantenha o rascunho daqui para baixo


vader::getVader(train_pp$text[1])



# Selecionar o maior twitter de uma amostra aleatoria:
ind <- sample(1:2, size = nrow(train_pp), replace = T, prob = c(.005, .95))
res <- parallel::mclapply(train_pp$text[ind == 1], ntoken, mc.cores = 4)
ind_max <- which.max(unlist(res))

# selecionado:
train_pp[ind_max, ]$text
train_pp[ind_max, ]$selected_text
train_pp[ind_max, ]$sentiment

# para o texto amostrado
results <- make_dataset(train_pp = train_pp[ind_max, ], train_pp[ind_max, ]$textID)

# Combinar metadata com parsed
left_join(
  train_parsed %>% select(-text_sentiment),
  train_metadata, by = c( "textID", "text", "sel_text")
) %>% View()


mean(train_metadata$text_n_words)
# 15

pos_text <- 
  tibble(x = str_split(text, " ")[[1]] %>% .[. %in% hash_emoticons$x]) %>% 
  left_join(hash_emoticons[x %in% emoticon,], by = "x")

substr(text, 4, 4) <- "t"

text = "goddamn dust out -), but =) I wore ;) out a clip on the camera panel so I had to glue it shut"
pos_emoticon <- 
pos_emoticon <- str_split(text, " ")[[1]] %>% {hash_emoticons[. %in% hash_emoticons$x,]}

text_splited <- str_split(text, " ")[[1]]
emoticon_found <- hash_emoticons[hash_emoticons$x %in%text_splited]


