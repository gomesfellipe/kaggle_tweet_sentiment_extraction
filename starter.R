# Starter Script - Develop

# todo: obter smiles dos textos
# todo: comferir se precisa melhorar funcao de limpeza (one grams mais frequentes)

# todo: montar baseline para modelo de ml fazendo uma selecao aleatoria entre as ngrams (esperar parse)
# todo: transformar emoticons em palavras antes de aplicar o vader
# (incluir regra para quando for neutro e realizar um teste depois para saber se a pena usar o neutro)
# todo: calcular kappa para saber se o vader esta acertando como agnt espera

# n de palavras positiva e enegativas

# Load dependencies ---------------------------------------------------------------------------

# packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(quanteda)
library(foreach)
library(doParallel)

# load fucntions
source("functions.R")
NEW_RUN = F # alterar para true caso seja necessario rodar um novo job

# Train data ----------------------------------------------------------------------------------

# train data
train_data <- read_csv("data/train.csv")

# get metadata:
train_metadata <- get_metadata(train_data) # aguarde um pouco..

# get parsed:
if(NEW_RUN == T){
  
  # Remover espacos em branco extras e 
  # amostrar tweets que nao sejam neutro
  train_pp <-
    train_data %>%
    mutate_at(c("text", "selected_text"), ~ .x %>%
                str_trim() %>% str_squish()) %>%
    filter(!is.na(text)) %>%
    filter(sentiment != "neutral")
  
  # Aninhar dados por textID
  nested_train_pp <-
    train_pp %>%
    group_by(textID) %>%
    tidyr::nest()
  
  cl <- makeCluster(detectCores()-1, outfile="")
  registerDoParallel(cl)
  
  
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
}else{
  train_parsed <- readRDS("data/train_parsed.rds")
}

# join metadata and parse:
{
  # combinar resultado com metadados e 
  # organizar ordem das colunas de interesse
  train_parsed <- 
    train_parsed %>% 
    left_join(train_metadata, by = c("textID", "text", "sel_text", "jaccard")) %>% 
    select(textID,
           text_sentiment, 
           text, sel_text, 
           all_ngrams, text_n_words, 
           ngram_len, ngram_prop, 
           ngram_vader, dif_text_ngram,
           dif_ngram_vader, text_nunique_words, 
           text_len:text_numbers, # metadados
           # start, end, # para outra opcao de modelagem
           jaccard)
  
  # preencher metadados para todas as ngrams
  train_parsed <- 
    train_parsed %>% 
    fill(text_nunique_words:text_numbers,.direction = "up")
  
  # remover dados de treino onde ocorreram erros na marcacao do texto selecionado, 
  # que nao respeita o espaco em branco como espacador
  train_parsed <- 
    train_parsed %>% 
    group_by(textID) %>% 
    nest() %>% 
    mutate(to_remove = map_lgl(data,~! any(.x$sel_text == .x$all_ngrams))) %>% 
    filter(to_remove != T) %>% 
    select(-to_remove) %>% 
    unnest(cols = c(data))
  
  # remover linhas onde all_ngram eh igual do texto selecionado porem 
  # o jaccard nao é igual a 1 para nao 'confundir' o algoritmo (ou vice-versa)
  ind <- train_parsed %>% 
    filter(all_ngrams == sel_text & jaccard != 1 | jaccard == 1 & all_ngrams != sel_text) %>% 
    pull(textID) %>% unique()
  
  train_parsed <-
    train_parsed %>% 
    ungroup() %>%
    filter(!textID %in% ind)
  
  # remover linhas aonde vader nao bate com o sentimento verdadeiro da frase
  # (Opcional, reduz consideravelmente o tamanho da base)
  train_parsed <-
    train_parsed %>%
    filter(ngram_vader == text_sentiment)
}
train_parsed

# Ensaio de ajuste de modelo:
{
  train_parsed<- 
    train_parsed %>% 
    select(- one_of('textID', 'text', 'sel_text','all_ngrams', 
                    # 'text_hashtags','text_atpeople','text_links', 'text_retweet',
                    'dif_text_ngram' ,'ngram_vader'))
  
  train_parsed
  
  rm(train_data, train_metadata)
  gc()
  
  hist(train_parsed$jaccard)
  
  # prep to beta model
  train_parsed <- train_parsed %>%
    mutate(jaccard = case_when(jaccard == 0 ~.Machine$double.eps,
                               jaccard == 1 ~ 1-.Machine$double.eps^(.4),
                               jaccard > 0 & jaccard < 1 ~ jaccard)) %>% 
    dplyr::select(-dif_ngram_vader)
  
  
  # m <- betareg::betareg(jaccard ~., data = train_parsed, link = 'logit',
  #                       control = betareg::betareg.control(trace = T))
  m <- lm(jaccard~., data = train_parsed)
  summary(m) 
}

# Test data -----------------------------------------------------------------------------------

# test data
test_data <- read_csv("data/test.csv")

# save neutral text to submit
test_neutral <- test_data %>% filter(sentiment == "neutral")

# get test metadata:
test_metadata <- get_metadata(test_data) # aguarde um pouco..

# get parsed:
if(NEW_RUN == T){
  
  # Remover espacos em branco extras e 
  # amostrar tweets que nao sejam neutro
  test_pp <-
    test_data %>%
    mutate_at(c("text"), ~ .x %>%
                str_trim() %>% str_squish()) %>%
    filter(!is.na(text)) %>%
    filter(sentiment != "neutral") # Amostrar um tweet que nao seja neutro
  
  # Aninhar dados por textID
  nested_test_pp <-
    test_pp %>%
    group_by(textID) %>%
    tidyr::nest()
  
  cl <- makeCluster(detectCores()-1, outfile="")
  registerDoParallel(cl)
  
  t0 <- Sys.time()
  test_parsed <-
    foreach(x=nested_test_pp$data,
            y=nested_test_pp$textID,
            n=1:length(nested_test_pp$textID),
            .export = ls(),
            .packages = c('dplyr', 'purrr', 'stringr'),
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
  cat(Sys.time() - t0)
  stopCluster(cl)
  saveRDS(test_parsed, "data/test_parsed.rds")
}else{
  test_parsed <- readRDS("data/test_parsed.rds")  
}

# join metadata and parse:
{
  # combinar resultado com metadados e 
  # organizar ordem das colunas de interesse
  test_parsed <- 
    test_parsed %>% 
    left_join(test_metadata, by = c("textID", "text")) %>%
    dplyr::select(textID,
                  text_sentiment, 
                  text, 
                  all_ngrams, text_n_words, 
                  ngram_len, ngram_prop, 
                  ngram_vader, dif_text_ngram,
                  dif_ngram_vader, text_nunique_words, 
                  text_len:text_numbers # metadados
    )
  
  test_parsed<- 
    test_parsed %>% 
    dplyr::select(- one_of('text', 'dif_ngram_vader',
                           # 'text_hashtags','text_atpeople','text_links', 'text_retweet',
                           'dif_text_ngram' ,'ngram_vader'))
}

# Predictions:
{
  nested_test_parsed <- 
    test_parsed %>% 
    group_by(textID) %>% 
    nest() %>% 
    ungroup()
  
  
  cl <- makeCluster(detectCores()-1, outfile="")
  registerDoParallel(cl)
  
  t0 <- Sys.time()
  test_predict <-
    foreach(x=nested_test_parsed$data,
            y=nested_test_parsed$textID,
            n=1:length(nested_test_parsed$textID),
            # .export = ls(),
            .packages = c('dplyr'),
            # .errorhandling = c("pass"),
            .combine = rbind,
            .inorder = F
    ) %dopar% {
      tryCatch({
        
        cat(paste0("Predict line ",n," textID:", y, "\n"))
        pred_max <- predict(m, newdata = dplyr::select(x, -all_ngrams)) %>% which.max()
        tibble(textID = y, selected_text = x$all_ngrams[pred_max]) 
        
      }, error = function(e){print(paste0("Error in tweet: ", y, "\n",
                                          "caused the error: '", e, "'\n")); NA})
      
    }
  cat(Sys.time() - t0)
  stopCluster(cl)
  
}

test_predict


# Prepare submission --------------------------------------------------------------------------

submission <- read_csv("data/sample_submission.csv")

sub <- 
  test_predict  %>%
  bind_rows(transmute(test_neutral, textID, selected_text = text))

submission %>% 
  select(-selected_text) %>% 
  left_join(sub)  
# %>% write_csv("submission.csv")











# rascunho fellipe ----------------------------------------------------------------------------

# rascunho modelo de regressao ---
# train_model <- 
#   train_parsed %>% 
#   select(-textID, -sel_text, -text, -all_ngrams)
# 
# library(caret)
# regressControl  <- trainControl(method="cv",
#                                 # repeats = 3,
#                                 number = 2,
#                                 verbose = T
# ) 
# 
# regress <- train(jaccard ~ .,
#                  data = na.omit(train_model),
#                  method  = "lm",
#                  trControl = regressControl)
# 
# summary(regress$finalModel)


# vader::getVader(train_pp$text[1])
# 
# 
# 
# # Selecionar o maior twitter de uma amostra aleatoria:
# ind <- sample(1:2, size = nrow(train_pp), replace = T, prob = c(.005, .95))
# res <- parallel::mclapply(train_pp$text[ind == 1], ntoken, mc.cores = 4)
# ind_max <- which.max(unlist(res))
# 
# # selecionado:
# train_pp[ind_max, ]$text
# train_pp[ind_max, ]$selected_text
# train_pp[ind_max, ]$sentiment
# 
# # para o texto amostrado
# results <- make_dataset(train_pp = train_pp[ind_max, ], train_pp[ind_max, ]$textID)
# 
# # Combinar metadata com parsed
# left_join(
#   train_parsed %>% select(-text_sentiment),
#   train_metadata, by = c( "textID", "text", "sel_text")
# ) %>% View()
# 
# 
# mean(train_metadata$text_n_words)
# # 15
# 
# pos_text <- 
#   tibble(x = str_split(text, " ")[[1]] %>% .[. %in% hash_emoticons$x]) %>% 
#   left_join(hash_emoticons[x %in% emoticon,], by = "x")
# 
# substr(text, 4, 4) <- "t"
# 
# text = "goddamn dust out -), but =) I wore ;) out a clip on the camera panel so I had to glue it shut"
# pos_emoticon <- 
# pos_emoticon <- str_split(text, " ")[[1]] %>% {hash_emoticons[. %in% hash_emoticons$x,]}
# 
# text_splited <- str_split(text, " ")[[1]]
# emoticon_found <- hash_emoticons[hash_emoticons$x %in%text_splited]



