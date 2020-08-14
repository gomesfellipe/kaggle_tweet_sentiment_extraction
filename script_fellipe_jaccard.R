# Dependencies --------------------------------------------------------------------------------

library(readr)      # read/write
library(dplyr)      # manipulate data
library(tidyr)      # tidy data
library(purrr)      # functional programming
library(stringr)    # text manipulation
library(qdapRegex)  # easy regex
library(tm)         # text mining
library(tidytext)   # text mining
library(ggplot2)
library(patchwork)
# devtools::install("../input/r-textfeatures-package/textfeatures/")
library(textfeatures)        
library(h2o)        
theme_set(theme_bw()) # set theme

# Functions -----------------------------------------------------------------------------------

jaccard <- function(str1, str2) {
  # r version for: https://www.kaggle.com/c/tweet-sentiment-extraction/overview/evaluation
  a <- unlist(strsplit(tolower(str1), split = " "))
  b <- unlist(strsplit(tolower(str2), split = " "))
  c <- intersect(a, b)
  length(c) / (length(a) + length(b) - length(c))
}

logit <- function(x) {
  x <- case_when(x == 0 ~.Machine$double.eps,
                 x == 1 ~ 1-.Machine$double.eps^(.4),
                 x > 0 & x < 1 ~ x)
  log(x / (1 - x))
}

clean_text <- function(x, stem = F) {
  
  x %>%
    str_remove_all("\\'\\w") %>%
    str_remove_all("\\n") %>%
    str_remove_all("\\&quot\\;") %>%
    str_remove_all("(RT|via)((?:\\b\\W*@\\w+)+)") %>%
    rm_date() %>% 
    rm_dollar() %>% 
    rm_angle() %>% 
    rm_email() %>% 
    #    rm_city_state_zip() %>% 
    rm_endmark() %>%
    rm_hash() %>% 
    rm_number() %>% 
    rm_percent() %>% 
    rm_phone() %>% 
    rm_tag() %>% 
    rm_time() %>% 
    str_to_lower() %>%
    str_remove_all("(http://.*?\\s)|(http://.*)") %>%
    str_remove_all("@\\w+") %>%
    str_replace_all('([[:alpha:]])\\1{2,}', "\\1") %>%
    str_remove_all("[[:digit:]]") %>% 
    str_replace_all("(?! )[^[:alnum:]]", " ") %>%
    str_remove_all("\\bh[aeiou]h[aeiou]{1,}\\b") %>%
    removeWords(stopwords() %>% .[!. %in% c('no', 'nor', 'not')]) %>% 
    # str_remove_all("\\b\\w{1,2}\\b") %>% 
    stringi::stri_trans_general(id = "Latin-ASCII") %>% 
    str_remove_all("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘") %>% 
    str_trim() %>%
    str_squish()
  
}

get_metadata <- function(x){
  t0 <- Sys.time() # to print time
  cat("Getting metadata, please wait ..\n")
  
  # get metadata with `textfeatures`
  metadata <- textfeatures::textfeatures(x, normalize = F, word_dims = 0)
  
  # discart default n_words and n_uq_words
  metadata <- metadata %>% select(-n_words, -n_uq_words)
  
  # more features
  metadata <- 
    tibble(text = x) %>% 
    rowwise() %>% 
    mutate(
      n_words = length(str_split(text, pattern = " ")[[1]]),
      n_uq_words = length(unique(str_split(text, pattern = " ")[[1]]))) %>% 
    ungroup() %>% 
    transmute(
      len = str_length(text),
      # n_upper = str_count(text, "[A-Z]"), # n_caps
      n_neg = str_count(str_to_lower(text), "(\\bno+\\b|\\bnor+\\b|\\bnot+\\b|n\\'t\\b)"), # negatives
      n_atpeople = str_count(text, "@\\w+"),
      n_question = str_count(text, "\\?+"),
      # n_dot = str_count(text, "\\.+"), # n_period
      n_retweet = str_count(text, "(RT|via)((?:\\b\\W*@\\w+)+)")
    ) %>% 
    bind_cols(metadata)
  
  # combine plural person in metadata
  metadata <- metadata %>% 
    mutate(n_first_person = n_first_person + n_first_personp,
           n_second_person = n_second_person + n_second_personp) %>% 
    select(-n_first_personp, -n_second_personp)
  
  cat(paste0("Metadata successfully obtained!\nThe process took: ",
             round(difftime(Sys.time(), t0, units = "mins")) ," min\n")) # Yeah!
  
  return(metadata)
}

# some interaction columns
parse_metadata <- function(metadata){
  metadata %>% 
    transmute(
      # text stats
      text_len,
      text_n_lowersp,
      text_n_capsp,
      text_n_charsperword,
      # sel_text stats        
      sel_text_len,
      sel_text_n_lowersp,
      sel_text_n_capsp,
      sel_text_n_charsperword,
      # interaction sel_text x text
      sd_sel_text_sent_afinn        = text_sent_afinn - sel_text_sent_afinn,
      sd_sel_text_sent_bing         = text_sent_bing - sel_text_sent_bing,
      sd_sel_text_sent_syuzhet      = text_sent_syuzhet - sel_text_sent_syuzhet,
      sd_sel_text_sent_vader        = text_sent_vader - sel_text_sent_vader,
      sd_sel_text_n_polite          = text_n_polite - sel_text_n_polite,
      prop_sel_text_len             = sel_text_len / text_len,
      prop_sel_text_n_chars         = if_else(text_n_chars == 0, 0, sel_text_n_chars / text_n_chars),
      prop_sel_text_n_uq_chars      = if_else(text_n_uq_chars == 0, 0, sel_text_n_uq_chars / text_n_uq_chars),
      prop_sel_text_n_lowers        = if_else(text_n_extraspaces == 0, 0, sel_text_n_extraspaces / text_n_extraspaces),
      prop_sel_text_n_caps          = if_else(text_n_caps == 0, 0, sel_text_n_caps / text_n_caps),
      prop_sel_text_n_neg           = if_else(text_n_neg == 0, 0, sel_text_n_neg / text_n_neg),
      prop_sel_text_n_periods       = if_else(text_n_periods == 0, 0, sel_text_n_periods / text_n_periods),
      prop_sel_text_n_commas        = if_else(text_n_commas == 0, 0, sel_text_n_commas / text_n_commas),
      prop_sel_text_n_question      = if_else(text_n_question == 0, 0, sel_text_n_question / text_n_question),
      prop_sel_text_n_exclaims      = if_else(text_n_exclaims == 0, 0, sel_text_n_exclaims / text_n_exclaims),
      prop_sel_text_n_puncts        = if_else(text_n_puncts == 0, 0, sel_text_n_puncts / text_n_puncts),
      prop_sel_text_n_digits        = if_else(text_n_digits == 0, 0, sel_text_n_digits / text_n_digits),
      prop_sel_text_n_extraspaces   = if_else(text_n_extraspaces == 0, 0, sel_text_n_extraspaces / text_n_extraspaces),
      prop_sel_text_n_tobe          = if_else(text_n_tobe == 0, 0, sel_text_n_tobe / text_n_tobe),
      prop_sel_text_n_prepositions  = if_else(text_n_prepositions == 0, 0, sel_text_n_prepositions / text_n_prepositions),
      prop_sel_text_n_first_person  = if_else(text_n_first_person == 0, 0, sel_text_n_first_person / text_n_first_person),
      prop_sel_text_n_second_person = if_else(text_n_second_person == 0, 0, sel_text_n_second_person / text_n_second_person),
      prop_sel_text_n_third_person  = if_else(text_n_third_person == 0, 0, sel_text_n_third_person / text_n_third_person),
      # dif_text stats
      dif_text_len,
      dif_text_n_lowersp,
      dif_text_n_capsp,
      dif_text_n_charsperword,
      # interaction dif_text x text
      sd_dif_text_sent_afinn        = text_sent_afinn - dif_text_sent_afinn,
      sd_dif_text_sent_bing         = text_sent_bing - dif_text_sent_bing,
      sd_dif_text_sent_syuzhet      = text_sent_syuzhet - dif_text_sent_syuzhet,
      sd_dif_text_sent_vader        = text_sent_vader - dif_text_sent_vader,
      sd_dif_text_n_polite          = text_n_polite - dif_text_n_polite,
      prop_dif_text_len             = dif_text_len / text_len,
      prop_dif_text_n_chars         = if_else(text_n_chars == 0, 0, dif_text_n_chars / text_n_chars),
      prop_dif_text_n_uq_chars      = if_else(text_n_uq_chars == 0, 0, dif_text_n_uq_chars / text_n_uq_chars),
      prop_dif_text_n_lowers        = if_else(text_n_extraspaces == 0, 0, dif_text_n_extraspaces / text_n_extraspaces),
      prop_dif_text_n_caps          = if_else(text_n_caps == 0, 0, dif_text_n_caps / text_n_caps),
      prop_dif_text_n_neg           = if_else(text_n_neg == 0, 0, dif_text_n_neg / text_n_neg),
      prop_dif_text_n_periods       = if_else(text_n_periods == 0, 0, dif_text_n_periods / text_n_periods),
      prop_dif_text_n_commas        = if_else(text_n_commas == 0, 0, dif_text_n_commas / text_n_commas),
      prop_dif_text_n_question      = if_else(text_n_question == 0, 0, dif_text_n_question / text_n_question),
      prop_dif_text_n_exclaims      = if_else(text_n_exclaims == 0, 0, dif_text_n_exclaims / text_n_exclaims),
      prop_dif_text_n_puncts        = if_else(text_n_puncts == 0, 0, dif_text_n_puncts / text_n_puncts),
      prop_dif_text_n_digits        = if_else(text_n_digits == 0, 0, dif_text_n_digits / text_n_digits),
      prop_dif_text_n_extraspaces   = if_else(text_n_extraspaces == 0, 0, dif_text_n_extraspaces / text_n_extraspaces),
      prop_dif_text_n_tobe          = if_else(text_n_tobe == 0, 0, dif_text_n_tobe / text_n_tobe),
      prop_dif_text_n_prepositions  = if_else(text_n_prepositions == 0, 0, dif_text_n_prepositions / text_n_prepositions),
      prop_dif_text_n_first_person  = if_else(text_n_first_person == 0, 0, dif_text_n_first_person / text_n_first_person),
      prop_dif_text_n_second_person = if_else(text_n_second_person == 0, 0, dif_text_n_second_person / text_n_second_person),
      prop_dif_text_n_third_person  = if_else(text_n_third_person == 0, 0, dif_text_n_third_person / text_n_third_person),
    )  
}

plot_model <- function(best_gbm){
  best_gbm_predict <- h2o.predict(object = best_gbm, newdata = vecs_valid_h2o)
  
  results <- 
    best_gbm_predict %>% 
    as_tibble() %>% 
    mutate(predict = predict) %>% 
    bind_cols(as_tibble(vecs_valid_h2o$jaccard))
  library(patchwork)
  (
    results %>% 
      gather(key, value) %>% 
      ggplot(aes(x = value, fill = key))+
      geom_density(alpha = .5)
    +
      results %>% 
      gather(key, value) %>% 
      ggplot(aes(y = value, x = key))+
      geom_boxplot(alpha = .5)
  ) /
    results %>% 
    ggplot(aes(y = predict, x = jaccard))+
    geom_point()+
    geom_smooth(method = "loess")+
    geom_abline(intercept=0, slope = 1, color="red", linetype="dashed")
}

# Load data -----------------------------------------------------------------------------------
train_data <- read_csv("data/train.csv")

# remove na
train_data <- train_data %>% filter(!is.na(text) | text == "")

# split valid data
samp <- sample(1:2,nrow(train_data), T, c(0.9, 0.1))
valid_data <- train_data[samp == 2,]
train_data <- train_data[samp == 1,]

# Feature engineering (train) -----------------------------------------------------------------

# create column dif_text and rename selected_text to sel_text
train_data <- train_data %>% 
  rename(sel_text = selected_text) %>% 
  mutate(dif_text = str_remove(text, str_replace_all(sel_text, "([[:punct:]]|\\*|\\+|\\.{1,}|\\:|\\|)", "\\\\\\1")))

## Metadata
metadata_text     <- get_metadata(x=train_data$text) %>% `colnames<-`(paste0("text_", colnames(.)))
metadata_sel_text <- get_metadata(x=train_data$sel_text) %>% `colnames<-`(paste0("sel_text_", colnames(.)))
metadata_dif_text <- get_metadata(x=train_data$dif_text) %>% `colnames<-`(paste0("dif_text_", colnames(.)))

# remove %zero > 0.95
to_remove <- metadata_text %>%map_dbl(~ sum(.x == 0) / length(.x)) %>% {which(. > .95)} %>% names() %>% str_remove("text_")
# dicotomize 0.80 %zero < 0.95
to_cat <- metadata_text %>% map_dbl(~ sum(.x == 0) / length(.x)) %>% {which(. > .8 & . < .95)} %>% names() %>% str_remove("text_")

# Bind cols `text_metadata`, `sel_metadata`, `dif_metadata` after initial clean
metadata <- 
  pmap_dfc(list(
    x = list(metadata_text, metadata_sel_text, metadata_dif_text),
    y = list(paste0("text_",to_remove), paste0("sel_text_",to_remove), paste0("dif_text_",to_remove)),
    z = list(paste0("text_",to_cat), paste0("sel_text_",to_cat), paste0("dif_text_",to_cat)) 
  ),
  function(x, y, z){
    x %>% select(-one_of(y)) %>% mutate_at(z, ~if_else(.x == 0, 0, 1))
  })

rm(metadata_text, metadata_sel_text, metadata_dif_text); gc()

# parse collected metadata (clean and get interactions)
metadata <- parse_metadata(metadata)

# bind cols with original texts:
train_data <- bind_cols(train_data, metadata)

rm(metadata); gc()

# Jaccard text x selected_text
train_data <- train_data %>% mutate(jaccard = map2_dbl(text, sel_text, ~ jaccard(.x, .y)))

# Feature engineering (valid) -----------------------------------------------------------------

# get all ngrams and dif text
pb <- progress_estimated(nrow(valid_data))
valid_data <- 
  valid_data %>% 
  group_by(textID) %>% 
  nest() %>% #pull(data) %>% .[[1]]
  mutate(data = map(data, ~{
    pb$tick()$print()
    tibble(
      text = .x$text,
      true_text = .x$selected_text,
      text_n_words = str_split(.x$text, pattern = " ", )[[1]] %>% length(),
      sentiment = .x$sentiment,
      sel_text = map2(1:text_n_words,text, ~ tau::textcnt(.y, method = "string", split = " ", n = .x, tolower = FALSE) %>% names()) %>% unlist()
    ) %>% 
      mutate(dif_text = str_remove(text, str_replace_all(sel_text, "([[:punct:]]|\\*|\\+|\\.{1,}|\\:|\\|)", "\\\\\\1")))
  })) %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

# Metadata
valid_data <- 
  valid_data %>% {
    
    metadata_text     <- get_metadata(x=.$text) %>% `colnames<-`(paste0("text_", colnames(.)))
    metadata_sel_text <- get_metadata(x=.$sel_text) %>% `colnames<-`(paste0("sel_text_", colnames(.)))
    metadata_dif_text <- get_metadata(x=.$dif_text) %>% `colnames<-`(paste0("dif_text_", colnames(.)))
    
    # Bind cols `text_metadata`, `sel_metadata`, `dif_metadata` after initial clean
    bind_cols(., 
              pmap_dfc(list(
                x = list(metadata_text, metadata_sel_text, metadata_dif_text),
                y = list(paste0("text_",to_remove), paste0("sel_text_",to_remove), paste0("dif_text_",to_remove)),
                z = list(paste0("text_",to_cat), paste0("sel_text_",to_cat), paste0("dif_text_",to_cat)) 
              ),
              function(x, y, z){
                x %>% select(-one_of(y)) %>% mutate_at(z, ~if_else(.x == 0, 0, 1))
              })
    )
  }

# parse collected metadata (clean and get interactions)
# and bind with original texts
valid_data <- 
  select(valid_data, textID, text, true_text, sel_text, dif_text, sentiment) %>% 
  bind_cols(parse_metadata(valid_data))

# Jaccard text x selected_text
valid_data <- valid_data %>% mutate(jaccard = map2_dbl(true_text, sel_text, ~ jaccard(.x, .y)))

# Check point ---------------------------------------------------------------------------------

# fast report
# DataExplorer::create_report(select(train_data, -starts_with("w")), y = "jaccard")

# saveRDS(train_data, "train_data.rds")
# train_data <- readRDS("train_data.rds")

# saveRDS(valid_data, "valid_data.rds")
# valid_data <- readRDS("valid_data.rds")

# h2o word2vec --------------------------------------------------------------------------------

h2o.init(nthreads=-1, max_mem_size="8g")
# h2o.no_progress() # Turn off progress bars
n_cores = NULL
# h2o.shutdown()

# clean_text and covert to h2o
train_h2o <- 
  train_data %>%
  mutate(text_clean = clean_text(sel_text))%>% 
  select(-textID, -text, -sel_text, -dif_text) %>% 
  as.h2o()

valid_h2o <- 
  valid_data %>%
  mutate(text_clean = clean_text(sel_text))%>% 
  select(-textID, -text, -sel_text, -dif_text) %>% 
  as.h2o()

words_train_h2o <- h2o.tokenize(train_h2o$text_clean, " ")
words_valid_h2o <- h2o.tokenize(valid_h2o$text_clean, " ")

w2v.model <- h2o.word2vec(words_train_h2o,vec_size = 20, sent_sample_rate = 0, epochs = 50)

vecs_train_h2o <- h2o.transform(w2v.model, words_train_h2o, aggregate_method = "AVERAGE")
vecs_valid_h2o <- h2o.transform(w2v.model, words_valid_h2o, aggregate_method = "AVERAGE")

ind_ok <-  !is.na(vecs_train_h2o$C1) # remove na for train_h2o

vecs_train_h2o <- h2o.cbind(train_h2o[ind_ok, setdiff(colnames(train_h2o), c("text_clean"))], vecs_train_h2o[ind_ok,])
vecs_valid_h2o <- h2o.cbind(valid_h2o[, setdiff(colnames(valid_h2o), c("text_clean"))], vecs_valid_h2o)



# h2o xgboost ---------------------------------------------------------------------------------

# Tunning Sequencial

# -----------------+-----------------------------------------+
# Parameto         |Descricao                                |
# -----------------+-----------------------------------------+
# nrounds          |Numero de arvores, default: 50          |
# max_depth        |Profundidade máxima da árvore, default: 6|
# learn_rate       |Taxa de Aprendizagem, default: 0.3       |
# gamma            |Ajustar a Regularização, default: 0      |
# colsample_bytree |Amostragem em coluna, default: 1         |
# min_child_weight |Peso mínimo das folhas, default: 1       |
# subsample        |Amostragem de linha, default: 1          |
# -----------------+-----------------------------------------+

an <- function(x){as.numeric(x)}

search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 60*1, seed = 1)

hyper_params1 <- list(tree_method="hist",      # lightgbm
                      grow_policy="lossguide", # lightgbm
                      ntrees = seq(from = 50, to = 1000, by = 50),
                      learn_rate = c(0.01, 0.015, 0.025, 0.05, 0.1, 0.2, 0.3),
                      max_depth = c(3, 5, 9),
                      gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                      col_sample_rate = c(0.4, 0.6, 0.8, 1),
                      min_child_weight = c(1, 2, 3),
                      subsample = c(0.5, 0.75, 1.0)
)

# Train and validate a grid of GBMs
gbm_grid1 <- h2o.grid(algorithm = "xgboost",
                      x = setdiff(names(vecs_train_h2o), "jaccard") ,
                      y = "jaccard",
                      grid_id = "gbm_grid1",
                      training_frame = vecs_train_h2o, 
                      nfolds = 5,
                      seed = 1,
                      hyper_params = hyper_params1,
                      search_criteria = search_criteria,
                      stopping_metric = "deviance",
                      parallelism = NULL)

# Get the grid results, sorted by validation r2
gbm_gridperf1 <- h2o.getGrid(grid_id = gbm_grid1@grid_id, 
                             sort_by = "r2", 
                             decreasing = TRUE)

# evalue
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
h2o.r2(best_gbm1)

# Preparar dados de teste por texto
vecs_valid <- bind_cols(select(valid_data, textID), as_tibble(vecs_valid_h2o))
vecs_valid <- vecs_valid %>% group_by(textID) %>% nest() %>% ungroup


predict(best_gbm1, as.h2o(vecs_valid %>% filter(textID == "6903cb08f2") %>% pull(data) %>% .[[1]])) %>% as_tibble() %>% pull(predict) %>% which.max()
valid_data %>% select(textID,text, true_text, sel_text, jaccard) %>% filter(textID == "6903cb08f2") %>% View()



# best_gbm_perf1 <- h2o.performance(model = best_gbm1, newdata = vecs_valid_h2o)
# h2o.r2(best_gbm_perf1)
# plot_model(best_gbm1)
# 
# # save grid
# best_grid1 <- gbm_gridperf1@summary_table[1,]


# # 2 Tunning ----------------------------------------------------------------
# # - min_child_weight
# md <- as.numeric(best_grid1[,"max_depth"])
# hyper_params2 <- list(tree_method="hist", # lightgbm
#                       grow_policy="lossguide", # lightgbm
#                       ntrees = seq(from = 50, to = 1000, by = 50),
#                       learn_rate = best_grid1[,"learn_rate"],
#                       max_depth = if(md == 2) md:4 else (md-1):(md+1),
#                       # gamma = 0,
#                       col_sample_rate = 1,
#                       min_child_weight = c(1, 2, 3),
#                       subsample = 1
# )
# 
# # Train and validate a grid of GBMs
# gbm_grid2 <- h2o.grid(algorithm = "xgboost",
#                       x = setdiff(names(vecs_train_h2o), "jaccard") ,
#                       y = "jaccard",
#                       grid_id = "gbm_grid2",
#                       training_frame = vecs_train_h2o, 
#                       nfolds = 5,
#                       seed = 1,
#                       hyper_params = hyper_params2,
#                       search_criteria = search_criteria,
#                       stopping_metric = "deviance",
#                       parallelism = n_cores)
# 
# # Get the grid results, sorted by validation r2
# gbm_gridperf2 <- h2o.getGrid(grid_id = gbm_grid2@grid_id, 
#                              sort_by = "r2", 
#                              decreasing = TRUE)
# 
# # evalue
# best_gbm2 <- h2o.getModel(gbm_gridperf2@model_ids[[1]])
# best_gbm_perf2 <- h2o.performance(model = best_gbm2, newdata = vecs_valid_h2o)
# h2o.r2(best_gbm_perf2)
# plot_model(best_gbm2)
# 
# # save grid
# best_grid2 <- gbm_gridperf2@summary_table[1,]
# 
# # 3 Tunning ---------------------------------------------------------------
# # - colsample_bytree
# # - subsample
# hyper_params3 <- list(tree_method="hist", # lightgbm
#                       grow_policy="lossguide", # lightgbm
#                       ntrees = seq(from = 50, to = 1000, by = 50),
#                       learn_rate = best_grid2[,"learn_rate"],
#                       max_depth = best_grid2[,"max_depth"],
#                       # gamma = 0,
#                       col_sample_rate = c(0.4, 0.6, 0.8, 1),
#                       min_child_weight = best_grid2[,"min_child_weight"],
#                       subsample = c(0.5, 0.75, 1.0)
# )
# 
# # Train and validate a grid of GBMs
# gbm_grid3 <- h2o.grid(algorithm = "xgboost",
#                       x = setdiff(names(vecs_train_h2o), "jaccard") ,
#                       y = "jaccard",
#                       grid_id = "gbm_grid3",
#                       training_frame = vecs_train_h2o, 
#                       nfolds = 5,
#                       seed = 1,
#                       hyper_params = hyper_params3,
#                       search_criteria = search_criteria,
#                       stopping_metric = "deviance",
#                       parallelism = n_cores)
# 
# # Get the grid results, sorted by validation r2
# gbm_gridperf3 <- h2o.getGrid(grid_id = gbm_grid3@grid_id, 
#                              sort_by = "r2", 
#                              decreasing = TRUE)
# 
# # evalue
# best_gbm3 <- h2o.getModel(gbm_gridperf3@model_ids[[1]])
# best_gbm_perf3 <- h2o.performance(model = best_gbm3, newdata = vecs_valid_h2o)
# h2o.r2(best_gbm_perf3)
# plot_model(best_gbm3)
# 
# # save grid
# best_grid3 <- gbm_gridperf3@summary_table[1,]
# 
# # 4 Tunning ------------------------------------------------------------
# # - eta
# hyper_params4 <- list(ntrees = seq(from = 200, to = 1000, by = 100),
#                       learn_rate = c(0.01, 0.015, 0.025, 0.05, 0.1),
#                       max_depth = best_grid3[,"max_depth"],
#                       # gamma = best_grid3[,"gamma"],
#                       col_sample_rate = best_grid3[,"col_sample_rate"],
#                       min_child_weight = best_grid3[,"min_child_weight"],
#                       subsample = best_grid3[,"subsample"]
# )
# 
# # Train and validate a grid of GBMs
# gbm_grid4 <- h2o.grid(algorithm = "xgboost",
#                       x = setdiff(names(vecs_train_h2o), "jaccard") ,
#                       y = "jaccard",
#                       grid_id = "gbm_grid4",
#                       training_frame = vecs_train_h2o, 
#                       nfolds = 5,
#                       seed = 1,
#                       hyper_params = hyper_params4,
#                       search_criteria = search_criteria,
#                       stopping_metric = "deviance",
#                       parallelism = n_cores)
# 
# # Get the grid results, sorted by validation r2
# gbm_gridperf4 <- h2o.getGrid(grid_id = gbm_grid4@grid_id, 
#                              sort_by = "r2", 
#                              decreasing = TRUE)
# 
# # Now let's evaluate the model performance on a test set
# # so we get an honest estimate of top model performance
# 
# best_gbm4 <- h2o.getModel(gbm_gridperf4@model_ids[[1]])
# best_gbm_perf4 <- h2o.performance(model = best_gbm4, newdata = vecs_valid_h2o)
# h2o.r2(best_gbm_perf4)
# plot_model(best_gbm4)
# 
# # Grab the top GBM model, chosen by validation AUC
# best_grid4 <- gbm_gridperf4@summary_table[1,]
# 
# # 5 Tunning ----------------------------------------------------------
# # - min_child_weight
# md <- as.numeric(best_grid4[,"max_depth"])
# hyper_params5 <- list(tree_method="hist", # lightgbm
#                       grow_policy="lossguide", # lightgbm
#                       ntrees = best_grid4[,"ntrees"],
#                       learn_rate = best_grid4[,"learn_rate"],
#                       max_depth = if(md == 2) md:4 else (md-1):(md+1),
#                       # gamma = 0,
#                       col_sample_rate = best_grid4[,"col_sample_rate"],
#                       min_child_weight = c(1, 2, 3),
#                       subsample = best_grid4[,"subsample"]
# )
# 
# # Train and validate a grid of GBMs
# gbm_grid5 <- h2o.grid(algorithm = "xgboost",
#                       x = setdiff(names(vecs_train_h2o), "jaccard") ,
#                       y = "jaccard",
#                       grid_id = "gbm_grid5",
#                       training_frame = vecs_train_h2o, 
#                       nfolds = 5,
#                       seed = 1,
#                       hyper_params = hyper_params5,
#                       search_criteria = search_criteria,
#                       stopping_metric = "deviance",
#                       parallelism = n_cores)
# 
# # Get the grid results, sorted by validation r2
# gbm_gridperf5 <- h2o.getGrid(grid_id = gbm_grid5@grid_id, 
#                              sort_by = "r2", 
#                              decreasing = TRUE)
# 
# # evalue
# best_gbm5 <- h2o.getModel(gbm_gridperf5@model_ids[[1]])
# best_gbm_perf5 <- h2o.performance(model = best_gbm5, newdata = vecs_valid_h2o)
# h2o.r2(best_gbm_perf5)
# plot_model(best_gbm5)
# 
# # save grid
# best_grid5 <- gbm_gridperf5@summary_table[1,]
# saveRDS(best_gbm5, "best_gbm5.rds")
# Beta Model ----------------------------------------------------------------------------------

# # fix jaccard to beta:
# train_data <- train_data %>%
#   mutate(jaccard = case_when(jaccard == 0 ~.Machine$double.eps,
#                              jaccard > 0 & jaccard <= 1 ~ jaccard))
# 
# set.seed(3) # reprodutibility
# n = nrow(train_data)
# samp <- sample(1:2, n, T, c(0.7, 0.3))
# 
# valid_metadata <- train_data[samp == 2,] %>% select(-textID, -text, -sel_text, -dif_text)
# train_metadata <- train_data[samp == 1,] %>% select(-textID, -text, -sel_text, -dif_text)
# 
# m0 <- gamlss::gamlss(jaccard ~., data = filter(train_metadata, sentiment =="negative") %>% select(-sentiment), family = gamlss.dist::BEINF1())
# plot(m0)
# gamlss::Rsq(m0)
# summary(m0)
# 
# train_metadata1 <- train_metadata %>% select(-text_len, -sel_text_len, -sd_sel_text_sent_syuzhet, -sd_sel_text_n_polite, -prop_sel_text_n_lowers, -prop_sel_text_n_question, -prop_sel_text_n_first_person, -prop_sel_text_n_second_person, -prop_sel_text_n_third_person, -dif_text_len, -sd_dif_text_sent_afinn, -sd_dif_text_sent_bing, -sd_dif_text_sent_vader, -prop_dif_text_n_uq_chars, -prop_dif_text_n_caps, -prop_dif_text_n_neg, -prop_dif_text_n_prepositions)
# 
# m1 <- gamlss::gamlss(jaccard ~., data = train_metadata1, family = gamlss.dist::BEINF1())
# plot(m1)
# gamlss::Rsq(m1)
# summary(m1)

# # h2o autoML ----------------------------------------------------------------------------------
# 
# library(h2o)
# h2o.init(nthreads=-1, max_mem_size="8g")
# # h2o.no_progress() # Turn off progress bars
# # h2o.show_progress() # turn ok
# n_cores = NULL
# # h2o.shutdown()
# 
# # Partition the data into training, validation and test sets
# train_h2o <- as.h2o(select(train_data, -textID, -text,-sel_text, -dif_text))
# splits <- h2o.splitFrame(data = train_h2o, ratios = c(0.7, 0.15), seed = 1)
# 
# train <- splits[[1]]
# valid <- splits[[2]]
# test <- splits[[3]]
# 
# # Set names for h2o
# y <- "jaccard"
# x <- setdiff(names(train), y)
# tictoc::tic()
# 
# # linear regression model used, but can use any model
# automl_models_h2o <- h2o.automl(
#   x = x,
#   y = y,
#   training_frame = train,
#   validation_frame = valid,
#   leaderboard_frame = test,
#   max_runtime_secs = (60*60*1),
#   stopping_metric = "deviance")
# tictoc::toc()
# 
# # Extract leader model
# automl_leader <- automl_models_h2o@leader
# 
# h2o.r2(automl_leader) # train
# perf <- h2o.performance(automl_leader, newdata = test)
# 
# h2o.r2(perf) # test
# pred_h2o <- h2o.predict(automl_leader, newdata = test)
# 
# # Investigate test error
# error_tbl <- as_tibble(test) %>% select(jaccard) %>%
#   tibble::add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
#   rename(actual = jaccard) %>%
#   mutate(
#     error     = actual - pred,
#     error_pct = error / actual
#   )
# error_tbl
# 
# error_tbl %>%
#   summarise(
#     me   = mean(error),
#     rmse = mean(error^2)^0.5,
#     mae  = mean(abs(error)),
#     mape = mean(abs(error_pct)),
#     mpe  = mean(error_pct)
#   ) %>%
#   glimpse()
