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
library(GGally)
library(patchwork)
# devtools::install("../input/r-textfeatures-package/textfeatures/")
library(textfeatures)        
library(h2o)        
theme_set(theme_bw()) # set theme
# h2o.shutdown()

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
  
  x <- x %>% str_trim() %>% str_squish()
  
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
      # n_stopword = map_chr(str_to_lower(text), ~sum(str_detect(.x, paste0("\\b", stopwords(), "\\b")))),
      n_title = str_count(text, "([A-Z][^\\s]*)"),
      # mean word len
      len_text = str_length(text),
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

plot_model <- function(results){
  
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
    ggplot(aes(y = predict, x = observed))+
    geom_point()+
    geom_smooth(method = "loess")+
    geom_abline(intercept=0, slope = 1, color="red", linetype="dashed")
}

to_search <- function(x){
  str_replace_all(x, "([[:punct:]]|\\*|\\+|\\^|\\_|\\:|\\.{1,}|\\$|\\?|\\|)", "\\\\\\1")
}
# Load data -----------------------------------------------------------------------------------
train_data <- read_csv("data/train.csv")

train_data <- train_data %>% 
  mutate(sentiment = case_when(sentiment == "negative"~-1,
                               sentiment == "neutral"~0,
                               sentiment == "positive"~1) )


# remove na
train_data <- 
  train_data %>% 
  filter(!is.na(text) | text == "") %>% 
  rename(sel_text = selected_text) 
  
# remove bad text (any sel text not in)
train_data <- train_data %>% 
  mutate(bad_text = map2_lgl(map(sel_text, ~str_split(.x, " ")[[1]]), 
                             map(text, ~str_split(.x, " ")[[1]]),
                             ~.x %in% .y %>% {sum(.)/length(.) != 1}) ) %>% 
  filter(bad_text != T) %>% 
  select(-bad_text)

prop.table(table(train_data$sentiment)) 

# make y
train_data <- 
  train_data %>% 
  mutate(text_s = map(text, ~.x %>% str_split(" ") %>% .[[1]]),
         sel_text_s = map(sel_text, ~.x %>% str_split(" ") %>% .[[1]]),
         pred_position = map2(text_s, sel_text_s,
                               ~{
                                 which(.x %in% .y) %>% 
                                   {split(., cumsum(c(1, diff(.) != 1)))} %>% {
                                     g <- map(., length) %>% which.max()
                                     .[g]
                                   } %>% unlist()
                               }),
         start = map_dbl(pred_position, ~range(.x)[1]),
         end = map_dbl(pred_position, ~range(.x)[2]),
         pred_text = map2_chr(pred_position, text_s,
                              ~{
                                .y[.x] %>% paste(collapse = " ")
                              })) %>% 
  mutate(len = end - start)

train_data <- 
  train_data %>% 
  mutate(jaccard = map2_dbl(sel_text, pred_text, ~jaccard(.x, .y)))

mean(train_data$jaccard)
train_data %>% group_by(sentiment) %>% summarise(jaccard = mean(jaccard))

train_data <- train_data %>% select(-text_s, -sel_text_s, -pred_position, -pred_text, -jaccard)

# train_neutral <- train_data %>% filter(sentiment == "neutral")
# train_data <- train_data %>% filter(sentiment != "neutral")

train_data$start <- NULL

# split valid data
samp <- sample(1:2,nrow(train_data), T, c(0.9, 0.1))
valid_data <- train_data[samp == 2,]
train_data <- train_data[samp == 1,]

# valid_data$end <- log( (valid_data$end - mean(train_data$end)) / sd(train_data$end) + 5)
# train_data$end <- log( (train_data$end - mean(train_data$end)) / sd(train_data$end) + 5)

# train_data$end <- log(train_data$end)
# valid_data$end <- log(valid_data$end)
# Feature engineering (train) -----------------------------------------------------------------

## Metadata
metadata <- get_metadata(x=train_data$text)

# remove %zero > 0.95
to_remove <- metadata %>% map_dbl(~ sum(.x == 0) / length(.x)) %>% {which(. > .95)} %>% names()
# dicotomize 0.80 %zero < 0.95
to_cat <- metadata %>% map_dbl(~ sum(.x == 0) / length(.x)) %>% {which(. > .8 & . < .95)}  %>% names()

# pre processing
metadata <- metadata %>% select(-one_of(to_remove)) %>% mutate_at(to_cat, ~if_else(.x == 0, 0, 1))

# bind cols with original texts:
train_data <- bind_cols(train_data, metadata)

# Feature engineering (valid) -----------------------------------------------------------------

## Metadata
metadata <- get_metadata(x=valid_data$text)

# pre processing
metadata <- metadata %>% select(-one_of(to_remove)) %>% mutate_at(to_cat, ~if_else(.x == 0, 0, 1))

# bind cols with original texts:
valid_data <- bind_cols(valid_data, metadata)

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
  mutate(text_clean = clean_text(text))%>% 
  select(-textID, -text, -sel_text) %>% 
  as.h2o()

valid_h2o <- 
  valid_data %>%
  mutate(text_clean = clean_text(text))%>% 
  select(-textID, -text, -sel_text) %>% 
  as.h2o()

words_train_h2o <- h2o.tokenize(train_h2o$text_clean, " ")
words_valid_h2o <- h2o.tokenize(valid_h2o$text_clean, " ")

set.seed(1)
w2v.model <- h2o.word2vec(words_train_h2o,vec_size = 20, sent_sample_rate = 0, epochs = 50)

vecs_train_h2o <- h2o.transform(w2v.model, words_train_h2o, aggregate_method = "AVERAGE")
vecs_valid_h2o <- h2o.transform(w2v.model, words_valid_h2o, aggregate_method = "AVERAGE")

ind_ok <-  !is.na(vecs_train_h2o$C1) # remove na for train_h2o

vecs_train_h2o <- h2o.cbind(train_h2o[ind_ok, setdiff(colnames(train_h2o), c("text_clean"))], vecs_train_h2o[ind_ok,])
vecs_valid_h2o <- h2o.cbind(valid_h2o[, setdiff(colnames(valid_h2o), c("text_clean"))], vecs_valid_h2o)


# Models --------------------------------------------------------------------------------------

train_data %>% 
  # mutate_at(c("end", "len"), ~log(.x+0.5)) %>% 
  select(end, len) %>% 
  ggpairs(upper = list(continuous = wrap("cor", method = "spearman")))

# model to end ----
x <- setdiff(names(vecs_train_h2o), c("len", "end"))
y <- "end"

# Random Forest
search_criteria <- list(strategy = "RandomDiscrete", 
                         # max_runtime_secs = 60*30*1, 
                         max_models = 5,
                         seed = 1)

hyper_grid_rf <- list(ntrees = seq(50, 500, by = 100),
                       mtries = seq(3, 5, by = 1),
                       max_depth = seq(10, 30, by = 10),
                       min_rows = seq(1, 3, by = 1),
                       nbins = seq(20, 30, by = 10),
                       sample_rate = c(0.55, 0.632, 0.75))

grid_rf <- h2o.grid("randomForest",
                      x = x, y = y,
                      training_frame = vecs_train_h2o,
                      nfolds = 7,
                      hyper_params = hyper_grid_rf,
                      search_criteria = search_criteria)




model_rf <- h2o.randomForest(x = x, y = y,
                             training_frame = vecs_train_h2o,
                             nfolds = 10, seed = 1)
h2o.r2(model_rf)

perf_rf <- h2o.performance(model = model_rf, newdata = vecs_valid_h2o)
h2o.r2(perf_rf)

predict(model_rf, vecs_valid_h2o) %>% 
  as_tibble() %>% 
  bind_cols(valid_data %>% select(observed = end)) %>% 
  plot_model()

# XGBoost
model_xgboost <- h2o.xgboost(x = x, y = y,
                             training_frame = vecs_train_h2o,
                             nfolds = 10, 
                             seed = 1
                             )

h2o.r2(model_xgboost)

perf_xgboost <- h2o.performance(model = model_xgboost, newdata = vecs_valid_h2o)
h2o.r2(perf_xgboost)

predict(model_xgboost, vecs_valid_h2o) %>% 
  as_tibble() %>% 
  bind_cols(valid_data %>% select(observed = end)) %>% 
  plot_model()




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

search_criteria1 <- list(strategy = "RandomDiscrete", 
                        # max_runtime_secs = 60*30*1, 
                        max_models = 10,
                        seed = 1,
                        )


## Hyper-Parameter Search
hyper_params1 <- list(ntrees = seq(100, 1000, 100),
                     learn_rate = seq(0.0001, 0.2, 0.0001),
                     max_depth = seq(1, 20, 1),
                     sample_rate = seq(0.5, 1.0, 0.0001),
                     col_sample_rate = seq(0.8, 1.0, 0.1)
                     )

# col_sample_rate learn_rate max_depth ntrees sample_rate
# 1          0.2816     0.0589         8    209      0.5059

# col_sample_rate learn_rate max_depth ntrees sample_rate
# 1          0.8262     0.0855         1    947      0.9055

# Train and validate a grid of GBMs
gbm.grid1 <- h2o.grid("xgboost",
                     x = x, y = y,
                     training_frame = vecs_train_h2o,
                     nfolds = 5,
                     distribution="gaussian",
                     stopping_metric = "deviance",
                     hyper_params = hyper_params1,
                     search_criteria = search_criteria1)

# Get the grid results, sorted by validation r2
# gbm_gridperf0 <- gbm_gridperf1
gbm_gridperf1 <- h2o.getGrid(grid_id = gbm.grid1@grid_id, 
                             sort_by = "r2", 
                             decreasing = TRUE)

# evalue
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
h2o.r2(best_gbm1)

best_gbm_perf1 <- h2o.performance(model = best_gbm1, newdata = vecs_valid_h2o)
h2o.r2(best_gbm_perf1)


predict(best_gbm1, vecs_valid_h2o) %>% 
  as_tibble() %>% 
  bind_cols(valid_data %>% select(observed = end)) %>% 
  plot_model()

# model to len ----
ifelse(train_data$len == 0, 0, 1) %>% table() %>% prop.table() 

library(patchwork)

g1 <- train_data %>% 
  ggplot(aes(x = len))+
  geom_histogram(aes(y = ..density..),alpha = .5, )+
  geom_density(alpha = .5)

g2 <- train_data %>% 
  filter(len != 0) %>% 
  ggplot(aes(x = len))+
  geom_histogram(aes(y = ..density..),alpha = .5, )+
  geom_density(alpha = .5)

g3 <- 
  train_data %>% 
  mutate(len = if_else(len == 0, 0, 1)) %>% 
  count(len) %>%
  ggplot(aes(x = len, y = n))+
  geom_bar(stat = "identity", alpha = 0.5)


(g1/g2) | (g3)

## GLM --

glm_model <- h2o.glm(x = x, y = y,
                     training_frame = vecs_train_h2o, 
                     standardize=TRUE,
                     family = "tweedie",
                     tweedie_variance_power = 0,
                     tweedie_link_power = 0,
                     lambda = 0,
                     # lambda_search = T,
                     score_each_iteration = T,
                     early_stopping = F,
                     max_runtime_secs = 0,
                     # ignore_const_cols = FALSE,
                     nfolds = 10)

# evalue
h2o.r2(glm_model)

best_glm_perf <- h2o.performance(model = glm_model, newdata = vecs_valid_h2o)
h2o.r2(best_glm_perf)

predict(glm_model, vecs_valid_h2o) %>% 
  as_tibble() %>% 
  bind_cols(valid_data %>% select(observed = len)) %>% 
  plot_model()

# Random Forest --

search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_runtime_secs = 60*30*1, seed = 1)

## Hyper-Parameter Search
hyper_params2 <- list(ntrees = seq(100, 1000, by = 100),
                         mtries = seq(3, 5, by = 1),
                         max_depth = seq(10, 30, by = 10),
                         min_rows = seq(1, 3, by = 1),
                         nbins = seq(20, 30, by = 10),
                         sample_rate = c(0.55, 0.632, 0.75)
                      )

x <- setdiff(names(vecs_train_h2o), c("len", "end"))
y <- "len"
# Train and validate a grid of GBMs
gbm.grid2 <- h2o.grid("randomForest",
                     x = x, y = y,
                     training_frame = vecs_train_h2o,
                     nfolds = 5,
                     # distribution="gamma",
                     stopping_metric = "deviance",
                     hyper_params = hyper_params2,
                     score_each_iteration = T,
                     search_criteria = search_criteria2, 
                     score_each_iteration = TRUE)

# Get the grid results, sorted by validation r2
# gbm_gridperf0 <- gbm_gridperf1
gbm_gridperf2 <- h2o.getGrid(grid_id = gbm.grid2@grid_id, 
                             sort_by = "r2", 
                             decreasing = TRUE)

# evalue
best_gbm2 <- h2o.getModel(gbm_gridperf2@model_ids[[1]])
h2o.r2(best_gbm2)

best_gbm_perf2 <- h2o.performance(model = best_gbm2, newdata = vecs_valid_h2o)
h2o.r2(best_gbm_perf2)


predict(best_gbm2, vecs_valid_h2o) %>% 
  as_tibble() %>% 
  bind_cols(valid_data %>% select(observed = len)) %>% 
  plot_model()

# Combine results start + len -----------------------------------------------------------------

results <- bind_cols(
  predict(best_gbm1, vecs_valid_h2o) %>% as_tibble() %>% transmute(end = round(predict)),
  predict(best_gbm2, vecs_valid_h2o) %>% as_tibble() %>% transmute(len = round(predict))
) %>% 
  mutate(start = end - len,
         start = ifelse(start <= 0, 1, 1),
         range = map2(start, end, ~.x:.y))

valid_data <- 
  valid_data %>% 
  bind_cols(results) %>% 
  mutate(pred_text = map2_chr(text, range, ~str_split(.x, " ")[[1]][.y] %>% paste(collapse = " ")))

valid_data <- valid_data %>% 
  mutate(jaccard = map2_dbl(sel_text, pred_text, ~jaccard(.x, .y)))

mean(valid_data$jaccard)

# valid_data %>% select(text, sel_text, pred_text, jaccard) %>% View()


# Submit --------------------------------------------------------------------------------------


submission <- read.csv("data/sample_submission.csv")
submission$selected_text <- valid_data$pred_text
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
