# Dependencies --------------------------------------------------------------------------------

library(caret)
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
library(doParallel)
library(foreach)
# library(h2o)        
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
    str_replace_all("(\\')(\\w)", "\\2") %>%
    str_remove_all("\\n") %>%
    str_remove_all("\\&quot\\;") %>%
    str_remove_all("(RT|via)((?:\\b\\W*@\\w+)+)") %>%
    rm_date() %>% 
    rm_dollar() %>% 
    rm_angle() %>% 
    rm_email() %>% 
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

get_metadata <- function(x, verbose = F){
  
  if(verbose == T){
    t0 <- Sys.time() # to print time
    cat("Getting metadata, please wait ..\n")  
  }
  
  # get metadata with `textfeatures`
  metadata <- textfeatures::textfeatures(x, normalize = F, word_dims = 0,verbose = verbose)
  
  # discart default n_words and n_uq_words
  metadata <- metadata %>% select(-n_words, -n_uq_words)
  
  # more features
  # quantas ngrams possiveis?
  # qual ngram antes e qual depois
  
  metadata <- 
    tibble(text = x) %>% 
    rowwise() %>% 
    mutate(
      n_words = length(str_split(text, pattern = " ")[[1]]),
      n_uq_words = length(unique(str_split(text, pattern = " ")[[1]]))) %>% 
    ungroup() %>% 
    transmute(
      n_vogals = str_count(str_to_lower(text), "[aeiou]"),
      n_consonants = str_count(str_to_lower(text), "[bcdfghjklmnpqrstvwxyz]"),
      n_str = str_length(text),
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
  
  if(verbose == T){
    cat(paste0("Metadata successfully obtained!\nThe process took: ",
               round(difftime(Sys.time(), t0, units = "mins")) ," min\n")) # Yeah!  
  }  
  
  
  return(metadata)
}

plot_model <- function(results){
  
  # library(patchwork)
  # (
    results %>% 
      gather(key, value) %>% 
      ggplot(aes(x = value, fill = key))+
      geom_density(alpha = .5)
  #   +
  #     results %>% 
  #     gather(key, value) %>% 
  #     ggplot(aes(y = value, x = key))+
  #     geom_boxplot(alpha = .5)
  # ) /
  #   results %>% 
  #   ggplot(aes(y = predict, x = observed))+
  #   geom_point()+
  #   geom_smooth(method = "loess")+
  #   geom_abline(intercept=0, slope = 1, color="red", linetype="dashed")
}

results_cross_validation <- function(h2o_model) {
  h2o_model@model$cross_validation_metrics_summary %>% 
    as.data.frame() %>% 
    select(-mean, -sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    mutate_all(as.numeric) %>% 
    select(mae = mae ,
           mean_residual_deviance = mean_residual_deviance,
           mse = mse,
           r2 = r2,
           residual_deviance = residual_deviance,
           rmse = rmse) %>% 
    return()
}

plot_cross_validation <- function(df_results) {
  df_results %>% 
    gather(Metrics, Values) %>% 
    ggplot(aes(Metrics, Values, fill = Metrics, color = Metrics)) +
    geom_boxplot(alpha = 0.3, show.legend = FALSE) + 
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +    
    facet_wrap(~ Metrics, scales = "free") + 
    labs(title = "Model Performance by Some Criteria Selected", y = NULL)
}

xgboost_model <- function(hyper_xgb,
                          search_criteria,
                          training_frame = training_frame,
                          validation_frame = NULL,
                          distribution = "AUTO",
                          nfolds = 5){
  
  # n models
  n_models <- map_dbl(hyper_xgb, length) %>% prod()
  print(glue::glue("Will train {n_models} models"))
  
  # model grid search
  grid_xgb <- h2o.grid(algorithm = "xgboost",
                       x = x, y = y,
                       hyper_params = hyper_xgb,
                       search_criteria = search_criteria,
                       training_frame = training_frame,
                       distribution = distribution,
                       # validation_frame = validation_frame,
                       seed = 1, nfolds = nfolds)
  
  # Get the grid results, sorted by validation r2
  gridperf_xgb <- h2o.getGrid(grid_id = grid_xgb@grid_id, 
                              # sort_by = "r2", 
                              decreasing = TRUE)
  
  # get model
  model_xgb <- h2o.getModel(gridperf_xgb@model_ids[[1]])
  
  # evaluate
  model_perf_xgb <- h2o.performance(model = model_xgb, newdata = validation_frame)
  
  return(list(
    n_models = n_models,
    grid_xgb = grid_xgb,
    gridperf_xgb = gridperf_xgb,
    model_xgb = model_xgb,
    model_perf_xgb = model_perf_xgb
  ))
  
}

# some interaction columns
parse_metadata <- function(metadata){
  metadata %>% 
    transmute(
      textID, text,  sel_text, ngram_text, dif_text, sentiment, jaccard,
      # text stats
      text_n_words = n_words,
      # text_n_lowersp,
      # text_n_capsp,
      # text_n_charsperword,
      # sel_text stats        
      sel_text_n_words = map_dbl(ngram_text, ~length(str_split(.x, pattern = " ")[[1]])),
      # sel_text_n_lowersp,
      # sel_text_n_capsp,
      # sel_text_n_charsperword,
      # interaction sel_text x text
      sd_sel_text_sent_afinn        = text_sent_afinn - sel_text_sent_afinn,
      sd_sel_text_sent_bing         = text_sent_bing - sel_text_sent_bing,
      sd_sel_text_sent_syuzhet      = text_sent_syuzhet - sel_text_sent_syuzhet,
      sd_sel_text_sent_vader        = text_sent_vader - sel_text_sent_vader,
      sd_sel_text_n_polite          = text_n_polite - sel_text_n_polite,
      prop_sel_text_n_vogals        = if_else(text_n_vogals == 0, 0, sel_text_n_vogals / text_n_vogals),
      prop_sel_text_n_consonants    = if_else(text_n_consonants == 0, 0, sel_text_n_consonants / text_n_consonants),
      prop_sel_text_n_str           = if_else(text_n_str == 0, 0, sel_text_n_str / text_n_str),
      prop_sel_text_len             = text_n_words / sel_text_n_words,
      prop_sel_text_n_chars         = if_else(text_n_chars == 0, 0, sel_text_n_chars / text_n_chars),
      prop_sel_text_n_uq_chars      = if_else(text_n_uq_chars == 0, 0, sel_text_n_uq_chars / text_n_uq_chars),
      prop_sel_text_n_lowers        = if_else(text_n_lowers == 0, 0, sel_text_n_lowers / text_n_lowers),
      prop_sel_text_n_caps          = if_else(text_n_caps == 0, 0, sel_text_n_caps / text_n_caps),
      prop_sel_text_n_periods       = if_else(text_n_periods == 0, 0, sel_text_n_periods / text_n_periods),
      prop_sel_text_n_commas        = if_else(text_n_commas == 0, 0, sel_text_n_commas / text_n_commas),
      prop_sel_text_n_exclaims      = if_else(text_n_exclaims == 0, 0, sel_text_n_exclaims / text_n_exclaims),
      prop_sel_text_n_puncts        = if_else(text_n_puncts == 0, 0, sel_text_n_puncts / text_n_puncts),
      prop_sel_text_n_prepositions  = if_else(text_n_prepositions == 0, 0, sel_text_n_prepositions / text_n_prepositions),
      cat_sel_text_n_neg            = if_else(sel_text_n_neg == 0, 0, 1),
      cat_sel_text_n_question       = if_else(sel_text_n_question == 0, 0, 1),
      cat_sel_text_n_digits         = if_else(sel_text_n_digits == 0, 0, 1),
      cat_sel_text_n_extraspaces    = if_else(sel_text_n_extraspaces == 0, 0, 1),
      cat_sel_text_n_tobe           = if_else(sel_text_n_tobe == 0, 0, 1),
      cat_sel_text_n_first_person   = if_else(sel_text_n_first_person == 0, 0, 1),
      cat_sel_text_n_second_person  = if_else(sel_text_n_second_person == 0, 0, 1),
      cat_sel_text_n_third_person   = if_else(sel_text_n_third_person == 0, 0, 1),
      
      # dif_text stats
      dif_text_n_words = map_dbl(dif_text, ~length(str_split(.x, pattern = " ")[[1]])),
      # dif_text_n_lowersp,
      # dif_text_n_capsp,
      # dif_text_n_charsperword,
      # interaction dif_text x text
      sd_dif_text_sent_afinn        = text_sent_afinn - dif_text_sent_afinn,
      sd_dif_text_sent_bing         = text_sent_bing - dif_text_sent_bing,
      sd_dif_text_sent_syuzhet      = text_sent_syuzhet - dif_text_sent_syuzhet,
      sd_dif_text_sent_vader        = text_sent_vader - dif_text_sent_vader,
      sd_dif_text_n_polite          = text_n_polite - dif_text_n_polite,
      prop_dif_text_n_vogals        = if_else(text_n_vogals == 0, 0, dif_text_n_vogals / text_n_vogals),
      prop_dif_text_n_consonants    = if_else(text_n_consonants == 0, 0, dif_text_n_consonants / text_n_consonants),
      prop_dif_text_n_str           = if_else(text_n_str == 0, 0, dif_text_n_str / text_n_str),
      prop_dif_text_len             = dif_text_n_words / text_n_words,
      prop_dif_text_n_chars         = if_else(text_n_chars == 0, 0, dif_text_n_chars / text_n_chars),
      prop_dif_text_n_uq_chars      = if_else(text_n_uq_chars == 0, 0, dif_text_n_uq_chars / text_n_uq_chars),
      prop_dif_text_n_lowers        = if_else(text_n_lowers == 0, 0, dif_text_n_lowers / text_n_lowers),
      prop_dif_text_n_caps          = if_else(text_n_caps == 0, 0, dif_text_n_caps / text_n_caps),
      prop_dif_text_n_periods       = if_else(text_n_periods == 0, 0, dif_text_n_periods / text_n_periods),
      prop_dif_text_n_commas        = if_else(text_n_commas == 0, 0, dif_text_n_commas / text_n_commas),
      prop_dif_text_n_exclaims      = if_else(text_n_exclaims == 0, 0, dif_text_n_exclaims / text_n_exclaims),
      prop_dif_text_n_puncts        = if_else(text_n_puncts == 0, 0, dif_text_n_puncts / text_n_puncts),
      prop_dif_text_n_prepositions  = if_else(text_n_prepositions == 0, 0, dif_text_n_prepositions / text_n_prepositions),
      cat_dif_text_n_neg            = if_else(dif_text_n_neg == 0, 0, 1),
      cat_dif_text_n_question       = if_else(dif_text_n_question == 0, 0, 1),
      cat_dif_text_n_digits         = if_else(dif_text_n_digits == 0, 0, 1),
      cat_dif_text_n_extraspaces    = if_else(dif_text_n_extraspaces == 0, 0, 1),
      cat_dif_text_n_tobe           = if_else(dif_text_n_tobe == 0, 0, 1),
      cat_dif_text_n_first_person   = if_else(dif_text_n_first_person == 0, 0, 1),
      cat_dif_text_n_second_person  = if_else(dif_text_n_second_person == 0, 0, 1),
      cat_dif_text_n_third_person   = if_else(dif_text_n_third_person == 0, 0, 1),
    )  
}

to_search <- function(x){
  str_replace_all(x, "([[:punct:]]|\\*|\\+|\\.{1,}|\\:|\\$|\\:|\\^|\\?|\\|)", "\\\\\\1")
}

# Load data -----------------------------------------------------------------------------------
train_data <- read_csv("data/train.csv") %>% 
  rename(sel_text = selected_text)

# remove na
train_data <- train_data %>% filter(!is.na(text) | text == "")

# extract neutral
# train_neutral <- train_data %>% filter(sentiment == "neutral")
# train_data <- train_data %>% filter(sentiment != "neutral")

# remove bad texts
{
  bad_text <- train_data %>% 
    mutate(texts = map(text, ~str_split(.x, " ")[[1]]),
           sel_texts = map(sel_text, ~str_split(.x, " ")[[1]]),
           bad_text = map2_lgl(texts,sel_texts, ~ sum(.x %in% .y)==0) ) %>% 
    pull(bad_text)
  
  train_data <- train_data[!bad_text,]
}

# colect all possible ngrams and dif
train_ngrams <- 
  train_data %>% 
  mutate(n_words = map_dbl(text, ~str_split(.x, pattern = " ", )[[1]] %>% length())) %>% 
  mutate(ngram_text = map2(text, n_words,  function(text, n_words){
    map(1:n_words, 
        ~ tau::textcnt(text, method = "string", split = " ", n = .x, tolower = FALSE) %>% names() %>% unlist()
    ) } )) %>% 
  mutate(ngram_text = map(ngram_text, unlist)) %>% 
  unnest(cols = c(ngram_text)) %>% 
  mutate(sel = ngram_text == sel_text) %>% 
  mutate(dif_text = str_remove(text, to_search(ngram_text)))

# Remove text without ngrams located
{
  to_remove <- 
    train_ngrams %>% 
    nest(-textID) %>% 
    mutate(sel = map_lgl(data, ~any(.x$ngram_text == .x$sel_text))) %>% 
    filter(sel != T) %>% 
    pull(textID)
  
  train_ngrams <- train_ngrams %>% filter(!textID %in% to_remove)
  
  }

# create y
train_ngrams <- train_ngrams %>% 
  mutate(jaccard = map2_dbl(sel_text, ngram_text, ~jaccard(.x, .y)))

# Remove text ngram selected with jaccard not 1
{
  to_remove <- 
    train_ngrams %>% 
    nest(-textID) %>% 
    mutate(sel = map_lgl(data, ~any(.x$jaccard == 1))) %>% 
    filter(sel != T) %>% 
    pull(textID)
  
  train_ngrams <- train_ngrams %>% filter(!textID %in% to_remove)
  
  }

g1 <- 
  train_ngrams %>% 
  ggplot(aes(x = jaccard, fill = sentiment))+
  geom_density(alpha = .5)+
  labs(title = "before random sample")

train_ngrams %>%
  mutate(jaccard = case_when(jaccard == 1 ~ 1,
                             jaccard == 0 ~ 0,
                             T ~ NaN)) %>%
  filter(!is.na(jaccard)) %>% 
  count(jaccard) %>% mutate(prop = n/sum(n))

# fast report
# DataExplorer::create_report(parsed_metadata, y = "jaccard")
set.seed(1)

# select random ngram by texdID and jaccard
train_ngrams <- 
  train_ngrams %>% 
  nest(-textID) %>%
  mutate(data = map(data, ~.x %>% 
                      group_by(jaccard) %>%
                      sample_n(1))) %>% 
  unnest()

g2 <- 
  train_ngrams %>% 
  ggplot(aes(x = jaccard, fill = sentiment))+
  geom_density(alpha = .5)+
  labs(title = "after random sample")

g1 / g2

# get text metadata
text_metadata <-
  bind_cols(tibble(textID = train_data$textID), get_metadata(train_data$text, verbose = T) %>% 
              `colnames<-`(paste0("text_",colnames(.)))) 

# get sel_text metadata
sel_text_metadata <-
  bind_cols(tibble(textID = train_ngrams$textID), get_metadata(train_ngrams$ngram_text, verbose = T) %>% 
              `colnames<-`(paste0("sel_text_",colnames(.)))) 
# saveRDS(sel_text_metadata,  "sel_text_metadata.rds")

# get dif_text metadata
dif_text_metadata <-
  bind_cols(tibble(textID = train_ngrams$textID), get_metadata(train_ngrams$dif_text, verbose = T) %>% 
              `colnames<-`(paste0("dif_text_",colnames(.)))) 
# saveRDS(dif_text_metadata , "dif_text_metadata.rds")

# join all in metadata
metadata <- 
  left_join(
    bind_cols(sel_text_metadata, select(dif_text_metadata, -textID)),
    bind_cols(train_data, select(text_metadata, -textID)),
    by = "textID"
  ) %>% 
  bind_cols(select(train_ngrams, ngram_text, dif_text, jaccard, n_words)) %>% 
  select(textID, text, sel_text, ngram_text, dif_text, sentiment, n_words, jaccard, everything())

# unique colnames
colnames(metadata) %>% str_remove("(text_|sel_text_|dif_text_)") %>% unique()

# Check point ---------------------------------------------------------------------------------
parsed_metadata <- parse_metadata(metadata)
saveRDS(parsed_metadata, "parsed_metadata.rds")
# parsed_metadata <- readRDS("parsed_metadata.rds")

# Model ---------------------------------------------------------------------------------------
# parsed_metadata <- parsed_metadata %>% filter(jaccard != 0 & jaccard != 1)
# split valid data
parsed_metadata <- parsed_metadata %>% group_by(textID) %>% nest() %>% ungroup()

samp <- sample(1:2,nrow(parsed_metadata), T, c(0.8, 0.2))

train_data <- 
  parsed_metadata[samp == 1,] 

valid_data <- 
  parsed_metadata[samp == 2,] 

g1 <- 
  train_data %>% 
  unnest() %>% 
  ggplot(aes(x = jaccard, fill = sentiment))+
  geom_density(alpha = .5)+
  labs(title = "train random sample")

g2 <- 
  valid_data %>% 
  unnest() %>% 
  ggplot(aes(x = jaccard, fill = sentiment))+
  geom_density(alpha = .5)+
  labs(title = "valid random sample")
g1 / g2

# h2o mmodel
library(h2o)
h2o.init(nthreads=-1, max_mem_size="8g")
# h2o.no_progress() # Turn off progress bars
n_cores = NULL
# h2o.shutdown()

train_data_h2o <- 
  train_data %>% 
  unnest(cols = c(data)) %>% 
  mutate(text_clean = clean_text(ngram_text))%>% 
  select(-textID, -text, -sel_text, -ngram_text, -dif_text) %>% 
  mutate(sentiment = case_when(sentiment == "positive"~1,
                               sentiment == "neutral"~0,
                               sentiment == "negative"~-1)) %>%
  as.h2o()

valid_data_h2o <- 
  valid_data %>% 
  unnest(cols = c(data)) %>% 
  mutate(text_clean = clean_text(ngram_text))%>% 
  select(-text, -sel_text, -dif_text) %>%
  mutate(sentiment = case_when(sentiment == "positive"~1,
                               sentiment == "neutral"~0,
                               sentiment == "negative"~-1)) %>%
  as.h2o()

# Word2vec ------------------------------------------------------------------------------------

words_train_h2o <- h2o.tokenize(train_data_h2o$text_clean, " ")
words_valid_h2o <- h2o.tokenize(valid_data_h2o$text_clean, " ")

set.seed(1)
w2v.model <- h2o.word2vec(words_train_h2o,vec_size = 20, sent_sample_rate = 0, epochs = 50)

vecs_train_h2o <- h2o.transform(w2v.model, words_train_h2o, aggregate_method = "AVERAGE")
vecs_valid_h2o <- h2o.transform(w2v.model, words_valid_h2o, aggregate_method = "AVERAGE")

ind_ok <-  !is.na(vecs_train_h2o$C1) # remove na for train_h2o

vecs_train_h2o <- h2o.cbind(train_data_h2o[ind_ok, setdiff(colnames(train_data_h2o), c("text_clean"))], vecs_train_h2o[ind_ok,])
vecs_valid_h2o <- h2o.cbind(valid_data_h2o[, setdiff(colnames(valid_data_h2o), c("text_clean"))], vecs_valid_h2o)


# XGBoost -------------------------------------------------------------------------------------


x <- setdiff(colnames(vecs_train_h2o), c("jaccard"))
y <- "jaccard"

xgb0 <- h2o.automl(x, y, 
                    training_frame = vecs_train_h2o,
                    nfolds = 5,
                    seed = 1)

h2o.r2(xgb0@leader)


# Plot predict x observed
pred <- predict(xgb0@leader, vecs_valid_h2o)

results <- 
  valid_data %>% 
  unnest(cols = c(data)) %>% 
  bind_cols(as_tibble(pred)) %>% 
  select(textID, text, sel_text, ngram_text, predict) %>% 
  group_by(textID) %>% 
  top_n(1, predict) %>% 
  rowwise() %>% 
  mutate(jaccard = jaccard(sel_text, ngram_text)) %>% 
  ungroup()

mean(results$jaccard)

results %>% 
  mutate(predict = if_else(predict > 1, 1, predict)) %>% 
  select(predict, observed = jaccard) %>% 
  plot_model()



