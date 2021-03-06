#' Custom Kabble plot
#' 
#' Custom kable to kaggle kernels
#' 
#' @param x dataframe
#' @return print a custom html table
#' @example 
#' kable2(mtcars)
kable2 <- function(x, height = "300px"){
  require(knitr)
  require(kableExtra)
  
  kable(x) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", "bordered"),
                  full_width = F, position = "center") %>%
    scroll_box(width = "100%", height = height)
}

#' Jaccard
#'
#' function developed to solve the problem in kaggle
#' https://www.kaggle.com/c/tweet-sentiment-extraction/
#'
#' @param str1 string
#' @param str2 string
#' @return Jaccard metric
#' @examples
#' str1 <- "Ola cOmo Vai voce"
#' str2 <- "Ola cOmo vai "
#' jaccard(str1, str2)
#'
#' str1 <- c("a a b", "a b c", "a c d")
#' str2 <- c("a d b", "a e c", "a a e")
#' purrr::map2_dbl(str1, str2, ~ jaccard(.x, .y))
jaccard <- function(str1, str2) {
  # r version for: https://www.kaggle.com/c/tweet-sentiment-extraction/overview/evaluation
  a <- unlist(strsplit(tolower(str1), split = " "))
  b <- unlist(strsplit(tolower(str2), split = " "))
  c <- intersect(a, b)
  length(c) / (length(a) + length(b) - length(c))
}

#' Clean Tweets
#'
#' fucntion to clean general text from twitter
#'
#' @param x string
#' @return cleaned string
#' @example
#' clean_text("HavE a niCe DAy!!! :)$$$")
clean_text <- function(x, stem = F) {
  # require(dplyr)
  # require(stringr)
  
  x %>%
    str_remove_all("\\n") %>%
    str_remove_all("\\[\\[User.*") %>%
    str_remove_all("\\&quot\\;") %>%
    str_remove_all("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}") %>% # ip
    str_remove_all("(RT|via)((?:\\b\\W*@\\w+)+)") %>%
    str_to_lower() %>%
    str_remove_all("@\\w+") %>%
    str_remove_all("[[:digit:]]") %>%
    str_remove_all("(http://.*?\\s)|(http://.*)") %>%
    str_remove_all("[ |\t]{2,}") %>%
    str_remove_all("w( |)/") %>%
    str_remove_all("(?! )[^[:alnum:]]") %>%
    str_trim() %>%
    str_squish() 
  
}

#' Get Metadata
#'
#' function developed to get metadata of twitter dataset
#' https://www.kaggle.com/c/tweet-sentiment-extraction/data
#'
#' @param x = load dataset
#' @return dataset dataset combined with metadata
#' @example
#' metadata <- get_metadata(train_data)
#'
# metadata
get_metadata <- function(x) {
  require(dplyr)
  require(stringr)
  require(quanteda)
  require(purrr)
  t0 <- Sys.time() # to print time
  cat("Getting metadata, please wait ..\n")
  
  is_test <- suppressWarnings(is.null(x$selected_text))
  
  x <-
    x %>%{
      if(!is_test){
        # rename selected_text to sel_text
        rename(., sel_text = selected_text) %>% 
          mutate_at(c("text", "sel_text"),
                    ~ .x %>% str_trim() %>% str_squish())
      }else{
        mutate(., text = text %>% str_trim() %>% str_squish())
      }
    } %>%
    # row operations
    rowwise() %>% 
    mutate(
      text_nwords = length(str_split(text, pattern = " ")[[1]]),
      text_nunique_words = length(unique(str_split(text, pattern = " ")[[1]])),
      # sel_text_nwords = length(str_split(sel_text, pattern = " ")[[1]]),
      # sel_text_nunique_words = length(unique(str_split(sel_text, pattern = " ")[[1]])),
    ) %>% 
    ungroup() %>% 
    # features engineering
    mutate(
      # on text
      # text_na = is.na(text),
      text_len = str_length(text),
      text_nupper = str_count(text, "[A-Z]"),
      text_npunc = str_count(text, "[[:punct:]]"),
      text_numbers = str_count(text, "[[:digit:]]"),
      # text_links = str_count(text, "http(s|).*"),
      text_hashtags = str_count(text, "#\\w+"),
      # text_retweet = str_count(text, "(RT|via)((?:\\b\\W*@\\w+)+)"),
      # text_atpeople = str_count(text, "@\\w+"),
      text_clean = clean_text(text),
    ) %>%
    {
      if(!is_test){
        mutate(.,
               # on sel_text
               # sel_text_na = is.na(sel_text),
               # sel_text_nupper = str_count(sel_text, "[A-Z]"),
               # sel_text_len = str_length(sel_text),
               # sel_text_npunc = str_count(sel_text, "[[:punct:]]"),
               # sel_text_numbers = str_count(sel_text, "[[:digit:]]"),
               # sel_text_links = str_count(sel_text, "http(s|).*"),
               # sel_text_hashtags = str_count(sel_text, "#\\w+"),
               # sel_text_retweet = str_count(sel_text, "(RT|via)((?:\\b\\W*@\\w+)+)"),
               # sel_text_atpeople = str_count(sel_text, "@\\w+"),
               sel_text_clean = clean_text(sel_text),
               # mutual statistics
               equal_texts = text == sel_text,
               jaccard = purrr::map2_dbl(text, sel_text, ~ jaccard(.x, .y))) %>% 
          # add columns start and end sel_text
          bind_cols(map2_dfr(
            .$text, .$sel_text,
            ~ { # include \\ before special characters before the search
              .y <- str_replace_all(.y, "([[:punct:]]|\\*|\\+|\\.)", "\\\\\\1")
              str_locate(.x, .y) %>%
                as_tibble() } ) )  
      }else{
        .
      }
    } %>% {
      if(!is_test){
        dplyr::select(., textID, text, sel_text, sentiment, start, end, jaccard, 
                      text_clean, sel_text_clean, everything())    
      }else{
        dplyr::select(., textID, text, sentiment, 
                      text_clean, everything())    
      }
    }
  
  
  cat(paste0("Metadata successfully obtained!\nThe process took: ",
             round(Sys.time()-t0) ," seconds")) # Yeah!
  
  return(x)
}

#' Collect compound Vader measure
#'
#' function developed to get parse of twitter dataset
#' https://www.kaggle.com/c/tweet-sentiment-extraction/data
#'
#' @param x = text
#' @return compound sentiment by text
#' @example
#' vader_compound("i love you")
#'
vader_compound <- function(x) {
  # normalized, weighted composite score
  # https://github.com/cjhutto/vaderSentiment#about-the-scoring
  if (str_length(x) == 0) {
    NA
  } else {
    tryCatch(
      vader::getVader(x) %>%
        .[names(.) == "compound"] %>%
        { case_when(. >= 0.05  ~ "positive",
                    . <= -0.05 ~ "negative",
                    T          ~ "neutral")
        },
      error = function(e) NA
    )
  }
}

#' Get parsed dataset
#'
#' function developed to get parse of twitter dataset
#' https://www.kaggle.com/c/tweet-sentiment-extraction/data
#'
#' @param data_pp = dataset pre processed
#' @return some dataset ngrams analysis
#' @example
#' train_parse <- get_parse(train_pp)
#'
get_parse <- function(data_pp, id) {
  require(dplyr)
  require(purrr)
  require(stringr)
  
  x <- data_pp
  
  if(!is.null(x$selected_text)){
    selected_text = x$selected_text
  }else{
    selected_text = NA
  }
  
  text_vader = x$text %>% map_chr(vader_compound)
  
  tibble(
    textID = id,
    text = x$text,
    sel_text = selected_text,
    text_n_words = str_split(x$text, pattern = " ", )[[1]] %>% length(),
    text_sentiment = x$sentiment,
    text_vader = text_vader,
    all_ngrams = map(1:text_n_words, ~ tau::textcnt(x$text, method = "string", split = " ", n = .x, tolower = FALSE) %>% names()) %>% unlist()
  ) %>%
    mutate(
      ngram_len = all_ngrams %>% map_dbl(~ str_split(.x, pattern = " ", )[[1]] %>% length()),
      ngram_prop = ngram_len / text_n_words,
      ngram_vader = all_ngrams %>% map_chr(vader_compound),
      dif_text_ngram = text_n_words - ngram_len,
      dif_prop_text_ngram = (text_n_words - ngram_len) / text_n_words,
      dif_ngram_vader = map2_chr(x$text, all_ngrams, ~ {
        tryCatch(str_remove_all(.x, .y),
                 error = function(e){str_remove_all(.x,str_replace_all(.y, "([[:punct:]]|\\*|\\+|\\.)", "\\\\\\1"))})
      }),
      dif_ngram_vader = map_chr(dif_ngram_vader, vader_compound)
    ) %>%
    rowwise() %>%
    mutate(jaccard = jaccard(selected_text, all_ngrams)) %>% # y
    ungroup()
}
