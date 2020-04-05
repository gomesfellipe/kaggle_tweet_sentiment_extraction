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
clean_text <- function(x) {
  require(dplyr)
  require(stringr)
  
  x %>%
    str_remove_all("(RT|via)((?:\\b\\W*@\\w+)+)") %>%
    str_to_lower() %>%
    str_remove_all("@\\w+") %>%
    str_remove_all("[[:digit:]]") %>%
    str_remove_all("http(s|).*") %>%
    str_remove_all("[ |\t]{2,}") %>%
    str_remove_all("w( |)/") %>%
    str_remove_all("(?! )[^[:alnum:]]") %>%
    str_remove_all("'\\b\\w{1,2}\\b'") %>%
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
  
  x <-
    x %>%
    # rename selected_text to sel_text
    rename(sel_text = selected_text) %>%
    # features engineering
    mutate(
      # on text
      text_na = is.na(text),
      text_len = str_length(text),
      text_n_words = ntoken(text),
      text_npunc = str_count(text, "[[:punct:]]"),
      text_numbers = str_count(text, "[[:digit:]]"),
      text_links = str_count(text, "http(s|).*"),
      text_hashtags = str_count(text, "#\\w+"),
      text_retweet = str_count(text, "(RT|via)((?:\\b\\W*@\\w+)+)"),
      text_atpeople = str_count(text, "@\\w+"),
      text_clean = clean_text(text),
      # on sel_text
      sel_text_na = is.na(sel_text),
      sel_text_len = str_length(sel_text),
      sel_text_n_words = ntoken(sel_text),
      sel_text_npunc = str_count(sel_text, "[[:punct:]]"),
      sel_text_numbers = str_count(sel_text, "[[:digit:]]"),
      sel_text_links = str_count(sel_text, "http(s|).*"),
      sel_text_hashtags = str_count(sel_text, "#\\w+"),
      sel_text_retweet = str_count(sel_text, "(RT|via)((?:\\b\\W*@\\w+)+)"),
      sel_text_atpeople = str_count(sel_text, "@\\w+"),
      sel_text_clean = clean_text(sel_text),
      # mutual
      equal_texts = text == sel_text,
      jaccard = purrr::map2_dbl(text, sel_text, ~ jaccard(.x, .y)),
    ) %>%
    {
      # add columns start and end sel_text
      bind_cols(.,
                map2_dfr(
                  .$text, .$sel_text,
                  ~ { # include \\ before special characters before the search
                    .y <- str_replace_all(.y, "([[:punct:]]|\\*)", "\\\\\\1")
                    str_locate(.x, .y) %>%
                      as_tibble() } ) ) } %>%
    select(textID, text, sel_text, sentiment, start, end, jaccard, 
           text_clean, sel_text_clean, everything())
  
  cat(paste0("Metadata successfully obtained!\nThe process took: ",
             round(Sys.time()-t0) ," seconds")) # Yeah!
  
  return(x)
}

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

make_dataset <- function(train_pp, id) {
  require(dplyr)
  require(purrr)
  require(stringr)
  
  x <- train_pp
  
  if(!is.null(x$selected_text)){
    selected_text = x$selected_text
  }else{
    selected_text = NA
  }
  
  text_vader = x$text %>% map_chr(vader_compound)
  
  tibble(
    textID = id,
    txt = x$text,
    sel_txt = selected_text,
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
      dif_ngram_vader = map2_chr(x$text, all_ngrams, ~ {
        tryCatch(str_remove_all(.x, .y),
                 error = function(e){str_remove_all(.x,str_replace_all(.y, "([[:punct:]]|\\*|\\+)", "\\\\\\1"))})
      }),
      dif_ngram_vader = map_chr(dif_ngram_vader, vader_compound)
    ) %>%
    rowwise() %>%
    mutate(jaccard = jaccard(selected_text, all_ngrams)) %>% # y
    ungroup()
}
