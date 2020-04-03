# Starter Script - Develop

# todo: obter smiles dos textos
# todo: n de palavras unicas
# todo: n de maiusculas
# todo: % do texto que foi coletado

# todo: comferir se precisa melhorar funcao de limpeza (one grams mais frequentes)

# functions -----------------------------------------------------------------------------------



# Script --------------------------------------------------------------------------------------

# packages
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(quanteda)
library(tibble)

train_data <- read_csv("data/train.csv")
test_data <- read_csv("data/train.csv")
# failures source: https://www.kaggle.com/c/tweet-sentiment-extraction/discussion/138272
source("functions.R")

train_data
dim(train_data)

test_data
dim(test_data)

# get metadata:
train_metadata <- get_metadata(train_data) # aguarde um pouco..
test_metadata <- get_metadata(test_data) # aguarde um pouco..











# Rascunho ------------------------------------------------------------------------------------

# Mantenha o rascunho daqui para baixo
