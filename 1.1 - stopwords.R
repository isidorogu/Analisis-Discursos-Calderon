# Cargando las librerias
library(tidyverse)
library(data.table)
library(lfe)
library(knitr)
library(lubridate)
library(broom)
library(tidytext)
library(tm)
library(readr)
library(pdftools)
library(stringi)
library(stringr)
library(RCT)
library(quanteda)


# Stop words de ambas 
stop_words_esp<-read.table("./Bases input/stopwords-es.txt", fileEncoding = "UTF-8")
stop_words_esp2<-read.table("./Bases input/stopwords-es2.txt", fileEncoding = "UTF-8")

stop_words_tm<-tm::stopwords("es")
stop_words_quantEDA<-stopwords(language = "es")

stopwords_final<-union_all(stop_words_esp$V1, stop_words_esp2$V1, stop_words_quantEDA, stop_words_tm)

rm(stop_words_esp, stop_words_esp2, stop_words_quantEDA, stop_words_tm)

stop_words_esp<-tibble(palabra = stopwords_final)

duplicados<-stop_words_esp %>% filter(duplicated(palabra))
stop_words_esp<-stop_words_esp %>% filter(!duplicated(palabra))

save(stop_words_esp, file =   "./Bases output/stop_words_esp.RData")
