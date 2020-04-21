# Analisis de Text de los Discursos de Calderon
#

# Cargando las librerias
library(tidyverse)
library(data.table)
library(lfe)
library(knitr)
library(stargazer)
library(lubridate)
library(broom)
library(tidytext)
library(tm)
library(readr)
library(pdftools)
library(stringi)
library(stringr)
library(RCT)


# Leyendo los pdfs 
# Cargando los archivos 
discursos<-list.files(path = "./Discursos/", pattern = ".pdf")
discursos
# Cargando todos los documentos
base_discursos<-map(discursos, ~pdf_text(str_c("./Discursos/", .)))

# Nombrando la lista de documentos 
(discursos<-str_sub(list.files(path = "./Discursos/", 
                              pattern = ".pdf"), 
                   end = -5))


names(base_discursos)<-discursos

# Viendo las dimensiones de cada partes
#Dimensiones de cada documento
map_dbl(base_discursos, ~length(.))

# Juntando las páginas en un solo cuerpo de texto ago - may
base_discursos[1:5]<-map(base_discursos[1:5], ~str_c(.[1], .[2], .[3]), sep = " ")
map_dbl(base_anuncios, ~length(.)) # length = 1 

#Para septiembre que tiene dos páginas
base_anuncios[6]<-str_c(base_anuncios[6][1], base_anuncios[6][2], sep = " ")
map_dbl(base_anuncios, ~length(.)) # length = 1 