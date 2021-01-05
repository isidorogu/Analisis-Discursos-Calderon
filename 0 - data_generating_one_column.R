# Analisis de Text de los Discursos de Calderon
#

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

# Leyendo la base limpia de 2 columnas 
load(file =  "./Bases output/discursos.RData")

discursos$contenido<-NULL

################################
# Leyendo los pdfs 
# Cargando los archivos 
#################################

discursos_docs<-list.files(path = "./Discursos/", pattern = "column.pdf")
discursos_docs

# Cargando todos los documentos
base_discursos<-map(discursos_docs, ~pdf_text(str_c("./Discursos/", .)))

# Nombrando la lista de documentos 
names(base_discursos)<-c("Parte_1", "Parte_2", "Parte_3")


# Viendo las dimensiones de cada partes
#Dimensiones de cada documento, hojas
(paginas<-map_dbl(base_discursos, ~length(.)))


###################
# Pegando contenido
###################
base_discursos1<-c(base_discursos$Parte_1, base_discursos$Parte_2, base_discursos$Parte_3)
length(base_discursos1)

# Quitemos los espacios del inicio de cada pagina 
base_discursos1<-str_replace(base_discursos1, pattern = "^[\\s]+", "")

base_discursos1[1]

paginas_inicio<-parse_number(base_discursos1)

paginas_inicio[paginas_inicio>147]<-NA
paginas_inicio[paginas_inicio<0]<-NA
paginas_inicio[round(paginas_inicio) == paginas_inicio]

paginas_inicio<-tibble(pagina = seq(1:length(paginas_inicio)), 
                       indice = paginas_inicio)

paginas_inicio<-read.table(file = "clipboard", header = T)
rm(base_discursos, discursos_docs, paginas)

## Me quedo solo con las paginas de incicio validas
paginas_inicio<-
  paginas_inicio %>% 
  filter(!is.na(indice))

discursos<-
  discursos %>% 
  select(-pagina_inicio_real, -pagina_final_real)

paginas_inicio<-
  paginas_inicio %>% 
  rename(pagina_inicio = pagina) %>% 
  mutate( pagina_final = lead(pagina_inicio)-1)

paginas_inicio$pagina_final[147]<-890


# Quitando los numeros 
# Localizamos los saldos de linea 
numero_pagina<-stri_locate_last(base_discursos1, regex = "[0-9]+")
base_discursos1<-str_sub(base_discursos1, end = numero_pagina[, 1] - 1)
rm(numero_pagina)

# Pegando contenido
contenido_x<-map2_chr(.x = paginas_inicio$pagina_inicio, .y = paginas_inicio$pagina_final, 
                    function(x,y) str_c(base_discursos1[x:y], collapse = " "))

discursos$contenido<-contenido_x

discursos<-
  discursos %>% 
  select(titulo, contenido, everything())

rm(paginas_inicio)

save(discursos, file =  "./Bases output/discursos_final.RData")
load(file =  "./Bases output/discursos_final.RData")

# Checando el resultado
writeLines(discursos$contenido[1])
rm(contenido_x, base_discursos1)

# Ver si tengo q quitar los enters y asi 
# Quitar dobles espacio
discursos<-
  discursos %>% 
  mutate(contenido = str_replace_all(contenido, "\\s+", " "))

discursos<-
  discursos %>% 
  mutate(contenido = str_replace_all(contenido, "- ", ""))


