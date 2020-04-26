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

################################
# Leyendo los pdfs 
# Cargando los archivos 
#################################
discursos<-list.files(path = "./Discursos/", pattern = ".pdf")
discursos

# Cargando todos los documentos
base_discursos<-map(discursos, ~pdf_text(str_c("./Discursos/", .)))

# Nombrando la lista de documentos 
names(base_discursos)<-c("Parte_1", "Parte_2", "Parte_3")


# Viendo las dimensiones de cada partes
#Dimensiones de cada documento, hojas
paginas<-map_dbl(base_discursos, ~length(.))

# En el indice podemos dividir las hojas de los discursos 
indice<-base_discursos$Parte_1[4:12]

# Quitamos los anios
indice<-str_remove(indice, pattern = "2006")
indice<-str_remove(indice, pattern = "2007")
indice<-str_remove(indice, pattern = "2008")
indice<-str_remove(indice, pattern = "2009")
indice<-str_remove(indice, pattern = "2010")
indice<-str_remove(indice, pattern = "2011")
indice<-str_remove(indice, pattern = "2012")
indice<-str_remove(indice, pattern = "Índice")

# Localizamos los saldos de linea 
saltos_linea<-str_locate_all(indice, pattern = "\\r\\n")

# Primero quitamos la numeracion de las paginas 
saltos_linea<-map(saltos_linea, function(x) x[nrow(x), ])
saltos_linea<-bind_rows(map(saltos_linea, ~as.data.frame.list(.)))


# Quitando 7,8 y 9 
indice[1:3]<-map_chr(1:3, ~str_sub(string = indice[.], 
                               start = 1, 
                               end = saltos_linea$start[.]-2))

# Quitando el resto 
indice[4:9]<-map_chr(4:9, ~str_sub(string = indice[.], 
                               start = 1, 
                               end = saltos_linea$start[.]-3))


writeLines(indice)

# Poniendo el indice en un único vector
indice<-str_c(indice,  collapse = " ")

# Poniendo en filas los discurso
a<-str_split(indice, ".[0-9][0-9]\\r\\n")
a<-as.data.frame.list(a)

discursos<-a
rm(a)


discursos<-
  discursos %>%
  rename(titulo = c........................................r.nPresentación.......................................................................)

discursos$titulo<-as.character(discursos$titulo)


# Limpiando lista de discursos 
discursos<-
  discursos %>%
  slice(2:149)

discursos$titulo[7]<-str_c(discursos$titulo[7], "857", discursos$titulo[8], sep = "")

discursos<-
  discursos %>%
  slice(1:7,9:148)


#########################
# 1. Limpiar el titulo 
######################
#
# Quitamos los puntos y espacio del final de cada titulo
#
discursos<-
  discursos %>% 
  mutate(titulo = str_remove(titulo, "[\\.\\s]*$"))


discursos$titulo


# Quitando el espacio del inicio
discursos<-
  discursos %>% 
  mutate(titulo = str_remove(titulo, "^[\\s]*"))

# el 10, 27, 44, 64, 81, 97, 114
discursos$titulo[10]<-str_c(discursos$titulo[10], " 2007")
discursos$titulo[27]<-str_c(discursos$titulo[27], " 2008")
discursos$titulo[44]<-str_c(discursos$titulo[44], " 2009")
discursos$titulo[64]<-str_c(discursos$titulo[64], " 2009")
discursos$titulo[81]<-str_c(discursos$titulo[81], " 2010")
discursos$titulo[97]<-str_c(discursos$titulo[97], " 2011")
discursos$titulo[114]<-str_c(discursos$titulo[114], " 2011")

# Quitemos los enters de los titulos
discursos<-
  discursos %>%
  mutate(titulo = str_remove_all(titulo, "\\r\\n"))

# Finalmente, quitando los dobles espacios
discursos<-
  discursos %>% 
  mutate(titulo = str_replace_all(titulo, "\\s+", " "))

discursos$titulo


############
# Fechas y lugar 
############
# Anio
discursos<-
  discursos %>% 
  mutate(anio = str_sub(titulo, -4))

# Mes 
discursos <-
  discursos %>% 
  mutate(mes = 
           str_extract(titulo, 
                       pattern = "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"))


# Dia 
discursos<-
  discursos %>% 
  mutate(dia = case_when(mes == "enero"~ str_sub(titulo, start = -19, end = -18), 
                         mes == "febrero" ~ str_sub(titulo, start = -21, end = -20),
                         mes == "marzo" ~ str_sub(titulo, start = -19, end = -18), 
                         mes == "abril" ~ str_sub(titulo, start = -19, end = -18), 
                         mes == "mayo" ~ str_sub(titulo, start = -18, end = -17),
                         mes == "junio" ~ str_sub(titulo, start = -19, end = -18), 
                         mes == "julio" ~ str_sub(titulo, start = -19, end = -18), 
                         mes == "agosto" ~ str_sub(titulo, start = -20, end = -19),
                         mes == "septiembre" ~ str_sub(titulo, start = -24, end = -23), 
                         mes == "octubre" ~ str_sub(titulo, start = -21, end = -20), 
                         mes == "noviembre" ~ str_sub(titulo, start = -23, end = -22),
                         mes == "diciembre" ~ str_sub(titulo, start = -23, end = -22)))

discursos<-
  discursos %>% 
  mutate(dia = parse_number(dia))

discursos<-
  discursos %>%
  mutate(fecha = parse_date(str_c(anio, mes, dia, sep = "-"), 
                            format = "%Y-%B-%d", locale = locale("es")))



#############3
# Numero de pagina
# Poniendo en filas los discurso
a<-str_extract_all(indice, "[0-9]+\\r\\n")
a
a<-as.data.frame.list(a)

a<-
  a %>%
  slice(2:149)

a <-
  a %>% 
  rename(pagina_inicio = c..17.r.n....21.r.n....27.r.n....31.r.n....35.r.n....39.r.n...)

a<-
  a %>% 
  slice(1:6, 8:148)

a<-
  a %>% 
  mutate(pagina_inicio = parse_number(as.character(pagina_inicio)))

discursos<-bind_cols(discursos, a)
rm(a, saltos_linea)

discursos<-
  discursos %>% 
  mutate(pagina_final = lead(pagina_inicio)-1)


discursos<-
  discursos %>% 
  mutate(pagina_inicio_real = pagina_inicio - 3, 
         pagina_final_real = pagina_final -3)


# Cambiando los errores del indice
discursos$pagina_inicio[64]<-342
discursos$pagina_inicio_real[64]<-342-3
discursos$pagina_final[64]<-344
discursos$pagina_final_real[64]<-344-3
discursos$pagina_final[90]<-497
discursos$pagina_final_real[90]<-497-3

discursos<-
  discursos %>% 
  mutate(parte = if_else(pagina_final<=344, 1, 
                         if_else(pagina_final<= 497, 2, 3)))

discursos<-
  discursos %>% 
  mutate(pagina_final_real = if_else(is.na(pagina_final_real), 844, pagina_final_real))

###################
# Pegando contenido
###################
base_discursos1<-c(base_discursos$Parte_1, base_discursos$Parte_2, base_discursos$Parte_3)

discursos<-as.data.table(discursos)
discursos<-discursos[parte==3, pagina_inicio_real := pagina_inicio -4]
discursos<-discursos[parte==3, pagina_final_real := pagina_final_real -4]


contenido_x<-map2_chr(.x = discursos$pagina_inicio_real, .y = discursos$pagina_final_real, 
                    function(x,y) str_c(base_discursos1[x:y], collapse = " "))

discursos$contenido<-contenido_x

discursos<-
  discursos %>% 
  select(titulo, contenido, everything())

discursos$contenido[147]<-str_c(base_discursos1[842:844], collapse = " ")
discursos$pagina_inicio[147]<-842
discursos$pagina_final[147]<-844
discursos$parte[147]<-3

rm(base_discursos, base_discursos1, contenido_x, indice, paginas)

save(discursos, file =  "./Bases output/discursos.RData")


# Checando el resultado
writeLines(discursos$contenido[1])







# Juntando las páginas en un solo cuerpo de texto 
base_discursos[1:3]<-map(base_discursos[1:3], ~str_c(.[1], .[2], .[3]), sep = " ")
map_dbl(base_anuncios, ~length(.)) # length = 1 

#Para septiembre que tiene dos páginas
base_anuncios[6]<-str_c(base_anuncios[6][1], base_anuncios[6][2], sep = " ")
map_dbl(base_anuncios, ~length(.)) # length = 1 




# Algunos analisis por hacer 
# Tokenize la base 
#   - n - gramas
# Sentiment analysis
# probabilidad de mencion por fecha 
# 
# Zip law 
# IDf
# n-gram
#   analisis antes depues 
