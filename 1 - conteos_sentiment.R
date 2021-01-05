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
library(wordcloud)
library(translateR)
library(ggridges)


#####################################
# Cargando la base del data_cleaning
#####################################
# Esta base esta en formato Corpus
load(file =  "./Bases output/discursos_final.RData")

# Cargando las stopwords de 1.1 stopwords
load("./Bases output/stop_words_esp.Rdata")

# Guardando mexico, mexicanos
stop_words_esp<-stop_words_esp %>% add_row(palabra = c("méxico", "mexico", "mexicanos", "mexicanas"))
save(stop_words_esp, file = "./Bases output/stop_words_esp.Rdata")

# Cambiando el formato a fila por palabra
# Notas: quita puntuaciones y pasa todo a minusculas en automatico
por_palabra<-
  discursos %>% 
  unnest_tokens(output = palabra, input = contenido)

# Quitamos las stopwords en espanol 
por_palabra<-
  por_palabra %>% 
  anti_join(stop_words_esp)

save(por_palabra, file = "Bases output/por_palabra.RData")
load("Bases output/por_palabra.Rdata")

######################
# Palabras más comunes 
#######################
conteos<-
  por_palabra %>% 
  group_by(palabra) %>% 
  tally() %>% 
  arrange(desc(n))

# Grafica de palabras
# Nota: Hay que declarar a Mexico stopword?
ggplot(conteos %>% filter(n>200), aes(reorder(palabra, n), n))+
  geom_col(fill = "hotpink", color = "darkgray")+
  coord_flip()+theme_bw()

# Conteos absolutos
ggplot(conteos %>% slice(1:50), aes(reorder(palabra, n), n))+
  geom_col(fill = "hotpink", color = "darkgray")+
  coord_flip()+theme_bw()+labs(y = "Frecuencia", x = "Palabra")

ggsave(filename = "./Graficas/conteos.png", width = 8, height = 6)

# Conteos relativos
conteos<-
  conteos %>%
  mutate(freq_rel = 100*n/sum(n))

ggplot(conteos %>% slice(1:50), aes(reorder(palabra, n), freq_rel))+
  geom_col(fill = "hotpink", color = "darkgray")+
  coord_flip()+theme_bw()+labs(y = "Frecuencia Relativa (%)", x = "Palabra")

ggsave(filename = "./Graficas/palabra_freq_rel.png", width = 6, height = 4)


# Conteos absolutos
conteos<-
  por_palabra %>% 
  group_by(palabra, anio) %>% 
  tally() %>% 
  arrange(desc(n))


conteos<-
  conteos %>% 
  group_by(anio) %>% 
  arrange(anio, desc(n)) %>%
  mutate(ranking= row_number())

ggplot(conteos %>% filter(anio>2006, ranking<=25) , aes(reorder(palabra, n), n))+
  geom_col(fill = "hotpink", color = "darkgray")+
  coord_flip()+theme_bw()+labs(y = "Frecuencia", x = "Palabra")+facet_grid(~anio)


ggsave(filename = "./Graficas/palabra_freq_rel2007-2012.png", width = 8, height = 6)

save(por_palabra, file =  "./Bases output/por_palabra.Rdata")
conteos<-
  por_palabra %>% 
  group_by(palabra) %>% 
  tally() %>% 
  arrange(desc(n))


###########################
# Conteos por año en scatter 
#############################
# Conteos absolutos
conteos<-
  por_palabra %>% 
  group_by(palabra, anio) %>% 
  tally() %>% 
  group_by(anio) %>%
  mutate(probabilidad = n/sum(n))


conteos<-
  conteos %>% 
  pivot_wider(id_cols = palabra, names_from = anio, values_from = probabilidad, 
              names_prefix = "prob_", values_fill = list(probabilidad = 0))


# 2007 vs 2006
library(scales)
ggplot(conteos, aes(prob_2006, prob_2007, color = abs(prob_2006-prob_2007)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2006", 
                                                  y = "Probabilidad de Mención, 2007")


ggsave(filename = "./Graficas/prob_06_07.png", width = 8, height = 6)


# 2008 vs 2007
ggplot(conteos, aes(prob_2007, prob_2008, color = abs(prob_2007-prob_2008)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2007", 
                                                  y = "Probabilidad de Mención, 2008")


ggsave(filename = "./Graficas/prob_07_08.png", width = 8, height = 6)


# 2009 vs 2008
ggplot(conteos, aes(prob_2008, prob_2009, color = abs(prob_2009-prob_2008)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2008", 
                                                  y = "Probabilidad de Mención, 2009")


ggsave(filename = "./Graficas/prob_08_09.png", width = 8, height = 6)



# 2009 2010
ggplot(conteos, aes(prob_2009, prob_2010, color = abs(prob_2010-prob_2009)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2009", 
                                                  y = "Probabilidad de Mención, 2010")


ggsave(filename = "./Graficas/prob_09_10.png", width = 8, height = 6)


# 2010 2011
ggplot(conteos, aes(prob_2010, prob_2011, color = abs(prob_2011-prob_2010)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2010", 
                                                  y = "Probabilidad de Mención, 2011")


ggsave(filename = "./Graficas/prob_10_11.png", width = 8, height = 6)


# 2011 2012
ggplot(conteos, aes(prob_2011, prob_2012, color = abs(prob_2012-prob_2011)))+
  geom_jitter(alpha =0.1, size = 2.5, width = 0.3, height= 0.3)+
  scale_x_log10(labels =percent_format())+
  geom_text(aes(label = palabra), check_overlap = T)+
  scale_y_log10(labels = percent_format())+
  geom_abline(linetype = "dashed")+
  theme_bw()+theme(legend.position = "none")+labs(x = "Probabilidad de Mención, 2011", 
                                                  y = "Probabilidad de Mención, 2012")


ggsave(filename = "./Graficas/prob_11_12.png", width = 8, height = 6)


###########################################
# Sentiment Analysis 
###########################################
load("./Bases output/por_palabra.Rdata")
# Primero cargo las palabras del diccionario 
diccionario<-list.files("./Bases input/", pattern = ".csv")
afin<-fread(str_c("./Bases input/", diccionario))
bing<-get_sentiments("bing")
loughran<-get_sentiments("loughran")
nrc<-get_sentiments("nrc")

# Hay que traducir bing, loughran, nrc
rm(bing, loughran, nrc)

sentimiento_afin<-
  inner_join(por_palabra, afin %>% select(Palabra, Puntuacion), by = c("palabra" = "Palabra"))


ggplot(sentimiento_afin, aes(Puntuacion))+geom_density(fill = "hotpink", color = "grey")+
  theme_bw()+labs(y = "Densidad")+geom_vline(xintercept = median(sentimiento_afin$Puntuacion), 
                                             linetype = "dashed")+
  geom_text(aes(0, 0.4), label = "Sentimiento \n Mediano", color = "gray20", fill = "white", 
            hjust = 0.6, inherit.aes = F)


ggsave(filename = "./Graficas/sentimiento_gral.png", width = 8, height = 6)

# Sentimiento por discurso
ggplot(sentimiento_afin, aes(Puntuacion, fecha, group = fecha, fill = anio))+
  geom_density_ridges() +theme_bw()+scale_y_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0)+coord_flip()

ggsave(filename = "./Graficas/sentimiento_discurso.png", width = 10, height = 7)


# Serie de tiempo 
sentimiento_afin<-
  sentimiento_afin %>% 
  group_by(fecha) %>% 
  summarise(sentimiento = mean(Puntuacion))


ggplot(sentimiento_afin, aes(fecha, sentimiento))+geom_point(aes(color = factor(year(fecha))))+
  geom_line()+theme_bw()+theme(legend.position = "none")+geom_hline(yintercept = 0)

ggsave(filename = "./Graficas/sentimiento_prom_discurso.png", width = 10, height = 7)

# Media movil 
sentimiento_afin <- sentimiento_afin %>% ungroup()
sentimiento_afin<-
  sentimiento_afin %>% 
  arrange(fecha) %>%
  mutate(lag_sent = lag(sentimiento),  lead_sent = lead(sentimiento), 
         sentimiento_media_movil = rowMeans(sentimiento_afin %>% select(sentimiento, lag_sent, lead_sent), na.rm = T))


ggplot(sentimiento_afin, aes(fecha, sentimiento_media_movil))+geom_point(aes(color = factor(year(fecha))))+
  geom_line()+theme_bw()+theme(legend.position = "none")+geom_hline(yintercept = 0)

ggsave(filename = "./Graficas/sentimiento_media_movil.png", width = 10, height = 7)


###############################################
# Palabras mas negativas 
# 1. Palabras más comunes en los discursos negativos 
###############################################
sentimiento_afin<-
  inner_join(por_palabra, afin %>% select(Palabra, Puntuacion), by = c("palabra" = "Palabra"))  

sentimiento_afin<-
  sentimiento_afin %>% 
  group_by(titulo, anio, mes, dia, fecha, )
# probabilidad de mencion por fecha 
# 
# Zip law 
# IDf
# n-gram
#   analisis antes depues 





