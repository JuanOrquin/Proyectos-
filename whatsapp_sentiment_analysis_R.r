## Limpiar la consola ##
rm(list = ls())

## Cargar paquetes ##
suppressPackageStartupMessages({
  
    library("rwhatsapp")
    library("dplyr")
    library("ggplot2")
    library("lubridate")
    library("tidyr")
    library("ggimage")
    library("tidytext")
    library("stopwords")
    library(tidyverse)
    library(kableExtra)
    library(RColorBrewer)
    library(knitr)

})


## Setear ruta de trabajo
path = "C:/Users/Juanchu/Desktop/Analisis_Predicciones/WhatsApp"
setwd(path)
dir()

miChat <- rwa_read("chat.txt")

chat <- rwa_read("chat.txt")

head(chat,n = 10)
tail(chat,n = 10)

theme_set(theme_minimal())



# PREPARACIÓN DE DATOS PARA ANÁLISIS POR DATE/TIME   (se uso un chat que tiene del 16/8/21 al 19/5/22 )
chat <- chat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    # SEGMENTACIÓN POR MES
    estacion = case_when(
      day >= dmy(16082021) & day <= dmy(22092021) ~ "Invierno 2021",
      day >= dmy(23092021) & day <= dmy(20122021) ~ "Primavera 2021",
      day >= dmy(21122021) & day <= dmy(20032022) ~ "Verano 2021",
      day >= dmy(21032022) & day <= dmy(21062022) ~ "Otoño 2022",
      day >= dmy(22062022) & day <= dmy(22092022) ~ "Invierno 2022",
      day >= dmy(23092022) & day <= dmy(20122022) ~ "Primavera 2022",
      day >= dmy(21122022) & day <= dmy(20032023) ~ "Verano 2023",
      day >= dmy(21032023) ~ "Otoño 2023",
      T ~ "Fuera de rango")
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))

# PALETA DE COLORES
paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]
# VERIFICANDO CUÁNTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
chat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

##segunda forma de ver por periodo (mas basico)
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Mensajes por día")


# MENSAJES POR DÍA DE LA SEMANA
chat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Número de mensajes por día de la semana", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

##Cantidad de mensajes por autor
chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Cantidad de Mensajes")


# REMOVEMOS PALABRAS SIN SIGNIFICADO RELEVANTE, COMO ARTÍCULOS, PRONOMBRES, ETC.     ##otra opcion es poner c(stopwords(language = "es")
remover_palabras <- c(stopwords(language = "pt"),
                     ##"multimedia",
                       "y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa", "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una", "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú")

# REMOVEMOS PALABRAS SIN SIGNIFICADO incluidos los archivos multimedia
remover_palabras2 <- c(stopwords(language = "pt"),
                     "multimedia","y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa", "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una", "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les", "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas", "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú", "jaja", "jajaja")





# CONTEO DE PALABRAS
chat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 
  # PLOT DEL TOP 30 DE PALABRAS MÁS USADAS EN CONVERSACIÓN
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Palabras más usadas en la conversación de manera general") +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  theme_minimal()


chat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras2) %>% 
  count(word) %>% 
  # PLOT DEL TOP 30 DE PALABRAS MÁS USADAS EN CONVERSACIÓN
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Palabras más usadas en la conversación de manera general") +
  xlab("Palabras") +
  ylab("Número de veces que se usó la palabra") +
  coord_flip() +
  theme_minimal()




## Palabras más utilizadas sin filtro de palabras
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más utilizadas")


## Palabras más utilizadas con filtro de palabras sin multimedia
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más utilizadas")


## Palabras más utilizadas con filtro de palabras con multimedia
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras2) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras más utilizadas")


##Palabras importante mediante tf-idf por Autor
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% remover_palabras2) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palabras importante mediante tf-idf por Autor")


## Diversidad Léxica
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 10000))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Palabras únicas") +
  xlab("") +
  ggtitle("Diversidad Léxica") +
  coord_flip()


  ## Elijo una persona del grupo
persona = "juan orquin"

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != persona) %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == persona) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # solo selecciona palabras que nadie más usa
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle(paste0("Palabras únicas de ",persona))


library(openxlsx)
write.xlsx(chat, "chat.xlsx")