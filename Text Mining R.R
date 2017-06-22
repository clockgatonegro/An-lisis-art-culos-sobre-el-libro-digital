library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(igraph)
library(ggraph)

#Reconocer texto en español
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

##Importar el texto a R como un data frame por parrafos
dataframe_articulos = read.csv("articulos.csv")  
head(dataframe_articulos)

dataframe_articulos$fecha <- as.Date(dataframe_articulos$fecha,"%m/%d/%Y")


class(dataframe_articulos$fecha)

#Convertir factores a caracter strings
dataframe_articulos %>% mutate_if(is.factor, as.character) -> dataframe_articulos
head(dataframe_articulos)

#volverla un tibble
parrafos_articulos <- as_data_frame(dataframe_articulos)
head(parrafos_articulos)

#Unnest por palabras
palabras_articulos <- parrafos_articulos %>% unnest_tokens(palabras,texto)

#volver las stop words un tibble
stopwords_df <- data_frame(line = 1:308, palabras = stopwords("spanish"))

# Quitar las stop words del data frame
palabras_articulos <- palabras_articulos %>% anti_join(stopwords_df)

#tabla organizada por 
palabras_articulos %>% count(palabras, sort = TRUE) 

#Histograma de palabras más usadas
palabras_articulos %>% count(palabras, sort = TRUE) %>% filter(n > 50) %>%
  mutate(word = reorder(palabras, n)) %>%
  ggplot(aes(palabras, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Ocurrencia de artículos por fecha y categoría
ggplot(palabras_articulos, aes(fecha, categoría)) + geom_point(colour = 'red') + xlab("Tiempo") + ylab("Categoría")

#Ocurrencia de artículos por fecha y sección
ggplot(palabras_articulos, aes(fecha, sección)) + geom_point(colour = 'green') + xlab("Tiempo") + ylab("Sección")

#Ocurrencia de artículos por título
ggplot(palabras_articulos, aes(fecha, titulo)) + geom_point(colour = 'blue') + xlab("Tiempo") + ylab("título")

#generar un wordcloud
palabras_articulos %>% count(palabras) %>%
  with(wordcloud(palabras, n, max.words = 200))

#generación de n-grams (bigramas)
articulos_bigramas <- unnest_tokens(parrafos_articulos, bigramas, texto, token = "ngrams", n = 2)
articulos_bigramas %>%
  count(bigramas, sort = TRUE)

#separar bigramas por columna
bigramas_separados <- articulos_bigramas %>%
  separate(bigramas, c("palabra1", "palabra2"), sep = " ")

#Filtrar stop words
bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% stopwords_df$palabras) %>%
  filter(!palabra2 %in% stopwords_df$palabras)

#Contar los bigramas sin las stopwords organizados de mayor a menor
bigramas_conteo <- bigramas_filtrados %>% 
  count(palabra1, palabra2, sort = TRUE)

#un tibble con los bigramas unidos de nuevo en una sola columna
bigramas_unidos <- bigramas_filtrados %>%
  unite(bigramas, palabra1, palabra2, sep = " ")
bigramas_unidos

#encontrar los trigramas
articulos_trigramas <- unnest_tokens(parrafos_articulos, trigramas, texto, token = "ngrams", n = 3) 
articulos_trigramas%>%
count(trigramas, sort = TRUE)  

trigramas_separados <- separate(articulos_trigramas,trigramas, c("palabra1", "palabra2", "palabra3"), sep = " ") 
  
trigramas_filtrados <-trigramas_separados%>%  
  filter(!palabra1 %in% stopwords_df$palabras)%>%
  filter(!palabra2 %in% stopwords_df$palabras)%>%
  filter(!palabra3 %in% stopwords_df$palabras) %>%
  count(palabra1, palabra2, palabra3, sort = TRUE)

#Filtrar bigramas por la palabra libro
bigramas_filtrados %>%
  filter(palabra1 == "libro") %>%
  count(titulo, palabra2, sort = TRUE)

#Filtrar bigramas por la palabra mercado
bigramas_filtrados %>%
  filter(palabra1 == "mercado") %>%
  count(titulo, palabra2, sort = TRUE)

#Generar un tabla con las columnas palabra1, palabra2 y 
bigramas_grafo <- bigramas_conteo %>%
  filter(n > 4) %>%
  graph_from_data_frame()

#Visualizar la red de bigramas
set.seed(2017)
ggraph(bigramas_grafo, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Visualizar la red usando tranparencia
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2016)
ggraph(bigramas_grafo, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#Visualizar la red de artículos y palabras
articulosporpalabra <- select(palabras_articulos, titulo, palabras) %>% 
   count(titulo, palabras, sort = TRUE)

palarticulo_grafo <- articulosporpalabra %>%
  filter(n > 5) %>%
  graph_from_data_frame()

#asignar un color a los títulos y otro color a las palabras
V(palarticulo_grafo)$color[1:37] <- "#4ECDC4"
V(palarticulo_grafo)$color[38:142] <- "#C7F464"

set.seed(2017)
ggraph(palarticulo_grafo, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
  arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color=V(palarticulo_grafo)$color, size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#Visualizar la red de articulos y bigramas
articulosporbigrama <- select(bigramas_unidos, titulo, bigramas) %>% 
  count(titulo, bigramas, sort = TRUE)

bigrarticulo_grafo <- articulosporbigrama %>%
  filter(n > 1) %>%
  graph_from_data_frame()

#para n>2 = V(bigrarticulo_grafo)[29] hasta [82] || para n>1 = V(bigrarticulo_grafo)[41] hasta [236]
V(bigrarticulo_grafo)$name[41]
V(bigrarticulo_grafo)$color[1:41] <- "#4ECDC4"
V(bigrarticulo_grafo)$color[42:236] <- "#C7F464"

ggraph(bigrarticulo_grafo, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color=V(bigrarticulo_grafo)$color, size = 5 )+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()