# Instalar el paquete tweetbotornot (Tuvimos que subirlo a Github porque el código de origen tenía un error)
library(devtools)
install_github("elcerebrohabla/tweetbotornot")
3

# Paquetes
library(pacman)
p_load(readxl, tidyverse, dplyr, cowplot, janitor, lmtest, 
     sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, 
     hexbin, janitor, mosaicData, scales, ggthemes, 
     tweetbotornot, rtweet)

#ÃPI de Twitter
token <- create_token(
  "My Application Name",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = ""
)

get_token()

view(datos_revocacion)

#datos_revocacion <- read_excel("01_datos/datos_revocacion.xlsx")

#Tweets de revocación
datos_revocacion <- search_tweets(q = "revocación", n = 300000)

#Seleccionar columnas
revocacion <- datos_revocacion %>% 
  dplyr::select(screen_name, text, source, favorite_count, retweet_count, followers_count, account_created_at)
  

etapa1 <- revocacion %>% 
  dplyr::select(text, screen_name, source, account_created_at, followers_count) %>%
  #Agrupamos por cadena de texto para encontrar los tuits que están siendo replicados
  #por varias cuentas y què cuentas los estàn replicando.
  group_by(text) %>% 
  mutate(n= n()) %>% 
  #Los tuits deben haber sido replicados màs de 3 veces
  filter(n > 3) %>% 
  #Los usuarios deben tener menos de 30 followers
  filter(followers_count < 30) %>% 
  #Y su cuenta debió crearse este año
  filter(account_created_at > "2022-01-01") %>% 
  arrange(desc(n)) %>% 
  ungroup()

view(etapa1)

etapa2 <- etapa1 %>% 
  dplyr::select(screen_name, account_created_at) %>%  
  group_by(screen_name) %>% 
  #Agrupamos por usuario y contamos cuàntas veces dichos usuarios replicaron
  #los tuits anteriores
  mutate(n= n()) %>% 
  distinct(screen_name, .keep_all=TRUE) %>% 
  arrange(desc(n))

etapa2

#Para finalizar, utilizamos un paquete llamado botornot que te arroja la probabilidad
#de que una cuenta sea un bot. No es perfecto, pero es útil utilizarlo después
#de haber hecho los pasos anteriores.


#Seleccionamos la columna con los nombres de usuario
etapa3 <- etapa2 %>% 
  dplyr::select(screen_name)

#Convertimos el tibble en una matriz para que el paquete pueda procesar las 
#cuentas
lista <- as.matrix(etapa3)

#Ejecutamoos el paquete
bots <- tweetbotornot(lista)

#Ordenamos los resultados por probabilidad de mayor a menor, y convertimos
#el resultado en porcentaja para mayor legibilidad
bots_final <- bots %>% 
  mutate(prob_bot = percent(prob_bot) ) %>% 
  arrange(desc(prob_bot))

#RESULTADOS
view(bots_final)

