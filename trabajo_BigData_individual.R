#I poner datos e instalar paquetes
install.packages("rlang")
install.packages("devtools")
install.packages("vctrs")
install.packages("rio")
install.packages("kableExtra")
install.packages("wordcloud2")
install.packages("plotly")
install.packages("DT")
install.packages("gganimate")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(rio)
library(kableExtra)
library(wordcloud2)
library(plotly)
library(DT)
library(gganimate)

#1 Introduccion









#II Datos de clasificacion de ese Github
my_url = "https://raw.githubusercontent.com/imontava10/trabajo_BigData/main/datos/clasificacion_total.csv"
archivo_de_destino <- here::here("datos", "clasificacion_total.csv")
download.file(my_url, archivo_de_destino)
Clasif <- rio::import(archivo_de_destino)

#III Datos de maximos goleadores de wikipedia (limpiamos para 10 primeros)
Goleadores <- rio::import(here::here("datos", "Maximos_goleadores.xlsx"))
Goleadores <- Goleadores %>% select(Jugador, Equipo, Goles, PJ, Media, Min., `Min./Gol`) %>% slice(c(0:17)) 


#IV Evolucion por jornada puntos (https://www.ceroacero.es/edition.php?jornada_in=38&id_edicao=70398&fase=70879) Y la pasamos a formato long para poder hacer la bar chart race.
Puntospjornada <- rio::import(here::here("datos", "Puntospjornada.xlsx"))

Puntospjornada <- Puntospjornada %>% tidyr::pivot_longer(cols = 2:39, names_to = "Jornada")

Puntospjornada <- Puntospjornada %>% rename(Puntos = value)

Puntospjornada$Jornada <- as.numeric(Puntospjornada$Jornada)

Puntospjornada <- Puntospjornada %>%
  group_by(Jornada) %>%
  arrange(Jornada, desc(Puntos)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <=15)

#V Asistencias
Asistencias <- rio::import(here::here("datos", "Asistencias.xlsx"))

Asistencias <- Asistencias %>% select(Jugador, Asist.) %>% slice(0:20)

#VI Datos MSN vs BBC
Comparacion <- full_join(Goleadores, Asistencias) %>% filter(Jugador %in% c("Cristiano Ronaldo", "Lionel Messi", "Neymar", "Luis Suárez", "Karim Benzema", "Gareth Bale"))

# Codigo para borrar el enviroment
rm(list = ls())
rm(Evolucion, Evolucion1, Evolucion2)
rm(p3)
rm(MSNgoles)
#IV Filtrar Clasificacion Total de La liga a años 2014/2015
Clasif <- Clasif %>% filter(Season %in% "2014-15")

#V Seleccionamos las variables que nos interesan y le cambiamos el nombre a la columna crest names a teams
Clasif <- Clasif %>% select(Position, `Crest names`, Points, Matches, Won, Draw, Lost, Scored, Conceded, GoalDifference) %>% rename(Teams = `Crest names`)

#VI Le cambiamos el nombre a los equipos para que se entienda bien
Clasif[c(3, 9, 19, 20), 2] <- c("Atletico de Madrid", "Malaga CF", "UD Almeria", "Cordoba CF")

#VII Añadir nueva variable de resultado para siguiente temporada
Clasif <- Clasif %>% mutate(Clasif2016 = case_when(Position == "1" ~ "Fase de grupos de la Liga de Campeones", Position == "2" ~ "Fase de grupos de la Liga de Campeones", Position == "3" ~ "Fase de grupos de la Liga de Campeones", Position == "5" ~ "Fase de grupos de la Liga de Campeones" , Position ==  "4" ~ "Play-offs de la Liga de Campeones",  Position == "6" ~ "Fase de grupos de la Liga Europa", Position == "7" ~ "	Tercera ronda previa de la Liga Europa", Position == "13" ~ "Descenso de categoria", Position == "19" ~ "Descenso de categoria", Position == "20" ~ "Descenso de categoria", TRUE ~ "NA"))

#2 Hacer tabla de la clasificacion de la Liga BBVA de la temporada 2014/15
kable(Clasif) %>%
  kableExtra::kable_styling(fixed_thead = list(enabled = T, background = "black")) %>% row_spec(row = 0, color = "white") %>% column_spec(3, bold = T, color = "black", background = "chartreuse") 

#3 Una forma de visualizar la distribucion de los puntos obtenidos por cada equipo en la Liga BBVA.
x <- c(94, 92, 78, 77, 76, 60, 55, 51, 50, 49, 49, 46, 41, 37, 37, 35, 35, 35, 32, 20)
lbl <- c("FC Barcelona", "Real Madrid", "Atletico de Madrid", "Valencia CF", "Sevilla FC", "Villareal CF", "Athletic Club", "RC Celta", "Malaga CF", "RCD Espanyol", "Rayo Vallecano", "Real Sociedad", "Elche CF", "Levante UD", "Getafe CF", "RC Deportivo", "Granada CF", "SD Eibar", "UD Almeria", "Cordoba CF")

Clasif2 <- Clasif %>% select(Teams, Points)
wordcloud2(data = Clasif2, size = 0.3)

#Una forma de ver la evolucion de la clasificacion (BAR CHART RACE). Evolucion por jornada puntos (https://www.ceroacero.es/edition.php?jornada_in=38&id_edicao=70398&fase=70879) Y la pasamos a formato long para poder hacer la bar chart race.

animacion <- Puntospjornada %>%
  ggplot() +
  geom_col(aes(ranking, Puntos, fill = Equipos)) +
  geom_text(aes(ranking, Puntos, label = Puntos), hjust=-0.1) +
  geom_text(aes(ranking, y=0 , label = Equipos), hjust=1.1) + 
  geom_text(aes(x=15, y=max(Puntos) , label = as.factor(Jornada)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(Jornada, state_length = 0, transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 

animate(animacion, width = 700, height = 432, fps = 25, duration = 15, rewind = FALSE)



#4 Analisis de Goles
#Equipos con mas Goles
# Grafico 1
Goles <- Clasif %>% select(Teams, Scored) %>% filter(Scored >= 48) %>% slice_max(Scored, n = 6)

p1 <- ggplot(Goles, aes(x = reorder(Teams, +Scored), y = Scored)) + 
  geom_col(fill = c("brown2", "brown2", "brown2", 
                    "brown2", "brown2", "brown2")) 

p1 + labs(title = "Goles anotados por equipo", subtitle = "(Top 6 equipos con mas goles)",
         x = "Equipos",
         y = "Goles totales") + theme_light() + 
  geom_text(aes(label = Scored),nudge_y = 5, colour = "black") + coord_flip()

# Grafico 2: Relacion de goles con partidos ganados.
Relacion <- Clasif %>% select(Teams, Scored, Won) %>% slice_max(Scored, n = 20)

#1st option (Falta poner titulo que no me deja ns xq)
p2 <- ggplot(Relacion, aes(Scored, Won, color = Teams)) + geom_point() + geom_smooth()
        p2
        ggplotly(p2)

p2 <- plot_ly(data = Relacion, x = ~ Scored, y = ~ Won, color = ~ Teams)
        p2 + labs(title = "Relacion Goles y Partidos Ganados")

#Maximos Goleadores.
datatable(Goleadores, rownames = FALSE, filter = "top") %>% formatStyle(columns = 3, backgroundColor = "lightgreen")

#Otra forma de visualizarlo
Goleadores2 <- Goleadores %>% select(Jugador, Goles)
wordcloud2(data = Goleadores2, size = 0.3)


#Analisis Asistencias
datatable(Asistencias, rownames = FALSE, filter = "top") %>% formatStyle(columns = 3, backgroundColor = "lightgreen")


#MSN vs BBC
#Goles
Comparaciong <- Comparacion %>% select(Jugador, Equipo, Goles)

Comparaciong <- Comparaciong %>% mutate(Equipo = ifelse(Equipo == "Fútbol Club Barcelona", "MSN", Equipo)) %>% mutate(Equipo = ifelse(Equipo == "Real Madrid Club de Fútbol", "BBC", Equipo))

MSNBBCgoles <- rio::import(here::here("datos", "MSNBBC_goles.xlsx"))

MSNBBCgoles <- MSNBBCgoles %>% rename(Equipo = `Etiquetas de fila`, Goles = `Suma de Goles`)




p3 <- ggplot(MSNBBCgoles) +
  aes(x = reorder(Equipo, desc(Goles)), y = Goles) +
  geom_col(fill = "#092400") +
  labs(x = "Equipo", 
       y = "Goles", title = "MSN vs BBC", subtitle = "Comparacion Goles") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), plot.subtitle = element_text(size = 13L, 
                                                                                         hjust = 0.5))+
  geom_text(aes(label=Goles), vjust=1.5,hjust=0.49, color="white",    
            position = position_dodge(0.1),  size=8
  )
p3
  
  
p4 <- ggplot(Comparacion) +
  aes(x = reorder(Jugador, desc(Goles)), y = Goles) +
  geom_col(fill = "#420057") +
  labs(x = "Jugador", 
       y = "Goles", title = "MSN vs BBC", subtitle = "Comparacion Goles") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), plot.subtitle = element_text(size = 13L, 
                                                                                         hjust = 0.5))+
  geom_text(aes(label=Goles), vjust=1.5,hjust=0.49, color="white",    
            position = position_dodge(0.1),  size=8
  )
p4



#Asistencias
Comparaciona <- Comparacion %>% select(Jugador, Equipo, Asist.)

Comparaciona$Asist. <- as.numeric(Comparaciona$Asist.)

Comparaciona <- Comparaciona %>% arrange(desc(Asist.))

MSNBBCasistencias <- rio::import(here::here("datos", "MSNBBC_asistencias.xlsx"))


p5 <- ggplot(MSNBBCasistencias) +
  aes(x = reorder(Equipo, desc(Asistencias)), y = Asistencias) +
  geom_col(fill = "#092400") +
  labs(x = "Equipo", 
       y = "Asistencias", title = "MSN vs BBC", subtitle = "Comparacion Asistencias") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), plot.subtitle = element_text(size = 13L, 
                                                                                         hjust = 0.5))+
  geom_text(aes(label=Asistencias), vjust=1.5,hjust=0.49, color="white",    
            position = position_dodge(0.1),  size=8
  )
p5


p6 <- ggplot(Comparaciona) +
  aes(x = reorder(Jugador, desc(Asist.)), y = Asist.) +
  geom_col(fill = "#420057") +
  labs(x = "Jugador", 
       y = "Asistencias", title = "MSN vs BBC", subtitle = "Comparacion Asistencias") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), plot.subtitle = element_text(size = 13L, 
                                                                                         hjust = 0.5))+
  geom_text(aes(label=Asist.), vjust=1.5,hjust=0.49, color="white",    
            position = position_dodge(0.1),  size=8
  )
p6

        
        
        
        
        
        
        
        




