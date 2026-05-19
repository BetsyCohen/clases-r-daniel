# Ejercicio de graficos con GGPLOT # 

# Otros recursos:

# https://r-graph-gallery.com/



## librerias -------------------------------------------------------------

library(tidyverse) # universo de librerías con filosofía tidy
library(janitor) # libreria para la limpieza y armado de tablas rápidas


# PARAMETROS --------------------------------------------------------------

anio_encuesta <- 2024
duracion_carrera <- 5


## CARGA Y TRANSFOMACION--------------------------------------------------------

df <-readRDS("input/base_2024_final_anoni1 (3).rds") |> 
  filter(carrera == "1- Sociología") |> # me quedo sólo con sociólogos
  clean_names() |> # emprolijo los nombres de las columnas
  # agregamos los segmentos con lo que ya venimos trabajando
  mutate(
    segmentos = case_when(
      p48 == "1- Soy graduadx o entregué la tesina y estoy esperando fecha de defensa" ~ "Graduado",
      p48 == "2-Soy estudiante" & p27 < anio_encuesta - duracion_carrera ~ "Estudiante demorado",
      p48 ==  "2-Soy estudiante" & p27 >= anio_encuesta - duracion_carrera   ~ "Estudiante a termino",
      TRUE ~ "Otra situación" )
  ) 

## TABLAS Y GRAFICOS -----------------------------------------------------------

# Un grafico siempre se va a alimentar de una tabla. 
# la tabla conviene tenerla ya en formato largo

# nuestro dataset ya es una tabla que tiene por ej. la edad de los alumnos

df |> 
  ggplot(aes(x = edad)) +
  geom_histogram(binwidth = 1)



# tabla estudiantes segun segmento y edad
tabla_1 <- df |> group_by(edad_r,segmentos) |> 
  summarise(alumnos_n = n_distinct(id),.groups = "drop")

tabla_1

# grafico de barras apiladas para los segmentos

tabla_1 |> 
  ggplot(aes(x = segmentos, y = alumnos_n))+
  geom_col()


# ahora agregamos una variable de "rellno" para la variable categórica edad

tabla_1 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r))+
  geom_col(position = "stack")

# tambien podmos elegir ver la categorias sin apilar
tabla_1 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r))+
  geom_col(position = "dodge")


# Podemos elegir otros colores para el relleno
# para eso usamos el parámetro fill que hace referencia al relleno de la geometría
# a no confundir con color hace referencia al color de la línea que rodea a la geometría

tabla_1 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd")

# Tenés varias familias de paletas en ggplot2. Las más usadas vienen de:

# scale_fill_brewer() → ColorBrewer que podes ver en https://colorbrewer2.org/
# scale_fill_viridis_d() → viridis
# scale_fill_manual() → elegís vos tipo hex por ej scale_fill_manual(values= c("#cf4c4c","#7cccaf"))
# scale_fill_distiller() → gradientes continuos basados en Brewer

# algunas de las que uso
# Cualitativas (categorías):  "Set1","Set2","Set3","Pastel1","Pastel2","Dark2"
# "Accent","Paired"
# 
# Secuenciales (ordenadas):"Blues","Greens",,"Oranges","Purples","Reds","BuGn"
# "YlOrRd"


#Si bien no es la capa que más vamos a usar al principio la función coord_flip
# te sea de utilidad para voltear los ejes de tu gráfico.

tabla_1 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip() 


# Facetado (oh sí) ------------------------------------------------------------

tabla_2 <- df |> group_by(genero_r,edad_r,segmentos) |> 
  summarise(alumnos_n = n_distinct(id),.groups = "drop")

tabla_2

tabla_2 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd") + 
  facet_wrap( ~ genero_r)

# Theme: enchulando el gráfico -------------------------------------------------


tabla_2 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd") + 
  facet_wrap( ~ genero_r) +
  theme_minimal()


# para cambiarle algo del theme en especifico hay que indicarselo aparte

tabla_2 |> 
  ggplot(aes(x = segmentos, y = alumnos_n, fill = edad_r)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd") + 
  facet_wrap( ~ genero_r) +
  theme_minimal()+
  theme(
    # cambio el angulo del texto en eje x
    axis.text.x = element_text(angle = 45, hjust = 1), 
  )

# librerías de themes

# ggthemes tiene temas muy populares que por ej. imitan temas clasicos de graficos de Excel, Google docs, FiveThirtyEigh entre otros
# ggdark: temas en modo escuro de los temas predeterminados de ggplot2.
# ggtech: proporciona temas inspirados por compañías tecnológicas, como Airbnb, Google, Twitter o Facebook.

# Otras geometrias posibles ----------------------------------------------------

# usamos geom_density() stat_density() para los gráficos de densidad
# usamos geom_boxplot() stat_boxplot() para los gráficos de cajas o boxplot
# geom_bar() geom_col() stat_count() son para las columnas y barras
# geom_point() para los gráficos de dispersión de puntos





# EJERCICIOS: ------------------------------------------------------------------

## Ejercicio “¿Quiénes trabajan más según el segmento?” -----------------------

# Construí un gráfico de barras apiladas proporcionales que permita comparar 
# la situación ocupacional según segmento de trayectoria.


tabla_trabajo <- df |> 
  group_by(segmentos, p13_r) |> 
  summarise(n = n(), .groups = "drop")


## Ejercicio 2 — Heatmap “¿Dónde se traban?”  ----------------------------------

# Construí un mapa de calor que muestre en qué instancia de la carrera se 
# traban los distintos segmentos.

tabla_traba <- df |> 
  group_by(segmentos, p38) |> 
  summarise(n = n(), .groups = "drop")

tabla_traba |> 
  ggplot(aes(x = segmentos, y = p38, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c()+coord_flip()


## Ejercicio 3 — Boxplot “¿Hay diferencias de edad entre segmentos?” -----------

# Realizá un boxplot de edad según segmento.
# colorear por segmento y probar usar coord_flip().

df |> # capa de la base
  ggplot(aes(x = segmentos, y = edad, fill = segmentos))+ # capa del mapeo
  geom_boxplot()+ # capa de la geometria
  geom_jitter(color="black", size=0.4, alpha=0.9) + #capa de geometria de punitos
  coord_flip() # capa cambiar coordnada de ejes
  


## Ejercicio 4 — Facetado “¿hay diferencias etarias por género?” ---------------

# Construí un gráfico facetado por género que permita observar la distribución etaria según segmento.




## Ejercicio 5 — Densidad “¿Cómo cambia la distribución de edades?” ------------

# construir un grafico de densidad para la edad (discreta) y rellena por segmento
# tip: para trabajar transparencias en color podes usar el parametro 
# alpha =  .4


# Ejercicio 6 — Scatterplot + jitter
# “¿Las personas con hijos están concentradas en ciertos segmentos?”



# Ejercicio integrador 
# “Armar una visualización que cuente una historia”

# Elegí una dimensión sociodemográfica y construí una visualización que permita 
# responder una pregunta sobre desigualdad en las trayectorias universitarias.
