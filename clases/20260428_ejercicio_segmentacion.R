# Ejercicio de segmentación de segmentos de estuiantes # 

# 00.setup -------------------------------------------------------------------
# En esta seeción preparamos todo lo que necesitamos para nuestro procesamiento

## librerias -------------------------------------------------------------

library(tidyverse) # universo de librerías con filosofía tidy
library(janitor) # libreria para la limpieza y armado de tablas rápidas
library(gt) # una librería para hermosear tablas

## cargo bases ------------------------------------------------------------

df <-readRDS("input/base_2024_final_anoni1 (3).rds") |> 
  filter(carrera == "1- Sociología") |> # me quedo sólo con sociólogos
  clean_names() # emprolijo los nombres de las columnas



# Partimos de la siguientes definiciones
# 
# Los graduados: personas que terminaron las materia y entregaron la tesis y estan a la espera de jurado...
# Los demorados: tienen más de 5 años respecto del año ingreso.
# Los no demorados: tienen menos de 5 años respecto del año ingreso.


# Paso 1: definimos dos vectores con el anio de la ecuesta y la duración de la carrera
anio_encuesta <- 2024
duracion_carrera <- 5


# Paso 2: construimos la variable segmento con el condicional case_when()
# más info sobre la funcion case_when: https://dplyr.tidyverse.org/reference/case-and-replace-when.html

df <- df |> 
  mutate(
    segmentos = case_when(
      p48 == "1- Soy graduadx o entregué la tesina y estoy esperando fecha de defensa" ~ "Graduado",
      p48 == "2-Soy estudiante" & p27 < anio_encuesta - duracion_carrera ~ "Estudiante demorado",
      p48 ==  "2-Soy estudiante" & p27 >= anio_encuesta - duracion_carrera   ~ "Estudiante a termino",
      TRUE ~ "Otra situación" )
  ) 


# Paso 3: ahora usando tabyl() vamos a explorar algunas tablas de contingencia simples
# y vamos a responder algunas preguntas de cada eje

## Eje sociodemográfico:

# ¿Que edad tienen cada segmento?
df |> 
  tabyl(edad_r,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0)

# ¿Hay diferencias de género entre los segmentos?
df |> 
  tabyl(genero_r,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0) |> 
  gt()


# ¿Laburan?
df |> 
  tabyl(p13_r,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0) |> 
  gt()


# ¿tienen pibes?
df |> 
  tabyl(p06,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0) |> 
  gt()


# cuanto tiempo laburan los estudiantes ocupados
df |> 
  filter(p13_r == "1- Ocupado" ) |> 
  tabyl(p21,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0) 


# Eje trayectoria universitaria:

# ¿Donde se trabaron cada uno (se traban en el taller que ya aprobaron)
df |> 
  tabyl(p38,segmentos) |> 
  adorn_percentages("col") |> 
  adorn_pct_formatting(digits = 0) |> 
  gt()





