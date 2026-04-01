#### Procesamiento de datos #####


# Objetivo: Elaborar las tablas para informe de tesina
# Fuente: base_2024_final_anoni1 (3).rds
# Observaciones: la planificación de las tablas y cruces se encuentra disponible 
# en el archivo PDC.xlsx



# 00.setup -------------------------------------------------------------------
# En esta seeción preparamos todo lo que necesitamos para nuestro procesamiento

## librerias -------------------------------------------------------------

library(tidyverse) # universo de librerías con filosofía tidy
library(janitor) # libreria para la limpieza y armado de tablas rápidas


## cargo bases ------------------------------------------------------------

df <-readRDS("input/base_2024_final_anoni1 (3).rds") |> 
  filter(carrera == "1- Sociología") |> # me quedo sólo con sociólogos
  clean_names() # emprolijo los nombres de las columnas


#01.Data Clean --------------------------------------------------------------

# En esta sección limpiamos y transformamos los datos para su procesamiento

# # Transformación de formatos de las variables por ej.
# df <- df |> 
#   # todas las variables que estan como chr a factor
#   mutate(
#     across(
#       where(is.character), 
#       as.factor)
#     ) 

# 02.MODULO 1  ---------------------------------------------------------

## tabla_1_1 -------------------------------------------------------------

# Tenemos muchas formas de llegar a el mismo resultado. Por ej para ver  
# "Alumnos según lugar de residencia" podemos:

# a) Como vimos en clases con dplyr (incluido en tidyverse), usando las 
# funciones group_by() y summarise()
df |> 
  group_by(p10_r) |> 
  summarise(n = n_distinct(id)) |> # recuento de casos
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) # agergar porcentajes

# b) la función tably() del paquete janitor 
df |> 
  tabyl(p10_r) |> # variable que analizo
  adorn_pct_formatting() # agregar porcentajes 


# Tambien podemos hacer frecuencias cruzadas

# a) Con dplyr 
df |> 
  group_by(genero_r,p10_r) |> 
  summarise(n = n_distinct(id)) |> # recuento de casos
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |>  # agergar porcentajes
  select(-n) |> # saco el n para que quede tabla solo con porcentajes
  pivot_wider(
    names_from = genero_r,
    values_from = c(percent)
    ) # pivoteamos para facilitar la lectura

# b) con janitor 
df |> 
  tabyl(p10_r,genero_r) |> # variable que analizo
  adorn_percentages("col") |> 
  adorn_pct_formatting() # agregar porcentajes 


#📝 TODO [1]: Automatizar el proceso con map para que tome la variables de cruce


