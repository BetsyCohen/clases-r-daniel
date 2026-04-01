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


# Automatizar el procesamiento -------------------------------------------------

# Este proceso podemos automatizarlo de creando una función e iterando. 

# Paso 1: creamos la función crruzar_tabla

cruzar_tabla <- function(data, var_i, var_c) {
  data |>
    group_by(.data[[var_c]], .data[[var_i]]) |>
    summarise(n = n_distinct(id), .groups = "drop") |>
    group_by(.data[[var_c]]) |>
    mutate(percent = round(n / sum(n) * 100, 1)) |>
    select(-n) |>
    pivot_wider(
      names_from = all_of(var_c),
      values_from = percent
    )
}



# Paso 2: arnmamos vectores con nuestras variables de interes y de cruce
vars_interes <- c("p10_r", "p13_r")
vars_cruce   <- c("genero_r", "edad_r")

# Paso 3: guardamos la lista de tablas en el objeto resultados
resultados <- list()


# Paso 4: iteramos creando un loop

for (v1 in vars_interes) {
  for (v2 in vars_cruce) {
    
    nombre <- paste(v1, "por", v2)
    
    resultados[[nombre]] <- cruzar_tabla(df, v1, v2)
  }
}


# Paso 5: veo los nombres de una tabla...
names(resultados)

## para cada cada nombre de las tablas listadas en resultados imprimimos su titulo y los resultados en cuestion 
for (nombre in names(resultados)) {
  print(nombre)
  print(resultados[[nombre]])
}

# o miro el resultado de una tabla especifica
resultados[["p10_r por genero_r"]]

# o me la guardo en el GE para usarla despues en un grafico o lo que sea
tabla_1<-resultados[["p10_r por genero_r"]]


tabla_1
