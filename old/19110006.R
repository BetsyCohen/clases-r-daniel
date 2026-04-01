install.packages("tidyverse")
install.packages("labelled")
install.packages("ggthemes")
install.packages("openxlsx")
library(tidyverse)
library(dplyr)
library(tidyr)
library(labelled)
library(ggplot2)
library(writexl)
library(foreign)
library(flextable)
library(officer)
library(scales)
library(openxlsx)
df <-readRDS("C:/Users/user/Downloads/base_2024_final_anoni1 (3).rds")
df_est <- df %>%
  filter(carrera == "1- Sociología")%>%
  filter(p29 == "2- No")%>%
  filter(p38 != "7- Entregué la tesina y estoy esperando fecha para defenderla")%>%
  
## 1.-GENERO DE LOS ALUMNOS
  
  etiqueta <- var_label(df_est$genero_r)

## Toma el dataframe df_est y filtra excluyendo la categoría "3- Otros"  
tabla_genero <- df_est %>%
  filter(genero_r != "3- Otros") %>%  
  ## Agrupa por la variable genero_r y cuenta cuántos casos hay en cada grupo  
  group_by(genero_r) %>%
  summarise(n = n()) %>%  
  ## Calcula el porcentaje de cada grupo (n / total * 100) y redondea a 2 decimales
  mutate(prop = round(n/sum(n), 2)) %>%
  mutate(porc = round(n/sum(n), 2)*100) %>%
  select(-prop) %>%
  ## Remueve la agrupación  
  ungroup()  

## 1.1 grafico del sexo de los alumnos sociologia

ggplot(tabla_genero, aes(x = genero_r, y = porc, fill = genero_r)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = paste0(porc, "%")), 
            vjust = 4.5,  # Texto dentro de la barra
            color = "white",
            size = 7,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "1- Masculino" = "#5DADE2",
    "2- Femenino" = "#E84393"
  )) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     labels = paste0(seq(0, 100, 10), "%")) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")


## 1.-GENERO Y EDAD  

genero_edad <- df_est %>%
  # Filtrar solo Masculino y Femenino
  filter(genero_r != "3- Otros") %>%
  # Crear grupos de edad CORREGIDOS
  mutate(grupo_edad = case_when(
    edad %in% c(21:25) ~ "1- 21-25 años",
    edad %in% c(26:30) ~ "2- 26-30 años",
    edad %in% c(31:35) ~ "3- 31-35 años",
    edad %in% c(36:45) ~ "4- 36-45 años",
    edad %in% c(46:55) ~ "5- 46-55 años",
    edad > 55 ~ "6- Mayor a 55 años")) %>%
  # Contar por género y grupo de edad
  group_by(genero_r, grupo_edad) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Calcular porcentajes por género
  group_by(genero_r) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup()

## Grafico con degradé mejorado

piramide_data <- genero_edad %>%
  mutate(porcentaje_ajustado = ifelse(genero_r == "1- Masculino", 
                                      -porcentaje, porcentaje))

ggplot(piramide_data, aes(x = grupo_edad, y = porcentaje_ajustado, fill = genero_r)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = paste0(round(abs(porcentaje_ajustado), 1), "%"),
                y = ifelse(porcentaje_ajustado < 0, 
                           porcentaje_ajustado - 2, 
                           porcentaje_ajustado + 2)),
            color = "black", size = 3, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("1- Masculino" = "#1f77b4", "2- Femenino" = "#ff7f0e")) +
  scale_y_continuous(
    breaks = seq(-60, 60, 20),
    labels = paste0(abs(seq(-60, 60, 20)), "%")
  ) +
  labs(title = "",
       x = "", 
       y = "",
       fill = "Género") +
  theme_minimal()

## 1.3 GRAFICO ESTUDIANTES CON/SIN HIJOS

ggplot(hijos, aes(x = "", y = porcentaje, fill = as.factor(p06))) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 5,
            fontface = "bold") +
  scale_fill_manual(
    values = c("1- Sí" = "#FF6B9D", "2- No" = "#6A89CC"),  # ¡CORREGIDO!
    labels = c("1" = "Sí tiene hijos", "2" = "No tiene hijos"),
    name = "Respuesta"
  ) + 
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_void() + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "plain"),
    plot.caption.position = "plot",
    plot.margin = margin(t = 20, r = 30, b = 30, l = 30),
    legend.text = element_text(size = 10)
  )


## 1.4-Genero estudiantes con hijos 

## los que tienen HIJOS POR GENERO

hijos_si_genero <- df_est %>%
  filter(genero_r %in% c("1- Masculino", "2- Femenino"),
         p06 == "1- Sí") %>%  # FILTRAR SOLO QUIENES SÍ TIENEN HIJOS
  group_by(genero_r) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(porcentaje = round((n/sum(n)*100), 2))

##1.5 los que no tienen HIJOS POR GENERO 

hijos_no_genero <- df_est %>%
  filter(genero_r %in% c("1- Masculino", "2- Femenino"),
         p06 != "1- Sí") %>%
  group_by(genero_r) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(porcentaje = round((n/sum(n)*100), 2),
         categoria = "No tiene hijos")

# Gráfico para QUIENES SÍ TIENEN HIJOS

ggplot(hijos_si_genero, aes(x = genero_r, y = porcentaje, fill = genero_r)) + 
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(porcentaje, "%")),
            vjust = -0.3,
            color = "black",
            size = 4,
            fontface = "bold",
            lineheight = 0.8) +
  scale_fill_manual(values = c("1- Masculino" = "#1f77b4", "2- Femenino" = "#ff7f0e")) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     expand = expansion(mult = c(0, 0.1))) +  # Un poco más de espacio arriba
  labs(
    title = "Distribución por género ENTRE QUIENES SÍ TIENEN HIJOS",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(size = 11)
  )

## graficos quien no tiene hijos

ggplot(hijos_no_genero, aes(x = genero_r, y = porcentaje, fill = genero_r)) + 
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(porcentaje, "%")),
            vjust = -0.3,
            color = "black",
            size = 4,
            fontface = "bold",
            lineheight = 0.8) +
  scale_fill_manual(values = c("1- Masculino" = "#1f77b4", "2- Femenino" = "#ff7f0e")) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  labs(
    title = "Distribución por género ENTRE QUIENES NO TIENEN HIJOS",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )


## 1.-LUGAR DE RESIDENCIA

Lugar_residencia <- df_est %>%
  filter(p10_r != "9- NS/NC" & p10_r != "5- Otros") %>%  
  group_by(p10_r) %>%
  summarise(n = n()) %>%
  mutate(porcentaje = round((n / sum(n) * 100), 2)) %>%
  select(-n)

## 1.5 GRAFICO LUGAR DE RESIDENCIA

ggplot(Lugar_residencia, aes(x = p10_r, y = porcentaje, fill = p10_r)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    vjust = -0.5, # Ajusta la posición vertical de las etiquetas
    size = 4,
    colour = "black"
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + 
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "",
    x = "",
    y = "",
    fill = "Lugar de residencia",
    caption = ""
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line('#F0F0F0'),
    panel.grid.minor = element_line('#F0F0F0'),
    legend.position = "none",
    axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust
                               = .5, vjust = .5, face = "plain")
  )    


  
  ## nivel de instrucción de los padres.
  
  # Definir el orden de los niveles
  
  orden1 <- c("Bajo", "Medio", "Alto")%>%

# CREAR DF_EST (todo en una misma tubería)
df_est <- df %>%
  filter(carrera == "1- Sociología") %>%
  filter(p29 == "2- No")%>%
  filter(p38 != "7- Entregué la tesina y estoy esperando fecha para defenderla")%>%
  # nivel instrucción padre
  mutate(p11_r = case_when(
    p11 %in% c("1- Sin instrucción o Primario incompleto", 
               "2- Primario completo", 
               "9- NS/NR") ~ "Bajo",
    p11 %in% c("3- Secundario incompleto", 
               "4- Secundario completo") ~ "Medio",
    TRUE ~ "Alto"
  )) %>%
  
  # nivel instrucción madre
  mutate(p12_r = case_when(
    p12 %in% c("1- Sin instrucción o Primario incompleto", 
               "2- Primario completo", 
               "9- NS/NR") ~ "Bajo",
    p12 %in% c("3- Secundario incompleto", 
               "4- Secundario completo") ~ "Medio",
    TRUE ~ "Alto"
  )) %>%
  
  # NE padres
  mutate(NE_padres = case_when(
    p11_r == "Bajo" & p12_r == "Bajo" ~ "Bajo",
    p11_r == "Bajo" & p12_r == "Medio" ~ "Bajo", 
    p11_r == "Bajo" & p12_r == "Alto" ~ "Medio",
    p11_r == "Medio" & p12_r == "Bajo" ~ "Bajo",
    p11_r == "Medio" & p12_r == "Medio" ~ "Medio",
    p11_r == "Medio" & p12_r == "Alto" ~ "Alto",
    p11_r == "Alto" & p12_r == "Bajo" ~ "Medio",
    p11_r == "Alto" & p12_r == "Medio" ~ "Alto",
    p11_r == "Alto" & p12_r == "Alto" ~ "Alto"
  ))

# CREAR LAS TABLAS INDIVIDUALES

# 1. Tabla de NE_padres
tabla_ne_padres <- df_est %>%
  group_by(NE_padres) %>%
  summarise(n = n()) %>%
  mutate(porc = round(n/sum(n) * 100, 2),
         NE_padres = factor(NE_padres, levels = orden1)) %>%
  arrange(NE_padres)

tabla_ne_formateada <- tabla_ne_padres %>%
  flextable() %>%
  set_header_labels(
    NE_padres = "Nivel Educativo", 
    n = "n",
    porc = "%"
  ) %>%
  add_header_lines("Nivel educativo máximo de los padres - Sociología EIDAES 2024") %>%
  theme_box() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all")

# 2. Tabla del padre
tabla_padre <- df_est %>%
  group_by(p11_r) %>%
  summarise(n = n()) %>%
  mutate(porc = round(n/sum(n) * 100, 2),
         p11_r = factor(p11_r, levels = orden1)) %>%
  arrange(p11_r)

tabla_padre_ft <- tabla_padre %>%
  flextable() %>%
  set_header_labels(
    p11_r = "Nivel Educativo Padre", 
    n = "n",
    porc = "%"
  ) %>%
  add_header_lines("Nivel educativo del padre - Sociología EIDAES 2024") %>%
  theme_box() %>%
  bold(part = "header")

# 3. Tabla de la madre
tabla_madre <- df_est %>%
  group_by(p12_r) %>%
  summarise(n = n()) %>%
  mutate(porc = round(n/sum(n) * 100, 2),
         p12_r = factor(p12_r, levels = orden1)) %>%
  arrange(p12_r)

tabla_madre_ft <- tabla_madre %>%
  flextable() %>%
  set_header_labels(
    p12_r = "Nivel Educativo Madre", 
    n = "n",
    porc = "%"
  ) %>%
  add_header_lines("Nivel educativo de la madre - Sociología EIDAES 2024") %>%
  theme_box() %>%
  bold(part = "header")


body_add_par("", style = "Normal") %>%
  body_add_flextable(tabla_ne_formateada) %>%
  body_add_par(" ") %>%
  
  body_add_par("", style = "Normal") %>%
  body_add_flextable(tabla_padre_ft) %>%
  body_add_par(" ") %>%
  
  body_add_par("", style = "Normal") %>%
  body_add_flextable(tabla_madre_ft) %>%
  body_add_par(" ") %>%
  
  #  body_add_par("Combinación de Niveles Educativos", style = "Normal") %>%
  #  body_add_flextable(tabla_cruzada_ft) %>%
  #  body_add_par(" ") %>%
  
  body_add_par("", style = "Normal")%>%

# GUARDAR DOCUMENTO
print(doc1, target = "tablas_nivel_educativo_padres.docx")

message("✅ Documento guardado exitosamente: tablas_nivel_educativo_padres.docx")






## fin papucho y mamucha  

# 1.1 Tabla Generacion universitaria

mutate(NE_padres = factor(NE_padres, levels = orden1)) %>%
  
  # generación universitaria - VERIFICAR QUE p11 Y p12 TIENEN EL VALOR "6- Terciario/universitario completo"
  mutate(
    p11_rr = ifelse(p11 == "6- Terciario/universitario completo", 1, 0),
    p12_rr = ifelse(p12 == "6- Terciario/universitario completo", 1, 0),
    generacion_num = p11_rr + p12_rr,
    generacion = case_when(
      generacion_num == 0 ~ "1ra generación",
      generacion_num >= 1 ~ "2da generación"
    )
  )

# VERIFICAR que generacion se creó
print("Variables en df_est:")
print(names(df_est))

print("Valores únicos en generacion:")
print(unique(df_est$generacion))

# Solo si generacion existe, proceder con el análisis
if("generacion" %in% names(df_est)) {
  resultado <- df_est %>%
    group_by(carrera, generacion) %>%
    summarise(n = n()) %>%
    mutate(porc = round(n/sum(n) * 100, 2))
  
  print(resultado)
} else {
  print("La variable 'generacion' no se creó correctamente")
}



## fin prueba

# Crear la tabla
# Agrupar solo por generacion (sin carrera)
tabla_generacion <- df_est %>%
  group_by(generacion) %>%  # Solo generacion, sin carrera
  summarise(n = n()) %>%
  mutate(porc = round(n/sum(n) * 100, 2))

# Verificar que la tabla tiene datos
print(tabla_generacion)

# Crear tabla formateada
tabla_formateada <- tabla_generacion %>%
  flextable() %>%
  set_header_labels(
    generacion = "Generación", 
    n = "n",
    porc = "%"
  ) %>%
  add_header_lines("Generación universitaria en Sociología - EIDAES 2024") %>%
  theme_box() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  width(width = c(3, 1.5, 1.5))  # Ajustar ancho de columnas

# Crear documento Word
doc <- read_docx() %>%
  body_add_par(" ") %>%
  body_add_flextable(tabla_formateada) %>%
  body_add_par(" ") %>%
  body_add_par("", style = "Normal")

# GUARDAR con verificación
tryCatch({
  print(doc, target = "informe_generacion.docx")
  message("✅ Archivo guardado exitosamente como 'informe_generacion.docx'")
  message("📁 Busca el archivo en: ", getwd())
}, error = function(e) {
  message("❌ Error al guardar: ", e$message)
})


## 2.1-GENERO SEGUN CONDICION DE ACTIVIDAD 

p13_genero <- df_est %>%
  filter(genero_r != "3- Otros")%>% 
  group_by(genero_r, p13_r) %>%
  summarise(n = n()) %>%
  mutate(porcentaje = round((n / sum(n) * 100), 2)) %>%
  select(-n)

p13_total <- df_est %>%
  ##  filter(p13_r == "1- Ocupado")%>% 
  group_by(p13_r) %>%
  summarise(n = n()) %>%
  mutate(
    genero_r = "Total", 
    porcentaje = round((n / sum(n) * 100), 2)
  ) %>%
  select(genero_r, p13_r, porcentaje)

p13_final <- bind_rows(p13_genero, p13_total)


## 2.1 GRAFICO GENERO SEGUN CONDICION DE ACTIVIDAD

ggplot(p13_final, aes(x=genero_r, y=porcentaje, fill= p13_r, position= 'fill')) +
  geom_bar(stat= "identity") +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_stack(vjust = 0.5),
    size = 4,
    colour = "#A2B5CD",
    stat = "identity"
  )+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(
    breaks = seq(0, 100, 25),                    
    labels = paste0(seq(0, 100, 25), "%")
  ) +  
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       fill = "Condición de actividad",
       caption = "")+
  theme( panel.background = element_rect(fill = "white") )+
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5, face = "plain", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b =
                                                                               20)),
        plot.caption = element_text(hjust = 0.5, size = 10, face = "plain"),
        plot.caption.position = "plot",
        plot.margin = margin(t = 30, r = 30, b = 30, l = 30)
  )+
  theme(axis.text.x = element_text(color = "grey20", size = 10,
                                   angle = 0, hjust = .5, vjust = .5, face =
                                     "plain"))

## 2.2-grupos de edad segun condicion de actividad 

trabajo_edad <- df_est %>%
  filter (p13_r != "3- Inactivo")%>%
  # Crear grupos de edad
  mutate(trabajo_edad = case_when(
    edad %in% c(20:30) ~ "1- 20-30 años",
    edad %in% c(31:40) ~ "2- 31-40 años",
    edad %in% c(41:50) ~ "3- 41-50 años", 
    edad > 50 ~ "4- Mayor a 50 años")) %>%
  # Contar por condicion de actividad y edad
  group_by(p13_r, trabajo_edad) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Calcular porcentajes por edad
  group_by(p13_r) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup()        

## 2.2 Grafico edad por trabajo

ggplot(trabajo_edad, aes(x = p13_r, y = porcentaje, fill = trabajo_edad)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4) +
  ##     coord_flip() +  # Hace el gráfico horizontal
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5),  # Centra el texto en cada segmento
            color = "white",
            size = 4,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "1- 20-30 años" = "#1f77b4",
    "2- 31-40 años" = "#ff7f0e", 
    "3- 41-50 años" = "#2ca02c",
    "4- Mayor a 50 años" = "#d62728",
    "5- Menor de 20 años" = "#9467bd"
  )) +
  scale_x_discrete(labels = c("Ocupado", "Desocupado", "Inactivo")) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),  # ← De 20 en 20
    labels = paste0(seq(0, 100, 20), "%")  # ← Con símbolo %
  )  +
  labs(title = "",
       x = "",
       y = "",
       fill = "Grupos de edad") +
  theme_minimal()
theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 2.3-Ocupados tipo de contratación      


tipo_contrato <- df_est %>%
  filter(!is.na(p20_r), p13_r == "1- Ocupado") %>%
  mutate(p20_r = case_when(
    p20_r == "1- No, es permanente/fijo/estable" ~ "Trabajo efectivo",
    p20_r == "2- Sí, es temporario renovable" ~ "Trabajo temporario",
    p20_r == "3- Sí, es temporario sin posibilidad de renovación" ~ "Trabajo eventual",
    TRUE ~ p20_r  # Mantiene cualquier otro valor que no coincida
  )) %>%
  group_by(p20_r) %>%
  summarise(n = n()) %>%
  mutate(porcentaje = round((n / sum(n) * 100), 2))

# Gráfico SIN Total
ggplot(tipo_contrato, aes(x = reorder(p20_r, -porcentaje), y = porcentaje, fill = p20_r)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    hjust = -0.1,
    size = 3.2,
    colour = "black",
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    limits = c(0, max(tipo_contrato$porcentaje) * 1.1),
    breaks = seq(0, 100, 25),                    
    labels = paste0(seq(0, 100, 25), "%"),
    expand = c(0, 0)
  ) +  
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    axis.text.y = element_text(size = 8.5, margin = margin(r = 5)),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_flip()

### 2.-Grupos de edad y cantidad de horas trabajadas

horas_edad <- df_est %>%
  filter(!is.na(edad_r), !is.na(p21)) %>%
  count(edad_r, p21, name = "n") %>%
  group_by(edad_r) %>%
  mutate(porcentaje = round(100 * n / sum(n), 2)) %>%
  ungroup()

horas_total <- df_est %>%
  filter(!is.na(p21)) %>%
  count(p21, name = "n") %>%
  mutate(
    edad_r = "Total",
    porcentaje = round(100 * n / sum(n), 2)
  ) %>%
  select(edad_r, p21, n, porcentaje)

# USAR LAS CATEGORÍAS REALES DE TUS DATOS
orden_edad <- c("Total", "6- Mayor a 55 años", "5- De 46 a 55 años",
                "4- De 36 a 45 años", "3- De 31 a 35 años", 
                "2- De 26 a 30 años", "1- De 21 a 25 años")

# USAR LAS CATEGORÍAS EXACTAS DE p21
orden_horas <- c("4- Más de 45 horas semanales", 
                 "3- Entre 35 y 45 horas semanales",
                 "2- Entre 15 y 34 horas semanales", 
                 "1- Menos de 15 horas semanales")

# CORRECCIÓN: CREAR TODAS LAS COMBINACIONES PARA CADA GRUPO DE EDAD
horastrabajo_final <- bind_rows(horas_edad, horas_total) %>%
  # Crear todas las combinaciones posibles de edad_r y p21
  complete(edad_r, p21, fill = list(n = 0, porcentaje = 0)) %>%
  # Para cada grupo de edad, recalcular los porcentajes si es necesario
  group_by(edad_r) %>%
  mutate(
    total_grupo = sum(n),
    porcentaje = ifelse(total_grupo > 0, round(100 * n / total_grupo, 2), 0)
  ) %>%
  ungroup() %>%
  select(-total_grupo) %>%
  mutate(
    edad_r = factor(edad_r, levels = orden_edad),
    p21 = factor(p21, levels = orden_horas)
  ) %>%
  filter(!is.na(edad_r), !is.na(p21))

## 2.5 GRÁFICO CORREGIDO

ggplot(horastrabajo_final, aes(x = edad_r, y = porcentaje, fill = p21)) +
  geom_col(color = "white", size = 0.3, position = "stack") + 
  geom_text(aes(label = ifelse(porcentaje > 0, paste0(porcentaje, "%"), "")),
            position = position_stack(vjust = 0.5), 
            size = 3.5, colour = "black", fontface = "bold") +
  scale_fill_manual(
    values = c(
      "1- Menos de 15 horas semanales" = "#C6DBEF",
      "2- Entre 15 y 34 horas semanales" = "#6BAED6",
      "3- Entre 35 y 45 horas semanales" = "#2171B5",
      "4- Más de 45 horas semanales" = "#08306B"
    ),
    name = "Horas semanales de trabajo",
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = paste0(seq(0, 100, 25), "%")
  ) +
  labs(x = "", y = "") +
  guides(fill = guide_legend(reverse = FALSE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.x = element_text(color = "grey20", size = 10),
    axis.text.y = element_text(color = "grey20", size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_flip()
## 2.-ocupados con o sin relación con la carrera

relacion_carrera <- df_est %>%
  filter(!is.na(p14), p13_r == "1- Ocupado") %>%
  group_by(p14) %>%  
  summarise(n = n()) %>%
  mutate(porcentaje = round((n / sum(n) * 100), 2))

# Usar directamente relacion_carrera sin agregar Total

relacion_carrera$p14 <- factor(relacion_carrera$p14, 
                               levels = c("1- Mucha relación", "2- Alguna relación", "3- Poca relación", "4- Ninguna relación"))


ggplot(relacion_carrera, aes(x = p14, y = porcentaje, fill = p14)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    colour = "white",
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    breaks = seq(0, 100, 25),                    
    labels = paste0(seq(0, 100, 25), "%"),
    expand = c(0, 0)
  ) +  
  labs(x = NULL, y = "") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  coord_flip()




table(df_est$p20, useNA = "always")

