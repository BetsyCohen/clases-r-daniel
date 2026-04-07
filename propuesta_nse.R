


# Dimensión 1: Inserción laboral ocupado vs no jerarquía tipo ocupación 
# Dimensión 2: Calidad del empleo registrado vs no estabilidad horas 
# Dimensión 3: Capital cultural de origen educación padre/madre


# 🧪 Supuestos 

# Se asume:
#   
#   trabaja (1 = sí, 2/3 = no)
# tipo_ocupacion (categorías tipo: profesional, administrativo, comercio, etc.)
# jerarquia (jefe / no jefe)
# registro (1 registrado, 2 no)
# estabilidad (1 estable, 2/3 temporal)
# horas (categorías)
# educ_padre, educ_madre (1–6 como en cuestionario)




library(dplyr)

df_nse <- df %>%
  
  # -------------------------------------------------
# 1. RECODIFICACIONES BASE (0 a 1)
# -------------------------------------------------
mutate(
  
  # --- INSERCIÓN LABORAL ---
  
  # Ocupación (binaria)
  ocupado = case_when(
    trabaja == 1 ~ 1,
    TRUE ~ 0
  ),
  
  # Jerarquía
  jerarquia_score = case_when(
    jerarquia %in% c("jefe", "director", "coordinador") ~ 1,
    TRUE ~ 0
  ),
  
  # Tipo de ocupación (proxy de status)
  ocupacion_score = case_when(
    tipo_ocupacion %in% c("profesional", "ejecutivo") ~ 1,
    tipo_ocupacion %in% c("administrativo", "docente") ~ 0.7,
    tipo_ocupacion %in% c("comercio") ~ 0.4,
    TRUE ~ 0.2
  ),
  
  
  # --- CALIDAD DEL EMPLEO ---
  
  registro_score = case_when(
    registro == 1 ~ 1,
    TRUE ~ 0
  ),
  
  estabilidad_score = case_when(
    estabilidad == 1 ~ 1,
    estabilidad == 2 ~ 0.5,
    TRUE ~ 0
  ),
  
  horas_score = case_when(
    horas == "más de 45" ~ 1,
    horas == "35-45" ~ 0.8,
    horas == "15-34" ~ 0.5,
    TRUE ~ 0.2
  ),
  
  
  # --- CAPITAL CULTURAL ---
  
  educ_padre_score = case_when(
    educ_padre == 6 ~ 1,
    educ_padre == 5 ~ 0.8,
    educ_padre == 4 ~ 0.6,
    educ_padre == 3 ~ 0.4,
    educ_padre == 2 ~ 0.2,
    TRUE ~ 0
  ),
  
  educ_madre_score = case_when(
    educ_madre == 6 ~ 1,
    educ_madre == 5 ~ 0.8,
    educ_madre == 4 ~ 0.6,
    educ_madre == 3 ~ 0.4,
    educ_madre == 2 ~ 0.2,
    TRUE ~ 0
  )
) %>%
  
  
  # -------------------------------------------------
# 2. ÍNDICES POR DIMENSIÓN
# -------------------------------------------------
mutate(
  
  # Dim 1: Inserción laboral (peso alto)
  dim_insercion = rowMeans(
    cbind(ocupado, jerarquia_score, ocupacion_score),
    na.rm = TRUE
  ),
  
  # Dim 2: Calidad empleo
  dim_calidad = rowMeans(
    cbind(registro_score, estabilidad_score, horas_score),
    na.rm = TRUE
  ),
  
  # Dim 3: Capital cultural
  dim_cultural = rowMeans(
    cbind(educ_padre_score, educ_madre_score),
    na.rm = TRUE
  )
) %>%
  
  
  # -------------------------------------------------
# 3. SCORE FINAL (ponderado)
# -------------------------------------------------
mutate(
  
  nse_score = 
    0.5 * dim_insercion +   # peso alto
    0.3 * dim_calidad +
    0.2 * dim_cultural
) %>%
  
  
  # -------------------------------------------------
# 4. SEGMENTACIÓN
# -------------------------------------------------
mutate(
  
  nse_nivel = case_when(
    nse_score >= quantile(nse_score, 0.8, na.rm = TRUE) ~ "Alto",
    nse_score >= quantile(nse_score, 0.6, na.rm = TRUE) ~ "Medio-alto",
    nse_score >= quantile(nse_score, 0.4, na.rm = TRUE) ~ "Medio",
    nse_score >= quantile(nse_score, 0.2, na.rm = TRUE) ~ "Medio-bajo",
    TRUE ~ "Bajo"
  )
)