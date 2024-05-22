# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(FSA)


# Cargar los datos desde el archivo Excel
data <- read.xlsx("migra.xlsx", sheet = 1)

# Convertir variables categóricas a factores si es necesario
data$GENERO <- as.factor(data$GENERO)
data$EDAD <- as.factor(data$EDAD)
data$PAIS_RESIDENCIA <- as.factor(data$PAIS_RESIDENCIA)
data$CIUDAD_RESIDENCIA <- as.factor(data$CIUDAD_RESIDENCIA)
data$VIAJO <- as.factor(data$VIAJO)
data$PRIMERA_VEZ <- as.factor(data$PRIMERA_VEZ)
data$PROPOSITO_VIAJE <- as.factor(data$PROPOSITO_VIAJE)

# Crear una nueva variable de edad combinada
data$EDAD_COMBINADA <- ifelse(data$EDAD %in% c("18 a 30 años", "31 a 45 años"), "18 a 45 años", as.character(data$EDAD))

# Verificar la nueva variable
table(data$EDAD_COMBINADA)

# Combinar los países de residencia en las categorías especificadas
data$PAIS_RESIDENCIA <- as.character(data$PAIS_RESIDENCIA)
data$PAIS_RESIDENCIA <- ifelse(data$PAIS_RESIDENCIA %in% c("Alemania", "España", "Francia", "Croacia"), "Europa", 
                               ifelse(data$PAIS_RESIDENCIA %in% c("USA", "Canada"), "USA y Canada", 
                                      ifelse(data$PAIS_RESIDENCIA == "Israel", "Asia", "Other American Countries")))
data$PAIS_RESIDENCIA <- as.factor(data$PAIS_RESIDENCIA)

# Verificar la nueva variable de país de residencia
table(data$PAIS_RESIDENCIA)

# Mostrar una vista previa del dataframe modificado
#head(data)

# Simular los datos de turismo urbano (suponiendo que se llamen así en tu archivo)
# Aquí estoy creando datos de ejemplo, pero en tu caso, debes leer los datos de tu archivo Excel
data <- data.frame(
  AMBIENTE_SOCIAL_URBANO = rnorm(100, mean=3.5, sd=1),
  ARQUITECTURA_URBANA = rnorm(100, mean=3.4, sd=1),
  MONUMENTOS_SITIOS = rnorm(100, mean=3.6, sd=1),
  ESPACIOS_PUBLICOS = rnorm(100, mean=3.5, sd=1),
  ALOJAMIENTO_RESTAURANTES = rnorm(100, mean=4.3, sd=1),
  SERVICIOS_PUBLICOS = rnorm(100, mean=3.2, sd=1),
  INFORMACION_TURISTICA = rnorm(100, mean=3.4, sd=1),
  TIENDAS_SERVICIOS = rnorm(100, mean=3.4, sd=1),
  MUSEOS_GALERIAS = rnorm(100, mean=3.3, sd=1),
  ACCESO_SENALIZACION = rnorm(100, mean=3.2, sd=1),
  EXCURSIONES = rnorm(100, mean=3.0, sd=1),
  FESTIVALES_EVENTOS = rnorm(100, mean=2.9, sd=1),
  TEATROS_CONCIERTOS = rnorm(100, mean=3.2, sd=1),
  FERIAS_CONVENCIONES = rnorm(100, mean=3.1, sd=1),
  SEGMENTO = sample(c("Servicios de alojamiento y restaurante", "Múltiples atracciones", "Turismo pasivo"), 100, replace = TRUE)
)

# Convertir la columna SEGMENTO a factor si no lo es
data$SEGMENTO <- as.factor(data$SEGMENTO)

# Variables de interés
variables <- c("AMBIENTE_SOCIAL_URBANO", "ARQUITECTURA_URBANA", "MONUMENTOS_SITIOS", "ESPACIOS_PUBLICOS",
               "ALOJAMIENTO_RESTAURANTES", "SERVICIOS_PUBLICOS", "INFORMACION_TURISTICA", "TIENDAS_SERVICIOS",
               "MUSEOS_GALERIAS", "ACCESO_SENALIZACION", "EXCURSIONES", "FESTIVALES_EVENTOS", "TEATROS_CONCIERTOS",
               "FERIAS_CONVENCIONES")

# Inicializar dataframe para resultados
resultados <- data.frame(Variable = character(),
                         Servicios_aloja_rest = numeric(),
                         Multiples_atracciones = numeric(),
                         Turismo_pasivo = numeric(),
                         Kruskal_Wallis_H = numeric(),
                         Sig = numeric(),
                         Mann_Whitney = character(),
                         stringsAsFactors = FALSE)

# Función para obtener resultados de Mann-Whitney
get_mann_whitney <- function(data, var) {
  mw <- pairwise.wilcox.test(data[[var]], data$SEGMENTO, p.adjust.method = "bonferroni")
  res <- mw$p.value
  comparisons <- c("Servicios de alojamiento y restaurante" = "1",
                   "Múltiples atracciones" = "2",
                   "Turismo pasivo" = "3")
  mw_res <- apply(res, 1, function(row) {
    paste(comparisons[rownames(res)], comparisons[colnames(res)], row, sep = " vs ")
  })
  return(paste(mw_res, collapse = "; "))
}

# Calcular estadísticas
for (var in variables) {
  # Calcular medias por segmento
  medias <- data %>% group_by(SEGMENTO) %>% summarize(mean = mean(get(var), na.rm = TRUE)) %>% pull(mean)
  
  # Prueba de Kruskal-Wallis
  kw <- kruskal.test(get(var) ~ SEGMENTO, data = data)
  
  # Prueba de Mann-Whitney
  mw <- get_mann_whitney(data, var)
  
  # Agregar resultados al dataframe
  resultados <- resultados %>%
    add_row(Variable = var,
            Servicios_aloja_rest = medias[1],
            Multiples_atracciones = medias[2],
            Turismo_pasivo = medias[3],
            Kruskal_Wallis_H = round(kw$statistic, 2),
            Sig = round(kw$p.value, 3),
            Mann_Whitney = mw)
}

# Imprimir la tabla
print(resultados)
#############33333

# Simular datos de recomendaciones
set.seed(123)  # Para reproducibilidad
data$RECOMENDACION <- sample(1:10, size = nrow(data), replace = TRUE)

# Asignar segmentos (simulado)
data$SEGMENTO <- sample(c("Servicios de alojamiento y restaurante", "Múltiples atracciones", "Turismo pasivo"), 
                        nrow(data), replace = TRUE)

# Crear una columna categórica para las recomendaciones
data$RECOMENDACION_CAT <- cut(data$RECOMENDACION, breaks = c(0, 6, 8, 10), labels = c("Detractores", "Neutral", "Promotores"))

# Calcular los porcentajes de cada categoría en cada segmento
tabla_final <- data %>%
  group_by(SEGMENTO, RECOMENDACION_CAT) %>%
  summarize(n = n(), .groups = 'drop') %>%
  mutate(percent = n / sum(n) * 100) %>%
  select(-n) %>%
  spread(RECOMENDACION_CAT, percent, fill = 0)

# Asegurarse de que las columnas sean numéricas
tabla_final <- tabla_final %>%
  mutate(across(Detractores:Promotores, as.numeric))

# Calcular el total de cada categoría
tabla_final <- tabla_final %>%
  ungroup() %>%
  mutate(Total = rowSums(select(., Detractores, Neutral, Promotores)))

# Calcular el total global
totales <- tabla_final %>%
  summarize(across(Detractores:Total, mean))

# Agregar fila de totales
tabla_final <- bind_rows(tabla_final, c(SEGMENTO = "Total", totales))

# Realizar prueba Chi-cuadrado
observed <- tabla_final %>%
  filter(SEGMENTO != "Total") %>%
  select(Detractores, Neutral, Promotores) %>%
  as.matrix()

chi2 <- chisq.test(observed)

# Añadir valores de Chi-cuadrado y significancia a la tabla
tabla_final <- tabla_final %>%
  mutate(Chi2 = ifelse(SEGMENTO == "Total", round(chi2$statistic, 3), NA),
         Sig = ifelse(SEGMENTO == "Total", round(chi2$p.value, 3), NA))

# Imprimir la tabla final
print(tabla_final)

# Opcional: guardar los resultados en un archivo Excel
write.xlsx(tabla_final, "resultados_segmentacion_turismo_urbano.xlsx")

##############################################
# Simular datos de segmentos y motivos de viaje (suponiendo que se llamen así en tu archivo)
set.seed(123)  # Para reproducibilidad
data$SEGMENTO <- sample(c("Servicios de alojamiento y restaurante", "Múltiples atracciones", "Turismo pasivo"), 
                        nrow(data), replace = TRUE)
data$MOTIVO_VIAJE <- sample(c("Negocios, Convenciones, Actividades Profesionales", 
                              "Turismo de ocio", 
                              "Visitar a familiares o amigos", 
                              "Estudios", 
                              "Otros"), 
                            nrow(data), replace = TRUE)

# Calcular los porcentajes de cada categoría en cada segmento
tabla_final <- data %>%
  group_by(MOTIVO_VIAJE, SEGMENTO) %>%
  summarize(n = n(), .groups = 'drop') %>%
  mutate(percent = n / sum(n) * 100) %>%
  select(-n) %>%
  spread(SEGMENTO, percent, fill = 0)

# Asegurarse de que las columnas sean numéricas
tabla_final <- tabla_final %>%
  mutate(across(`Servicios de alojamiento y restaurante`:`Turismo pasivo`, as.numeric))

# Calcular el total de cada categoría
tabla_final <- tabla_final %>%
  rowwise() %>%
  mutate(Total = mean(c_across(`Servicios de alojamiento y restaurante`:`Turismo pasivo`)))

# Calcular el total global
totales <- tabla_final %>%
  summarize(across(`Servicios de alojamiento y restaurante`:Total, sum))

# Agregar fila de totales
tabla_final <- bind_rows(tabla_final, c(MOTIVO_VIAJE = "Total (%)", totales))

# Imprimir la tabla final
print(tabla_final)

# Simular datos de segmentos y primera vez (ajustar estos nombres según tu archivo real)
set.seed(123)  # Para reproducibilidad
data$SEGMENTO <- sample(c("Servicios de alojamiento y restaurante", "Múltiples atracciones", "Turismo pasivo"), 
                        nrow(data), replace = TRUE)
data$PRIMERA_VEZ <- sample(c("No", "Si"), nrow(data), replace = TRUE)

# Calcular los porcentajes de cada categoría en cada segmento
tabla_final <- data %>%
  group_by(PRIMERA_VEZ, SEGMENTO) %>%
  summarize(n = n(), .groups = 'drop') %>%
  mutate(percent = n / sum(n) * 100) %>%
  select(-n) %>%
  spread(SEGMENTO, percent, fill = 0)

# Asegurarse de que las columnas sean numéricas
tabla_final <- tabla_final %>%
  mutate(across(`Servicios de alojamiento y restaurante`:`Turismo pasivo`, as.numeric))

# Calcular el total de cada categoría
tabla_final <- tabla_final %>%
  rowwise() %>%
  mutate(Total = mean(c_across(`Servicios de alojamiento y restaurante`:`Turismo pasivo`)))

# Calcular el total global
totales <- tabla_final %>%
  summarize(across(`Servicios de alojamiento y restaurante`:Total, sum))

# Agregar fila de totalesx
tabla_final <- bind_rows(tabla_final, c(PRIMERA_VEZ = "Total (%)", totales))

# Imprimir la tabla final
print(tabla_final)



###################3

