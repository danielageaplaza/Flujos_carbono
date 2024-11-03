#Serie temporal de los índices LSWI y NDWI frente a la precipitación diaria

setwd('C:/Users/Usuario/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos <- read.csv('datos_combinados_graficas.csv', header = TRUE, sep = ";", dec = ".")
datos$Date <- as.Date(datos$Date, format = "%d/%m/%Y")
datos$Treatment <- as.factor(datos$Treatment)
datos$Location <- as.factor(datos$Location)
datos$Number_Collar <- as.factor(datos$Number_Collar)
datos$NDWI<-abs(datos$NDWI) #Lo ponemos en valor absoluto

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

#----------------------------------------------------------------------------#

#NACIMIENTO

# Filtrar para que solo incluya la Location llamada "nacimiento"
datos_nacimiento <- datos %>%
  filter(Location == "nacimiento")

# Calcular la media y el error estándar para LSWI, NDWI y Pp
datos_resumen <- datos_nacimiento %>%
  group_by(Location, Date) %>%
  summarise(
    mean_LSWI = mean(LSWI, na.rm = TRUE),
    sd_LSWI = sd(LSWI, na.rm = TRUE) / sqrt(n()),  # Error estándar para LSWI
    mean_NDWI = mean(NDWI, na.rm = TRUE),
    sd_NDWI = sd(NDWI, na.rm = TRUE) / sqrt(n()),  # Error estándar para NDWI
    mean_Pp = mean(Pp, na.rm = TRUE),
    sd_Pp = sd(Pp, na.rm = TRUE) / sqrt(n())       # Error estándar para Pp
  ) %>%
  ungroup()

# Crear el gráfico con dos ejes Y y diferentes símbolos para los índices
ggplot(data = datos_resumen, aes(x = Date)) +
  
  # Línea y puntos para LSWI (eje Y izquierdo)
  geom_point(aes(y = mean_LSWI, shape = "puntos"), size = 3, color = "#1b9e77") +
  geom_errorbar(aes(ymin = mean_LSWI - sd_LSWI, ymax = mean_LSWI + sd_LSWI), width = 0.2, color = "#1b9e77") +
  
  # Línea y triángulos para NDWI (eje Y izquierdo)
  geom_point(aes(y = mean_NDWI, shape = "triangulos"), size = 3, color = "#d95f02") +
  geom_errorbar(aes(ymin = mean_NDWI - sd_NDWI, ymax = mean_NDWI + sd_NDWI), width = 0.2, color = "#d95f02") +
  
  # Línea para la precipitación ajustada al eje derecho con factor de escala
  # Barras para la precipitación acumulada mensual
  geom_bar(aes(y = mean_Pp / 50, fill = "Precipitación"), 
           stat = "identity", position = "identity", width = 5) +
  geom_errorbar(aes(ymin = (mean_Pp - sd_Pp) / 50, ymax = (mean_Pp + sd_Pp) / 50), width = 0.2, color = "#7570b3") +
  
  scale_fill_manual(values = "#219ebc")+
  
  # Escala para el eje Y izquierdo (Índices espectrales)
  scale_y_continuous(
    name = "Índices espectrales",
    breaks = seq(0, 1, by = 0.2),  # Ajusta según el rango de tus índices
    limits = c(0, 1),
    expand=c(0,0),
    
    # Eje Y derecho para Precipitación con escala ajustada
    sec.axis = sec_axis(~ . * 50, name = "Precipitación (mm)", breaks = seq(0, 50, by = 10))
  ) +
  
  # Escala de fechas en el eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  
  # Ajuste de formas y linetype para diferenciar visualmente las variables
  scale_shape_manual(values = c("puntos" = 16, "triangulos" = 17),
                     labels = c("LSWI", "NDWI")) +
  
  # Etiquetas y tema
  labs(x = "", shape = "Variables", fill='') +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

#ggsave("LSWI_NDWI_PP_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  


#----------------------------------------------------------------------------#

#PORTUGOS

# Filtrar para que solo incluya la Location llamada "portugos"
datos_portugos <- datos %>%
  filter(Location == "portugos")

# Calcular la media y el error estándar para LSWI, NDWI y Pp
datos_resumen <- datos_portugos %>%
  group_by(Location, Date) %>%
  summarise(
    mean_LSWI = mean(LSWI, na.rm = TRUE),
    sd_LSWI = sd(LSWI, na.rm = TRUE) / sqrt(n()),  # Error estándar para LSWI
    mean_NDWI = mean(NDWI, na.rm = TRUE),
    sd_NDWI = sd(NDWI, na.rm = TRUE) / sqrt(n()),  # Error estándar para NDWI
    mean_Pp = mean(Pp, na.rm = TRUE),
    sd_Pp = sd(Pp, na.rm = TRUE) / sqrt(n())       # Error estándar para Pp
  ) %>%
  ungroup()

# Crear el gráfico con dos ejes Y y diferentes símbolos para los índices
ggplot(data = datos_resumen, aes(x = Date)) +
  
  # Línea y puntos para LSWI (eje Y izquierdo)
  geom_point(aes(y = mean_LSWI, shape = "puntos"), size = 3, color = "#1b9e77") +
  geom_errorbar(aes(ymin = mean_LSWI - sd_LSWI, ymax = mean_LSWI + sd_LSWI), width = 0.2, color = "#1b9e77") +
  
  # Línea y triángulos para NDWI (eje Y izquierdo)
  geom_point(aes(y = mean_NDWI, shape = "triangulos"), size = 3, color = "#d95f02") +
  geom_errorbar(aes(ymin = mean_NDWI - sd_NDWI, ymax = mean_NDWI + sd_NDWI), width = 0.2, color = "#d95f02") +
  
  # Línea para la precipitación ajustada al eje derecho con factor de escala
  # Barras para la precipitación acumulada mensual
  geom_bar(aes(y = mean_Pp / 50, fill = "Precipitación"), 
           stat = "identity", position = "identity", width = 5) +
  geom_errorbar(aes(ymin = (mean_Pp - sd_Pp) / 50, ymax = (mean_Pp + sd_Pp) / 50), width = 0.2, color = "#7570b3") +
  
  scale_fill_manual(values = "#219ebc")+
  
  # Escala para el eje Y izquierdo (Índices espectrales)
  scale_y_continuous(
    name = "Índices espectrales",
    breaks = seq(0, 1, by = 0.2),  # Ajusta según el rango de tus índices
    limits = c(0, 1),
    expand = c(0, 0),
    
    # Eje Y derecho para Precipitación con escala ajustada
    sec.axis = sec_axis(~ . * 50, name = "Precipitación (mm)", breaks = seq(0, 50, by = 10))
  ) +
  
  # Escala de fechas en el eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  
  # Ajuste de formas y linetype para diferenciar visualmente las variables
  scale_shape_manual(values = c("puntos" = 16, "triangulos" = 17),
                     labels = c("LSWI", "NDWI")) +
  
  # Etiquetas y tema
  labs(x = "", shape = "Variables", fill='') +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

#ggsave("LSWI_NDWI_PP_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
