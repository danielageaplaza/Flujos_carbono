#Radiación solar diaria vs. albedo diario para cada lado

setwd('C:/Users/Usuario/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos <- read.csv('datos_combinados_graficas.csv', header = TRUE, sep = ";", dec = ".")
datos$Date <- as.Date(datos$Date, format = "%d/%m/%Y")
datos$Treatment <- as.factor(datos$Treatment)
datos$Location <- as.factor(datos$Location)
datos$Number_Collar <- as.factor(datos$Number_Collar)

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

#----------------------------------------------------------------------------#
#NACIMIENTO

# Filtrar para que solo incluya la Location llamada "nacimiento"
datos_nacimiento <- datos %>%
  filter(Location == "nacimiento")

# Calcular la media y el error estándar para Rad y Albedo
datos_resumen_radiacion <- datos_nacimiento %>%
  group_by(Location, Date) %>%
  summarise(
    mean_Rad = mean(Rad, na.rm = TRUE),  # Media de Radiación
    sd_Rad = sd(Rad, na.rm = TRUE) / sqrt(n()),  # Error estándar para Rad
    mean_Albedo = mean(albedo, na.rm = TRUE),  # Media de Albedo
    sd_Albedo = sd(albedo, na.rm = TRUE) / sqrt(n())  # Error estándar para Albedo
  ) %>%
  ungroup()


# Generar el gráfico solo para "nacimiento" con el segundo eje Y ajustado
ggplot(data = datos_resumen_radiacion, aes(x = Date)) +
  
  # Línea y puntos para Albedo con el eje Y izquierdo (valores reales)
  geom_point(aes(y = mean_Albedo, shape = "Albedo"), size = 3, color = "#4daf4a") +  # Color de Albedo
  geom_errorbar(aes(ymin = (mean_Albedo - sd_Albedo), ymax = (mean_Albedo + sd_Albedo)), width = 0.2, color = "#ffb703") +
  
  # Línea y puntos para Rad con el eje Y derecho
  geom_point(aes(y = mean_Rad / 350, shape = "Rad"), size = 3, color = "#fc8d62") +  # Color de Radiación
  geom_errorbar(aes(ymin = (mean_Rad - sd_Rad) / 350, ymax = (mean_Rad + sd_Rad) / 350), width = 0.2, color = "#fb8500") +
  
  # Escala de formas para diferenciar entre las dos variables
  scale_shape_manual(values = c("Rad" = 16, "Albedo" = 17), 
                     labels = c("Albedo", "Radiación")) +
  
  # Escala para el eje Y izquierdo (Albedo) con límite hasta 0.1
  scale_y_continuous(
    name = "Albedo",
    breaks = seq(0, 0.2, by = 0.02),  # Albedo real en el eje Y izquierdo
    expand = c(0, 0),
    
    # Eje Y derecho (Rad) mostrando valores reales
    sec.axis = sec_axis(~ .*350, name = expression("Radiación solar (MJ/m"^2*" d)"), 
                        breaks = seq(0, 40, by = 5))  # Radiación en el eje Y derecho
  ) +
  
  # Escala de fechas en el eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  
  # Etiquetas y tema
  labs(x = "", shape = "Variable") +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  guides(
    shape = guide_legend(order = 1, title = "Variables")
  )

ggsave("Albedo_radiacion_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#Comprobamos si existe correlación entre variables
correlacion<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header = TRUE, sep = ";", dec = ".")
correlacion_nacimiento<-subset(correlacion,Location=='nacimiento')
cor.test(correlacion_nacimiento$Rad_ifapa,correlacion_nacimiento$albedo)
plot(correlacion_nacimiento$Rad_ifapa,correlacion_nacimiento$albedo)
#Existe una correlación positiva de un 59% y significación de p-valor<0.05

# Crear el gráfico
ggplot(data = correlacion_nacimiento, aes(x = Rad_ifapa, y = albedo)) +
  # Agregar los puntos
  geom_point() +
  # Agregar la línea de tendencia
  geom_smooth(method = "lm", color = "blue") +
  # Etiquetas de los ejes
  labs(x = "Radiación solar", y = "Albedo") +
  # Estilo de la gráfica
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),    # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14)      # Tamaño de los valores en cada eje
  )

ggsave("correlacion_rad_albedo_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#----------------------------------------------------------------------------#
#PORTUGOS

# Filtrar para que solo incluya la Location llamada "portugos"
datos_portugos <- datos %>%
  filter(Location == "portugos")

# Calcular la media y el error estándar para Rad y Albedo
datos_resumen_radiacion <- datos_portugos %>%
  group_by(Location, Date) %>%
  summarise(
    mean_Rad = mean(Rad, na.rm = TRUE),  # Media de Radiación
    sd_Rad = sd(Rad, na.rm = TRUE) / sqrt(n()),  # Error estándar para Rad
    mean_Albedo = mean(albedo, na.rm = TRUE),  # Media de Albedo
    sd_Albedo = sd(albedo, na.rm = TRUE) / sqrt(n())  # Error estándar para Albedo
  ) %>%
  ungroup()

# Generar el gráfico solo para "nacimiento" con el segundo eje Y ajustado
ggplot(data = datos_resumen_radiacion, aes(x = Date)) +
  
  # Línea y puntos para Albedo con el eje Y izquierdo (valores reales)
  geom_point(aes(y = mean_Albedo, shape = "Albedo"), size = 3, color = "#4daf4a") +  # Color de Albedo
  geom_errorbar(aes(ymin = (mean_Albedo - sd_Albedo), ymax = (mean_Albedo + sd_Albedo)), width = 0.2, color = "#ffb703") +
  
  # Línea y puntos para Rad con el eje Y derecho
  geom_point(aes(y = mean_Rad / 350, shape = "Rad"), size = 3, color = "#fc8d62") +  # Color de Radiación
  geom_errorbar(aes(ymin = (mean_Rad - sd_Rad) / 350, ymax = (mean_Rad + sd_Rad) / 350), width = 0.2, color = "#fb8500") +
  
  
  # Escala de formas para diferenciar entre las dos variables
  scale_shape_manual(values = c("Rad" = 16, "Albedo" = 17), 
                     labels = c("Albedo", "Radiación")) +
  
  # Escala para el eje Y izquierdo (Albedo) con límite hasta 0.1
  scale_y_continuous(
    name = "Albedo",
    breaks = seq(0, 0.2, by = 0.02),  # Albedo real en el eje Y izquierdo
    expand = c(0, 0),
    
    # Eje Y derecho (Rad) mostrando valores reales
    sec.axis = sec_axis(~ .*350, name = expression("Radiación solar (MJ/m"^2*" d)"), 
                        breaks = seq(0, 40, by = 5))  # Radiación en el eje Y derecho
  ) +
  
  # Escala de fechas en el eje X
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  
  # Etiquetas y tema
  labs(x = "", shape = "Variable") +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  guides(
    shape = guide_legend(order = 1, title = "Variables")
  )

ggsave("Albedo_radiacion_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#Comprobamos si existe correlación entre variables
correlacion<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header = TRUE, sep = ";", dec = ".")
correlacion_portugos<-subset(correlacion,Location=='portugos')
cor.test(correlacion_portugos$Rad_ifapa,correlacion_portugos$albedo)
plot(correlacion_portugos$Rad_ifapa,correlacion_portugos$albedo)
#correlación relativamente baja, con un 18% pero significativa con p-valor<0.05

# Crear el gráfico
ggplot(data = correlacion_portugos, aes(x = Rad_ifapa, y = albedo)) +
  # Agregar los puntos
  geom_point() +
  # Agregar la línea de tendencia
  geom_smooth(method = "lm", color = "blue") +
  # Etiquetas de los ejes
  labs(x = "Radiación solar", y = "Albedo", size=18) +
  # Estilo de la gráfica
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),    # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14)      # Tamaño de los valores en cada eje
  )

ggsave("correlacion_rad_albedo_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
