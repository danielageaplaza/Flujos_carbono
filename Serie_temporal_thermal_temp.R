#Temperatura diaria vs. Thermal diario para cada lado

setwd('C:/Users/Usuario/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos <- read.csv('datos_combinados_graficas.csv', header = TRUE, sep = ";", dec = ".")
datos$Date <- as.Date(datos$Date, format = "%d/%m/%Y")
datos$Treatment <- as.factor(datos$Treatment)
datos$Location <- as.factor(datos$Location)
datos$Number_Collar <- as.factor(datos$Number_Collar)
datos$thermal_celsius<-datos$Thermal -273.15
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

#----------------------------------------------------------------------------#
#NACIMIENTO

# Filtrar para que solo incluya la Location llamada "nacimiento"
datos_nacimiento <- datos %>%
  filter(Location == "nacimiento")

# Calcular la media y el error estándar para Thermal y T_med
datos_resumen_temperatura <- datos_nacimiento %>%
  group_by(Location, Date) %>%
  summarise(
    mean_Thermal = mean(thermal_celsius, na.rm = TRUE),
    sd_Thermal = sd(thermal_celsius, na.rm = TRUE) / sqrt(n()),  # Error estándar para Thermal
    mean_T_med = mean(T_med, na.rm = TRUE),
    sd_T_med = sd(T_med, na.rm = TRUE) / sqrt(n())  # Error estándar para T_med
  ) %>%
  ungroup()

# Generar el gráfico solo para "nacimiento" con el segundo eje Y ajustado
ggplot(data = datos_resumen_temperatura, aes(x = Date)) +
  
  # Línea y puntos para Thermal con el eje Y derecho
  geom_point(aes(y = mean_Thermal , shape = "Thermal"), size = 3, color="#e63946") +
  #geom_errorbar(aes(ymin = (mean_Thermal - sd_Thermal) , ymax = (mean_Thermal + sd_Thermal) ), width = 0.2) +
  
  # Línea y puntos para T_med con el eje Y izquierdo
  geom_point(aes(y = mean_T_med, shape = "T_med"), size = 3, color="#457b9d") +
  #geom_errorbar(aes(ymin = mean_T_med - sd_T_med, ymax = mean_T_med + sd_T_med), width = 0.2) +
  
  # Escala de formas para diferenciar entre las dos variables
  scale_shape_manual(values = c("Thermal" = 16, "T_med" = 17), 
                     labels = c("Temperatura\nambiental", "Temperatura\nsatelital")) +
  
  # Escala para el eje Y izquierdo (Temperatura media) con límite hasta 40
  scale_y_continuous(
    name = "Temperatura (ºC)",
    breaks = seq(0, 50, by = 5),  # Cambiar para que llegue hasta 40
    expand = c(0, 0),
    
    # Eje Y derecho (Thermal) con factor de escala ajustado
   # sec.axis = sec_axis(~ . * 10, name = "Valores del índice", 
                        #breaks = seq(250, 350, by = 10))  # Cambiar para que llegue hasta 350
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

ggsave("Thermal_temp_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#Comprobamos si existe correlación entre variables
correlacion<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header = TRUE, sep = ";", dec = ".")
correlacion_nacimiento<-subset(correlacion,Location=='nacimiento')
cor.test(correlacion_nacimiento$T_med_ifapa,correlacion_nacimiento$Thermal)
plot(correlacion_nacimiento$T_med_ifapa,correlacion_nacimiento$Thermal)
#Existe una correlación positiva de un 87% y significación de p-valor<0.05

# Crear el gráfico
ggplot(data = correlacion_nacimiento, aes(x = T_med_ifapa, y = Thermal)) +
  # Agregar los puntos
  geom_point() +
  # Agregar la línea de tendencia
  geom_smooth(method = "lm", color = "blue") +
  # Etiquetas de los ejes
  labs(x = "Temperatura ambiente", y = "Thermal") +
  # Estilo de la gráfica
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),    # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14)      # Tamaño de los valores en cada eje
  )

ggsave("correlacion_thermal_temp_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#----------------------------------------------------------------------------#
#PORTUGOS

# Filtrar para que solo incluya la Location llamada "portugos"
datos_portugos <- datos %>%
  filter(Location == "portugos")

# Calcular la media y el error estándar para Thermal y T_med
datos_resumen_temperatura <- datos_portugos %>%
  group_by(Location, Date) %>%
  summarise(
    mean_Thermal = mean(thermal_celsius, na.rm = TRUE),
    sd_Thermal = sd(thermal_celsius, na.rm = TRUE) / sqrt(n()),  # Error estándar para Thermal
    mean_T_med = mean(T_med, na.rm = TRUE),
    sd_T_med = sd(T_med, na.rm = TRUE) / sqrt(n())  # Error estándar para T_med
  ) %>%
  ungroup()

# Generar el gráfico solo para "nacimiento" con el segundo eje Y ajustado
ggplot(data = datos_resumen_temperatura, aes(x = Date)) +
  
  # Línea y puntos para Thermal con el eje Y derecho
  geom_point(aes(y = (mean_Thermal) , shape = "Thermal"), size = 3, color = "#e63946") +  # Ajuste para comenzar en 250
  #geom_errorbar(aes(ymin = (mean_Thermal - sd_Thermal) , ymax = (mean_Thermal + sd_Thermal) ), width = 0.2, color = "#e63946") +
  
  # Línea y puntos para T_med con el eje Y izquierdo
  geom_point(aes(y = mean_T_med, shape = "T_med"), size = 3, color = "#457b9d") +
  #geom_errorbar(aes(ymin = mean_T_med - sd_T_med, ymax = mean_T_med + sd_T_med), width = 0.2, color = "#457b9d") +
  
  # Escala de formas para diferenciar entre las dos variables
  scale_shape_manual(values = c("Thermal" = 16, "T_med" = 17), 
                     labels = c("Temperatura\nambiental", "Temperatura\nsatelital")) +
  
  # Escala para el eje Y izquierdo (Temperatura media) con límite hasta 40
  scale_y_continuous(
    name = "Temperatura (ºC)",
    breaks = seq(0, 50, by = 5),  # Límite superior de 40
    expand = c(0, 0),
    
    # Eje Y derecho (Thermal) comenzando en 250
    #sec.axis = sec_axis(~ . + 260, name = "Valores del índice", 
                        #breaks = seq(260, 350, by = 10))  # Eje derecho comenzando en 250 y con límite superior de 350
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

ggsave("Thermal_temp_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#Comprobamos si existe correlación entre variables
correlacion<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header = TRUE, sep = ";", dec = ".")
correlacion_portugos<-subset(correlacion,Location=='portugos')
cor.test(correlacion_portugos$T_med_ifapa,correlacion_portugos$Thermal)
plot(correlacion_portugos$T_med_ifapa,correlacion_portugos$Thermal)
#Existe una correlación positiva de un 88% y significación de p-valor<0.05

# Crear el gráfico
ggplot(data = correlacion_portugos, aes(x = T_med_ifapa, y = Thermal)) +
  # Agregar los puntos
  geom_point() +
  # Agregar la línea de tendencia
  geom_smooth(method = "lm", color = "blue") +
  # Etiquetas de los ejes
  labs(x = "Temperatura ambiente", y = "Thermal") +
  # Estilo de la gráfica
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),    # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14)      # Tamaño de los valores en cada eje
  )

ggsave("correlacion_thermal_temp_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
