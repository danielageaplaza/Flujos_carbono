setwd('C:/Users/Sobis/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos<-read.csv('ifapa.csv', header=T, sep=";", dec=".")
datos$Date<-as.Date(datos$Date, format="%d/%m/%Y")



# Instalar y cargar ggplot2 si no lo tienes instalado
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


# Factor de escala para alinear los ejes
factor_escala <- 1.8 * max(datos$Pp_ifapa, na.rm = TRUE) / max(datos$T_med_ifapa, na.rm = TRUE)

# Crear una columna de mes y año para agrupar
datos <- datos %>%
  mutate(Month = floor_date(Date, "month"))

# Agrupar y resumir por mes y ubicación
datos_mensuales <- datos %>%
  group_by(Month, Location) %>%
  summarise(
    Pp_ifapa_acum = sum(Pp_ifapa, na.rm = TRUE),  # Precipitación acumulada mensual
    T_ifapa_mean = mean(T_med_ifapa, na.rm = TRUE)  # Temperatura media mensual
  ) %>%
  filter(!is.na(Month))  # Asegurarnos de que no haya meses con valores NA

# Crear la gráfica
ggplot(datos_mensuales, aes(x = Month)) +
  
  # Barras para la precipitación acumulada mensual, adyacentes para cada ubicación
  geom_bar(aes(y = Pp_ifapa_acum, fill = Location), 
           stat = "identity", position = "dodge", width = 25) +  # Ancho de barras ajustado
  
  # Línea y puntos para la temperatura de "Ladera Norte" (nacimiento)
  geom_line(aes(y = T_ifapa_mean * factor_escala, color = "Ladera Norte", group = Location),
            size = 1, data = subset(datos_mensuales, Location == "nacimiento")) +  # Línea roja oscura
  geom_point(aes(y = T_ifapa_mean * factor_escala), shape = 16, size = 2, color = "#9e2a2b",
             data = subset(datos_mensuales, Location == "nacimiento")) +  # Puntos para Ladera Norte
  
  # Línea y triángulos para la temperatura de "Ladera Sur" (portugos)
  geom_line(aes(y = T_ifapa_mean * factor_escala, color = "Ladera Sur", group = Location),
            size = 1, data = subset(datos_mensuales, Location == "portugos")) +  # Línea verde
  geom_point(aes(y = T_ifapa_mean * factor_escala), shape = 17, size = 2, color = "#e09f3e",
             data = subset(datos_mensuales, Location == "portugos")) +  # Triángulos para Ladera Sur
  
  # Eje Y izquierdo para la precipitación acumulada mensual
  scale_y_continuous(
    name = "Precipitación (mm)", 
    limits = c(0, 100),  # Ajustar límite superior al máximo de la precipitación acumulada (con margen)
    breaks = pretty(c(0, 100), n = 6),  # Escala de precipitación ajustada
    expand = c(0, 0),
    sec.axis = sec_axis(~ ./factor_escala, 
                        name = expression("Temperatura (ºC)"), 
                        breaks = pretty(c(0, 30), n = 6)  # Escala para la temperatura
    )
  ) +
  
  # Colores para las barras de precipitación
  scale_fill_manual(
    values = c("nacimiento" = "#4361ee", "portugos" = "#4cc9f0"),
    labels = c("nacimiento" = "Ladera Norte", "portugos" = "Ladera Sur")  # Cambiar etiquetas en la leyenda
  ) +
  
  # Colores para las líneas de temperatura
  scale_color_manual(values = c("Ladera Norte" = "#9e2a2b", "Ladera Sur" = "#e09f3e")) +  # Rojo oscuro y verde
  
  # Ajustar la escala temporal de marzo 2022 a noviembre 2023
  scale_x_date(limits = c(as.Date("2022-03-01"), as.Date("2023-10-31")),
               date_breaks = "1 month", date_labels = "%b %Y") +
  
  # Etiquetas y leyenda
  labs(x = "", fill = "Precipitación acumulada", color = "Temperatura media") +
  
  # Ajustes de tema
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(size = 14),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title=element_text(size=14, hjust=0.5),
    legend.key.size = unit(1.5, "lines"),
    legend.key = element_rect(fill = "white", color = "white"),
    axis.title.y.right = element_text(margin = margin(l = 10), size = 16),
    axis.title.y.left = element_text(margin = margin(r = 10), size = 16),
    axis.title.x = element_text(size = 16),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.5)
  ) +
  
  # Modificar la leyenda para agrupar precipitación y temperatura bajo títulos específicos
  guides(
    fill = guide_legend(title = "Precipitación\nacumulada"),  # Título para la precipitación
    color = guide_legend(title = "Temperatura media")  # Título para la temperatura
  )

#ggsave("serie_temporal_pp_acumu_temp.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

