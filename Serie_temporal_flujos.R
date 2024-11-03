setwd('C:/Users/Usuario/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header=T, sep=";", dec=".")
datos$Date<-as.Date(datos$Date, format="%d/%m/%Y")
datos$Treatment<-as.factor(datos$Treatment)
datos$Location<-as.factor(datos$Location)
datos$Number_Collar<-as.factor(datos$Number_Collar)

 
#Mostramos con un grafico de lineas la serie temporal completa, para cada flujo y lugar por separado
#en base al tipo de tratamiento que se aplica

#---------------------------------------------------------------------------------#

#Nacimiento CO2  
  
  # Librerias necesarias
  library(ggplot2)
  library(dplyr)
  
  # Filtrado de los datos
  datos_filtrados <- datos %>%
    filter(tipo_gas == "CO2", Location == "nacimiento") %>%
    group_by(Date, Treatment) %>%
    summarise(
      mean_flux = mean(Exp_Flux, na.rm = TRUE),
      sd_flux = sd(Exp_Flux, na.rm = TRUE)
    )
  
  # Crear el grafico
  ggplot(datos_filtrados, aes(x = Date, y = mean_flux, color = Treatment)) +
    geom_point(size = 4) +    # Aumentar tama?o de los puntos
    geom_errorbar(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), 
                  width = 2, size = 0.1) +  # Aumentar grosor de las barras de error
    scale_color_manual(values = c("VP" = "#4F772D", "BS" = "#a7c957"),
                       labels = c("VP" = "Suelo cubierto", "BS" = "Suelo desnudo")) +
    labs(x = "", y = expression("Flujo de CO"[2]~" ("*mu~mol~m^{-2}~s^{-1}*")"), color = "Tratamiento") +
    theme_minimal() +
    scale_x_date(limits=c(as.Date("2022/03/01"),as.Date("2023/11/01")),date_labels = "%b %Y", date_breaks = "1 month") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size=14),
      axis.title.y = element_text(size = 16),  # Aumentar tama?o de letra del t?tulo del eje Y
      axis.text.y = element_text(size = 14),   # Aumentar tama?o de los n?meros del eje Y
      panel.border = element_rect(color = "white", fill = NA, size = 1),  # Marco en los ejes
      axis.line.y.left = element_line(color = "black", size = 0.5),  # L?nea lateral izquierda
      axis.line.x.bottom = element_line(color = "black", size = 0.5), # L?nea de la base
      axis.line.x.top=element_line(color = ""),
      legend.title = element_text(size=14),
      legend.text = element_text(size=14)
    )

ggsave("lineas_flujos_co2_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
  
  #---------------------------------------------------------------------------------#

  #Portugos CO2  
  
  
  # Librerias necesarias
  library(ggplot2)
  library(dplyr)
  
  # Filtrado de los datos
  datos_filtrados <- datos %>%
    filter(tipo_gas == "CO2", Location == "portugos") %>%
    group_by(Date, Treatment) %>%
    summarise(
      mean_flux = mean(Exp_Flux, na.rm = TRUE),
      sd_flux = sd(Exp_Flux, na.rm = TRUE)
    )
  
  # Crear el grafico
  ggplot(datos_filtrados, aes(x = Date, y = mean_flux, color = Treatment)) +
    geom_point(size = 4) +    # Aumentar tama?o de los puntos
    geom_errorbar(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), 
                  width = 2, size = 0.1) +  # Aumentar grosor de las barras de error
    scale_color_manual(values = c("VP" = "#4F772D", "BS" = "#a7c957"),
                       labels = c("VP" = "Suelo cubierto", "BS" = "Suelo desnudo")) +
    labs(x = "", y = expression("Flujo de CO"[2]~" ("*mu~mol~m^{-2}~s^{-1}*")"), color = "Tratamiento") +
    theme_minimal() +
    scale_x_date(limits=c(as.Date("2022/03/01"),as.Date("2023/11/01")),date_labels = "%b %Y", date_breaks = "1 month") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1,size=14),
      axis.title.y = element_text(size = 16),  # Aumentar tama?o de letra del t?tulo del eje Y
      axis.text.y = element_text(size = 14),   # Aumentar tama?o de los n?meros del eje Y
      panel.border = element_rect(color = "white", fill = NA, size = 1),  # Marco en los ejes
      axis.line.y.left = element_line(color = "black", size = 0.5),  # L?nea lateral izquierda
      axis.line.x.bottom = element_line(color = "black", size = 0.5), # L?nea de la base
      axis.line.x.top=element_line(color = ""),
      legend.title=element_text(size=14),
      legend.text = element_text(size=14)
    )
  
ggsave("lineas_flujos_co2_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
 
  #---------------------------------------------------------------------------------#
  
#Nacimiento CH4  
  
  
  # Librerias necesarias
  library(ggplot2)
  library(dplyr)
  
  # Filtrado de los datos
  datos_filtrados <- datos %>%
    filter(tipo_gas == "CH4", Location == "nacimiento") %>%
    group_by(Date, Treatment) %>%
    summarise(
      mean_flux = mean(Exp_Flux, na.rm = TRUE),
      sd_flux = sd(Exp_Flux, na.rm = TRUE)
    )
  
  # Crear el grafico
  ggplot(datos_filtrados, aes(x = Date, y = mean_flux, color = Treatment)) +
    geom_point(size = 4) +    # Aumentar tama?o de los puntos
    geom_errorbar(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), 
                  width = 2, size = 0.1) +  # Aumentar grosor de las barras de error
    scale_color_manual(values = c("VP" = "darkorange3", "BS" = "orange"),
                       labels = c("VP" = "Suelo cubierto", "BS" = "Suelo desnudo")) +
    labs(x = "", y = expression("Flujo de CH"[4]~" ("*n~mol~m^{-2}~s^{-1}*")"), color = "Tratamiento") +
    theme_minimal() +
    scale_x_date(limits=c(as.Date("2022/03/01"),as.Date("2023/11/01")),date_labels = "%b %Y", date_breaks = "1 month") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size=14),
      axis.title.y = element_text(size = 16),  # Aumentar tama?o de letra del t?tulo del eje Y
      axis.text.y = element_text(size = 14),   # Aumentar tama?o de los n?meros del eje Y
      panel.border = element_rect(color = "white", fill = NA, size = 1),  # Marco en los ejes
      axis.line.y.left = element_line(color = "black", size = 0.5),  # L?nea lateral izquierda
      axis.line.x.bottom = element_line(color = "black", size = 0.5), # L?nea de la base
      axis.line.x.top=element_line(color = ""),
      legend.title=element_text(size=14),
      legend.text = element_text(size=14)
    )

ggsave("lineas_flujos_ch4_nacimiento.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  

#---------------------------------------------------------------------------------#
  #Portugos CH4  
  
  
  # Librerias necesarias
  library(ggplot2)
  library(dplyr)
  
  # Filtrado de los datos
  datos_filtrados <- datos %>%
    filter(tipo_gas == "CH4", Location == "portugos") %>%
    group_by(Date, Treatment) %>%
    summarise(
      mean_flux = mean(Exp_Flux, na.rm = TRUE),
      sd_flux = sd(Exp_Flux, na.rm = TRUE)
    )
  
  # Crear el grafico
  ggplot(datos_filtrados, aes(x = Date, y = mean_flux, color = Treatment)) +
    geom_point(size = 4) +    # Aumentar tama?o de los puntos
    geom_errorbar(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), 
                  width = 2, size = 0.1) +  # Aumentar grosor de las barras de error
    scale_color_manual(values = c("VP" = "darkorange3", "BS" = "orange"),
                       labels = c("VP" = "Suelo cubierto", "BS" = "Suelo desnudo")) +
    labs(x = "", y = expression("Flujo de CH"[4]~" ("*n~mol~m^{-2}~s^{-1}*")"), color = "Tratamiento") +
    theme_minimal() +
    scale_x_date(limits=c(as.Date("2022/03/01"),as.Date("2023/11/01")),date_labels = "%b %Y", date_breaks = "1 month") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size=14),
      axis.title.y = element_text(size = 16),  # Aumentar tama?o de letra del t?tulo del eje Y
      axis.text.y = element_text(size = 14),   # Aumentar tama?o de los n?meros del eje Y
      panel.border = element_rect(color = "white", fill = NA, size = 1),  # Marco en los ejes
      axis.line.y.left = element_line(color = "black", size = 0.5),  # L?nea lateral izquierda
      axis.line.x.bottom = element_line(color = "black", size = 0.5), # L?nea de la base
      axis.line.x.top=element_line(color = ""),
      legend.title=element_text(size=16),
      legend.text = element_text(size=14)
    )
  
ggsave("lineas_flujos_ch4_portugos.jpg", width = 14, height = 6, dpi = 600) #Para guardarlo como JPG  
  