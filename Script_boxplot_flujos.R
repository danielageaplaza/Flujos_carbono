#Vamos a realizar un analisis visual con los datos de flujo de CO2 y CH4 a través de boxplot, 
#con la base de datos total para ambos lugares.

setwd('C:/Users/Sobis/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos<-read.csv('Datos_filtrados_melt_R2_temp.csv', header=T, sep=";", dec=".")
datos$Date<-as.Date(datos$Date, format="%d/%m/%Y")
datos_pre_dcast <- datos[, c("Exp_Flux", "tipo_gas", "Treatment", "Estacion","Location","Date")]

#install.packages("reshape2")
library(reshape2)
#install.packages("ggplot2") 
library(ggplot2)

# Creamos 2 dataframe: uno solamente para el flujo de CO2 y otro para el de CH4

indice<-datos_pre_dcast$tipo_gas=='CO2'
table(indice)
df_co2<-datos_pre_dcast[indice,]
df_ch4<-datos_pre_dcast[!indice,]


# Definir colores para cada lugar

colores_CO2<-c("VP"="#4F772D","BS"="#a7c957")
colores_CH4<-c("VP"="darkorange3","BS"="orange")

#------------------------------------------------------------------------------#

#CALCULAMOS LOS VALORES ANUALES CO2

library(ggplot2)

ggplot(df_co2, aes(x = Location, y = Exp_Flux, fill = Treatment)) +
  geom_boxplot() +
  
  labs(
    x = "", 
    y = expression("Flujo de CO"[2] ~ "(µmol m"^{-2}~"s"^{-1}~")"),  # Notación científica para las unidades
    fill = "Tratamiento"  # Cambia el título de la leyenda a "Tratamiento"
  ) +
  
  scale_fill_manual(values = colores_CO2,
                    labels = c("BS" = "Suelo desnudo", "VP" = "Suelo cubierto")) +
  
  theme(
    axis.text.x = element_text(color = "black", size = 14),  # Aumenta el tamaño del texto en el eje X
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(face = "bold", size = 16),  # Aumenta el tamaño del título del eje Y
    panel.background = element_rect(fill = "grey96"),
    
    # Establecer el marco solo para el eje Y y el eje X
    panel.border = element_blank(),  # Eliminar el borde del panel
    axis.line.x = element_line(size = 0.5, color = "black"),  # Marco inferior (eje X)
    axis.line.y = element_line(size = 0.5, color = "black"),   # Marco izquierdo (eje Y)
    legend.title=element_text(size=14),
    legend.text=element_text(size=14)
  ) +
  
  scale_x_discrete(labels = c("Ladera Norte", "Ladera Sur")) +  # Asegura que comiencen con mayúscula
  scale_y_continuous(limits = c(0, 9), expand = expansion(mult = c(0, 0)))  # Asegura que 0 esté en la línea base

#ggsave("boxplot_co2_anual.jpg", width = 8, height = 6, dpi = 600) #Para guardarlo como JPG

#------------------------------------------------------------------------------#

#CALCULAMOS LOS VALORES ANUALES CH4

ggplot(df_ch4, aes(x = Location, y = Exp_Flux, fill = Treatment)) +
  geom_boxplot() +
  labs(
    x = "", 
    y = expression("Flujo de CH"[4] ~ "(nmol m"^{-2}~"s"^{-1}~")"),  # Notaci?n cient?fica para las unidades
    fill = "Tratamiento" # Cambia el t?tulo de la leyenda a "Tratamiento"
    
  ) +
  scale_fill_manual(values = colores_CH4,
                    labels=c("BS"="Suelo desnudo","VP"="Suelo cubierto")) +
  theme(
    axis.text.x = element_text(color="black",size = 14),  # Aumenta el tama?o del texto en el eje X
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(face="bold",size = 16),  # Aumenta el tama?o del t?tulo del eje Y
    panel.background = element_rect(fill = "grey96"),
    panel.border = element_blank(),  # Eliminar el borde del panel
    axis.line.x = element_line(size = 0.5, color = "black"),  # Marco inferior (eje X)
    axis.line.y = element_line(size = 0.5, color = "black"),   # Marco izquierdo (eje Y)
    legend.title=element_text(size=14),
    legend.text=element_text(size=14)
  ) +
  scale_x_discrete(labels = c("Ladera Norte", "Ladera Sur"))+   # Asegura que comiencen con may?scula
  ylim(-1.5, 0.5) 

#ggsave("boxplot_ch4_anual.jpg", width = 8, height = 6, dpi = 600) #Para guardarlo como JPG
