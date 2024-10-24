setwd('C:/Users/Usuario/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos<-read.csv('Datos_filtrados_melt_R2_satelites_combinados.csv', header=T, sep=";", dec=".",fileEncoding = "UTF-8-BOM")

#Vamos a comprobar si se le puede aplicar un modelo estadistico ANOVA

 datos$Date<-as.Date(datos$Date, format="%d/%m/%Y")
 datos$Treatment<-as.factor(datos$Treatment)
 datos$Location<-as.factor(datos$Location)
 datos$Number_Collar<-as.factor(datos$Number_Collar)
 datos$Estacion<-as.factor(datos$Estacion)

 library(lubridate)
 library(MASS)
 library(vcd)
 library(randomForest)
 library(performance)
 library(permuco)
 
 #Añadimos el año a la base de datos de partida
 datos$year<-year(datos$Date)
 
 #Creamos data frame para cada tipo de gas
 
 indice<-datos$tipo_gas=='CO2'
 table(indice)
 df_co2<-datos[indice,]
 df_ch4<-datos[!indice,]
 
 #Creamos data frame para cada gas y cada localizacion
 
df_co2_nacimiento<-subset(df_co2,Location=='nacimiento')
df_co2_portugos<-subset(df_co2,Location=='portugos')
df_ch4_nacimiento<-subset(df_ch4,Location=='nacimiento')
df_ch4_portugos<-subset(df_ch4,Location=='portugos')

#ANOVA es estadistico parametrico, teniendo que cumplir previamente: 
# - Independencia de datos
# - Normalidad de los residuos (test de Saphiro-Wilk)
# - Homocedasticidad, que es la homogeneidad de varianza de los grupos a comparar
#(test de Levene)

#En este caso, no hay independencia de datos, ya que son subréplica cada collar,
#puesto que se mide cada punto varias veces en el tiempo. Así, probamos con un 
#ANOVA con permutaciones

#-----------------------------------------------------------------------------#

#ANOVA no paramétrico para CO2 en Nacimiento

resultado_anova_co2 <- aovperm(
  Exp_Flux ~ Treatment,
  data = df_co2_nacimiento,
  np = 5000  # Número de permutaciones
)

# Ver los resultados
summary(resultado_anova_co2)

#-----------------------------------------------------------------------------#

#ANOVA no paramétrico para CO2 en Portugos

resultado_anova_co2 <- aovperm(
  Exp_Flux ~ Treatment,
  data = df_co2_portugos,
  np = 5000  # Número de permutaciones
)

# Ver los resultados
summary(resultado_anova_co2)

#-----------------------------------------------------------------------------#

#ANOVA no parametrico para CH4 en Nacimiento

resultado_anova_ch4 <- aovperm(
  Exp_Flux ~ Treatment * Location, 
  data = df_ch4_nacimiento,
  np = 5000  # N?mero de permutaciones
)

# Ver los resultados
summary(resultado_anova_ch4)

# Graficar diagn?sticos del modelo
plot(resultado_anova_ch4)

#----------------------------------------------------------------------------#

#ANOVA no parametrico para CH4 en Portugos

resultado_anova_ch4 <- aovperm(
  Exp_Flux ~ Treatment * Location, 
  data = df_ch4_portugos,
  np = 5000  # N?mero de permutaciones
)

# Ver los resultados
summary(resultado_anova_ch4)

# Graficar diagn?sticos del modelo
plot(resultado_anova_ch4)

#----------------------------------------------------------------------------#

shapiro.test(df_co2_nacimiento$Exp_Flux) #Primero lo hacemos para ver si hay normalidad. No lo hay

shapiro.test(log(df_co2_nacimiento$Exp_Flux)) #No muestra normalidad si se hace una correccion logaritmica

hist(df_co2_nacimiento$Exp_Flux) #Parece poisson la distribucion

#Comprobamos si nuestra distribucion es de poisson ya que no cumple la normal
#y visualmente (con hist), parece poisson


#install.packages("vcd")

library(vcd)

gf=goodfit(df_co2_nacimiento$Exp_Flux,type="poisson",method="ML")

summary(gf) #Es >0.05, por lo que aceptamos la hipptesis nula: no hay diferencias con distribucion Poisson
#es decir, tenemos distribucion Poisson
plot(gf)


#La comprobación la realizamos para cada flujo y localización. Las conclusiones han sido las mismas:
#Nuestra distribución sigue un modelo Poisson


#Usaremos un GLMM (modelo mixto generalizado con distribución Poisson y la réplica como factor aleatorio)
