setwd('C:/Users/Sobis/OneDrive - UNIVERSIDAD DE GRANADA/Tesis y TFG/Datos')
datos<-read.table('Datos_filtrados_melt_R2_satelites_combinados.csv', header=T, sep=";", dec=".")

#Tras las conclusiones de los script anteriores, procedemos a realizar un GLMM

datos$Date<-as.Date(datos$Date, format="%d/%m/%Y")
datos$Treatment<-as.factor(datos$Treatment)
datos$Location<-as.factor(datos$Location)
datos$Number_Collar<-as.factor(datos$Number_Collar)
datos$Estacion<-as.factor(datos$Estacion)

indice<-datos$tipo_gas=='CO2'
table(indice)
df_co2<-datos[indice,]
df_ch4<-datos[!indice,]

library(lubridate)
library(MASS)
library(vcd)
library(randomForest)

#Treatment ANUAL CO2 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "nacimiento"),family=poisson)
summary(model)
boxplot(df_co2$Treatment,df_co2$Exp_Flux)

#Al ser p-valor <0'05, diferencia significativa


#Treatment Otono CO2 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "nacimiento" & Estacion =='Otono'),family=poisson) 
summary (model)

#Al ser p-valor <0'05, diferencia significativa


#Treatment Invierno CO2 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "nacimiento" & Estacion =='Invierno'),family=poisson) 
summary (model)

#Al ser p-valor <0'05, diferencia significativa

#Treatment Primavera CO2 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "nacimiento" & Estacion =='Primavera'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Verano CO2 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "nacimiento" & Estacion =='Verano'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#----------------------------------------------------------------------#

#Treatment ANUAL CH4 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "nacimiento"),family=poisson)
summary(model)
boxplot(df_ch4$Treatment,df_ch4$Exp_Flux)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Otono CH4 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "nacimiento" & Estacion =='Otono'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa


#Treatment Invierno CH4 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "nacimiento" & Estacion =='Invierno'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Primavera CH4 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "nacimiento" & Estacion =='Primavera'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Verano CH4 en Nacimiento

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "nacimiento" & Estacion =='Verano'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#-----------------------------------------------------------------------------#

#Treatment ANUAL CO2 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "portugos"),family=poisson)
summary(model)
boxplot(df_co2$Treatment,df_co2$Exp_Flux)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Otono CO2 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "portugos" & Estacion =='Otono'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa


#Treatment Invierno CO2 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "portugos" & Estacion =='Invierno'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Primavera CO2 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "portugos" & Estacion =='Primavera'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Verano CO2 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_co2, Location == "portugos" & Estacion =='Verano'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#----------------------------------------------------------------------#

#Treatment ANUAL CH4 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "portugos"),family=poisson)
summary(model)
boxplot(df_ch4$Treatment,df_ch4$Exp_Flux)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Otono CH4 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "portugos" & Estacion =='Otono'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa


#Treatment Invierno CH4 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "portugos" & Estacion =='Invierno'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Primavera CH4 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "portugos" & Estacion =='Primavera'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#Treatment Verano CH4 en Portugos

model<-glmmPQL(abs(Exp_Flux) ~  Treatment, random= ~ 1|Number_Collar,data=subset(df_ch4, Location == "portugos" & Estacion =='Verano'),family=poisson) 
summary (model)

#Al ser p-valor >0'05, diferencia NO significativa

#------------------------------------------------------------------------------#

# RF CO2: HAY QUE HACER 8 GRÁFICAS (4 Y 4) PARA CO2 CON VP Y CON BS PARA CADA SITIO Y PARA CADA GAS. 
#Las represento de 4 en 4 (las de co2 en un bloque y las de ch4 en otro)

#----------------------------------------------------------------------------#

#CO2

# Escalamos las variables para que sus distintas magnitudes no tengan influencia sobre el modelo
df_co2_rfs0<-scale(df_co2[,c(15:25)])

cor(df_co2_rfs0, method='spearman') 

df_co2_rfs<-cbind(df_co2[,c(1,5,9,13)],df_co2_rfs0)

#Portugos CO2 VP

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_co2_rfs, Treatment == 'VP' & Location=='portugos'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#plot(df_rf_bs$LS9_EVI,df_rf_bs$Exp_Flux)

#Portugos CO2 BS

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_co2_rfs, Treatment == 'BS' & Location=='portugos'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#Nacimiento CO2 VP

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_co2_rfs, Treatment == 'VP' & Location=='nacimiento'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#Nacimiento CO2 BS

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_co2_rfs, Treatment == 'BS' & Location=='nacimiento'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#----------------------------------------------------------------------------#

#CH4

# Escalamos las variables para que sus distintas magnitudes no tengan influencia sobre el modelo
df_ch4_rfs0<-scale(df_ch4[,c(15:25)])

cor(df_ch4_rfs0, method='spearman') 

df_ch4_rfs<-cbind(df_ch4[,c(1,5,9,13)],df_ch4_rfs0)

#Portugos CH4 VP

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_ch4_rfs, Treatment == 'VP' & Location=='portugos'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#plot(df_rf_bs$LS9_EVI,df_rf_bs$Exp_Flux)

#Portugos CH4 BS

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_ch4_rfs, Treatment == 'BS' & Location=='portugos'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#Nacimiento CH4 VP

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_ch4_rfs, Treatment == 'VP' & Location=='nacimiento'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

#Nacimiento CH4 BS

rf_model<-randomForest(Exp_Flux ~ T_med_ifapa+Rad_ifapa+Pp_ifapa+Pp_acumulada_7+EVI+LSWI+NDVI+NDWI+Thermal+albedo, data = subset(df_ch4_rfs, Treatment == 'BS' & Location=='nacimiento'), importance=T, na.action=na.omit, proximity=T,ntree=2000)

rf_model
varImpPlot(rf_model)

