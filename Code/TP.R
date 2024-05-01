#BORRA TODAS LAS VENTANAS DE MEMORIA
rm(list=ls(all = TRUE))

##limpia consola
cat("\014")

#Librerias a utilizar
#Librerias de exploracion
install.packages("mlr",dependencies=TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("agricolae",dependencies=TRUE)
install.packages("DescTools",dependencies=TRUE)

library(DescTools) #descripcion de too/ herramientas
library(mlr) #summary
library(ggplot2) 
library(agricolae)## tabla de frecuencia

#1.CARGAR DATOS
setwd("C:/Users/marsi/OneDrive/Escritorio/CURSOS/CICLO 4/Fundamentos de Data Science/")
df <- read.csv ("hotel_bookings.csv", header= TRUE, sep = ",", stringsAsFactors = FALSE)  #numeros separados por decimales => dec

#2.INSPECCIONAR DATOS
head(df)
tail(df)
names(df)
str(df)
summary(df)
summarizeColumns(df)

#Definir formatos
df$hotel<- as.factor(df$hotel)
df$is_canceled<- as.factor(df$is_canceled)
df$arrival_date_month <- factor(df$arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
df$meal<- as.factor(df$meal)
df$country<- as.factor(df$country)
df$market_segment <- as.factor(df$market_segment)
df$distribution_channel <- as.factor(df$distribution_channel)
df$is_repeated_guest<- as.factor(df$is_repeated_guest)
df$reserved_room_type <- as.factor(df$reserved_room_type)
df$assigned_room_type<- as.factor(df$assigned_room_type)
df$deposit_type<- as.factor(df$deposit_type)
df$customer_type<- as.factor(df$customer_type)
df$reservation_status<- as.factor(df$reservation_status)
df$reservation_status_date<- as.Date(df$reservation_status_date)

#Inspeccion de datos con nuevos formatos
str(df)
summary(df)
summarizeColumns(df)

class(df$reservation_status_date)

table(df$hotel)
table(df$arrival_date_month)
table(df$country)

prop.table(table(df$arrival_date_month))


#3.PRE-PROCESAR DATOS
#funcion sin_valor(dataframe) que despliega cuantos valores NA posee cada variable
sin_valor <- function(x){
  for(i in 1:ncol(x)){
    na_indices <- which(is.na(x[[i]])) 
    cat("En la columna", colnames(x)[i], "hay un total de valores NA:", length(na_indices), "\n")
    if(length(na_indices) > 0) {
      cat("Los valores NA se encuentran en las filas:", na_indices, "\n")
    }
  }
}

sin_valor(df)


#funcion en_blanco(dataframe) :  cuantos valores en blanco posee cada variable
en_blanco <- function(x){
  sum = 0 
  for(i in 1:ncol(x)){
    cat("En la columna",colnames(x[i]), "total de valores EN BLANCO:", colSums(x[i]==""), "\n")
  }
}

en_blanco(df)

#OBS: Se observa que la columna childre tiene 4 valores NA
#Los valores NA se encuentran en las filas: 40601 40668 40680 41161 

df$hotel[is.na(df$children)]

summary(df$children)
str(df$children)
table(df$children)  #se observa que solo 1 tiene 10 hijos, lo cual afecta la media al ser un valor atipico
#Analizando esto, decidimos cambiar los 4 valores NA's por la mediana, la cual tiene un valor de 0
#Esto es debido a que la mediana no es afectada por los valores atipicos (en este caso el 10)

df$children[c(40601, 40668, 40680, 41161)] <- 0

summary(df$children)
table(df$children)


#Identificación de datos atípicos
summarizeColumns(df)

identificar_outliers <- function(df) {
  outliers <- list()
  columnas_con_outliers <- c()  
  
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      Q1 <- quantile(df[[i]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[i]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      limite_inferior <- Q1 - 1.5 * IQR
      limite_superior <- Q3 + 1.5 * IQR
      indices_outliers <- which(df[[i]] < limite_inferior | df[[i]] > limite_superior)
      
      if (length(indices_outliers) > 0) {
        outliers[[colnames(df)[i]]] <- indices_outliers
        columnas_con_outliers <- c(columnas_con_outliers, colnames(df)[i])
      }
    }
  }
  
  
  if (length(columnas_con_outliers) > 0) {
    cat("Columnas con outliers:", paste(columnas_con_outliers, collapse = ", "), "\n")
  } else {
    cat("No se encontraron outliers en ninguna columna.\n")
  }
  
  return(outliers)
}


outliers_list <- identificar_outliers(df)
print(outliers_list)

#tecnicas utilizadas para transformar datos atípicos (PREGUNTAR)



#4.VISUALIZAR DATOS 


#ANALISIS DE PREGUNTAS

#¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?
table(df$hotel)
ggplot(df, aes(hotel,fill=hotel)) + geom_bar()+ylab("Numero de reservas por hotel") +theme_get()
#Se observa que la gente prefiere City Hotel sobre Resort Hotel por una gran diferencia


# ¿Está aumentando la demanda con el tiempo?
summary(df$arrival_date_year)
table(df$arrival_date_year)
ggplot(df, aes(arrival_date_year,fill=arrival_date_year)) + geom_bar()+ylab("Numero de reservas") +theme_get()
#Se identifica que el año en que mas reservas hicieron fue el 2016, sin embargo,
#en el 2017, este numero decreció pero sigue siendo mucho mayor al numero de 
#reservas del año 2015

#falta: reservar de distintos años por lo 2 distintos hoteles 



#¿Cuándo se producen las temporadas de reservas: alta, media y baja?
#¿Cuándo es menor la demanda de reservas?
summary(df$arrival_date_month)
ggplot(df, aes(arrival_date_month,fill=arrival_date_month)) + geom_bar()+ylab("Numero de reservas") +theme_get()
#Los meses con mas demandas son agosto y julio
#Temporada alta: mayo, julio, agosto
#Temporada media: marzo, abril, junio, setiembre, octubre
#Temporada baja: noviembre, diciembre, enero, febrero


#¿Cuántas reservas incluyen niños y/o bebes?
summary(df$children)
table(df$children)
sum(df$children)
#hay 12403 de 119390 reservar que incluyen niños
#Esto corresponde a un 10.38864%
(12403*100)/119390

table(df$babies)
sum(df$babies)
(949*100)/119390
#Esto corresponde a un 0.7948739% de personas que reservan con bebes
#Lo cuales un valor muy bajo


#¿Es importante contar con espacios de estacionamiento?
summary(df$required_car_parking_spaces)
table(df$required_car_parking_spaces)
sum(df$required_car_parking_spaces)
(7464*100)/119390
#Solo el 6.25178% requiere espacios de estacionamiento


#¿En qué meses del año se producen más cancelaciones de reservas?
table(df$is_canceled)
table(df$arrival_date_month)
table(df$is_canceled, df$arrival_date_month) 
