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
install.packages("dplyr",dependencies=TRUE)

library(DescTools) #descripcion de too/ herramientas
library(mlr) #summary
library(ggplot2) 
library(agricolae)## tabla de frecuencia
library(dplyr)


##OBJETIVO GENERAL
#Realizar un análisis exploratorio de un conjunto de datos (EDA), creando visualizaciones, 
#preparando los datos y obteniendo inferencias básicas utilizando R/RStudio como herramienta de software

#OBJETIVOS ESPECÍFICOS
#Fidelizar a los clientes más recurrentes de cada hotel
#Identificar los servicios necesarios que deben mejorarse en cada hotel

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
df$required_car_parking_spaces<- as.factor(df$required_car_parking_spaces)
df$babies<- as.numeric(df$babies)
df$children<-as.numeric(df$children)

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
#Esto es debido a que la mediana no es muy afectada por los valores atipicos (en este caso el 10)

df$children[c(40601, 40668, 40680, 41161)] <- 0

summary(df$children)
table(df$children)


#Identificación de datos atípicos
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
    for (columna in columnas_con_outliers) {
      cat("Número de outliers en", columna, ":", length(outliers[[columna]]), "\n")
    }
  } else {
    cat("No se encontraron outliers en ninguna columna.\n")
  }
  
  return(outliers)
}



outliers_list <- identificar_outliers(df)
print(outliers_list)

#Interpretacion de los valores atipicos
#Las variables que tienen outliers son 12 de 32 en total:
#lead_time, stays_in_weekend_nights, stays_in_week_nights, adults, 
#children, babies, previous_cancellations, previous_bookings_not_canceled, 
#booking_changes, days_in_waiting_list, adr, required_car_parking_spaces, total_of_special_requests 

#Analizaremos cada caso para identificar cuales son las variables que son logicas que tengan
#valores atipicos y cuales no

#Columna lead_time: Número de días que transcurrieron desde la fecha de reserva y la fecha de llegada
summary(df$lead_time)
#Identificamos que es normal que hayan datos atipicos, pues en caso de la vida real, sucede que existen personas que
#les gusta reservar con anticipación y no se puede dejar de lado ello.
#Sin embargo, para la investigacion de nuestros objetivos, esta variable no es necesaria

#Para stays_in_weekend_nights, stays_in_week_nights y adults
summary(df$stays_in_weekend_nights) 
summary(df$stays_in_week_nights) 
summary(df$adults) 
#consideramos normal que haya valores atipicos debido a la variedad de tipos de personas que suelen haber en las reservas
#por tanto, no aplicariamos ninguna tecnica para reemplazarlo

#Para children, babies, required_car_parking_spaces
table(df$children) 
table(df$babies) 
table(df$required_car_parking_spaces)
#Identificamos que para las variables children, babies,required_car_parking_spaces hay valores atipicos que modifican mucho sus estadisticas
#Es por ello, que recomendamos categorizar la variable en dos grupos:
#0-2
#3 a mas

#Para previous_cancellations, previous_bookings_not_canceled, booking_changes, total_of_special_requests consideramos logico que hayan outliers
summary(df$previous_cancellations) 
summary(df$previous_bookings_not_canceled) 
summary(df$booking_changes) 
summary(df$total_of_special_requests) 


#Para adr y days_in_waiting_list
summary(df$days_in_waiting_list) 
summary(df$adr)  
table(df$adr)
#consideramos que no es un logico estos valores atipico
#por tanto, recomendamos reemplazar los valores atipicos por el 3 cuartil



#Guardamos el nuevo dataset 
write.csv2(df, file = "C:/Users/marsi/OneDrive/Escritorio/CURSOS/CICLO 4/Fundamentos de Data Science/df_limpio.csv", row.names = FALSE)


#4.VISUALIZAR DATOS 


#ANALISIS DE PREGUNTAS GENERALES

#¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?
table(df$hotel)
ggplot(df, aes(hotel,fill=hotel)) + geom_bar()+ylab("Numero de reservas") +theme_get()
#Se observa que la gente prefiere City Hotel sobre Resort Hotel, practicamente por el doble



# ¿Está aumentando la demanda con el tiempo?
summary(df$arrival_date_year)
table(df$arrival_date_year, df$hotel)
ggplot(as.data.frame(table(Año = df$arrival_date_year, Hotel = df$hotel)), aes(x = Año, y = Freq, fill = Hotel)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Número de Reservas por Año y Tipo de Hotel",
       x = "Año de Llegada",
       y = "Número de Reservas",
       fill = "Tipo de Hotel") +
  theme_minimal()
#Se identifica que el año en que mas reservas hicieron fue el 2016
#Sin embargo, hay que tener en cuenta que el 2016 se tomo en cuenta los 12 meses del año
#Mientras que en 2017 solo 8 de estos, por lo cual la proporcion de clientes que asistieron en 2017 es similar a la de 2016

#Para ello, tambien se analizara los cliente que asistieron en distintos años por meses en la siguiente pregunta

#¿Cuándo se producen las temporadas de reservas: alta, media y baja?
#¿Cuándo es menor la demanda de reservas?
summary(df$arrival_date_month)
ggplot(df, aes(arrival_date_month,fill=arrival_date_month)) + geom_bar()+ylab("Numero de reservas") + labs(title = "Clientes en los distintos meses") + theme_get()

#Si li vemos para cada hotel
ggplot(df, aes(x = arrival_date_month, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Clientes en los distintos meses divididos por Hotel",
       x = "Mes de Llegada",
       y = "Número de Reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Nos damos cuenta que los meses de demanda coinciden, sin embargo, el Resort Hotel tiene el numero de clientes mas bajos
#Los meses con mas demandas son agosto y julio
#Temporada alta: mayo, julio, agosto
#Temporada media: marzo, abril, junio, setiembre, octubre
#Temporada baja: noviembre, diciembre, enero, febrero

#Ahora veremos la proporcion de meses en cada año 
ggplot(df, aes(x = arrival_date_month, fill = as.factor(arrival_date_year))) +
  geom_bar(position = "fill") +     #"fill"' para la proporción de cada año dentro de cada mes
  labs(title = "Distribución de Reservas por Mes Dividido por Año",
       x = "Mes de Llegada",
       y = "Proporción de Reservas",
       fill = "Año de Llegada") +
  ylab("Proporción de Reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#¿Cuántas reservas incluyen niños y/o bebes?
summary(df$children)
table(df$children, df$hotel)
sum(df$children)
#hay 12403 de 119390 reservar que incluyen niños
#Esto corresponde a un 10.38864%
(12403*100)/119390
#Distribucion de Niños por Tipo de Hotel
ggplot(data = df, aes(x = children, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Niños por Tipo de Hotel",
       x = "Número de Niños",
       y = "Cantidad de Reservas",
       fill = "Tipo de Hotel") +
  theme_minimal()
 

table(df$babies)
sum(df$babies)
(949*100)/119390
#Esto corresponde a un 0.7948739% de personas que reservan con bebes
#Lo cuales un valor muy bajo

#Distribucion de bebes por Tipo de Hotel
ggplot(data = df, aes(x = babies, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de bebes por Tipo de Hotel",
       x = "Número de Niños",
       y = "Cantidad de Reservas",
       fill = "Tipo de Hotel") +
  theme_minimal()


#¿Es importante contar con espacios de estacionamiento?
summary(df$required_car_parking_spaces)
sum(df$required_car_parking_spaces)
(7464*100)/119390
#Solo el 6.25178% requiere espacios de estacionamiento

#Ahora, especificando por hotel
table(df$required_car_parking_spaces, df$hotel)
ggplot(data = df, aes(x = required_car_parking_spaces, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Requerimiento de estacionamiento por Tipo de Hotel",
       x = "Número de Niños",
       y = "Cantidad de Reservas",
       fill = "Tipo de Hotel") +
  theme_minimal()
table(df$hotel)
#Porcentaje de Requerimiento de estacionamiento en City Hotel
(1926*100)/79330     #2.427833% requieren estacionamiento
#Porcentaje de Requerimiento de estacionamiento en Resort Hotel
(5490*100)/40060     #13.70444% requieren estacionamiento
#Por lo tanto, una gran diferencia que se encontro es que para 
#Resort Hotel los estacionamientos son importantes

#¿En qué meses del año se producen más cancelaciones de reservas?
table(df$is_canceled)
table(df$arrival_date_month)
table(df$is_canceled, df$arrival_date_month, df$hotel) 

tabla_cancelacionesxmes <- table(df$arrival_date_month, df$is_canceled)

df_cancelacionesxmes <- as.data.frame(tabla_cancelacionesxmes)  #convertir a dataframe
df_cancelacionesxmes

names(df_cancelacionesxmes) <- c("Mes", "Cancelaciones", "Reservas")  #renombrar el df

#Graficas de barras apíladas
ggplot(df_cancelacionesxmes, aes(x = Mes, y = Reservas, fill = Cancelaciones)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes", y = "Numero de reservaciones", fill = "Estado de cancelacion") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Número de Reservaciones canceladas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Hablando por cada tipo de hotel
table(df$is_canceled, df$arrival_date_month, df$hotel) 
#En ambos hoteles, se observa la mayor cantidad de cancelaciones en Agosto, en general de Abril a Agosto



#ANALISIS DE PREGUNTAS ESPECIFICAS PARA NUESTROS OBJETIVOS

#tipo de clientes
table(df$customer_type, df$hotel)
#En ambos hoteles reciben reservas individuales

#fidelizar clientes que suelen ir a hoteles
table(df$is_repeated_guest, df$hotel, df$customer_type)
#La mayoria de clientes que vuelve a reservar en ambos hoteles, son reservas "Transient"
table(df$is_repeated_guest, df$stays_in_week_nights)
#Ademas, suelen quedarse un promedio de 2-3 noches


#¿El tiempo en lista de espera influye en la cancelación de la reserva?
table(df$days_in_waiting_list, df$is_canceled)
#No influye mucho el tema de los tiempos


#Cancelaciones por hoteles
table( df$is_canceled,df$hotel)

cancel_data <- as.data.frame(table(df$is_canceled, df$hotel))
names(cancel_data) <- c("Canceled", "Hotel", "Count")
cancel_data$Canceled <- factor(cancel_data$Canceled, labels = c("No Cancelado", "Cancelado"))

ggplot(cancel_data, aes(x = Hotel, y = Count, fill = Canceled)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Cancelaciones por Tipo de Hotel",
       x = "Tipo de Hotel",
       y = "Número de Reservaciones",
       fill = "Estado de Reservación") +
  theme_minimal() 


(33102*100)/(46228+33102)  #41.72696 de City hotel son reservas canceladas

(11122*100)/(28938+11122)  #27.76335% de Resort hotel son reservas canceladas

#Se observa que en City Hotel hay mas cancelaciones de reservas
#Sin embargo, en ambos hoteles equivale a un gran porcentaje
#Por esto, se podría cobrar un costo por reservar en los hoteles, y si 
#hay cancelaciones, el dinero ya no es devuelto
#Esto para minimizar las perdidas causadas por las cancelaciones


#¿Qué tipo de cliente suele hacer más cancelaciones de reserva?
table( df$is_canceled,df$hotel, df$customer_type)
#los Transient-party suelen cancelar mas en ambos hoteles