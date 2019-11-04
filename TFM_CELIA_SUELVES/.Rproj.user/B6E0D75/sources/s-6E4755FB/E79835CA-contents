# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Title: Análisis exploratorio de depresión, ansiedad y estrés
# Universidad Internacional de La Rioja (UNIR)
# Autor: Celia Suelves Serrano
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# 1. PREPARACIÓN DE LOS DATOS
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # #
# Instalación de paquetes               # 
# # # # # # # # # # # # # # # # # # # # # 


if(! "readxl" %in% installed.packages()) install.packages("readxl", depend = TRUE)
if(! "kableExtra" %in% installed.packages()) install.packages("kableExtra", depend = TRUE)
if(! "highcharter" %in% installed.packages()) install.packages("highcharter", depend = TRUE)
if(! "plotly" %in% installed.packages()) install.packages("plotly", depend = TRUE)
if(! "dplyr" %in% installed.packages()) install.packages("dplyr", depend = TRUE)
if(! "rmarkdown" %in% installed.packages()) install.packages("rmarkdown", depend = TRUE)
if(! "knitr" %in% installed.packages()) install.packages("knitr", depend = TRUE)
if(! "summarytools" %in% installed.packages()) install.packages("summarytools", depend = TRUE)
if(! "tables" %in% installed.packages()) install.packages("tables", depend = TRUE)
if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)
if(! "RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer", depend = TRUE)
if(! "plyr" %in% installed.packages()) install.packages("plyr", depend = TRUE)
if(! "likert" %in% installed.packages()) install.packages("likert", depend = TRUE)
if(! "ca" %in% installed.packages()) install.packages("ca", depend = TRUE)
if(! "FactoMineR" %in% installed.packages()) install.packages("FactoMineR", depend = TRUE)
if(! "factoextra" %in% installed.packages()) install.packages("factoextra", depend = TRUE)
if(! "psych" %in% installed.packages()) install.packages("psych", depend = TRUE)
if(! "openxlsx" %in% installed.packages()) install.packages("openxlsx", depend = TRUE)
if(! "corrplot" %in% installed.packages()) install.packages("corrplot", depend = TRUE)
if(! "PerformanceAnalytics" %in% installed.packages()) install.packages("PerformanceAnalytics", depend = TRUE)
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
if(! "datasets" %in% installed.packages()) install.packages("datasets", depend = TRUE)
if(! "dummies" %in% installed.packages()) install.packages("dummies", depend = TRUE)
if(! "NbClust" %in% installed.packages()) install.packages("NbClust", depend = TRUE)
if(! "sqldf" %in% installed.packages()) install.packages("sqldf", depend = TRUE)


# # # # # # # # # # # # # # # # # # # # #
# Carga de librerías                    # 
# # # # # # # # # # # # # # # # # # # # # 

library(readxl)
library(kableExtra)
library(highcharter)
library(plotly)
library(dplyr) 
library(rmarkdown)
library(knitr)
library(summarytools)
library(tables)  
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(likert)
library(sjPlot)
library(ca)
library(FactoMineR)
library(factoextra)
library(psych)
library(openxlsx)
library(corrplot)
library(PerformanceAnalytics)
library(arules)
library(arulesViz)
library(datasets)
library(dummies)
library(Rcmdr)
library(NbClust)
library(sqldf)

Commander()

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Incremento del límite de impresión por pantalla   # 
# # # # # # # # # # # # # # # # # # # # # # # # # # #

options(max.print=999999999)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Creación del dataframe con los datos objeto de estudio
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

datos<-read_excel("../DATA/data.xlsx")
View(datos)
colnames(datos) 
str(datos)
warnings()



# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Backup de datos originales                        # 
# # # # # # # # # # # # # # # # # # # # # # # # # # #

backup_datos <- datos



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Se realiza una primera exploración muy básica de los datos # 
# # # # # # # # # # # # # # # # # # # # # # # # # # #  # # # #

boxplot(datos$age, 
        main = "Valores atípicos variable edad",
        boxwex = 0.5,col="gray70")

summary(datos)


str(datos)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Modificación de los nombres de las variables a mayúsculas 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

colnames(datos) <- toupper(colnames(datos))
colnames(datos) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Eliminación de las variables que no son objeto de estudio en este trabajo
# # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # #  # # # 

for(i in 1:42) {
  col_names_I <- (paste("Q",i,"I",sep=""))
  datos[,col_names_I] <- NULL
  col_names_I <- (paste("Q",i,"E",sep=""))
  datos[,col_names_I] <- NULL
}

for(i in 1:10) {
  col_names <- (paste("TIPI",i,sep=""))
  datos[,col_names] <- NULL
}

for(i in 1:16) {
  col_names <- (paste("VCL",i,sep=""))
  datos[,col_names] <- NULL
}

datos$SOURCE <- NULL
datos$INTROELAPSE <- NULL
datos$TESTELAPSE <- NULL
datos$SURVEYELAPSE <- NULL
datos$EDUCATION <- NULL
datos$URBAN <- NULL
datos$ENGNAT <- NULL
datos$SCREENSIZE <- NULL
datos$UNIQUENETWORKLOCATION <- NULL
datos$HAND <- NULL
datos$RELIGION <- NULL
datos$ORIENTATION <- NULL
datos$RACE<- NULL
datos$VOTED <- NULL
datos$MARRIED <- NULL
datos$FAMILYSIZE <- NULL
datos$MAJOR<- NULL

colnames(datos) 

str(datos)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Renombrado de columnas                                             # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

datos = rename(datos, c(COUNTRY="PAIS"))
datos = rename(datos, c(GENDER="SEXO"))
datos = rename(datos, c(AGE="EDAD"))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Corrección del error encontrado en los datos
# Se ha encontrado en el dataset de que cada item puntua una unidad más 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


for (i in 1:42){
  datos[,i][ datos[,i] == 1 ]    <- 0
  datos[,i][ datos[,i] == 2 ]    <- 1
  datos[,i][ datos[,i] == 3 ]    <- 2
  datos[,i][ datos[,i] == 4 ]    <- 3
}

str(datos)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creación de las variables DEPRESION, ANSIEDAD, ESTRES para almacenar
# la puntuacion total en cada escala.                                     
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

datos <- transform( datos,
                    DEPRESION  = (Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A +
                                     Q24A + Q26A + Q31A + Q34A + Q37A + Q38A + Q42A) ,
                    ANSIEDAD     = (Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A +
                                     Q23A + Q25A + Q28A + Q30A + Q36A + Q40A + Q41A) , 
                    ESTRES      = (Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A +
                                     Q22A + Q27A + Q29A + Q32A + Q33A + Q35A + Q39A))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Se crean las variables GRADO_DEPRESION, GRADO_ANSIEDAD, GRADO_ESTRES para almacenar el
# grado de severidad de cada escala en funcion de la puntuación obtenida en las variables 
# DEPRESION, ANSIEDAD y ESTRES 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


datos$GRADO_DEPRESION <- 0
datos$GRADO_ANSIEDAD  <- 0
datos$GRADO_ESTRES    <- 0

datos$GRADO_DEPRESION[ datos$DEPRESION >= 0  & datos$DEPRESION <= 9 ]    <- "1 Normal"
datos$GRADO_DEPRESION[ datos$DEPRESION >= 10 & datos$DEPRESION <= 13 ]   <- "2 Mild"
datos$GRADO_DEPRESION[ datos$DEPRESION >= 14 & datos$DEPRESION <= 20 ]   <- "3 Moderate"
datos$GRADO_DEPRESION[ datos$DEPRESION >= 21 & datos$DEPRESION <= 27 ]   <- "4 Severe"
datos$GRADO_DEPRESION[ datos$DEPRESION >= 28 ]                           <- "5 Extremely severe"


datos$GRADO_ANSIEDAD[ datos$ANSIEDAD >= 0  & datos$ANSIEDAD <= 7 ]       <- "1 Normal"
datos$GRADO_ANSIEDAD[ datos$ANSIEDAD >= 8  & datos$ANSIEDAD <= 9 ]       <- "2 Mild"
datos$GRADO_ANSIEDAD[ datos$ANSIEDAD >= 10 & datos$ANSIEDAD <= 14 ]      <- "3 Moderate"
datos$GRADO_ANSIEDAD[ datos$ANSIEDAD >= 15 & datos$ANSIEDAD <= 19 ]      <- "4 Severe"
datos$GRADO_ANSIEDAD[ datos$ANSIEDAD >= 20 ]                             <- "5 Extremely severe"


datos$GRADO_ESTRES[ datos$ESTRES >=  0 & datos$ESTRES <= 14 ]            <- "1 Normal"
datos$GRADO_ESTRES[ datos$ESTRES >= 15 & datos$ESTRES <= 18 ]            <- "2 Mild"
datos$GRADO_ESTRES[ datos$ESTRES >= 19 & datos$ESTRES <= 25 ]            <- "3 Moderate"
datos$GRADO_ESTRES[ datos$ESTRES >= 26 & datos$ESTRES <= 33 ]            <- "4 Severe"
datos$GRADO_ESTRES[ datos$ESTRES >= 34 ]                                 <- "5 Extremely severe"



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Selección del conjunto de datos que cumplen que:
# La edad de los sujetos es hasta 72 años incluido
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

print( sort(table(datos$EDAD),decreasing=T) )

datos <- datos[ datos$EDAD <= 72, ]



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Realización de una segunda selección del conjunto de datos en la que se seleccionan los registros que cumplen que:
# Paises con una frecuencia mayor de 30 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

print( sort(table(datos$PAIS),decreasing=T) )

datos <- datos[     datos$PAIS =='MY' | datos$PAIS =='US' | datos$PAIS =='GB'| datos$PAIS =='CA'| datos$PAIS =='ID'| datos$PAIS =='PH'|
                    datos$PAIS =='AU' | datos$PAIS =='IN' | datos$PAIS =='DE'| datos$PAIS =='SG'| datos$PAIS =='NZ'| datos$PAIS =='FR'|
                    datos$PAIS =='BR' | datos$PAIS =='PL' | datos$PAIS =='BN'| datos$PAIS =='MX'| datos$PAIS =='RO'| datos$PAIS =='IT'|
                    datos$PAIS =='NL' | datos$PAIS =='ES' | datos$PAIS =='JP'| datos$PAIS =='FI'| datos$PAIS =='TR'| datos$PAIS =='PK'|
                    datos$PAIS =='ZA' | datos$PAIS =='SE' | datos$PAIS =='EG'| datos$PAIS =='GR'| datos$PAIS =='IE'| datos$PAIS =='RS'|
                    datos$PAIS =='PT' | datos$PAIS =='RU' | datos$PAIS =='CZ'| datos$PAIS =='HK'| datos$PAIS =='SA'| datos$PAIS =='AR'|
                    datos$PAIS =='HR' | datos$PAIS =='AE' | datos$PAIS =='DK'| datos$PAIS =='JM'| datos$PAIS =='NO'| datos$PAIS =='HU'|
                    datos$PAIS =='CH' | datos$PAIS =='AT' | datos$PAIS =='VN'| datos$PAIS =='BG'| datos$PAIS =='BE'| datos$PAIS =='KR', ]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Conversion a tipo númerico las variables EDAD, DEPRESION, ANSIEDAD, ESTRES
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

str(datos)

datos$DEPRESION <- as.numeric(datos$DEPRESION)
datos$ANSIEDAD <- as.numeric(datos$ANSIEDAD)
datos$ESTRES<- as.numeric(datos$ESTRES)
datos$EDAD <- as.numeric(datos$EDAD)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Conversión a tipo factor y recodificación de las variables cualitativas
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


for (i in 1:42){
  datos[,i]<- factor( datos[,i],levels = c(0,1,2,3),labels = c(
    "No me aplicó", 
    "Me aplicó un poco",
    "Me aplicó bastante",
    "Me aplicó mucho"))
}

datos$SEXO <- factor( datos$SEXO, levels = c(1,2), labels = c(
    "Hombre", 
    "Mujer"))

datos$PAIS <- as.factor(datos$PAIS)
datos$GRADO_DEPRESION <- as.factor(datos$GRADO_DEPRESION)
datos$GRADO_ANSIEDAD <- as.factor(datos$GRADO_ANSIEDAD)
datos$GRADO_ESTRES <- as.factor(datos$GRADO_ESTRES)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Se crea la variable RANGO_EDAD para almacenar los distintos rangos de edad 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

datos$RANGO_EDAD[ datos$EDAD >= 13 & datos$EDAD <= 18 ]    <- "13-18"
datos$RANGO_EDAD[ datos$EDAD >= 19 & datos$EDAD <= 24 ]    <- "19-24"
datos$RANGO_EDAD[ datos$EDAD >= 25 & datos$EDAD <= 30 ]    <- "25-30"
datos$RANGO_EDAD[ datos$EDAD >= 31 & datos$EDAD <= 36 ]    <- "31-36"
datos$RANGO_EDAD[ datos$EDAD >= 37 & datos$EDAD <= 42 ]    <- "37-42"
datos$RANGO_EDAD[ datos$EDAD >= 43 & datos$EDAD <= 48 ]    <- "43-48"
datos$RANGO_EDAD[ datos$EDAD >= 49 & datos$EDAD <= 54 ]    <- "49-54"
datos$RANGO_EDAD[ datos$EDAD >= 55 & datos$EDAD <= 60 ]    <- "55-60"
datos$RANGO_EDAD[ datos$EDAD >= 61 & datos$EDAD <= 66 ]    <- "61-66"
datos$RANGO_EDAD[ datos$EDAD >= 67 & datos$EDAD <= 72 ]    <- "67-72"

datos$RANGO_EDAD <- as.factor(datos$RANGO_EDAD)

table(datos$RANGO_EDAD)



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# Construcción dataframe metadatos para aprovisionar:
#     Nombre de la variable,
#     Descripción,
#     Tipo variable, 
#     Número nulos 
#     Número de NA
# # # # # # # # # # # # # # # # # # # # # # # # # #


variables <- data.frame(names(datos), "", sapply(datos, class), 0, 0)
names(variables)<-c("VARIABLE", "DESCRIPTION", "TIPO")
names(variables)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Aprovisionamiento del campo descripción del dataset de metadatos
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

variables$DESCRIPTION<-as.character(variables$DESCRIPTION)

variables$DESCRIPTION[1]=  "Me encontré molesto por cosas bastante triviales"
variables$DESCRIPTION[2]=  "Fuí consciente de sequedad en mi boca"
variables$DESCRIPTION[3]=  "No podía sentir ningún sentimiento positivo"
variables$DESCRIPTION[4]=  "Experimenté dificultad para respirar"
variables$DESCRIPTION[5]=  "Se me hizo difícil tomar la iniciativa para hacer cosas"
variables$DESCRIPTION[6]=  "Reaccioné exEDADradamente en ciertas situaciones"
variables$DESCRIPTION[7]=  "Sentí temblores"
variables$DESCRIPTION[8]=  "Encontré difíciltad para relajarme"
variables$DESCRIPTION[9]=  "Me encontré en situaciones que me pusieron tan ansioso que me sentí más aliviado cuando terminaron"
variables$DESCRIPTION[10]= "Sentí que no tenia nada por lo que vivir"
variables$DESCRIPTION[11]= "Me encontré agitado con bastante facilidad"
variables$DESCRIPTION[12]= "Sentí que estaba muy nervioso"
variables$DESCRIPTION[13]= "Me sentí triste y deprimido"
variables$DESCRIPTION[14]= "Me encontré impaciente cuando me retrasé por algún motivo"
variables$DESCRIPTION[15]= "Tuve sentimientos de desmayo"
variables$DESCRIPTION[16]= "Sentí que había perdido interés en casi todo"
variables$DESCRIPTION[17]= "Sentí que no valía mucho como persona"
variables$DESCRIPTION[18]= "Sentí que estaba muy irritable"
variables$DESCRIPTION[19]= "Sentí latidos en mi corazón a pesar de no haber hecho ningún esfuerzo físico"
variables$DESCRIPTION[20]= "Tuve miedo sin razón"
variables$DESCRIPTION[21]= "Sentí que la vida no tenia ningún sentido"
variables$DESCRIPTION[22]= "Me resultó difícil relajarme" 
variables$DESCRIPTION[23]= "Tuve dificultad para tragar" 
variables$DESCRIPTION[24]= "No pude disfrutar de nada de lo que hice" 
variables$DESCRIPTION[25]= "Fuí consciente de la acción de mi corazón en ausencia de esfuerzo físico" #todo
variables$DESCRIPTION[26]= "Me sentí triste"  
variables$DESCRIPTION[27]= "Estuve muy irritable" 
variables$DESCRIPTION[28]= "Sentí que estaba cerca del pánico" 
variables$DESCRIPTION[29]= "Me resultó difícil calmarme después de que algo me molestara" 
variables$DESCRIPTION[30]= "Temía que me arrojaran cosas triviales pero desconocidas" 
variables$DESCRIPTION[31]= "No pude entusiasmarme con nada" 
variables$DESCRIPTION[32]= "Me resultó difícil tolerar interrupciones en lo que estaba haciendo" 
variables$DESCRIPTION[33]= "Estuve en estado de tensión nerviosa" 
variables$DESCRIPTION[34]= "Sentí que no valía nada" 
variables$DESCRIPTION[35]= "Fuí intolerante con todo lo que me impedía seguir adelante con lo que estaba haciendo" 
variables$DESCRIPTION[36]= "Me senti aterrado" 
variables$DESCRIPTION[37]= "No podía ver nada en un futuro sobre lo que tener esperanzas" 
variables$DESCRIPTION[38]= "Sentí que la vida no tenía sentido" 
variables$DESCRIPTION[39]= "Me encontré agitado" 
variables$DESCRIPTION[40]= "Estuve preocupado por situaciones en las que podría hacer el ridículo y entrar en pánico" 
variables$DESCRIPTION[41]= "Experimenté temblores" 
variables$DESCRIPTION[42]= "Me resultó difícil tomar la iniciativa para hacer cosas" 
variables$DESCRIPTION[43]= "ISO País"
variables$DESCRIPTION[44]= "SEXO"
variables$DESCRIPTION[45]= "Edad"
variables$DESCRIPTION[46]= "Puntuación depresión"
variables$DESCRIPTION[47]= "Puntuación ansiedad"
variables$DESCRIPTION[48]= "Puntuación estrés"
variables$DESCRIPTION[49]= "Grado depresión"
variables$DESCRIPTION[50]= "Grado ansiedad"
variables$DESCRIPTION[51]= "Grado estrés"
variables$DESCRIPTION[52]= "Rango Edad"

str(datos)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creación variable ’variables_numericas’ para almacenar los nombres de los campos 
# con valores numéricos
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

variables_numericas <-  as.character(variables[variables$TIPO == c("numeric"),1])
variables_numericas



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Creación variable ’variables_discretas’ 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

variables_discretas <- as.character(variables[variables$TIPO != c("character"),1])
variables_discretas



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Creación variable ’variables_discretas_names’ para almacenar los nombres de los campos 
# para almacenar las descripciones de los campos con valores finitos (todos menos los campos de tipo character), 
# servirá como lista para los tíıtulos de los gráaficos y los nombres las figuras.
# # # # # # # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

variables_discretas_names<-as.character(variables[variables$TIPO!=c("character"),2])
variables_discretas_color<-as.character(variables[variables$TIPO!=c("character"),6])

variables_discretas
variables_discretas_names
variables_discretas_color



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.- ANÁLISIS DE LA CALIDAD DE LOS DATOS
# # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1 Identificación de valores NULLS y NA
# # # # # # # # # # # # # # # # # # # # # # # # # #


for(i in 1:length(variables$VARIABLE)){
  
  var2<-paste(variables$VARIABLE[i])  
  
  variables$NUMERO_NULLS[i]<-sum(sapply(datos[var2], function(x) sum(is.null(x))))
  variables$NUMERO_NULLS[i]
  
  variables$NUMERO_NA[i]<-sum(sapply(datos[var2], function(x) sum(is.na(x))))
  variables$NUMERO_NA[i]
 
}

nrow(variables[variables$NUMERO_NULLS>0,])
nrow(variables[variables$NUMERO_NA>0,])

sapply (datos, function(x) (sum(is.na(x))))
sapply (datos, function(x) (sum(is.null(x))))



# # # # # # # # # # # # # # # # # # # # # # # #
# Eliminación valores perdidos NULLS y NA      
# Se eliminan los registros que contienen 
# la variable SEXO = "Other"(589 registros).
# # # # # # # # # # # # # # # # # # # # # # # #

datos <- na.omit(datos)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Identificación de valores atípicos o outliers 
# Se comprueba si en las variables numéricas hay valores atípicos o outliers.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

boxplot(datos[variables_numericas], 
        main = "Valores Atípicos variables numéricas",
        boxwex = 0.5,col="gray70")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Gráfico boxplot valores atípicos o outliers de la variable edad
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

boxplot(datos[variables_numericas[1]], 
        main = "Valores atípicos variable edad",
        boxwex = 0.5,col="gray70")


write.xlsx(datos, '../DATOS_EXPORTADOS/exportado.xlsx')



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 3.- Visualización e Interpretación de los datos                     # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 3.1 Función para dibujar los gráficos                                # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

graficos_plot <- function(var2, var_name, color){
  
  df_plot<-as.data.frame(table(datos[,var2]))
  df_plot
  names(df_plot)<-c(var2, "count")
  
  if( nrow(df_plot)>6 ){ lab<-paste0(df_plot$count)  }
  else{ lab<-paste0(df_plot$count) }
  switch(color,
         doscolores={ cols=c("gray8","gray65") 
         },
         trescolores={ cols=c("gray83","gray48", "gray68") 
         },
         cuatrocolores={ cols=c("gray86","gray48", "gray68", "gray88" ) 
         },
         cincocolores={ cols=c("gray86","gray48", "gray68", "gray88", "gray93" ) 
         },
         { cols=palette() }
  )
  plot<- ggplot(data=df_plot, aes(x=reorder(df_plot[,1], -df_plot[,2]) , y=df_plot[,2], fill=df_plot[,1]))  +
  
  #else { plot<-ggplot(data=df_plot, aes(x=df_plot[,1] , y=df_plot[,2], fill=df_plot[,1])) }   +
     geom_bar(stat="identity")+  
    theme_bw() + 
    theme(axis.line = element_line(colour = "white"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank(),
          axis.ticks = element_line(colour = "white")
    ) +
    scale_fill_manual(values=cols) +
    geom_text(aes(label=lab), 
              vjust=-0.3, 
              size=4.5) + 
    ggtitle(paste("\n", "\n", var_name,"\n") )  +
    xlab(var2) + ylab("number of people") + labs(fill=var2)+  
    labs(x = NULL, y = NULL, caption = NULL, #caption="\n\nSource: Elaboración propia a partir de los datos del cuestionario DASS-42 de Open-Source Psychometrics", 
         subtitle = "")+
    theme(
      legend.position = "none",
      axis.text.x=element_text(vjust=-0.5,  size=12, face = "bold"), #size=rel(1)
      axis.text.y = element_text(colour = "white"),
      plot.title = element_text( size = 13, face = "bold", hjust = 0.5),
      # texto pie de grafico
      plot.caption = element_text( size = 11, hjust = 0.1)
    )
  show(plot)
}

graficos_plot_sin_ordenar <- function(var2, var_name, color){
  df_plot<-as.data.frame(table(datos[,var2]))
  df_plot
  names(df_plot)<-c(var2, "count")
  if( nrow(df_plot)>6 ){ lab<-paste0(df_plot$count)  }
  else{ lab<-paste0(df_plot$count
  )   }
  switch(color,
         doscolores={ cols=c("gray8","gray65") 
         },
         trescolores={ cols=c("gray68","gray48", "gray86") #48, oscuro, 68 es intermedio, 86 el mas claro
         },
         cuatrocolores={ cols=c("gray86","gray48", "gray68", "gray88" ) 
         },
         cincocolores={ cols=c("gray48", "gray68", "gray86", "gray93", "gray88", "gray95" ) 
         },
         { cols={ cols=palette() } }
  )
  plot<-ggplot(data=df_plot, aes(x=df_plot[,1] , y=df_plot[,2], fill=df_plot[,1])) +
    geom_bar(stat="identity")+  
    theme_bw() + 
    theme(axis.line = element_line(colour = "white"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank(),
          axis.ticks = element_line(colour = "white")
    ) +
    scale_fill_manual(values=cols) +
    geom_text(aes(label=lab), 
              vjust=-0.3, 
              size=4.5) + 
    ggtitle(paste("\n", "\n", var_name,"\n") )  +
    xlab(var2) + ylab("number of people") + labs(fill=var2)+  
    labs(x = NULL, y = NULL, caption = NULL, #caption="\n\nSource: Elaboración propia a partir de los datos del cuestionario DASS-42 de Open-Source Psychometrics", 
         subtitle = "")+
    theme(
      legend.position = "none",
      axis.text.x=element_text(vjust=-0.5,  size=12, face = "bold"), #size=rel(1)
      axis.text.y = element_text(colour = "white"),
      plot.title = element_text( size = 13, face = "bold", hjust = 0.5),
      # texto pie de grafico
      plot.caption = element_text( size = 8, hjust = 0.1)
      
    )
  show(plot)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.-   ANÁLISIS DE LA CALIDAD DE LOS DATOS: 
# 2.1 - ANÁLISIS ESTADÍSTICO UNIDIMENSIONAL
# # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.1 Análisis estadístico unidimensional de las variables cuantitativas 
# # # # # # # # # # # # # # # # # # # # # # # # # #


summary(datos$DEPRESION)

summary(datos$ANSIEDAD)

summary(datos$ESTRES)

summary(datos$EDAD)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.2 Test de normalidad para las variables cuantitativas
#
# Pruebas de Normalidad:
# - H0: La muestra proviene de una distribución normal.
# - H1: La muestra no proviene de una distribución normal.
#   El nivel de significancia con el que se trabajará es de 0.05(Alfa=0.05).
#
# Criterio de Decisión:
# - Si P < Alfa: Se rechaza Ho
# - Si p >= Alfa: No se rechaza Ho
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


#Test de normalidad ANDERSON-Darling para la variable DEPRESION
normalityTest(~DEPRESION, test = "ad.test", data = datos)

#Test de normalidad Cramer- Von Mises para la variable DEPRESION
normalityTest(~DEPRESION, test = "cvm.test", data = datos)

#Test de normalidad Lilliefors para la variable DEPRESION
normalityTest(~DEPRESION, test = "lillie.test", data = datos)

#Test de normalidad Lilliefors para la variable DEPRESION
normalityTest(~DEPRESION, test = "pearson.test", data = datos)

plot(density(datos$DEPRESION))
qqnorm(datos$DEPRESION)
qqline(datos$DEPRESION)


#Test de normalidad ANDERSON-Darling para la variable ANSIEDAD
normalityTest(~ANSIEDAD, test = "ad.test", data = datos)

#Test de normalidad Cramer- Von Mises para la variable ANSIEDAD
normalityTest(~ANSIEDAD, test = "cvm.test", data = datos)

#Test de normalidad Lilliefors para la variable ANSIEDAD
normalityTest(~ANSIEDAD, test = "lillie.test", data = datos)

#Test de normalidad Lilliefors para la variable ANSIEDAD
normalityTest(~ANSIEDAD, test = "pearson.test", data = datos)

plot(density(datos$ANSIEDAD))
qqnorm(datos$ANSIEDAD)
qqline(datos$ANSIEDAD)


#Test de normalidad ANDERSON-Darling para la variable ESTRES
normalityTest(~ESTRES, test = "ad.test", data = datos)

#Test de normalidad Cramer- Von Mises para la variable ESTRES
normalityTest(~ESTRES, test = "cvm.test", data = datos)

#Test de normalidad Lilliefors para la variable ESTRES
normalityTest(~ESTRES, test = "lillie.test", data = datos)

#Test de normalidad Lilliefors para la variable ESTRES
normalityTest(~ESTRES, test = "pearson.test", data = datos)

plot(density(datos$ESTRES))
qqnorm(datos$ESTRES)
qqline(datos$ESTRES)


#Test de normalidad ANDERSON-Darling para la variable EDAD
normalityTest(~EDAD, test = "ad.test", data = datos)

#Test de normalidad Cramer- Von Mises para la variable EDAD
normalityTest(~EDAD, test = "cvm.test", data = datos)

#Test de normalidad Lilliefors para la variable EDAD
normalityTest(~EDAD, test = "lillie.test", data = datos)

#Test de normalidad Lilliefors para la variable EDAD
normalityTest(~EDAD, test = "pearson.test", data = datos)


plot(density(datos$EDAD))
qqnorm(datos$EDAD)
qqline(datos$EDAD)



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.3 Distribución de frecuencias según país
# # # # # # # # # # # # # # # # # # # # # # # # # #


frecuencia_PAIS <- freq(datos$PAIS, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")

kable(frecuencia_PAIS, format = "html", round(digits = 2))

frecuencia_PAIS



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.4 Distribución de frecuencias según SEXO
# # # # # # # # # # # # # # # # # # # # # # # # # #

frecuencia_SEXO <- freq(datos$SEXO, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")

kable(frecuencia_SEXO, format = "html", round(digits = 2))

frecuencia_SEXO

#Distribución de frecuencia absoluta según el SEXO. Elaboración propia.
graficos_plot(var2="SEXO",var_name="",color="trescolores")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# 2.1.5 Distribución de frecuencias según rango edad
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

frecuencia_rango_EDAD <- freq(datos$EDAD, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")
table(datos$RANGO_EDAD)
kable(frecuencia_rango_EDAD, format = "html", round(digits = 2))

frecuencia_rango_EDAD



# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.6 Distribución de frecuencias según grado depresión
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

frecuencia_grado_depresion <- freq(datos$GRADO_DEPRESION, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")

kable(frecuencia_grado_depresion, format = "html", round(digits = 2))

frecuencia_grado_depresion

#Distribución de frecuencia absoluta de la escala depresión. Elaboración propia.
graficos_plot_sin_ordenar(var2="GRADO_DEPRESION",var_name="",color="cols")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.7 Distribución de frecuencias según grado ansiedad
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

frecuencia_grado_ANSIEDAD <- freq(datos$GRADO_ANSIEDAD, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")

kable(frecuencia_grado_ANSIEDAD, format = "html", round(digits = 2))

frecuencia_grado_ANSIEDAD

#Distribución de frecuencia absoluta de la escala ANSIEDAD. Elaboración propia.
graficos_plot_sin_ordenar(var2="GRADO_ANSIEDAD",var_name="",color="cols")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.1.8 Distribución de frecuencias según grado estrés
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

frecuencia_grado_estres <- freq(datos$GRADO_ESTRES, justify = "center", caption = "My table", headings = FALSE, report.nas = FALSE, ord="Freq")

kable(frecuencia_grado_estres, format = "html", round(digits = 2))

frecuencia_grado_estres

#Distribución de frecuencia absoluta según el grado de estrés. Elaboración propia.
graficos_plot_sin_ordenar(var2="GRADO_ESTRES",var_name="",color="cols")



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.-   ANÁLISIS DE LA CALIDAD DE LOS DATOS: 
# 2.1 - ANÁLISIS ESTADÍSTICO BIDIMENSIONAL
# # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.1 Tabla de medidas descriptivas de depresión, ansiedad y estrés por país                                    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


tabla_des_PAIS <- tabular ( PAIS ~ # para cada uno de los niveles de este factor
                             ( DEPRESION + ANSIEDAD + ESTRES ) *  # de estas variables 
                             ( mean + median + sd ) +  # mostrar sus descriptivos
                              #( mean ) +  # mostrar sus descriptivos
                             ( n = 1 ), # el núm. total de observaciones
                               data = datos ) # base de datos que usamos
tabla_des_PAIS



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.2 Tabla estadística de medidas descriptivas de depresión, ansiedad y estrés por SEXO                                   
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


tabla_des_SEXO <- tabular ( SEXO ~ # para cada uno de los niveles de este factor
                            ( DEPRESION + ANSIEDAD + ESTRES ) *  # de estas variables 
                            ( mean ) +  # mostrar sus descriptivos
                            ( n = 1 ), # el núm. total de observaciones
                              data = datos ) # base de datos que usamos
tabla_des_SEXO


html( tabla_des_SEXO,
      options = htmloptions( HTMLcaption = "Descriptivos depresión, ansiedad y estrés por SEXO",
                             justification = "c",
                             pad = TRUE ) )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.3.3 Tabla estadística de medidas descriptivas de depresión, ansiedad y estrés por rango edad                                    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


tabla_de_an_es_rango_EDAD <- tabular ( RANGO_EDAD  ~ # para cada uno de los niveles de este factor
                                    (  DEPRESION + ANSIEDAD + ESTRES ) *  # de estas variables 
                                    ( mean ) +  # mostrar sus descriptivos
                                    ( n = 1 ), # el núm. total de observaciones
                                      data = datos ) # base de datos que usamos
tabla_de_an_es_rango_EDAD

html( tabla_de_an_es_rango_EDAD,
      options = htmloptions( HTMLcaption = "Descriptivos depresión, ansiedad y estrés por rango edad",
                             justification = "c",
                             pad = TRUE ) )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2.3.3 Tabla estadística de medidas descriptivas de depresión, ansiedad y estrés por edad y sexo                                   
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

tabla_de_an_es_rango_EDAD_SEXO <- tabular ( RANGO_EDAD  ~ # para cada uno de los niveles de este factor
                                         ( SEXO) *  # de estas variables
                                         (  DEPRESION + ANSIEDAD + ESTRES ) *  # de estas variables 
                                         ( mean + median + sd ) +  # mostrar sus descriptivos
                                         ( n = 1 ), # el núm. total de observaciones
                                       data = datos ) # base de datos que usamos

tabla_de_an_es_rango_EDAD_SEXO

html( tabla_de_an_es_rango_EDAD_SEXO,
      options = htmloptions( HTMLcaption = "Descriptivos depresión, ansiedad y estrés por rango edad",
                             justification = "c",
                             pad = TRUE ) )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.4 Gráfico items Depresión:                                      
# Porcentaje de respuesta a cada item en la escala de depresión
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de depresión"

items_DEPRESION <- select(datos, Q3A, Q5A, Q10A, Q13A, Q16A, Q17A, Q21A, 
                          Q24A, Q26A, Q31A, Q34A, Q37A, Q38A, Q42A) 

likert_DEPRESION <- likert(items_DEPRESION)

plot(likert_DEPRESION, centered = TRUE, plot.percents = T) + ggtitle(title) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.5 Gráfico items ansiedad:      
# Porcentaje de respuesta a cada item en la escala de ansiedad
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de ansiedad"

items_ANSIEDAD <- select(datos, Q2A, Q4A, Q7A, Q9A, Q15A, Q19A, Q20A, 
                         Q23A, Q25A, Q28A, Q30A, Q36A, Q40A, Q41A) 

likert_ANSIEDAD <- likert(items_ANSIEDAD)

plot(likert_ANSIEDAD, centered = TRUE, plot.percents = T) + ggtitle(title)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.6 Gráfico items estrés:                                       
# Porcentaje de respuesta a cada item en la escala de estrés 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de estrés"

items_ESTRES <- select(datos, Q1A, Q6A, Q8A, Q11A, Q12A, Q14A, Q18A, 
                       Q22A, Q27A, Q29A, Q32A, Q33A, Q35A, Q39A) 

likert_ESTRES <- likert(items_ESTRES)
plot(likert_ESTRES, centered = TRUE, plot.percents = T) + ggtitle(title)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.7 Gráfico items Depresión por SEXO                              # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de depresión por sexo"

items_DEPRESION <- select(datos, Q3A, Q5A, Q10A, Q13A, Q16A, Q17A, Q21A, 
                           Q24A, Q26A, Q31A, Q34A, Q37A, Q38A, Q42A ) 

likert_DEPRESION <- likert(items_DEPRESION)

plot(likert_DEPRESION, centered = TRUE, plot.percents = T) + ggtitle(title)

likert29g_DEPRESION <- likert(items_DEPRESION, grouping = datos$SEXO)
a = summary(likert29g_DEPRESION,ordered = TRUE)

plot(likert29g_DEPRESION ) + ggtitle(title)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.8 Gráfico items ansiedad por SEXO                                    # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de ansiedad por SEXO"

items_ANSIEDAD <- select(datos, Q2A, Q4A, Q7A, Q9A, Q15A, Q19A, Q20A, 
                           Q23A, Q25A, Q28A, Q30A, Q36A, Q40A, Q41A) 

likert_ANSIEDAD <- likert(items_ANSIEDAD)
plot(likert_ANSIEDAD, centered = TRUE, plot.percents = T) + ggtitle(title)

likert29g_ANSIEDAD <- likert(items_ANSIEDAD, grouping = datos$SEXO)
a = summary(likert29g_ANSIEDAD,ordered = TRUE)

plot(likert29g_ANSIEDAD, col= c("gray50", "gray57", "gray76", "gray81")) + ggtitle(title)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2.3.9 Gráfico items Estrés por SEXO                                    # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

title <- "Porcentaje de respuesta a cada item en la escala de estrés por SEXO"

items_ESTRES <- select(datos, Q1A, Q6A, Q8A, Q11A, Q12A, Q14A, Q18A, 
                           Q22A, Q27A, Q29A, Q32A, Q33A, Q35A, Q39A) 

likert_ESTRES <- likert(items_ESTRES)
plot(likert_ESTRES, centered = TRUE, plot.percents = T) + ggtitle(title)

likert29g_ESTRES <- likert(items_ESTRES, grouping = datos$SEXO)
a = summary(likert29g_ESTRES,ordered = TRUE)

plot(likert29g_ESTRES) + ggtitle(title)



# # # # # # # # # # # # # # # # 
# 2.3 Relación entre variables  
# # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MEDIDAS DE ASOCIACIÓN PARA VARIABLES CUANTITATIVAS  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#Pruebas paramétricas: variables cuantitativas, distribución normal, muestras grandes

# Podemos interpretar que:
# - La correlación entre la depresion y ansiedad es de 00.69. Es decir, 
# es una relación positiva que indica que a mayor puntuación en depresión mayor es la puntuación en ansiedad.
# - La correlación entre la depresion y estrés es de 0.76. Es decir, 
# es una relación positiva que indica que a mayor puntuación en depresión mayor es la puntuación en estrés.
# - La correlación entre el estrés y ansiedad es de 0.81. Es decir, 
# es una relación positiva que indica que a mayor puntuación en estrés mayor es la puntuación en ansiedad.



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1.- RELACION DE VARIABLES CUANTITATIVAS #
# Los resultados de los test de normalidad han indicado que la muestra no proviene de una distribución normal. 
# El teorema central del límite (TCL) establece que, dada una muestra suficientemente grande de la población(de tamaño mayor que 30), 
# la distribución de las medias muestrales seguirá una distribución normal, es por ello que se van a utilizar en este
# trabajo los estadisticos parámetricos.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1.1 Coeficiente de correlacion de pearson 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

correlacion <- round(cor(datos[, c("ANSIEDAD", "DEPRESION", "ESTRES", "EDAD")], method = "pearson", use = "complete"), 2)
correlacion



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1.2 Gráfico de correlaciones de las variables  númericas
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

corrplot(correlacion, method = "number", tl.col = "black"
         , title = "Correlación variables depresión, ansiedad, estrés y edad")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Gráficos de frecuencia de los items de depresión, ansiedad y estrés(42 en total)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

for(i in 1:42){
  var2<-paste(variables_discretas[i])
  var_name<-paste(variables_discretas_names[i])
  color<-variables_discretas_color[i]
  print(var2)
  print(var_name)
  print(color)
  graficos_plot_sin_ordenar(var2,var_name,"rainbow")
  ggsave(paste(variables_discretas[i], ".jpg"))
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Consultas: Se obtiene la media de depresion, ansiedad y estrés agrupado por país
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

sql_media_de_an_es_pais <- sqldf('
          SELECT 
                PAIS
              , ROUND(avg(DEPRESION),2) as DEPRESION_MEDIA
              , ROUND(avg(ANSIEDAD),2) as ANSIEDAD_MEDIA
              , ROUND(avg(ESTRES),2) as ESTRES_MEDIA
          FROM datos
          GROUP BY PAIS')

as.data.frame.matrix(sql_media_de_an_es_pais)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Consultas: Se obtiene la media de depresion, ansiedad y estrés agrupado por edad y sexo
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

sql_media_de_an_es_edad_sexo <- sqldf('
          SELECT 
                EDAD, SEXO
              , ROUND(avg(DEPRESION),2) as DEPRESION_MEDIA
              , ROUND(avg(ANSIEDAD),2) as ANSIEDAD_MEDIA
              , ROUND(avg(ESTRES),2) as ESTRES_MEDIA
          FROM datos
          GROUP BY EDAD, SEXO;')

as.data.frame.matrix(sql_media_de_an_es_edad_sexo)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Consultas: Se obtiene la media de depresion, ansiedad y estrés agrupado por sexo
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

sql_media_de_an_es_edad_sexo <- sqldf('
          SELECT 
                SEXO
              , ROUND(avg(DEPRESION),2) as DEPRESION_MEDIA
              , ROUND(avg(ANSIEDAD),2) as ANSIEDAD_MEDIA
              , ROUND(avg(ESTRES),2) as ESTRES_MEDIA
          FROM datos
          GROUP BY SEXO;')


as.data.frame.matrix(sql_media_de_an_es_edad_sexo)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Exporto por separado los datos de dataframe de sexo femenino y los del masculino.
# Estos excel se necesitan para alimentar los graficos que se van a crear desde plotly RStudio
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

print( sort(table(datos$SEXO),decreasing=T) )

datos_sexo_mujer <- datos[ datos$SEXO == 'Mujer', ]
print( sort(table(datos_sexo_mujer$SEXO),decreasing=T) )

datos_sexo_hombre <- datos[ datos$SEXO =='Hombre', ]
print( sort(table(datos_sexo_hombre$SEXO),decreasing=T) )

write.xlsx(datos_sexo_mujer, '../DATOS_EXPORTADOS/datos_sexo_mujer.xlsx')
write.xlsx(datos_sexo_hombre, '../DATOS_EXPORTADOS/datos_sexo_hombre.xlsx')



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ALGORITMO APRIORY
# El aprendizaje de reglas de asociación es una técnica para descubrir la relación entre varios elementos, 
# o diversas variables en una base de datos muy grande.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

datos_apriory <- datos

str(datos_apriory)

datos_apriory <- select(datos, -EDAD, -DEPRESION, -ANSIEDAD, -ESTRES)

names(datos_apriory)

str(datos_apriory)

# Convertir los datos del dataframe a formato de transacción
transacciones <- as(datos_apriory, "transactions")

itemFrequencyPlot(transacciones, topN=20)

# Soporte de los items que aparecen en las transacciones
itemFrequencyPlot(transacciones, support=0.3, cex.names=0.9)

#Reglas apriory con supp = 0.25, conf = 0.8
reglas_apriory = apriori(transacciones, parameter = list( supp = 0.1, conf = 0.80)) 
reglas_apriory_ordenadas <- sort(reglas_apriory, by = "confidence")
reglas_apriory_ordenadas <- sort(reglas_apriory, by = "support")
inspect(reglas_apriory_ordenadas)
inspect(reglas_apriory_ordenadas[1:300])


#Reglas apriory con consecuente depresion y con supp = 0.25, conf = 0.8
reglas_apriory_depresion <-apriori(transacciones, parameter = list ( supp = 0.1, conf = 0.80, target = "rules"), #supp = 0.2, conf= 0.7
                           appearance = list(rhs=c("GRADO_DEPRESION=1 Normal",
                                                   "GRADO_DEPRESION=2 Mild",
                                                   "GRADO_DEPRESION=3 Moderate",
                                                   "GRADO_DEPRESION=4 Severe",
                                                   "GRADO_DEPRESION=5 Extremely severe"), default="lhs"))


reglas_apriory_depresion_ordenadas <- sort(reglas_apriory_depresion, by = "confidence")
reglas_apriory_depresion_ordenadas <- sort(reglas_apriory_depresion, by = "support")
inspect(reglas_apriory_depresion_ordenadas)
inspect(reglas_apriory_depresion_ordenadas[1:300])


#Reglas apriory con consecuente ansiedad y con supp = 0.25, conf = 0.8
reglas_apriory_ansiedad <-apriori(transacciones, parameter = list ( supp = 0.2, conf= 0.7  , target = "rules"),
                                   appearance = list(rhs=c("GRADO_ANSIEDAD=1 Normal",
                                                           "GRADO_ANSIEDAD=2 Mild",
                                                           "GRADO_ANSIEDAD=3 Moderate",
                                                           "GRADO_ANSIEDAD=4 Severe",
                                                           "GRADO_ANSIEDAD=5 Extremely severe"), default="lhs"))

reglas_apriory_ansiedad_ordenadas <- sort(reglas_apriory_ansiedad, by = "confidence")
inspect(reglas_apriory_ansiedad_ordenadas)
#inspect(reglas_apriory_ansiedad_ordenadas[1:100])


#Reglas apriory con consecuente estres y con supp = 0.25, conf = 0.8
reglas_apriory_estres<-apriori(transacciones, parameter = list ( supp = 0.15, conf= 0.75 , target = "rules"),
                                  appearance = list(rhs=c("GRADO_ESTRES=1 Normal",
                                                          "GRADO_ESTRES=2 Mild",
                                                          "GRADO_ESTRES=3 Moderate",
                                                          "GRADO_ESTRES=4 Severe",
                                                          "GRADO_ESTRES=5 Extremely severe"), default="lhs"))

reglas_apriory_estres_ordenadas <- sort(reglas_apriory_estres, by = "confidence")
inspect(reglas_apriory_estres_ordenadas)
#inspect(reglas_apriory_estres_ordenadas[1:100])


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# KMEANS 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

table(datos$EDAD)

variables_cluster<-c("SEXO", "EDAD", "DEPRESION", "ANSIEDAD", "ESTRES")

data_cluster <-datos[variables_cluster]

head(data_cluster)
str(datos)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Las variables cualitativas se se convierten a variables 
# ficticias o variables dummy que son variables binarias que toman valor 0,1.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

data_cluster_dummy <- dummy.data.frame(data_cluster, sep = ".")

names(data_cluster_dummy)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Escalado de las variables del dataset data_cluster_dummy 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

data_cluster_scale <- scale(data_cluster_dummy)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Aplicación del algoritmo kmeans con 3 cluster y se visualizan los resultados:
# - Los centros de grupos (centers), 
# - La suma de cuadrados totales (totss), 
# - Las sumas de cuadrados dentro de cada grupo y para todos de forma conjunta (withinss y tot.withinss) 
# - La suma de cuadrados entre grupos (betweenss). 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

 
set.seed(123)
clus2_k3<-kmeans(data_cluster_scale,centers=3)
clus2_k3$centers
clus2_k3$totss
clus2_k3$withinss
clus2_k3$tot.withinss
clus2_k3$betweenss
clus2_k3

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Para elegir el número de clústers más apropiado buscaremos el k que haga que los individuos pertenecientes a un mismo grupo son lo más homogéneos posible y los individuos pertenecientes a distintos grupos son lo más heterogéneos posible tiendo en cuenta que cuanto más grande sea k más cálculos debe hacer el algoritmo.
# Los parámetros que usamos son las sumas de cuadrados dentro de todos los grupos (tot.withinss) y la suma de cuadrados entre grupos (betweenss). 
# Queremos maximizar la suma de cuadrados entre grupos(betweenss) y 
# minimizar la suma de cuadrados dentro de los grupos (tot.withinss). 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1º Método para seleccionar el mejor número de cluster
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(123)

bss <- kmeans(data_cluster_scale,centers=1)$betweenss

for (i in 2:10) bss[i] <- kmeans(data_cluster_scale,centers=i)$betweenss
plot(1:10, bss, type="l", xlab="Número de grupos",ylab="Sumas de cuadrados entre grupos")

set.seed(123)
tw <- kmeans(data_cluster_scale,centers=1)$tot.withinss
for (i in 2:10) tw[i] <- kmeans(data_cluster_scale,centers=i)$tot.withinss

plot(1:10, tw, type="l", xlab="Número de grupos",ylab="Sumas de cuadrados dentro de grupos")


# Fusionado de los dos grupos
{
  plot(1:10, tw, type="l",col="red", xlab="",ylab="", ylim=c(0,200000))
  axis(side=2, col="red") #Minimizar
  par(new=TRUE)
  plot(1:10, bss, type="l",col="blue", xlab="Número de grupos",ylab="", ylim=c(0,200000), axes=FALSE)
  axis(side=4, col="blue") #Maximizar 
  title(main="Sumas de cuadrados dentro de grupos (rojo) y entre grupos (azul)")
  abline(v=3, col="green")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2º Método para seleccionar el mejor número de cluster
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#Determinar un numero de cluster óptimo

sumbt <- kmeans(data_cluster_scale , centers = 1)$betweenss
sumbt
for(i in 2:20){
  sumbt[i] <- kmeans(data_cluster_scale , centers =  i)$betweenss
  print(sumbt[i])
}
plot(1:20, sumbt, type = "b", xlab = "número de cluster", ylab = "suma de cuadrados inter grupos")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3º Método para seleccionar el mejor número de cluster
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Clustering iterativo kmeans
x <- NULL
for ( i in 1:20 ){
  kc     <- kmeans( data_cluster_scale, i )
  x[ i ] <- kc$tot.withinss
}
plot( c( 1:20 ), x, type = "b" )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Comprobacion del mejor número de cluster a elegir. Para ello se busca:
# Minimizar las sumas de cuadrados dentro de todos los grupos (tot.withinss) 
# Maximizar la suma de cuadrados entre grupos (betweenss). 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Finalmente se escoge 3 números de clúster

set.seed(123)
clus2_k3<-kmeans(data_cluster_scale,centers=3)
clus2_k3$tot.withinss
clus2_k3$betweenss
clus2_k3


head(data_cluster_scale)

names(data_cluster_scale)<-c("SEXO.Male_st","SEXO.Female_st", "EDAD_st", "DEPRESION_st", "ANSIEDAD_st", "ESTRES_st") 

data_cluster_result<-data.frame(data_cluster, data_cluster_scale, as.factor(clus2_k3$cluster))

names(data_cluster_result)[12]<-"cluster"

head(data_cluster_result)

data_cluster_result
sql1 <- sqldf('
                 select 
                 cluster
                 , avg(EDAD) as EDAD_center
                 , avg(DEPRESION) as DEPRESION_MEDIA
                 , avg(ANSIEDAD) as ANSIEDAD_MEDIA
                 , avg(ESTRES) as ESTRES_MEDIA
                 , count(*) as N
                 from data_cluster_result
                 group by cluster
                 ORDER BY EDAD_center;')
 
nrow(data_cluster_result)
as.data.frame.matrix(sql1)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Diferencias en las medias
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


AnovaModel.1 <- aov(DEPRESION ~ SEXO, data = datos)
summary(AnovaModel.1)

AnovaModel.2 <- aov(ANSIEDAD ~ SEXO, data = datos)
summary(AnovaModel.2)

AnovaModel.3 <- aov(ESTRES ~ SEXO, data = datos)
summary(AnovaModel.3)

AnovaModel.20 <- lm(DEPRESION ~ PAIS, data = datos, contrasts = list(PAIS = "contr.Sum"))
AnovaModel.21 <- lm(ANSIEDAD ~ PAIS, data = datos, contrasts = list(PAIS = "contr.Sum"))
AnovaModel.22 <- lm(ESTRES ~ PAIS, data = datos, contrasts = list(PAIS = "contr.Sum"))


