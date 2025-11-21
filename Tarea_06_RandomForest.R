
# Carga de librerias

install.packages("randomForest")
library(readxl)
library(arules)
library(genero)
library(rpart)
library(rpart.plot)
library(randomForest)

# Carga de datos
data <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\05 Random Forest\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")
dataRF <- data[, c("HEC_DEPTO", "VIC_SEXO", "VIC_TRABAJA", "VIC_EDAD", "AGR_TRABAJA", "VIC_ESCOLARIDAD")]

# Limpieza de datos
dataRF <- na.omit(dataRF)

# Convertir a factor, para caracterizacion de variables
dataRF$HEC_DEPTO <- as.factor(dataRF$HEC_DEPTO) 

# Definiendo semilla aleatoria
set.seed(150)

# Aleatorizacion de los datos
dataRF <- dataRF[sample(1:nrow(dataRF)),]

# Datos: 20% Para muestra del modelo y 80% Para entrenamiento
index <- sample(1:nrow(dataRF), 0.8*nrow(dataRF))

# Define el training para el modelo de arbol y su entrenamiento (80-20)
train <- dataRF[index,] # Los datos de entrenamiento
test <- dataRF[-index,] # Los datos de prueba

# Definicion del Arbol de desicion para departamento del hecho de victima (Train)
RF <- randomForest (HEC_DEPTO ~ 
                VIC_SEXO +
                VIC_TRABAJA +
                VIC_EDAD,
               data = train, ntree = 100, mtry = 3
)

# Prueba del modelo de Bosque Aleatorio
prueba <- predict(RF, test)
prueba

# Verificando el modelo con correlacion
matriz <- table(test$HEC_DEPTO, prueba)
matriz

# Verificando la presicion del modelo
pre <- sum(diag(matriz))/sum(matriz)
pre

# Como se observo  en los otros metodos, la victima sera mujer en los casos en que
# el agresor cumpla con las siguientes condiciones.

# Parametros a evaluar
persona <- data.frame(
  VIC_SEXO = c(2),
  VIC_TRABAJA = c(2),
  VIC_EDAD = c(48)
)

pred <- predict(RF, persona, type="prob")
pred
