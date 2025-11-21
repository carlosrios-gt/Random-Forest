# Tarea 4 - Carlos Rios
# Arbol de desicion

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(readxl)

# Carga de datos
data <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\04 Arbol de Decision\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

# Creando Data Frame (Por Patron 1, identificado en metodos anteriores)
# variable depto= ciudad=1
data$ciudad = ifelse(data$HEC_DEPTO == 1, 1, 0)
data$ciudad

# Definicion del Arbol de desicion para genero de victima
arbol <- rpart(VIC_SEXO ~
                 VIC_TRABAJA +
                 VIC_EDAD +
                 AGR_TRABAJA +
                 VIC_ESCOLARIDAD,
               data = data, method = "class"
)
rpart.plot(arbol, type = 2, extra = 0, under = TRUE, fallen.leaves = TRUE,
           box.palette = "BuGn", main = "Genero de la victima", cex = 0.5)

# Como se observo  en los otros metodos, la victima sera mujer en los casos en que
# el agresor cumpla con las siguientes condiciones.

# Parametros a evaluar
persona <- data.frame(
  VIC_TRABAJA = c(2),
  VIC_EDAD = c(48),
  AGR_TRABAJA = c(2), # No trabaja
  VIC_ESCOLARIDAD = c(57)
)

pred <- predict(arbol, persona, type="prob")
pred
