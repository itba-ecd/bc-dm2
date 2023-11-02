# Arbol de Regresion elemental con libreria  rpart

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("ggplot2")

PARAM <- list()
PARAM$experimento <- "reg-3450"

#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd( "x:\\gdrive\\itba2023-ciudad2a\\" )

# cargo el dataset
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/regresion_pequeno.tsv.gz
dataset <- fread( "./datasets/regresion_pequeno.tsv.gz" )

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


dtrain <- dataset[foto_mes == 202105] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202107] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir fplazo_fijo_dolares a partir de el resto de las variables
modelo_cero <- rpart(
        formula = "fplazo_fijo_dolares ~ .",
        data = dtrain[ mplazo_fijo_dolares==0], # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 0, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 8
) # profundidad maxima del arbol

modelo_NOcero <- rpart(
        formula = "fplazo_fijo_dolares ~ .",
        data = dtrain[ mplazo_fijo_dolares>0], # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 0, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 8
) # profundidad maxima del arbol


# aplico el modelo a los datos nuevos que son cero
prediccion_cero <- predict(
        object = modelo_cero,
        newdata = dapply[ mplazo_fijo_dolares==0] )

dapply[ mplazo_fijo_dolares==0, pred := prediccion_cero]


# aplico el modelo a los datos nuevos que son mayores a cero
prediccion_NOcero <- predict(
        object = modelo_NOcero,
        newdata = dapply[ mplazo_fijo_dolares>0] )

dapply[ mplazo_fijo_dolares>0, pred := prediccion_NOcero]


# Calculo el  RMSE
RMSE <- dapply[ , sqrt( mean( (fplazo_fijo_dolares - pred ) ^ 2, na.rm=TRUE ) ) ]
cat( "Root Mean Squared Error :", RMSE, "\n" )


# Grafico
ggplot(dapply, aes(x = pred, y = fplazo_fijo_dolares)) +
  geom_point() +
  geom_abline( col= "red")


ggplot(dapply, aes(x = pred, y = fplazo_fijo_dolares)) +
  geom_point() +
  xlim(0, 1e+08) +
  ylim(0, 1e+08) +
  geom_abline( col= "red") +
  scale_x_log10() +
  scale_y_log10()


# trampa para graficar con logaritmos
dapply[ pred==0, pred := 1 ]
dapply[ fplazo_fijo_dolares==0, fplazo_fijo_dolares := 1 ]

ggplot(dapply, aes(x = pred, y = fplazo_fijo_dolares)) +
  geom_point() +
  geom_abline( col= "red") +
  scale_x_log10() +
  scale_y_log10()

