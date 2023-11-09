# Arbol de Regresion elemental con libreria  rpart

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

PARAM <- list()
PARAM$experimento <- "reg-5730"

#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd( "x:\\gdrive\\itba2023-ciudad2a\\" )

# cargo el dataset
dataset <- fread( "./datasets/regresion_FE_03.tsv.gz" )

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


dtrain <- dataset[foto_mes %in%  c(202105) ] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202107] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir fplazo_fijo_dolares a partir de el resto de las variables
modelo <- rpart(
        formula = "fplazo_fijo_dolares ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 10, # minima cantidad de registros para que se haga el split
        minbucket = 5, # tamaño minimo de una hoja
        maxdepth = 5
) # profundidad maxima del arbol



tb_importancia <- as.data.table( 
  list( "Feature"= names(modelo$variable.importance), 
        "Importance"= modelo$variable.importance )  )


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply
)


# agrego a dapply una columna nueva que es la prediccion
dapply[, pred := prediccion]

# veo como fue la prediccion
dapply[ , list( numero_de_cliente, fplazo_fijo_dolares, pred ) ]

# la prediccion para los que SI tienen plazo fijo
dapply[ mplazo_fijo_dolares>0, 
  list( numero_de_cliente, mplazo_fijo_dolares, fplazo_fijo_dolares, pred ) ]

# Calculo el  RMSE
RMSE <- dapply[ , sqrt( mean( (fplazo_fijo_dolares - pred ) ^ 2, na.rm=TRUE ) ) ]
cat( "Root Mean Squared Error :", RMSE, "\n" )

