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
PARAM$experimento <- "reg-3230"

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
modelo <- rpart(
        formula = "fplazo_fijo_dolares ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 0, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 10
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


tb_importancia <- as.data.table( 
  list( "Feature"= names(modelo$variable.importance), 
        "Importance"= modelo$variable.importance )  )


# imprimo a un archivo para poder ampliarlo
pdf("arbol_regresion_01.pdf")

prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)
dev.off()


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

