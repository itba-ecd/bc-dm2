# Regresion trivial  FUTURO = presente

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito

require("ggplot2")

PARAM <- list()
PARAM$experimento <- "reg-3120"

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


dapply <- dataset[foto_mes == 202107] # defino donde voy a aplicar el modelo

# Dentro de dos meses tendre lo mismo que ahora en dolares
dapply[ , pred := mplazo_fijo_dolares ]

# Calculo el  RMSE
RMSE <- dapply[ , sqrt( mean( (fplazo_fijo_dolares - pred ) ^ 2, na.rm=TRUE ) ) ]
cat( "Root Mean Squared Error :", RMSE, "\n" )


# Grafico
ggplot(dapply, aes(x = pred, y = fplazo_fijo_dolares)) +
  geom_point() +
  geom_abline( col= "red")


ggplot(dapply, aes(x = pred, y = fplazo_fijo_dolares)) +
  geom_point() +
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

