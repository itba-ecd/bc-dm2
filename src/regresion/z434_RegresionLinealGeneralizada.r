# Arbol de Regresion elemental con libreria  rpart

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito
require("data.table")
require("glmnet")

require("ggplot2")


PARAM <- list()
PARAM$experimento <- "reg-4340"

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


# Creo la Regresion Lineal Ridge
# grid <- 10^seq(10, -2, length = 100)
grid <- 10^seq(10, -0.1, length = 100)

x_matrix <- model.matrix( fplazo_fijo_dolares ~  mplazo_fijo_pesos + mpasivos_margen +
    mrentabilidad  + mrentabilidad_annual + cliente_edad + cliente_antiguedad, 
    data= dtrain[ ! is.na( fplazo_fijo_dolares)] )
    
modelo <- cv.glmnet(
  x =  x_matrix,
  y = dtrain[ ! is.na( fplazo_fijo_dolares), fplazo_fijo_dolares ],
  alpha = 0, # ridge regression
  lambda = grid )


plot(modelo)

modelo$lambda.min  # el mejor lambda

x_matrix_new <- as.matrix( dapply[ , list( 1, mplazo_fijo_pesos, mpasivos_margen,
    mrentabilidad, mrentabilidad_annual, cliente_edad, cliente_antiguedad) ] )


prediccion <- predict( modelo, 
      newx = x_matrix_new,
      s = modelo$lambda.min  # el mejor lambda
      )


# agrego a dapply una columna nueva que es la prediccion
dapply[, pred := prediccion ]

# veo como fue la prediccion
dapply[ , list( numero_de_cliente, fplazo_fijo_dolares, pred ) ]

# la prediccion para los que SI tienen plazo fijo
dapply[ mplazo_fijo_dolares>0, 
    list( numero_de_cliente, mplazo_fijo_dolares, fplazo_fijo_dolares, pred ) ]

# Calculo el  RMSE
RMSE <- dapply[ , sqrt( mean( (fplazo_fijo_dolares - pred ) ^ 2, na.rm=TRUE ) ) ]
cat( "Root Mean Squared Error :",  RMSE, "\n" )


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

