# Arbol de Regresion elemental con libreria  rpart

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito
require("data.table")
require("lightgbm")

require("ggplot2")

PARAM <- list()
PARAM$experimento <- "reg-3560"

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

campos_buenos <- setdiff( colnames(dtrain), c("numero_de_cliente", "fplazo_fijo_dolares") )

dentrenar  <- lgb.Dataset( data=  data.matrix( dtrain[ !is.na(fplazo_fijo_dolares), campos_buenos, with=FALSE] ),
                           label= dtrain[ !is.na(fplazo_fijo_dolares), fplazo_fijo_dolares ],
                           free_raw_data= FALSE )

param_cv  <-  list( 
  objective= "regression",
  metric= "rmse",
  first_metric_only= TRUE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  extra_trees= TRUE,
  num_leaves= 32,
  min_data_in_leaf= 500,
  learning_rate= 0.05,
  feature_fraction= 0.7,
  max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split= 0.0, #por ahora, lo dejo fijo
  lambda_l1= 0.0,         #por ahora, lo dejo fijo
  lambda_l2= 0.0,         #por ahora, lo dejo fijo
  max_bin= 255,           #por ahora, lo dejo fijo
  num_iterations= 5000,   #un numero muy grande, lo limita early_stopping_rounds
  early_stopping= 200,
  force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
  seed= 102191 )


# busco con 5-fold cross validation el num_iterations optimo
modelo_cv  <- lgb.cv( 
  data= dentrenar,
  stratified= TRUE, #sobre el cross validation
  nfold= 5,    #folds del cross validation
  param= param_cv)


# Preparo los parametros del modelo final
param_train <- copy( param_cv )
param_train$early_stopping <- NULL
param_train$num_iterations <- modelo_cv$best_iter


# genero el modelo final
modelo <- lgb.train( 
  data= dentrenar,
  param= param_train )


# predigo sobre los datos nuevos
prediccion <- predict( modelo,
                      data.matrix( dapply[ , campos_buenos, with=FALSE]) )


# agrego a dapply una columna nueva que es la prediccion
dapply[, pred := prediccion]

dapply[ pred < 0 , .N ]
dapply[ pred < 0 ,  pred := 0 ]

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

