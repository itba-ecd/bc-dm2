# Optimizacion Bayesian de Hiperparametros
# LightGBM en modeo Regresion

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "HT5560"

PARAM$workingdirectory <- "X:\\gdrive\\itba2023-ciudad2a\\"
PARAM$input$dataset <- "./datasets/regresion_pequeno.tsv.gz"
PARAM$input$train <- c(202105)

PARAM$input$campo_clase <- "fplazo_fijo_dolares"

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds <- 5

# Aqui poner su segunda semilla
PARAM$hyperparametertuning$semilla_azar <- 200177


PARAM$lgb_basicos <- list(
   boosting= "gbdt",               # puede ir  dart  , ni pruebe random_forest
   objective= "regression",        # indico que es una REGRESION
   metric= "rmse",                 # indico metrica Root Mean Squared Error
   first_metric_only= TRUE,
   boost_from_average= TRUE,
   feature_pre_filter= FALSE,
   force_row_wise= TRUE,           #para que los alumnos no se atemoricen con tantos warning
   verbosity= -100,
   max_depth=  -1L,                # -1 significa no limitar,  por ahora lo dejo fijo
   min_gain_to_split= 0.0,         # min_gain_to_split >= 0.0
   min_sum_hessian_in_leaf= 0.001, #  min_sum_hessian_in_leaf >= 0.0
   lambda_l1= 0.0,                 # lambda_l1 >= 0.0
   lambda_l2= 0.0,                 # lambda_l2 >= 0.0
   max_bin= 255L,                   #

   bagging_fraction= 1.0,          # 0.0 < bagging_fraction <= 1.0
   pos_bagging_fraction= 1.0,      # 0.0 < pos_bagging_fraction <= 1.0
   neg_bagging_fraction= 1.0,      # 0.0 < neg_bagging_fraction <= 1.0
   is_unbalance=  FALSE,           #
   scale_pos_weight= 1.0,          # scale_pos_weight > 0.0

   drop_rate=  0.1,                # 0.0 < neg_bagging_fraction <= 1.0
   max_drop= 50,                   # <=0 means no limit
   skip_drop= 0.5,                 # 0.0 <= skip_drop <= 1.0

   extra_trees= FALSE,             # Magic Sauce

   seed=  PARAM$hyperparametertuning$semilla_azar
   )

# Aqui se cargan los bordes de los hiperparametros
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 20L, upper = 8000L),
  makeIntegerParam("num_leaves", lower = 16L, upper = 2048L)
)

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) # Escribo los titulos
    {
      linea <- paste0(
        "fecha\t",
        paste(list.names(reg), collapse = "\t"), "\n"
      )

      cat(linea, file = archivo)
    }

  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )

  cat(linea, file = archivo, append = TRUE) # grabo al archivo

  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales

Estimar_error_lightgbm <- function(x) {
  gc() # libero memoria

  # llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # junto los parametros
  param_completo <- c(PARAM$lgb_basicos, x)

  # Configuro el early stopping
  param_completo$num_iterations <- 1000
  param_completo$early_stopping <- 50

  # seteo la semilla para replicabilidad
  set.seed( PARAM$hyperparametertuning$semilla_azar, kind= "L'Ecuyer-CMRG")

  modelocv <- lgb.cv(
    data = dtrain,
    stratified = TRUE, # sobre el cross validation
    nfold = PARAM$hyperparametertuning$xval_folds, # folds del cross validation
    param = param_completo,
    verbose = -100
  )

  # obtengo error
  best_error <- unlist(modelocv$record_evals$valid$rmse$eval[modelocv$best_iter] )

  ds  <- list( "cols"= ncol(dtrain),  "rows"= nrow(dtrain) )
  xx  <- c( ds, copy(param_completo) )
  xx$num_iterations <- NULL
  xx$early_stopping <- NULL

  xx$num_iterations <- modelocv$best_iter
  xx$error <- best_error # el mejor error que obtuve
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)

  cat( "iteracion : ", GLOBAL_iteracion, 
    " ", best_error, "\n" )

  set.seed( PARAM$hyperparametertuning$semilla_azar, kind= "L'Ecuyer-CMRG")
  return( best_error )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd(PARAM$workingdirectory) # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")


GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_error_menor <- +Inf # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_error_menor <- tabla_log[, min(error) ]
}


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c( PARAM$input$campo_clase, "fold_train")
)

# defino los datos donde voy a entrenar
dataset[ , fold_train := 0 L ]
dataset[ foto_mes %in% PARAM$input$train, fold_train := 1L ]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[ fold_train==1L, campos_buenos, with = FALSE]),
  label = dataset[ fold_train==1L, get(PARAM$input$campo_clase) ], # el campo que uso como clase
  free_raw_data = FALSE
)



# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- Estimar_error_lightgbm # la funcion que voy a minimizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = TRUE, # estoy Minimizando el RMSE
  noisy = FALSE,
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$hyperparametertuning$iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
set.seed( PARAM$hyperparametertuning$semilla_azar, kind= "L'Ecuyer-CMRG")

if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")


#  debugging

x <- list()
x$learning_rate <-  0.1
x$feature_fraction <- 0.5
x$min_data_in_leaf <- 100L
x$num_leaves <- 100L

Estimar_error_lightgbm(x)
