# A partir del dataset grande creo nuevos dataset
#  agregando campos historicos

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

# cargo las librerias que necesito
require("data.table")

#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd( "x:\\gdrive\\itba2023-ciudad2a\\" )

# cargo el dataset
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/regresion_grande.tsv.gz
dataset <- fread( "./datasets/regresion_grande.tsv.gz" )


# Hago el feature Engineering
cols_lagueables <- copy(setdiff( colnames(dataset),
  c("numero_de_cliente", "foto_mes", "fplazo_fijo_dolares") ))


# muy importante, ordeno el dataset por < cliente, periodo >
# si esto no esta, estalla todo por los aires
setorder( dataset, numero_de_cliente, foto_mes )

# agrego lag de orden 1
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
  by = numero_de_cliente,
  .SDcols = cols_lagueables ]

# agrego lag de orden 2
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
  by = numero_de_cliente,
  .SDcols = cols_lagueables ]


# grabo el dataset
fwrite( dataset[ foto_mes %in% c( 202105, 202107 ) ],
       file= "./datasets/regresion_FE_02.tsv.gz",
       sep= "\t" )
