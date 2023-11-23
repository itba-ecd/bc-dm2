# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

PARAM <- list()
PARAM$experimento <- "journey-01"

#------------------------------------------------------------------------------
#deflaciona por IPC
#momento 1.0  31-dic-2020 a las 23:59

drift_deflacion  <- function( campos_monetarios )
{
  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105, 202106,
                  202107  )

  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213, 0.8003763543,
              0.7763107219  )

  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )

  dataset[ tb_IPC,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("X:\\gdrive\\itba2023-ciudad2a")

# leo el dataset
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/dataset_journey.tsv.gz
dataset <- fread("./datasets/dataset_journey.tsv.gz")

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# ordeno el dataset
setorder( dataset, numero_de_cliente, foto_mes )

# conteos fecha de ingreso a Paquete Premium
tb_nacimiento <- dataset[ , list(foto_mes_min= min(foto_mes) ),
                         numero_de_cliente ]

setorder( tb_nacimiento, foto_mes_min)
tb_nacimiento[ , .N, foto_mes_min ]


# Exploro artesanalmente la evolucion de El Primer Cliente que ingreso en 201902
tb_primer_cliente <- dataset[ numero_de_cliente== 29193622 ]

# Exploro atesanalmente la evolucion de El Ultimo Cliente que ingreso en 201902
tb_ultimo_cliente <- dataset[ numero_de_cliente== 168348419 ]



# calculo la minima y maxima foto_mes
dataset[ , foto_mes_min := min(foto_mes), numero_de_cliente ]
dataset[ , foto_mes_max := max(foto_mes), numero_de_cliente ]
dataset[ , cliente_antiguedad_inicial := min(cliente_antiguedad), numero_de_cliente ]


#-----------------------------------------------------
# Paso las variables monetarias a moneda constante
#  deflaciono
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

drift_deflacion( campos_monetarios )

#-----------------------------------------------------
# Clustering
# campos arbitrarios 

campos_cluster <- c("cliente_edad", "cliente_antiguedad", 
  "cliente_antiguedad_inicial", "ctrx_quarter",
  "mpayroll", "mcaja_ahorro", "mtarjeta_visa_consumo",
  "mtarjeta_master_consumo", "mprestamos_personales",
  "Visa_status", "Master_status", "cdescubierto_preacordado",
  "mplazo_fijo_dolares", "mplazo_fijo_pesos" )


# arreglo los valores NA
dataset[ is.na(Visa_status), Visa_status := 11 ]
dataset[ is.na(Master_status), Master_status := 11 ]


# en kmeans DEBO MANDATORIAMENTE escalar las variables
#   sino la Distancia Euclidea funciona muy mal
# genero el dchico Escalando las variables 
dchico <- dataset[, list(numero_de_cliente)]

for( campo in campos_cluster ){

  dchico[ , paste0(campo,"_esc") :=
    dataset[, (get(campo) - mean(get(campo))) /  sd(get(campo)) ] ]
}


# invoco a kmedias --------------------

# aqui va SU semilla
set.seed(102191)

kclusters <- 7  # cantidad de clusters

# la llamada  a kmeans
campos_cluster_esc <- setdiff( colnames( dchico), "numero_de_cliente" )

kmedias <- kmeans( dchico[, campos_cluster_esc, with=FALSE],
  centers= kclusters,
  nstart= 20)

#--------------------------------------
# Aqui queda el cluster asignado a cada registro
#  kmedias$cluster

dataset[ , cluster := kmedias$cluster ]
setorder( dataset, numero_de_cliente, foto_mes, cluster )


#--------------------------------------
# Analisis de resultados de k-means
# cantidad de registros por cluster

dcentroides <- dataset[, lapply(.SD, mean, na.rm=TRUE), 
    by= cluster, 
    .SDcols= campos_cluster ]

dcentroides

fwrite(dcentroides,
       file= "centroides.txt",
       sep= "\t" )

#--------------------------------------

# calculo el lag anterior
setorder( dataset, numero_de_cliente, foto_mes )

dataset[ , cluster_lag1 := shift(cluster, 1, NA, "lag"), 
         by= numero_de_cliente ]

dataset[ foto_mes <  202107, cluster_lead1 := shift(cluster, 1, NA, "lead"), 
         by= numero_de_cliente ]

dataset[  ,.N, list( cluster_lag1, cluster, cluster_lead1 ) ]


fwrite( dataset[ , list( qty = .N ), list( cluster, cluster_lead1 ) ] ,
        file = "transicion_clusters.txt",
        sep = "\t" )


#--------------------------------------

