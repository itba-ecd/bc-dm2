# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

PARAM <- list()
PARAM$experimento <- "clu-kmeans-02"

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("/media/Shared/gustavo/b1/")

# leo el dataset
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/BAJA2_20210_202107.tsv
dataset <- fread("./datasets/BAJA2_20210_202107.tsv")


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# campos arbitrarios 
campos_cluster <- c("cliente_edad", "cliente_antiguedad", "ctrx_quarter",
  "mpayroll", "mcaja_ahorro", "mtarjeta_visa_consumo",
  "mtarjeta_master_consumo", "mprestamos_personales",
  "Visa_status", "Master_status", "cdescubierto_preacordado")


# arreglo los valores NA
dataset[ is.na(Visa_status), Visa_status := 11 ]
dataset[ is.na(Master_status), Master_status := 11 ]


# genero el dchico Escalando las variables 
dchico <- dataset[, list(numero_de_cliente)]

for( campo in campos_cluster ){

  dchico[ , paste0(campo,"_esc") :=
    dataset[, (get(campo) - mean(get(campo))) /  sd(get(campo)) ] ]
}


# invoco a kmedias --------------------

# aqui va SU semilla
set.seed(102191)

kclusters <- 5  # cantidad de clusters

# la llamada  a kmeans
campos_cluster_esc <- setdiff( colnames( dchico), "numero_de_cliente" )

kmedias <- kmeans( dchico[, campos_cluster_esc, with=FALSE],
  centers= kclusters,
  nstart= 20)

#--------------------------------------
# Aqui queda el cluster asignado a cada registro
#  kmedias$cluster

dataset[ , cluster := paste0("cluster_", kmedias$cluster) ]
setorder( dataset, cluster, foto_mes, numero_de_cliente )

fwrite(dataset[ ,c("numero_de_cliente", campos_cluster), with=FALSE],
       file= "dchico.txt",
       sep= "\t")

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
# gafico los clusters en forma bivariada

# Solo voy a mostrar un porcentaje del dataset
dataset[, azar := runif(nrow(dataset)) ]
muestra <- 0.1  # me voy a quedar con los menores a este valor

# calculo la cantidad de campos
n <- length(campos_cluster)


# voy a graficar en escala logaritmica
# cuidado con 

pdf("bivariado.pdf")

for( i in 1:(n-1) ){
  for( j in (i+1):n ){

  grafico <- ggplot( dataset[azar< muestra],
      aes_string(x= campos_cluster[i],
                 y= campos_cluster[j],
                 color= "cluster"))  +
      scale_colour_brewer(palette = "Dark2") +
      geom_point(alpha = 0.50) +
      xlab(campos_cluster[i]) +
      scale_x_continuous(trans = pseudolog10_trans) +
      ylab(campos_cluster[j]) +
      scale_y_continuous(trans = pseudolog10_trans)

   print( grafico )
  }
}

dev.off()

# -----------------------------------------------------------------------------
# Ahora incorporo la evolucion historica antes de la BAJA

# leo la historia ( desde donde hay,  201901 )
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/BAJA2_20210_202107_historia.tsv
dhistoria <- fread("~/buckets/b1/datasets/BAJA2_20210_202107_historia.tsv")

# asigno el cluster a los 
dhistoria[ dataset,
           on= "numero_de_cliente",
           cluster := i.cluster ]

# asigno cuentra regresiva antes de la BAJA
setorder( dhistoria, numero_de_cliente, -foto_mes )

dhistoria[, periodo := - rowid(numero_de_cliente)]

# ejemplo
dhistoria[numero_de_cliente==29194886, list( numero_de_cliente, foto_mes, periodo ) ]


# grafico la evolucion de cada < cluster, variable >  univariado ------

# todos los campos menos los que no tiene sentido
campos_totales <- setdiff( colnames(dhistoria),
  c("numero_de_cliente","foto_mes","clase_ternaria","cluster","periodo") )



# Genero el grafico intervalo confianza 95%
pdf("evol_kmeans.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dhistoria[periodo >= -18],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "gam", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()



#--------------------------------------------------------------------
# quito los CEROS  de los graficos

# reemplazo los CEROS  por NA
#  los registros NA no se grafican
dhistoria[ dhistoria==0, ] <- NA

# Genero el grafico intervalo confianza 95%
pdf("evol_noceros_kmeans.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dhistoria[periodo >= -18],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "gam", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()


