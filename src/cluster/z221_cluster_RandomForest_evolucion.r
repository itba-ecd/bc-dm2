# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

require("randomForest")
require("ranger")

PARAM <- list()
PARAM$experimento <- "clu-randomforest-02"

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


# genero el dataset chico
dchico <- dataset[, c("numero_de_cliente",campos_cluster), with=FALSE]

# arreglo los valores NA
dchico  <- na.roughfix( dchico )
# no hace falta escalar

#i nvoco a la distancia de Random Forest
 #Ahora, a esperar .. con esta libreria de la prehistoria
#  que NO corre en paralelo

# aqui va SU semilla
set.seed(102191)

modelo <- randomForest( x= dchico[, campos_cluster, with=FALSE ],
                        y= NULL,
                        ntree= 1000, #se puede aumentar a 10000
                        proximity= TRUE,
                        oob.prox=  TRUE )

# genero los clusters jerarquicos
# distancia = 1.0 - proximidad
hclust.rf <- hclust( as.dist ( 1.0 - modelo$proximity),
                     method= "ward.D2" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


kclusters <- 5  # cantidad de clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=kclusters & distintos <=kclusters ) )
{
  h <- h - 1
  rf.cluster <- cutree( hclust.rf, h)

  dchico[, cluster := paste0("cluster_", rf.cluster) ]

  distintos <- nrow( dchico[, .N, cluster ] )
  cat( distintos, " " )
}


#--------------------------------------

setorder( dchico, cluster, numero_de_cliente )

fwrite(dchico,
       file= "dchico.txt",
       sep= "\t")

#--------------------------------------
# Analisis de resultados de k-means
# cantidad de registros por cluster

dcentroides <- dchico[, lapply(.SD, mean, na.rm=TRUE), 
    by= cluster, 
    .SDcols= campos_cluster ]

dcentroides

fwrite(dchico,
       file= "centroides.txt",
       sep= "\t" )

#--------------------------------------
# gafico los clusters en forma bivariada

# Solo voy a mostrar un porcentaje de dchico
dchico[, azar := runif(nrow(dchico)) ]
muestra <- 0.1  # me voy a quedar con los menores a este valor

# calculo la cantidad de campos
n <- length(campos_cluster)


# voy a graficar en escala logaritmica
# cuidado con 

pdf("bivariado.pdf")

for( i in 1:(n-1) ){
  for( j in (i+1):n ){

  grafico <- ggplot( dchico[azar< muestra],
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
dhistoria[ dchico,
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
pdf("evol_RandomForest.pdf")

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
pdf("evol_noceros_RandomForest.pdf")

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


