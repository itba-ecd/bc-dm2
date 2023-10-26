# conociendo la variable  mplazo_fijo_dolares
limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("ggplot2")

#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd( "x:\\gdrive\\itba2023-ciudad2a\\" )

# leo el dataset
# https://storage.googleapis.com/open-courses/itba2023-ciudad2a/regresion_pequeno.tsv.gz
dataset <- fread( "./datasets/regresion_pequeno.tsv.gz" )

#----- Estadisticos Basicos -----------
# calculo de la media
dmes <- dataset[ foto_mes==202105 ]
dmes[ , mean(mplazo_fijo_dolares) ]

# cuento cuanta gente NO tiene, y SI tiene plazo fijo en dolares
dmes[ mplazo_fijo_dolares==0, .N ]
dmes[ mplazo_fijo_dolares!=0, .N ]

# min. media, max
dmes[ mplazo_fijo_dolares>0, min(mplazo_fijo_dolares) ]
dmes[ mplazo_fijo_dolares>0, mean(mplazo_fijo_dolares) ]
dmes[ mplazo_fijo_dolares>0, max(mplazo_fijo_dolares) ]

# como mas de la mitad no tiene plazo fijo, 
#   la mediana obviamente va a dar ...
dmes[ , median(mplazo_fijo_dolares) ]

# cuantiles
quantile( dmes[ , mplazo_fijo_dolares] )

quantile( dmes[ mplazo_fijo_dolares>0, mplazo_fijo_dolares] )

quantile( dmes[ mplazo_fijo_dolares>0, mplazo_fijo_dolares], probs=seq(0, 1, 0.10) )


#-------- BoxPlot ---------------------

# boxplot  en R Base
boxplot( dmes[, mplazo_fijo_dolares ] )

# boxplot  en ggplot2
ggplot(dmes, aes(x=mplazo_fijo_dolares)) +
  geom_boxplot()

# ahora con escala logaritmica
ggplot(dmes[ mplazo_fijo_dolares>0], aes(x= mplazo_fijo_dolares)) + 
  geom_boxplot() +
  scale_x_log10()


#-------- Densidad --------------------

# la densidad tal cual es
ggplot(dmes, aes(x= mplazo_fijo_dolares)) + 
  geom_density()
  
# la densidad quintan los ceros
ggplot(dmes[mplazo_fijo_dolares>0], aes(x= mplazo_fijo_dolares)) +
  geom_density() 

# la densidad quintan los ceros
ggplot(dmes[mplazo_fijo_dolares>0], aes(x= mplazo_fijo_dolares)) +
  geom_density() +
  scale_x_log10()


#-------- Violin ---------------------

ggplot(dmes, aes(x=1, y=mplazo_fijo_dolares)) +
  geom_violin()


ggplot(dmes[ mplazo_fijo_dolares>0], aes(x= 1, y=m plazo_fijo_dolares)) +
  geom_violin() +
  scale_y_log10()

