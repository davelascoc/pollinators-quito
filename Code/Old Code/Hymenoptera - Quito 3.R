                      ## <<<<<  HYMENOPTERA | QUITO  >>>>> ##

## Daniel Velasco
## 12/9/2024

### ECOLOGICAL NETWORKS ###

# DATA MANAGEMENT

#load packages
library(readxl)
library(tidyverse)
library(GGally)
library(factoextra)
library(reshape2)
library(bipartite)
library(patchwork)

#import database
master  <-  read_excel("Datos/Datos Originales/Hymenoptera_Quito.xlsx")

#select taxa information
master  <-  master    %>%   select(16:25)

#remove indet values
m1      <-  master    %>%   filter(Insect != "Indet", Plant  != "Indet")
m2      <-  master    %>%   filter(ITribe != "Indet", PGenus != "Indet")

#en m1 solo hay registros a nivel de especie
#y en m2, a nivel de tribu y género para insectos y plantas respectivamente.

#mínimo 3 registros
m3      <-  m2        %>%   group_by(ITribe, PGenus)   %>%
                            tally(sort = T)            %>%
                            filter(n > 3)              #al menos 4 registros
#turn to matrix
m3                   <-     dcast(m3, ITribe ~ PGenus, value.var = "n")

#row names
m3                   <-     m3 %>% column_to_rownames("ITribe")

#put 0s in NA values
m3[is.na(m3)]        <-     0

#create matrix playing with "table" function
m1      <-  as.data.frame.matrix(table(m1$Insect, m1$Plant ))
m2      <-  as.data.frame.matrix(table(m2$ITribe, m2$PGenus))

#order of species
spp     <-  read_excel("Datos/Datos Analizados/Species Order.xlsx")
m1      <-  m1[, names(spp)]      #ordenar especies de plantas

class(m1)
class(m2)
class(m3)

#factors
master  <-  master %>% mutate_at(1:10, factor)

#summary
summary(master)

rm(spp)



# PLOTTING  << Insect-Plant Networks >> #

#----------------------------------------------------------------------------
plotweb(m1,                          ## << SPECIES-LEVEL >>
        method = "cca",
        col.interaction = "grey20",   # color de la interacción
        col.high = "#2E8B57",         # color de especies arriba
        col.low  = "#EEC900",         # color de especies abajo
        bor.col.interaction = NA,     # bordes
        bor.col.high = NA, 
        bor.col.low  = NA,
        y.width.high = 0.1,           # ancho de bloques
        y.width.low	 = 0.1,
        low.y    = 0.675,             # posición de boxes en el y-axis
        high.y   = 1.6,
        text.rot = 90,                # rotación de texto
        labsize  = 0.7,               # tamaño de label
        #ybig     = 1.3,              # distancia entre upper and lower boxes
        arrow    = "no",              # forma geométrica de la conexión
        high.lablength = 0)           # num de caracteres en etiquetas



plotweb(m1,                          ## << SPECIES-LEVEL >>
        method = "cca",
        col.interaction = "grey20",   # color de la interacción
        col.high = "#2E8B57",         # color de especies arriba
        col.low  = "#EEC900",         # color de especies abajo
        bor.col.interaction = NA,     # bordes
        bor.col.high = NA, 
        bor.col.low  = NA,
        low.y    = 0.675,             # posición de boxes en el y-axis
        high.y   = 1.6,
        text.rot = 90,                # rotación de texto
        labsize  = 0.7,               # tamaño de label
        #ybig     = 1.3,              # distancia entre upper and lower boxes
        arrow    = "no",              # forma geométrica de la conexión
        high.lablength = 0)           # num de caracteres en etiquetas












