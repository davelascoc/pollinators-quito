                  ## <<<<<  HYMENOPTERA | QUITO  >>>>> ##


# <<< IMPORT DATA >>>

library(readxl)
library(dplyr)
library(ggplot2)
library(bipartite)    

mydata <-  as.data.frame(select(read_excel("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Originales/Quito Hymenoptera - DataBase.xlsx"),
              IFamily, ITribe, IGenus, ISpecies, Insect,
              PFamily, PGenus, PSpecies, Plant, POrigin))


# <<< DATA MANAGEMENT >>>

attach(mydata)
l_insect <-  data.frame(table(Insect))   # playing with "Table" function
l_plant  <-  filter(data.frame(table (Plant, POrigin)), Freq > 0)

l1      <-  filter(as.data.frame(table(Insect, Plant, POrigin)), Freq>0)
                    # lista de interacciones species-level
l2      <-  filter(as.data.frame(table(ITribe, PGenus)), Freq > 0)
                    # lista de interacciones tribus vs géneros

m1              <-  as.data.frame.matrix(table(Insect, Plant))      # matriz

m1$Indet   <-  NULL                                   # eliminar indet col
m1$Spnames <-  rownames(m1)
m1         <-  subset(m1, Spnames!="Indet")      # eliminar indet row
m1$Spnames <-  NULL

m1$`Bellis perennis`                <- NULL        # spp sin interaccion
m1$`Chenopodium murale`             <- NULL
m1$`Eragrostis sp.`                 <- NULL
m1$`Erucastrum gallicum`            <- NULL
m1$`Hippeastrum puniceum`           <- NULL
m1$`Hypochaeris aff. sessiliflora`  <- NULL
m1$`Lamourouxia virgata`            <- NULL
m1$`Matricaria recutita`            <- NULL
m1$`Rosa x alba`                    <- NULL
m1$`Senna multiglandulosa`          <- NULL
m1$`Thunbergia alata`               <- NULL
m1$`Tradescantia zebrina`           <- NULL
m1$`Veronica persica`               <- NULL

m2            <-  as.data.frame.matrix(table(ITribe, PGenus))
m2$Indet      <- NULL                             # eliminar indet col
m2$Groupnames <- rownames(m2)
m2            <- subset(m2, Groupnames!="Indet")         # eliminar indet row
m2            <- subset(m2, Groupnames!="Diapriinae*")   # eliminar Diapriinae*
m2$Groupnames <- NULL

m2$Chenopodium <- NULL                             # eliminar spp sin interaccion
m2$Eragrostis  <- NULL
m2$Erucastrum  <- NULL
m2$Hippeastrum <- NULL
m2$Matricaria  <- NULL
m2$Thunbergia  <- NULL

##write.csv2(x = m1, file = "m1.txt")    # < EXPORTAR >
##write.csv2(x = m2, file = "m2.txt")

      ## Sin Apis mellifera

# mydata_noApis=subset(mydata, Insect!="Apis mellifera")
# attach(mydata_noApis)
# list_1_noApis=filter(as.data.frame(table(Insect, Plant)), Freq>0)
# list_2_noApis=filter(as.data.frame(table(ITribe, PGenus)), Freq>0)
# m1_noApis=as.data.frame.matrix(table(Insect, Plant))
# m2_noApis=as.data.frame.matrix(table(ITribe, PGenus))


# << IMPORTAR MATRIZ ORDENADA >>

m1_tax <- as.data.frame(read_excel("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Analizados/Matrix Database.xlsx",
                        sheet = "matrix1"))
attach(m1_tax)
rownames(m1_tax) <- Taxa
m1_tax$Taxa <- NULL

m2_tax <- as.data.frame(read_excel("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Analizados/Matrix Database.xlsx",
                        sheet = "matrix2"))
attach(m2_tax)
rownames(m2_tax) <- Taxa
m2_tax$Taxa <- NULL

m2_min <- as.data.frame(read_excel("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Analizados/Matrix Database.xlsx",
                        sheet = "matrix2 min"))
attach(m2_min)
rownames(m2_min) <- Taxa
m2_min$Taxa <- NULL

m1_p  <-  as.data.frame(t(m1_tax))
  
  
# PLOTTING - <<< BIPARTITE Package >>>

  ## Insect-Plant Network

plotweb(m1_tax,                ## << SPECIES-LEVEL >>
        method = "cca",
        col.interaction = "grey20",  # color de la interacción
        col.high = "#2E8B57",         # color de especies arriba
        col.low  = "#EEC900",         # color de especies abajo
        bor.col.interaction = NA,     # bordes
        bor.col.high = NA, 
        bor.col.low  = NA,
        y.width.high = 0.1,           # ancho de bloques
        y.width.low	 = 0.1,
        low.y = 0.675,
        high.y = 1.6,
        text.rot = 90,                # rotación de texto
        labsize  = 0.7,               # tamaño de label
    #   ybig     = 1.3,               # distancia entre upper and lower boxes
        arrow    = "no",              # forma geométrica de la conexión
        high.lablength = 0)           # num de caracteres en etiquetas

plotweb(m1_tax, 
        method = "cca",
        col.interaction = c(        # colorear Apis mellifera
          rep("grey80", 5974),
          rep("#8B3626", 206),
          rep("grey80", 4120)),
        col.high = "grey30",
        col.low  = c(
          rep("grey80", 29),
          rep("#8B3626",  1),
          rep("grey80",  20)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low = NA,
        low.y = 0.40,
        high.y = 1.6,
        text.rot = 90,
        labsize  = 0.7, 
        high.lablength = 0,
        low.lablength = 0)

plotweb(m1_tax, 
        method = "cca",
        col.interaction = c(        # colorear nativos
          rep("#00868B", 5974),
          rep("grey80", 206),
          rep("#00868B", 4120)),
        col.high = "grey30",
        col.low  = c(
          rep("#00868B", 29),
          rep("grey80",  1),
          rep("#00868B",  20)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low = NA,
        low.y = 0.40,
        high.y = 1.6,
        text.rot = 90,
        labsize  = 0.7, 
        high.lablength = 0,
        low.lablength = 0)

plotweb(m1_p, 
        method = "cca",
        col.interaction = c(        # color enfasis en plantas nativas
          rep("grey80",     200),
          rep("grey80",6150),
          rep("#27408B", 3950)),
        col.high = "grey30",
        col.low = c(
          rep("grey80", 4),
          rep("grey80", 123),
          rep("#27408B", 79)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low  = NA,
        low.y    = 0.40,
        high.y   = 1.6,
        text.rot = 90,
        labsize  = 0.7, 
        high.lablength = 0,
        low.lablength  = 0)

plotweb(m1_p, 
        method = "cca",
        col.interaction = c(        # color enfasis en plantas no-nativas
          rep("grey80",     200),
          rep("#CD6600",6150),
          rep("grey80",     3950)),
        col.high = "grey30",
        col.low = c(
          rep("grey80", 4),
          rep("#CD6600", 123),
          rep("grey80", 79)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low  = NA,
        low.y    = 0.40,
        high.y   = 1.6,
        text.rot = 90,
        labsize  = 0.7, 
        high.lablength = 0,
        low.lablength  = 0)

plotweb(sortweb(m1_tax,            ## << SORTWEB >> : ordenar por FRECUENCIA
        sort.order = "dec"),
        method = "normal",
        col.interaction = "#8B3626",
        col.high = "#2E8B57",
        col.low  = "#EEEE00",
        bor.col.interaction = NA,   
        bor.col.high = NA, 
        bor.col.low  = NA,
        text.rot     = 90,
        labsize = 0.7,
        ybig    = 1.3,
        high.lablength = NULL)

plotweb(m2_min,                   ## << TRIBUS vs GÉNEROS (FREQ > 3) >>
        method = "cca",
        col.interaction = "#8B3626",
        col.high = "#2E8B57",
        col.low  = "#EEEE00",
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low  = NA,
        text.rot     = 90,
    #  labsize = 0.7,
        ybig   = 1.3,
        high.lablength = NULL,
        arrow = "no")

plotweb(m2_min, 
        method = "cca",
        col.interaction = c(        # colorear Apis mellifera (Freq>3)
          rep("grey80", 792),
          rep("#8B3626", 72),
          rep("grey80", 648)),
        col.high = "#2E8B57",
        col.low  = c(
          rep("grey80", 11),
          rep("#8B3626",  1),
          rep("grey80",  9)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low = NA,
        text.rot = 90,
        labsize  = 0.75,
        ybig = 1.3,
        high.lablength = NULL)

plotweb(m2_tax,                  # Matriz en Orden Taxonómico
        method = "cca",
        col.interaction = c(          # color enfasis en Apis mellifera
          rep("grey80", 3243),
          rep("#8B3626", 141),
          rep("grey80", 1833)),
        col.high = "#2E8B57",
        col.low  = c(
          rep("grey80", 23),
          rep("#8B3626",  1),
          rep("grey80", 13)),
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low = NA,
        text.rot = 90,
        labsize  = 0.75,
        ybig = 1.3,
        high.lablength = NULL)

###NOTA: Para colorear las interacciones, la función rep() lee la matrix en
###      dirección horizontal. En Excel usé la función count para contar las
###      interacciones y especificar en el código las de Apis mellifera.

###      En m1 original, hay que eliminar Bellis perennis, Chenopodium murale,
###      Eragrostis sp., Erucastrum gallicum, Hippeastrum puniceum,
###      Hypochaeris aff. sessiliflora, Lamourouxia virgata,
###      Matricaria recutita, Rosa x alba, Senna multiglandulosa,
###      Thunbergia alata, Tradescantia zebrina, Veronica persica

###      En m2 original, hay que eliminar a Diapriinae* en insectos
###      porque no tiene interacciones (solo con Indet que fue excluido).
###      También eliminar a las plantas con 0 interacciones:
      #  Chenopodium, Eragrostis, Erucastrum, Hippeastrum, Matricaria y Thunbergia


  # MATRIX PLOT | << VISWEB() >>

visweb(m2_min,
       type   = "diagonal",
       square = "compartment",
       text   = "none",
       frame  = TRUE)

visweb(m2_min,
       type   = "diagonal",
       text   = "none",
       frame  = TRUE)


  ## BARPLOTTING | <<<< TOP PLANTS >>>>

top_Plants             <-   l_plant[order(l_plant$Freq,
                            decreasing = TRUE),]
row.names(top_Plants)  <-   1:220 
top_Plants$Plant       <-   as.factor(top_Plants$Plant)
top_Plants$POrigin     <-   as.character(top_Plants$POrigin)
top_Plants             <-   subset(top_Plants, Plant!="Indet")
top_Plants             <-   head(top_Plants, 20)
top_Plants$POrigin     <-   as.factor(top_Plants$POrigin)

summary(top_Plants$POrigin)

attach(top_Plants)
                            # Función par()
par(las = 2)                # Orientación de Letra: par(las =)
par(mar = c(10, 3, 1, 0.5),  # Margenes  : par(mar=c(abajo, izq, arriba, der))
oma = c(1.25,0,0,0))        ## default: par(mar=c(5, 4, 4, 2) + 0.1)

myplot         <- barplot(Freq, names.arg = Plant,
              #   density = 90,
              #   angle   =,
                  xlab    = "", ylab = "",
                  col     = c("lightgreen",
              #   "grey20",
                  "#2E8B57")[POrigin],
                  main    = "", ylim = c(0, 200),
                  border  = "black",
                  cex     = 0.75, cex.names=0.75
                  )
text(myplot, Freq + 10,labels = Freq,
                  cex = 0.7,
                  )
legend(x = "topright",  legend = c("Alien", "Native"),
                  fill    = c("lightgreen", "#2E8B57"),
                  border  = "black",
                  box.col = NA,
                  title   = "Origin",
                  inset   = 0.025,
                  cex     = 0.75)

  ## BARPLOTTING with << GGPLOT2 >>

library(ggplot2)

ggplot(top_Plants, aes(x = (names.arg = Plant),
                  y = Freq, fill = POrigin)) +
                  geom_col(
                  color = "black",
                  lwd = 0.2) +
                  ylim(c(0,200)) +
                  geom_text(aes(label = Freq), vjust = -1,
                  colour = "black") +
                  scale_fill_manual(
                  values = c("Native"="#2E8B57",
            #     "Indet" = "grey20"
                  "Alien" = "lightgreen")) +
                  scale_x_discrete(limits = Plant) +
                  labs(x = "", y = "", fill = "Origin") +
                  ggtitle("Most Common Plants") +
                  theme(panel.background = element_rect(fill = "transparent"),
                  axis.text.x = element_text(angle = 90,
                  vjust = 0.5, hjust=1), legend.position = c(0.95, 0.8))
            #     legend.position = "none)


## EXTRA

# Net de 3 niveles  (failed)

#pmatrix2_tax_Apis <- as.data.frame(read_xlsx("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/R Pollinators Quito/Datos/pmatrix2_tax_Apis.xlsx"))
#attach(pmatrix2_tax_Apis)     # matrix solo con Apis
#rownames(pmatrix2_tax_Apis) <- Group
#pmatrix2_tax_Apis$Group <- NULL

#pmatrix2_tax_noApis <- as.data.frame(read_xlsx("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/R Pollinators Quito/Datos/pmatrix2_tax_noApis.xlsx"))
#attach(pmatrix2_tax_noApis)   # matriz con todo menos Apis
#rownames(pmatrix2_tax_noApis) <- Group
#pmatrix2_tax_noApis$Group <- NULL


# COLOR MATRIX PLOT (gris indica nivel de interacción)

#class(pmatrix1)  # dataframe
#pmform1_noApis<-data.matrix(pmatrix1_noApis)
#pmform2_noApis<-data.matrix(pmatrix2_noApis)
#visweb(pmform1_noApis, type = "diagonal", square = "interaction", labsize = 0.8)
#visweb(pmform2_noApis, type = "diagonal", square = "interaction", labsize = 0.8)





