                    ## <<<<<  HYMENOPTERA | QUITO  >>>>> ##



### DATA ###

library(readxl)
library(tidyverse)
library(GGally)
library(factoextra)
library(reshape2)
library(bipartite)
library(patchwork)



master  <-  read_excel("Datos/Datos Originales/Hymenoptera_Quito.xlsx")

master  <-  master    %>%   select(16:25)
m1      <-  master    %>%   filter(!Insect == "Indet") %>%    #limpieza
                            filter(!Plant  == "Indet")
m2      <-  master    %>%   filter(!ITribe == "Indet") %>%
                            filter(!PGenus == "Indet")

m3      <-  m2        %>%   group_by(ITribe, PGenus)   %>%
                            tally(sort = T)            %>%
                            filter(n > 3)              #al menos 4 registros

m3                   <-     dcast(m3, ITribe ~ PGenus, value.var = "n")
m3                   <-     m3 %>% column_to_rownames("ITribe")
m3[is.na(m3)]        <-     0

m1      <-  as.data.frame.matrix(table(m1$Insect, m1$Plant)) #playing wth table
m2      <-  as.data.frame.matrix(table(m2$ITribe, m2$PGenus))

spp     <-  read_excel("Datos/Datos Analizados/Species Order.xlsx")
m1      <-  m1[, names(spp)]      #ordenar especies de plantas

class(m1)
class(m2)
class(m3)

master  <-  master %>% mutate_at(1:10, factor)
summary(master)

table(master["Insect"])   #playing with "table" function
table(master["Plant"])

rm(spp)


#library(readr)
#m1t  <-  as.data.frame(t(m1))
#write.table(m1t, file = "matrix.csv")



### PLOTTING  << Insect-Plant Networks >> ###
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


#colorear Apis mellifera
plotweb(m1, method = "cca", col.interaction = c(rep("grey80", 1020),
        rep("#8B3626", 204), rep("grey80", 8976)), col.high = "grey30",
        col.low  = c(rep("grey80", 5), rep("#8B3626", 1),
        rep("grey80", 44)), bor.col.interaction = NA, bor.col.high = NA,
        bor.col.low = c(rep(NA, 5), rep("black", 1),
        rep(NA, 44)), low.y = 0.40, high.y = 1.6, text.rot = 90,
        labsize  = 0.7, high.lablength = 0, low.lablength = 0)

#colorear insectos nativos
plotweb(m1, method = "cca", col.interaction = c(rep("#0072B2", 1020),
        rep("grey80", 204), rep("#0072B2", 8976)), col.high = "grey30",
        col.low  = c(rep("#0072B2", 5), rep("grey80",  1),
        rep("#0072B2", 44)), bor.col.interaction = NA, bor.col.high = NA, 
        bor.col.low = c(rep(NA, 8), rep("black", 2), rep(NA, 15),
        rep("black", 1), rep(NA, 11), rep("black", 1), rep(NA, 11),
        rep("black", 1)), low.y = 0.40, high.y = 1.6, text.rot = 90,
        labsize  = 0.7, high.lablength = 0, low.lablength = 0)

#colorear plantas nativas
m1 %>% t() %>%
  plotweb(method = "cca", col.interaction = c(rep("grey80", 200),
        rep("grey80", 6050), rep("#009E73", 3950)), col.high = "grey30",
        col.low = c(rep("grey80", 4), rep("grey80", 123),
        rep("#009E73", 79)), bor.col.interaction = NA, bor.col.high = NA, 
        bor.col.low  = c(rep(NA, 131), rep("black", 1), rep(NA, 10),
        rep("black", 1), rep(NA, 61)), low.y    = 0.40, high.y   = 1.6, text.rot = 90,
        labsize  = 0.7, high.lablength = 0, low.lablength  = 0)

#colorear plantas introducidas
m1 %>% t() %>%
  plotweb(method = "cca", col.interaction = c(rep("grey80", 200),
        rep("#E69F00", 6050), rep("grey80", 3950)), col.high = "grey30",
        col.low = c(rep("grey80", 4), rep("#E69F00", 123),
        rep("grey80", 79)), bor.col.interaction = NA, bor.col.high = NA, 
        bor.col.low  = c(rep(NA, 99), rep("black", 1), rep(NA, 16),
        rep("black", 1), rep(NA, 1), rep("black", 1), rep(NA, 85)), low.y    = 0.40, high.y   = 1.6, text.rot = 90,
        labsize  = 0.7, high.lablength = 0, low.lablength  = 0)

#sortweb: ordenar por n()
plotweb(sortweb(m1, sort.order = "dec"), method = "normal",
        col.interaction = "#8B3626", col.high = "#2E8B57",
        col.low  = "#EEEE00", bor.col.interaction = NA,   
        bor.col.high = NA, bor.col.low  = NA, text.rot = 90, labsize = 0.7,
        ybig    = 1.3, high.lablength = NULL)

#tribus vs géneros (n > 3)
plotweb(m3, method = "cca", col.interaction = "#8B3626", col.high = "#2E8B57",
        col.low  = "#EEEE00", bor.col.interaction = NA, bor.col.high = NA, 
        bor.col.low  = NA, text.rot = 90, ybig   = 1.3, arrow = "no",
        #labsize = 0.7,
        high.lablength = NULL)

#colorear Apis mellifera (Freq>3)
plotweb(m3, method = "cca", col.interaction = c(rep("grey80", 108),
        rep("#8B3626", 54), rep("grey80", 648)), col.high = "#2E8B57",
        col.low  = c(rep("grey80", 2), rep("#8B3626",  1), rep("grey80",  12)),
        bor.col.interaction = NA, bor.col.high = NA, bor.col.low = NA,
        text.rot = 90, labsize  = 0.75, ybig = 1.3, high.lablength = NULL)

#colorear Apis mellifera
plotweb(m2, method = "cca", col.interaction = c(rep("grey80", 564),
        rep("#8B3626", 141), rep("grey80", 4512)), col.high = "#2E8B57",
        col.low  = c(rep("grey80", 4), rep("#8B3626",  1), rep("grey80", 32)),
        bor.col.interaction = NA, bor.col.high = NA, bor.col.low = NA,
        text.rot = 90, labsize  = 0.75, ybig = 1.3, high.lablength = NULL)
#-----------------------------------------------------------------------------
rm(m2, m3)


## PLOTING - GRAPHICAL ABSTRACT
#----------------------------------------------------------------------------

#bipartite graph
m1.abs   <- m1      %>% select("Taraxacum officinale", "Trifolium repens",
                        "Baccharis latifolia", "Ruta graveolens",
                        "Dalea coerulea", "Citrus medica",
                        "Hypochaeris radicata", "Aptenia cordifolia",
                        "Salvia microphylla", "Leonotis nepetifolia",
                        "Bidens pilosa", "Argyranthemum frutescens",
                        "Raphanus raphanistrum")
m1.abs   <- m1.abs  %>% filter(row.names(m1.abs) %in% c("Apis mellifera",
                        "Xylocopa cf. viridigastra",
                        "Bombus robustus group", "Thygater aethiops",
                        "Bombus funebris", "Megachile ecuadoria",
                        "Lasioglossum sp.", "Pygodasis ephippium",
                        "Anthophora pilifrons", "Agapostemon cf. nasutus",
                        "Pepsis cf. grossa"))

plotweb(m1.abs, method = "cca", col.interaction = c(rep("grey70", 26),
        rep("tomato2", 13), rep("grey80", 104)), col.high = "#2E8B57",
        col.low  = c(rep("orange", 2), rep("tomato4", 1),
        rep("orange", 8)), bor.col.interaction = NA, bor.col.high = NA,
        bor.col.low = NA, low.y = 0.40, high.y = 1.6, text.rot = 90,
        labsize  = 0.7, high.lablength = 0, low.lablength = 0)


#circular graph (with igraph)

library(igraph)

mynet  <-  graph_from_incidence_matrix(m1.abs, weight=T)
mynet

V(mynet)$type
V(mynet)$type+1

deg = centr_degree(mynet, mode="all")

#V(mynet)$shape = c("circle", "square")[V(mynet)$type+1]
V(mynet)$shape = "circle"
V(mynet)$size  = 8.5 * sqrt(deg$res)

E(mynet)$width = E(mynet)$weight/6
V(mynet)$label.cex  = 0.6
V(mynet)$label.dist = 0
V(mynet)$frame.width = 0

V(mynet)$color = c("orange","seagreen")[V(mynet)$type+1]

V(mynet)$color = c(rep("grey80", 2), rep("tomato4", 1),
                   rep("grey80", 21))

V(mynet)$color = "white"
V(mynet)$label.color = "black"

E(mynet)$color = c(rep("grey70", 8),
                   rep("tomato4", 13), rep("grey70", 43))

V(mynet)$label = NA

plot(mynet)     # graficar
plot(mynet, layout=layout.circle, edge.curved=.07)



rm(plantcol, polcol, deg, clrs, mynet)
rm(m1.abs)

#-----------------------------------------------------------------------------


### NETWORK STATS ###

net.stats      <-  specieslevel(m1)             #importance of species

plants    <-  net.stats[["higher level"]]
insects   <-  net.stats[["lower level"]]

summary(plants)

plants    <-   plants  %>% 
                    select("degree", "species.strength", "weighted.betweenness",
                           "weighted.closeness",
                           "species.specificity.index")            %>%
                    rename(k  = degree, s = species.strength,
                           bc = weighted.betweenness,
                           cc = weighted.closeness,
                           d  = species.specificity.index)
summary(plants)

insects    <-   insects  %>% 
                    select("degree", "species.strength", "weighted.betweenness",
                           "weighted.closeness",
                           "species.specificity.index")            %>%
                    rename(k  = degree, s = species.strength,
                           bc = weighted.betweenness,
                           cc = weighted.closeness,
                           d  = species.specificity.index)
summary(insects)


plant.list     <-  read_excel("Datos/Datos Analizados/List of Plants.xlsx",
                              sheet = "Species (no cambiar)")

plant.list     <-  plant.list   %>%
                   filter(!Plant %in% c("Indet", "Bellis perennis",
                                        "Chenopodium murale",
                                        "Erucastrum gallicum",
                                        "Matricaria recutita","Rosa x alba",
                                        "Thunbergia alata",
                                        "Tradescantia zebrina",
                                        "Veronica persica", "Eragrostis sp.",
                                        "Hippeastrum puniceum",
                                        "Hypochaeris aff. sessiliflora",
                                        "Lamourouxia virgata",
                                        "Senna multiglandulosa"))

plants         <-  cbind(plants, plant.list[2:6])

insect.list    <- read_excel("Datos/Datos Analizados/List of Insects.xlsx", 
                             sheet = "Species (no mover)")

insect.list    <- insect.list %>% filter(!Species == "Indet")

insects        <- cbind(insects, insect.list[2:4])

ls()

#pairs plots
ggpairs(plants,  lower = list(continuous = "smooth"), columns = 1:5) +
                  ggtitle("Plant Species Network Stats") + theme_test()
ggpairs(plants,  aes(col = Origin), lower = list(continuous = "smooth"),
                  upper = list(continuous = wrap("cor", size = 3)),
                  columns = 1:5) +
                  ggtitle("Plant Species Network Stats") + theme_test()

ggpairs(insects,  lower = list(continuous = "smooth"), columns = 1:5) +
                  ggtitle("Insect Species Network Stats") + theme_test()


## Principal Component Analysis (PCA)
#-------------------------------------------------------------------------------
str(plants)
summary(plants)

str(insects)
summary(insects)

pca.plant   <-  prcomp(plants[1:5], scale. = T)
pca.plant

summary(pca.plant)

pca.insect  <-  prcomp(insects[1:5], scale. = T)
pca.insect

summary(pca.insect)

plants  <-  cbind(plants,  pca.plant[["x"]])

insects <-  cbind(insects, pca.insect[["x"]])

#plants   <-  plants %>% mutate(Spp = row.names(plants))
#library(writexl)
#write_xlsx(plants, "Datos/Datos Analizados/plants_nettats.xlsx")

ggplot(plants, aes(x = Origin, y = k)) +  geom_boxplot() +
ggplot(plants, aes(x = Origin, y = s)) +  geom_boxplot() +
ggplot(plants, aes(x = Origin, y = bc)) + geom_boxplot() +
ggplot(plants, aes(x = Origin, y = cc)) + geom_boxplot() +
ggplot(plants, aes(x = Origin, y = d )) + geom_boxplot()


#scree plots
fviz_eig(pca.plant, addlabels = T, ylim = c (0, 80)) +
            ggtitle("Scree Plot - Plants") + theme_test() +
fviz_eig(pca.plant, addlabels = T, ylim = c(0, 4), choice = "eigenvalue") +
            geom_hline(yintercept = 1, linetype = "dashed") + ggtitle("") +
            theme_test()

fviz_eig(pca.insect, addlabels = T, ylim = c (0, 80)) +
            ggtitle("Scree Plot - Insects") + theme_test() +
fviz_eig(pca.insect, addlabels = T, ylim = c(0, 4), choice = "eigenvalue") +
            geom_hline(yintercept = 1, linetype = "dashed") +
            ggtitle("") + theme_test()

#variables
fviz_pca_var(pca.plant,   repel = T, col.var = "royalblue") +
            ggtitle("Variables - PCA Plants") + theme_test() +
fviz_pca_var(pca.insect, repel = T, col.var = "royalblue") +
            ggtitle("Variables - PCA Insects") + theme_test()

#individuals
fviz_pca_ind(pca.plant, label = "var", habillage = plants$Habit,
            addEllipses = F, ellipse.level = 0.95) +
            ggtitle("Individuals - PCA Plants") + theme_test()

#biplots
fviz_pca_biplot(pca.plant, labelsize = 3, col.var = "royalblue", label = F) + 
            ggtitle("PCA - Biplot Plants") + theme_test() +
            geom_text(label = plants$Acr, hjust=1.25, vjust=0.1, size = 3)
fviz_pca_biplot(pca.plant, label = "var", col.var = "royalblue") +
            ggtitle("PCA - Biplot Plants") + theme_test()

#plant pca
pcap1 <- fviz_pca_biplot(pca.plant, label = "var", col.var = "royalblue",
            habillage = plants$Origin) + scale_color_brewer(palette = "Dark2") +
            geom_text(label = plants$Acr, hjust=1.25, vjust=0.1, size = 3) +
            ggtitle("") + theme_test() + theme(legend.position = "none")

#insect pca
pcai1 <- fviz_pca_biplot(pca.insect, labelsize = 3,label = "var",
            col.var = "royalblue") +
            geom_text(label = insects$Acr, hjust=-.25, vjust=0.1, size = 3) +
            ggtitle("") + theme_test()

pcap1 + pcai1 + plot_annotation(tag_levels = "A")

#k = degree;              s  = species.strength;     bc = weighted.betweenness;
#cc = weighted.closeness; d  = species.specificity

#data mgmt for error bars
habits   <-   plants %>% group_by(Habit)  %>%
              summarise(Mean = mean(PC1), SD = sd(PC1)) %>%
              mutate(Habit  = factor(Habit,  levels = c("Tree", "Vine",
                                                        "Herb", "Shrub",
                                                        "Parasite")))
origins  <-   plants %>% group_by(Origin) %>%
              summarise(Mean = mean(PC1), SD = sd(PC1)) %>%
              mutate(Origin = factor(Origin, levels = c("Alien", "Native")))

#error bars
habits %>% filter(!Habit == "Parasite") %>%
  ggplot(aes(Habit, Mean, ymin = Mean - SD, ymax = Mean + SD)) +
                geom_pointrange() + ylab("PC1 (63.2%)") +
                ggtitle("Plant Importance") +
                geom_errorbar(width = .2) + theme_test() +
ggplot(origins, aes(Origin, Mean, ymin = Mean - SD, ymax = Mean + SD)) +
                geom_pointrange() + ylab("") +
                geom_errorbar(width = .2) + theme_test()

rm(habits, origins, plants, insects, net.stats, pca.insect, pca.plant)
#-------------------------------------------------------------------------------

## Principal Component Analysis (PCA) - Sin Apis mellifera
#-------------------------------------------------------------------------------
m1.native  <-  m1 %>% filter(!row.names(m1) == "Apis mellifera")

net.stats.native  <-  specieslevel(m1.native)

plants.native     <-  net.stats.native[["higher level"]]
insects.native    <-  net.stats.native[["lower level"]]


plants.native    <-    plants.native  %>% 
                select("degree", "species.strength", "weighted.betweenness",
                       "weighted.closeness",
                       "species.specificity.index")            %>%
                rename(k  = degree, s = species.strength,
                       bc = weighted.betweenness,
                       cc = weighted.closeness,
                       d  = species.specificity.index)

insects.native    <-   insects.native  %>% 
                select("degree", "species.strength", "weighted.betweenness",
                       "weighted.closeness",
                       "species.specificity.index")            %>%
                rename(k  = degree, s = species.strength,
                       bc = weighted.betweenness,
                       cc = weighted.closeness,
                       d  = species.specificity.index)

plant.list.native <- read_excel("Datos/Datos Analizados/List of Plants.xlsx", 
                                sheet = "Species (no cambiar) (noApis)")

plants.native     <-  cbind(plants.native, plant.list.native[2:6])

insect.list.native <- insect.list %>% filter(!Species == "Apis mellifera")

insects.native     <- cbind(insects.native, insect.list.native[2:4])

#pairs plots
ggpairs(plants.native,  lower = list(continuous = "smooth"), columns = 1:5) +
  ggtitle("Plant Species Network Stats") + theme_test()

#pca
pca.plant.native  <-  prcomp(plants.native[1:5], scale. = T)
pca.plant.native

summary(pca.plant.native)

pca.insect.native  <-  prcomp(insects.native[1:5], scale. = T)
pca.insect.native

plants.native     <-  cbind(plants.native,  pca.plant.native[["x"]])


#scree plots
fviz_eig(pca.plant.native, addlabels = T, ylim = c (0, 70)) +
  ggtitle("Scree Plot - Plants") + theme_test() +
fviz_eig(pca.plant.native, addlabels = T, ylim = c(0, 3.5),
         choice = "eigenvalue") +
  geom_hline(yintercept = 1, linetype = "dashed") + ggtitle("") +
  theme_test()

#variables
fviz_pca_var(pca.plant.native,   repel = T, col.var = "royalblue") +
  ggtitle("Variables - PCA Plants") + theme_test()

#biplots
fviz_pca_biplot(pca.plant.native, labelsize = 3, col.var = "royalblue",
                label = "var") + ggtitle("PCA - Biplot Plants (noApis)") +
                theme_test()
fviz_pca_biplot(pca.plant.native, labelsize = 3, col.var = "royalblue",
                label = "var",) + theme_test()

pcap2 <- fviz_pca_biplot(pca.plant.native, label = "var", col.var = "royalblue",
          habillage = plant.list.native$Origin) +
          scale_color_brewer(palette = "Dark2") +
          geom_text(label = plants.native$Acr, hjust=1.25, vjust=.1, size = 3) +
          ggtitle("") + theme_test() + theme(legend.position = "none")

pcai2 <- fviz_pca_biplot(pca.insect.native, labelsize = 3,label = "var",
          col.var = "royalblue") + geom_text(label = insects.native$Acr,
                                             hjust=-.25, vjust=0.1, size = 3) +
          ggtitle("") + theme_test()

#Gráfico PCAs FINAL Ecol. Ent.
pcap1 + pcai1 + pcap2 + pcai2 + plot_annotation(tag_levels = "A")

#ggsave("pcas.png", width = 8, height = 7, dpi = 800)

rm(m1.native, net.stats, pca.plant, plant.list, insects, plants)  
#-------------------------------------------------------------------------------

## Other STATS
#-------------------------------------------------------------------------------
#Complementary specialization (H2’)
H2fun(m1, H2_integer = T)

#Network level stats
networklevel(m1)

#Modularity
modules  <-  computeModules(m1, method = "Beckett")

#Normalised degree, betweenness and closeness centrality
ND(m1, normalised = T)
#-------------------------------------------------------------------------------







