                          ### NMDS | Hymenoptera ###


library(readxl)
library(dplyr)

comunidad <- as.data.frame(read_excel("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/R Pollinators Quito/Datos/Datos Analizados/Matrix1 Tax..xlsx"))

attach(comunidad)
rownames(comunidad) <- Species
comunidad$Species <- NULL
comunidad <- as.data.frame(t(comunidad))

origin <- c(rep.int("Native", 4), rep.int("Alien", 123), rep.int("Native", 79))


library(vegan)
set.seed(1)
NMDSdata           <- metaMDS(comunidad, distance = "bray", k = 2)  # Bray-Curtis
mypoints           <- as.data.frame(scores(NMDSdata, display = "sites"))
mypoints$Origin    <- as.factor(origin)

library(ggplot2)
NMDSplot           <- ggplot(mypoints, aes(x = NMDS1, y = NMDS2, color = Origin)) +
                      geom_point(aes(shape = Origin), size = 2.5, alpha = 0.7) +
                      stat_ellipse() + stat_ellipse(type = "norm", linetype = 2)

NMDSplot + theme_test() + ylim(c(-7.5, -4)) + xlim(c(-163, -160))

## PERMANOVA

PERMANOVA     <- adonis2(comunidad ~ Origin, data = mypoints,
                         permutations = 999, method = "bray")
PERMANOVA 


## Check assumption of homogeneity of multivariate dispersion

distancesdata <- vegdist(comunidad)
homog_data     <- anova(betadisper(distancesdata, mypoints$Origin))
homog_data
