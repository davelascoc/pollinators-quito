                   # Intro to BipartiteD3 and others #


# IMPORT DATA

library(bipartite)
library(readxl)
library(tidyverse)


pmatrix1_tax <- as.data.frame(read_xlsx("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Analizados/Matrix Database.xlsx"))
rownames(pmatrix1_tax) <- pmatrix1_tax$Species
pmatrix1_tax$Species <- NULL
class(pmatrix1_tax)

    # pmatrix1_tax <- as.matrix(pmatrix1_tax)
    # class(pmatrix1_tax)


# BIPARTITE D3


mydata=select(read_xlsx("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Datos Analizados/Master DataBase.xlsx"),
              IFamily, ITribe, IGenus, ISpecies, Insect,
              PFamily, PGenus, PSpecies, Plant)

attach(mydata)
plist1=filter(as.data.frame(table(Insect, Plant)), Freq>0)

plist1$lower <- plist1$Insect
plist1$Insect <- NULL
plist1$higher <- plist1$Plant
plist1$Plant <- NULL
plist1$webID <- c(rep("bug", 556))

frame2webs(plist1, varnames = c("Insect", "Plant", "webID", "Freq")) -> myweb1

bipartite_D3(myweb1)

plotweb(myweb1)
class(myweb1)








# SAFARILAND SAMPLE

data(Safariland)
class(Safariland)
plotweb(Safariland)


plotweb(Safariland, method = "normal", sequence = "seq.high")


# shorter labels
plotweb(Safariland, high.lablength=3, low.lablength=0, arrow="down")

# centered triangles for displaying interacions
plotweb(Safariland, text.rot=90, arrow="down.center", col.interaction="wheat2",
        y.lim=c(-1,2.5))

#orginal sequence, up arrows and different box width
plotweb(Safariland, method="normal", arrow="up", y.width.low=0.3, low.lablength=4)
# interactions as lines
plotweb(Safariland, arrow="both", y.width.low=0.05, text.rot=90, col.high="blue", 
        col.low="green")

# add an abundance vector for lower trophic species 
low.abun = round(runif(dim(Safariland)[1],1,40)) #create
names(low.abun) <- rownames(Safariland)
plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction="purple", 
        y.width.low=0.05, y.width.high=0.05)

plotweb(Safariland, text.rot=90, low.abun=low.abun, col.interaction ="red", 
        bor.col.interaction="red", arrow="down")

# now vectors for all colours can be given, to mark certain species or 
# interactions. Colour vectors are recycled if not of appropriate length
plotweb(Safariland,col.high=c("orange","green"))
plotweb(Safariland,col.low=c("orange","green"),col.high=c("white","grey","purple"),
        text.high.col=c("blue","red"), col.interaction=c("red",rep("green",26),rep("brown",242)),
        bor.col.interaction=c(rep("green",26),rep("brown",242)),method="normal", 
        text.rot=90, low.lablength=5, high.lablength=5)


#example one (tritrophic)
plotweb(Safariland,y.width.low=0.1, y.width.high=0.05,method="normal", 
        y.lim=c(0,3), arrow="up", adj.high=c(0.5,1.5), col.high="orange",
        high.lablength=3,high.lab.dis=0)

plotweb(t(Safariland), y.width.low=0.05, y.width.high=0.1, method="normal",
        add=TRUE,low.y=1.5,high.y=2.5, col.low="green", text.low.col="red", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1),low.plot=FALSE)

#example two (4 trophic with abundance)
low.abun = round(runif(dim(Safariland)[1],1,40)) #create
names(low.abun) <- rownames(Safariland)
plotweb(Safariland, text.rot=90, high.abun=low.abun, col.interaction="purple", 
        y.lim=c(0,4.5), high.lablength=0, arrow="up", method="normal", 
        y.width.high=0.05)

plotweb(t(Safariland), y.width.low=0.05, y.width.high=0.1, method="normal", 
        add=TRUE, low.y=1.7,high.y=2.7, col.low="green", text.low.col="black", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1), low.lablength=4, 
        high.lablength=0)

plotweb(Safariland,y.width.low=0.05, y.width.high=0.1, method="normal", 
        add=TRUE, low.y=2.95, high.y=3.95, col.low="green", text.low.col="black", 
        low.lab.dis=0, arrow="down", adj.low=c(0.5,1.1), low.lablength=4)

# now some examples with the abuns.type-option:
plotweb(Safariland, abuns.type='independent',arrow="down.center")
plotweb(Safariland, abuns.type='additional',arrow="down.center")




data(Safariland)
visweb(Safariland)

visweb(Safariland, type = "diagonal")
visweb(Safariland, type = "nested")
visweb(Safariland, type = "none")

visweb(Safariland, type="diagonal", square="compartment", text="none", 
       frame=TRUE)
visweb(Safariland, type="nested", text="compartment")
visweb(Safariland, circles=TRUE,  boxes=FALSE,  labsize=1, circle.max=3, 
       text="no")
visweb(Safariland,square="b",box.col="green",box.border="red")

#define your colors here,length has to be the numbers of different entries
cols <-0:(length(table(Safariland))-1) 
visweb(Safariland, square="defined", def.col=cols) 



testdata <- data.frame(higher = c("bee1","bee1","bee1","bee2","bee1","bee3"), 
                       lower = c("plant1","plant2","plant1","plant2","plant3","plant4"), 
                       webID = c("meadow","meadow","meadow","meadow","meadow","meadow"), freq=c(5,9,1,2,3,7))
bipartite::frame2webs(testdata)-> SmallTestWeb

SmallTestWeb

library(bipartiteD3)
bipartite_D3(SmallTestWeb)


