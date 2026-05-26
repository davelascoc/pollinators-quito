                    ####  LEARNING TO PLOT NETWORKS  ####


## Polinización Ejemplo                    

library(readxl)
library(bipartite) #paquete bipartite
                    
pmatrix=read.csv("Datos/Learning/net_sample_m.csv",
                 row.names="Species")

pmatrix=read.csv("Datos/Learning/net_sample.csv",
                 row.names="Species")
         ##row.names: 1era columna no se incluya en la matriz, que sea títulos.

plotweb(pmatrix,
        col.interaction = c(rep("black", 3), rep("#9AC0CD", 249)),
        col.high = c(rep("#FFD700", 10), "tomato", rep("#FFD700", 7)),
        col.low = "#458B00",
    #   col.interaction=c("#C1CDC1","#838B83","#E0EEE0","#F0FFF0"),
    #   col.high=c("#8B8B00","#CDCD00", "#FFFF00"),
    #   col.low=c("#008B00","#00CD00"),
        text.rot=90, labsize=1, ybig = 1.4, high.lablength = NULL,
        method = "cca",
        bor.col.interaction = NA,
        bor.col.high = NA, 
        bor.col.low = NA)
        ####text.rot es para rotar el texto.
        ##labsize es para definir el tamaño de letra.
        ##ybig es la distancia entre bloques de arriba y abajo.
        ##col.interaction, col.high y col.low para colorear conexiones
        ##y bloques.
        ##lab.lenght sirve para definir el número de letras.


library(igraph) # paquete igraph.

mynet=graph_from_incidence_matrix(pmatrix, weight=T)
mynet

V(mynet)$type #Display the vertex types. They are "FALSE" or "TRUE"

V(mynet)$type+1
                #If you add 1 to FALSE/TRUE vector in R,
                #they convert to 1 and 2, respectively.

V(mynet)$shape=c("square","circle")[V(mynet)$type+1]
                # asignar forma geométrica a los vertices 

V(mynet)$color=c("tomato","lightblue")[V(mynet)$type+1]
V(mynet)$frame.color="black"
V(mynet)$label.color="black"

                # asignar colores 

set.seed(5)     # fijar semilla
plot(mynet)     # graficar


#Graficar de forma BIPARTITA
     
plot(mynet, layout=layout_as_bipartite(mynet),
     edge.width=1.5,vertex.label.dist=1,
     vertex.label.degree=c(rep(-pi/2, 4), rep(pi/2, 5)))
                          # red bipartita


#Graficar de forma CIRCULAR

V(mynet)$frame.color="white"
plot(mynet,layout=layout.circle(mynet))

#Para grado de centralidad:

deg = centr_degree(mynet, mode="all")
V(mynet)$size=8.5*sqrt(deg$res)
V(mynet)$shape="circle"

plantcol = rep("#EE7621",dim(pmatrix)[2])
polcol = rep("#6B8E23",dim(pmatrix)[1])
clrs = rbind(as.matrix(polcol),as.matrix(plantcol))
V(mynet)$color = clrs

#Grosor de Líneas

E(mynet)$width = E(mynet)$weight/1.1
V(mynet)$label.cex=0.6
V(mynet)$label.dist=0

E(mynet)$color=c("#A6A6A6","#BFBFBF")

V(mynet)$frame.width=0
plot(mynet, layout=layout.circle, edge.curved=.07)


    # Redes dirigidas a la Fuerza
    
    V(mynet)$size=7.5*sqrt(deg$res)   # cambiar tamaño (mas pequeño)
    
    plot(mynet,layout=layout_with_fr(mynet)) # Algoritmo Fruchterman-Reingold
            
              # cada vez que se ejecuta el algoritmo, la configuración
              # de la red es ligeramente diferente (respecto a posición).

     # OTRAS formas de Graficar
              
              # layout_with_fr
              # layout_with_kk
              # layout_with_dh
              # layout_with_gem
              # layout_as_tree
              # layout_in_circle
              # layout_on_grid
    
    
#ONE-MODE Projection     ## Red solo con INSECTOS / RED solo con PLANTAS

mononet=bipartite.projection(mynet, multiplicity = T)
mononet
                # proyección mono-modal

class(mononet)  # es de tipo lista

plot(mononet$proj1, edge.color="#666666")
                    # gráfico de insectos

plot(mononet$proj2, edge.color="#666666")
                    #gráfico de plantas




#Colorear según módulo

# Identificar Modulos

mynet.com = fastgreedy.community(mynet)
plantmod = mynet.com$membership[1:dim(pmatrix)[1]]
pollmod = mynet.com$membership[(dim(pmatrix)[1]+1):(length(mynet.com$membership))]

# Reordenar filas y columnas por ID de módulo

onet = pmatrix[order(plantmod),] 
onet = t(t(onet)[order(pollmod),])
mynet = graph_from_incidence_matrix(onet, weight=T)

# Agregar paleta de colores (color palette)

library(wesanderson)    # Wes Anderson Palettes

com.mem = rbind(as.matrix(sort(plantmod)),as.matrix(sort(pollmod)))
colrs = wes_palette(name="Cavalcanti1",
                    n=max(mynet.com$mem),
                    type="continuous")

# Especificar parámetros

deg = centr_degree(mynet, mode="all")
V(mynet)$size=8*sqrt(deg$res)
V(mynet)$frame.color="white"
V(mynet)$color=colrs[com.mem]
V(mynet)$label.color="black"
E(mynet)$width = E(mynet)$weight/1.1
V(mynet)$label.cex=0.6
E(mynet)$color=c("#A6A6A6","#BFBFBF")
V(mynet)$frame.width=0
V(mynet)$shape=c("square","circle")[V(mynet)$type+1]


plot(mynet, layout=layout.circle)

set.seed(5)
plot(mynet)


#Graficar Matrices (bipartite)

class(pmatrix)

pmform<-data.matrix(pmatrix)

visweb(pmform, type = "diagonal", square = "interaction", labsize = 0.8)
                    # gráfico de matriz.
                    # gris indica nivel de interacción.



#ASIGNAR ATRIBUTOS   aiuddaaaaaa

dev.off()     # despejar la zona de plotting

pmatrix=as.matrix(read.csv("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Learning/net_sample.csv",
                header=T, row.names="Species"))
                  
mynet=graph_from_incidence_matrix(pmatrix, weight=T)
plot(mynet, edge.width=E(mynet)$weight)

V(mynet)$type

V(mynet)$type+1
                  
p_attrib=read.csv("C:/Users/David/OneDrive - Universidad San Francisco de Quito/Proyectos/Lab ZSFQ/Abejas & Polinizadores/Polinizadores Quito/Análisis Polinizadores - Quito/Datos/Learning/plant_atributes.csv")
p_attrib

V(mynet)$Origin=factor(p_attrib[match(V(mynet)$Species,
                p_attrib$Species), "Origin"])
mynet

V(mynet)$color=c("tomato","slateblue")[as.numeric(V(mynet)$Origin)]
V(mynet)$shape=c("square","circle")[V(mynet)$type+1]


set.seed(100)

l=layout_with_fr(mynet)

plot(mynet,vertex.label="", vertex.size=12, edge.width=1, edge.color="black")












## IGRAPH SAMPLE - PLOT & ATTRIBUTES ##

# https://dshizuka.github.io/networkanalysis/03_plots.html#vertex-attributes

library(igraph)

am=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_adjmatrix.csv", header=T, row.names=1))
am

g=graph_from_adjacency_matrix(am, mode="undirected", weighted=T)
plot(g, edge.width=E(g)$weight)

plot(g, layout=layout_in_circle(g))

l=layout_in_circle(g) 
plot(g, layout=l)

class(l)
l

# Force-directed layouts

plot(g, layout=layout_with_fr(g))

set.seed(25) 
l=layout_with_fr(g) 
plot(g, layout=l)

dev.off()     # despejar la zona de plotting

l=matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,6,7),ncol=2)
plot(g,layout=l,edge.curved=TRUE, vertex.label="")


attrib=read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_attrib.csv")
attrib


V(g)$sex=factor(attrib[match(V(g)$name, attrib$Name), "Sex"])
# factor() preserves data as M/F
V(g)$age=attrib[match(V(g)$name, attrib$Name), "Age"]
g

V(g)$color=c("gold","slateblue")[as.numeric(V(g)$sex)]

set.seed(10)
l=layout_with_fr(g)
plot(g, layout=l,vertex.label="", vertex.size=V(g)$age,
     edge.width=E(g)$weight, edge.color="black")
legend("topleft", legend=c("Female", "Male"), pch=21,
       pt.bg=c("gold", "slateblue"))




