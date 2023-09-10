library(ggplot2)
library(igraph)
library(ggmap)

rm(list=ls())
setwd("C:/Users/Victor/Desktop/UTFPR/Metodos_analiticos_para_redes_sociais")

router_inet <- read.graph('router_inet/router_INET.txt', 'edgelist', directed = FALSE)

#coordenadas <- layout_with_drl(router_inet)
#save(coordenadas, file = "prova/roteadores/data.Rdata")

load(file = "prova/roteadores/data.Rdata")

plot(router_inet,vertex.label = NA,vertex.size=1.5,layout=coordenadas)

plot(x=c(0:1071),y=degree.distribution(router_inet,cumulative=TRUE),xlab="Grau",ylab="Frequência")
lines(x=c(0:1071),y=degree.distribution(router_inet,cumulative=TRUE))


router_distances_histogram <- distance_table(router_inet,directed=FALSE)
save(router_distances_histogram, file = "prova/roteadores/hist_distancia.Rdata")

no_mais_importante <- V(router_inet)[which.max(betweenness(router_inet, normalized = TRUE,weights = NA, directed = FALSE))]
777
save(no_mais_importante, file = "prova/roteadores/no_mais_importante.Rdata")
load(file = "prova/roteadores/no_mais_importante.Rdata")
V(router_inet)[717]

aresta_mais_importante <- E(router_inet)[which.max(edge_betweenness(router_inet, directed = FALSE,weights = NA))]
355172
40413--124247
save(aresta_mais_importante, file = "prova/roteadores/aresta_mais_importante.Rdata")

comunidade <- cluster_walktrap(router_inet)
plot(comunidade,router_inet,vertex.label = NA,vertex.size=1.5,layout=coordenadas)


transitivity(router_inet, type='undirected')


degree(router_inet,v=1737)
a <- degree(router_inet)
a[a==1071]

are.connected(router_inet, 777, 1737)

length(V(router_inet))










