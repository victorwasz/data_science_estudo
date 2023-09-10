library(ggplot2)
library(igraph)
library(ggmap)

rm(list=ls())
setwd("C:/Users/Victor/Desktop/UTFPR/Metodos_analiticos_para_redes_sociais")

e_coli <- read_graph('e_coli/e_coli.net', 'ncol',directed=TRUE)
e_coli_nodes <- read.table('e_coli/e_coli_nodes.txt')

f <- function(x) e_coli_nodes$V2[match(x, e_coli_nodes$V1)]
res <- sapply(V(e_coli), f)
V(e_coli)$name <- res

rm(e_coli_nodes,res,f)

activator_plus_dual <- delete.edges(e_coli, E(e_coli)[E(e_coli)$weight %in% c(2)])
repressor_plus_dual <- delete.edges(e_coli, E(e_coli)[E(e_coli)$weight %in% c(1)])

Pesos_originais_activator_plus_dual <- E(activator_plus_dual)$weight
Pesos_originais_repressor_plus_dual <- E(repressor_plus_dual)$weight
Pesos_originais_ecoli <- E(e_coli)$weight

E(activator_plus_dual)$weight <- 1 
E(repressor_plus_dual)$weight <- 1 
E(e_coli)$weight <- 1 

#-------------------------------------------------------------------------------------------------------

coordenadas <- layout_with_fr(e_coli)

par(mfrow=c(1,3))

plot(activator_plus_dual,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_activator_plus_dual+1,edge.arrow.size = 0.1, main = "Activator + Dual")
plot(repressor_plus_dual,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_repressor_plus_dual+1,edge.arrow.size = 0.1, main = "Repressor + Dual")
plot(e_coli,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_ecoli+1,edge.arrow.size = 0.1,main="Activator + Repressor + Dual")

par(mfrow=c(1,1))

#-------------------------------------------------------------------------------------------------------

degree_activator_plus_dual <- degree_distribution(activator_plus_dual,cumulative = TRUE,mode="out")
degree_repressor_plus_dual <- degree_distribution(repressor_plus_dual,cumulative = TRUE,mode="out")

plot(x=c(0:65), y= degree_activator_plus_dual,xlab="Grau",ylab="Frequência",main="Activator + Dual")
lines(x=c(0:65), y= degree_activator_plus_dual)

plot(x=c(0:18), y= degree_repressor_plus_dual,xlab="Grau",ylab="Frequência",main="Repressor + Dual")
lines(x=c(0:18), y= degree_repressor_plus_dual)

plot(x=c(0:72), y= degree_distribution(e_coli,cumulative = TRUE,mode="out"),xlab="Grau",ylab="Frequência",main="Activator + Repressor + Dual")
lines(x=c(0:72), y= degree_distribution(e_coli,cumulative = TRUE,mode="out"))

dist_activator_plus_dual <- distance_table(activator_plus_dual, directed = TRUE)
dist_repressor_plus_dual <- distance_table(repressor_plus_dual, directed = TRUE)
dist_all <- distance_table(e_coli, directed = TRUE)

#-------------------------------------------------------------------------------------------------------

plot(x=c(1:3), y= dist_activator_plus_dual$res,xlab="Caminhos",ylab="Frequência",main="Activator + Dual")
lines(x=c(1:3), y= dist_activator_plus_dual$res)

plot(x=c(1:2), y= dist_repressor_plus_dual$res,xlab="Caminhos",ylab="Frequência",main="Repressor + Dual")
lines(x=c(1:2), y= dist_repressor_plus_dual$res)

plot(x=c(1:4), y= dist_all$res,xlab="Caminhos",ylab="Frequência",main="Activator + Repressor + Dual")
lines(x=c(1:4), y= dist_all$res)

#-------------------------------------------------------------------------------------------------------

transitivity(activator_plus_dual, type = 'global',weights = NULL)
transitivity(repressor_plus_dual, type = 'global',weights = NULL)
transitivity(e_coli, type = 'global',weights = NULL)

#-------------------------------------------------------------------------------------------------------

comunidade_01 <- cluster_walktrap(activator_plus_dual)
comunidade_02 <- cluster_walktrap(repressor_plus_dual)
comunidade_03 <- cluster_walktrap(e_coli)

par(mfrow=c(1,3))

plot(comunidade_01,activator_plus_dual,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_activator_plus_dual+1,edge.arrow.size = 0.1, main = "Activator + Dual")
plot(comunidade_02,repressor_plus_dual,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_repressor_plus_dual+1,edge.arrow.size = 0.1, main = "Repressor + Dual")
plot(comunidade_03,e_coli,vertex.size=2,vertex.label=NA,layout=coordenadas,
     edge.color=Pesos_originais_ecoli+1,edge.arrow.size = 0.1,main="Activator + Repressor + Dual")

par(mfrow=c(1,1))

#-------------------------------------------------------------------------------------------------------

a <- betweenness(activator_plus_dual,directed = TRUE)
names(which.max(a))

b <- betweenness(repressor_plus_dual,directed = TRUE)
names(which.max(b))

c <- betweenness(e_coli,directed = TRUE)
names(which.max(c))

rm(a,b,c)

#-------------------------------------------------------------------------------------------------------

a <- edge_betweenness(activator_plus_dual,directed = TRUE)
E(activator_plus_dual)[which.max(a)]

b <- edge_betweenness(repressor_plus_dual,directed = TRUE)
E(repressor_plus_dual)[which.max(b)]

c <- edge_betweenness(e_coli,directed = TRUE)
E(e_coli)[which.max(c)]

rm(a,b,c)

#-------------------------------------------------------------------------------------------------------

diameter(activator_plus_dual)
diameter(repressor_plus_dual)
diameter(e_coli)

#-------------------------------------------------------------------------------------------------------




















































































