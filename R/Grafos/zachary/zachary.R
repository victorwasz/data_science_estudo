library(ggplot2)
library(igraph)
library(ggmap)

rm(list=ls())

zachary_graph <- read.graph('zachary.txt', 'ncol', directed = FALSE)

com <- cluster_walktrap(zachary_graph)

group_1 <- c(1,2,3,4,5,6,7,8,11,12,13,14,17,18,20,22)
group_2 <- c(9,10,15,16,19,21,23,24,25,26,27,28,29,30,31,32,33,34)
groups <- c()

for (i in 1:34){
  if (i %in% group_1){groups <- append(groups,0)}
  if (i %in% group_2){groups <- append(groups,1)}
}

V(zachary_graph)$type <- groups

coordenadas <- layout_with_dh(zachary_graph)
par(mfrow = c(1,2))
plot(zachary_graph,vertex.color=V(zachary_graph)$type+1,edge.color=E(zachary_graph)$weight,edge.width=1,layout=coordenadas)
plot(com,zachary_graph,edge.color=E(zachary_graph)$weight,edge.width=1,layout=coordenadas)


distribuicao <- function(valor,tipo) {
  contagem <- c()
  for (i in 0:max(valor)){
    contagem <- append(contagem, sum(valor==i))
  }
  
  contagem_norm = contagem/sum(contagem)
  
  contagem_cumulativa <- c(1)
  for (i in 1:max(valor)){
    contagem_cumulativa <- append(contagem_cumulativa, contagem_cumulativa[i-1] - contagem_norm[i])
  }
  contagem_cumulativa <- c(1,contagem_cumulativa)
  
  if (tipo == 1){return(contagem)}
  if (tipo == 2){return(contagem_norm)}
  if (tipo == 3){return(contagem_cumulativa)}
}

par(mfrow=c(1,2))
plot(x=c(0:17),y=degree.distribution(zachary_graph,cumulative=TRUE),xlab="Grau",ylab="Frequência")
lines(x=c(0:17),y=degree.distribution(zachary_graph,cumulative=TRUE))
plot(x=c(0:48),y=distribuicao(strength(zachary_graph),3),xlab="Grau Ponderado",ylab="Frequência")
lines(x=c(0:48),y=distribuicao(strength(zachary_graph),3))

zachary_distances_no_weight <- distances(zachary_graph,weights = NA)
zachary_distances_weight <- distances(zachary_graph,algorithm = c("dijkstra"))

zachary_distribuicao_no_weight <- distribuicao(c(zachary_distances_no_weight,rep(0,34)),1)/2
zachary_distribuicao_weight <- distribuicao(c(zachary_distances_weight,rep(0,34)),1)/2

par(mfrow=c(1,2))
plot(x=c(0:5),y=zachary_distribuicao_no_weight,xlab="Distâcia",ylab="Quantidade")
lines(x=c(0:5),y=zachary_distribuicao_no_weight)
plot(x=c(0:13),y=zachary_distribuicao_weight,xlab="Distâcia Ponderada",ylab="Quantidade")
lines(x=c(0:13),y=zachary_distribuicao_weight)


transitivity(zachary_graph, type = 'globalundirected',weights = NULL)
transitivity(zachary_graph, type = 'localundirected',weights = NULL)
transitivity(zachary_graph, type = 'weighted')

comunidade <- cluster_walktrap(zachary_graph)
comunidade <- cluster_fast_greedy(zachary_graph)
comunidade <- cluster_label_prop(zachary_graph)
comunidade <- cluster_infomap(zachary_graph)
comunidade <- cluster_optimal(zachary_graph)
comunidade <- cluster_fluid_communities(zachary_graph,no.of.communities=2)
vetor <- membership(comunidade)

coordenadas = layout.auto(zachary_graph)
par(mfrow=c(1,2))
plot(zachary_graph,vertex.color=groups+1,layout=coordenadas)
plot(comunidade,zachary_graph,layout=coordenadas)

V(zachary_graph)[which.max(betweenness(zachary_graph, normalized = TRUE,weights = NA))]
V(zachary_graph)[which.max(betweenness(zachary_graph, normalized = TRUE))]

E(zachary_graph)[which.max(edge_betweenness(zachary_graph, directed = TRUE,weights = NA))]
E(zachary_graph)[which.max(edge_betweenness(zachary_graph, directed = TRUE))]

diameter(zachary_graph, weights = NA)
get_diameter(zachary_graph, weights = NA)

diameter(zachary_graph)
get_diameter(zachary_graph)











