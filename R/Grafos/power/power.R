library(ggplot2)
library(igraph)
library(ggmap)

rm(list=ls())

power <- read.graph('power.gml', format = "gml")

coordenadas <- layout_with_drl(power)
plot(power,vertex.label = NA,vertex.size=1.5,layout=coordenadas)

plot(x=c(0:19),y=degree.distribution(power,cumulative=TRUE),xlab="Grau",ylab="Frequência")
lines(x=c(0:19),y=degree.distribution(power,cumulative=TRUE))

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

power_distances_no_weight <- distances(power,weights = NA)
power_distribuicao_no_weight <- distribuicao(c(power_distances_no_weight,rep(0,4941)),1)/2

a <- distance_table(power,directed=FALSE)

plot(x=c(0:46),y=power_distribuicao_no_weight,xlab="Distância",ylab="Quantidade")
lines(x=c(0:46),y=power_distribuicao_no_weight)

transitivity(power, type = 'globalundirected',weights = NULL)

comunidade <- cluster_walktrap(power)
comunidade <- cluster_fast_greedy(power)
comunidade <- cluster_label_prop(power)
comunidade <- cluster_infomap(power)
comunidade <- cluster_optimal(power)
comunidade <- cluster_fluid_communities(power,no.of.communities=2)
vetor <- membership(comunidade)

plot(comunidade,power,vertex.label = NA,vertex.size=1.5,layout=coordenadas)

V(power)[which.max(betweenness(power, normalized = TRUE,weights = NA))]

E(power)[which.max(edge_betweenness(power, directed = TRUE,weights = NA))]

diameter(power, weights = NA)
get_diameter(power, weights = NA)

















