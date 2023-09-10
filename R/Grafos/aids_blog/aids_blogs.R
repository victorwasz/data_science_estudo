library(ggplot2)
library(igraph)
library(ggmap)

rm(list=ls())
setwd("C:/Users/Victor/Desktop/UTFPR/Metodos_analiticos_para_redes_sociais")

blog_graph <- read_graph('aids_blog/AIDSBlog.txt', 'ncol', directed=TRUE)

coordenadas <- layout_with_fr(blog_graph)
plot(blog_graph,vertex.size=6, layout=layout_with_fr,edge.arrow.size = 0.3 )

plot(x=c(0:42),y=degree.distribution(blog_graph,cumulative=TRUE,mode="out"),xlab="Grau",ylab="Frequência")
lines(x=c(0:42),y=degree.distribution(blog_graph,cumulative=TRUE,mode="out"))

blog_distances_no_weight <- distances(blog_graph,weights = NA)
blog_distribuicao_no_weight <- distance_table(blog_graph,directed = TRUE)

plot(x=c(1:6),y=blog_distribuicao_no_weight$res,xlab="Distância",ylab="Quantidade")
lines(x=c(1:6),y=blog_distribuicao_no_weight$res)

transitivity(blog_graph, type = 'global',weights = NULL)

comunidade <- cluster_walktrap(blog_graph)
plot(comunidade,blog_graph,vertex.size=6, layout=coordenadas,edge.arrow.size = 0.3)

V(blog_graph)[which.max(betweenness(blog_graph, normalized = TRUE,weights = NA))]

E(blog_graph)[which.max(edge_betweenness(blog_graph, directed = TRUE,weights = NA))]

diameter(blog_graph, weights = NA)
get_diameter(blog_graph, weights = NA)

