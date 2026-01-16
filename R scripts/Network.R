#network
library(ieggr)
library(vegan)
library(ggplot2)
library(MENA)
library(igraph)
library(dplyr)
library(Hmisc)
library(multtest)
library(psych)
library(WGCNA)
library(writexl)
library(openxlsx)
library(maxnodf)
setwd("./graphml")

#.graphml based on MENA outputs
files = list.files( pattern = ".correlation.rb.graphml", full.names = F) 

for (loop in 1:length(files)){
  g1=read.graph(files[loop], format = "graphml")
  
  ## calculation
  set.seed(123)
  V(g1)$modularity <- membership(cluster_fast_greedy(g1))
  
  cols <- c(paletteer::paletteer_d("rcartocolor::Vivid")) 
  col_g <- "#C1C1C1"
  
  #add color
  V(g1)$label <- V(g1)$name
  V(g1)$label <- NA
  modu_sort <- V(g1)$modularity %>% table() %>% sort(decreasing = T)
  modu_name <- names(modu_sort[1:8])
  modu_cols <- cols[1:length(modu_name)]
  names(modu_cols) <- modu_name
  V(g1)$color <- V(g1)$modularity
  V(g1)$color[!(V(g1)$color %in% modu_name)] <- col_g
  V(g1)$color[(V(g1)$color %in% modu_name)] <- modu_cols[match(V(g1)$color[(V(g1)$color %in% modu_name)],modu_name)]
  V(g1)$frame.color <- V(g1)$color
  
  #edge color
  E(g1)$color <- gsub("blue", "#CCEEF9FF", E(g1)$color)
  E(g1)$color <- gsub("red", "#F3BFCBFF", E(g1)$color)
  
  E(g1)$color <- col_g
  for ( i in modu_name){
    col_edge <- cols[which(modu_name==i)]
    otu_same_modu <-V(g1)$name[which(V(g1)$modularity==i)]
    E(g1)$color[(data.frame(as_edgelist(g1))$X1 %in% otu_same_modu)&(data.frame(as_edgelist(g1))$X2 %in% otu_same_modu)] <- col_edge
  }
  
  #layout
  set.seed(123)
  sub_net_layout <- layout_with_fr(g1, dim = 2,start.temp = sqrt(vcount(g1)),niter=999,grid = 'nogrid')
  #sub_net_layout <- layout_with_graphopt(g1, niter=999)
  #sub_net_layout <- layout_with_kk(g1,coords = NULL,dim = 2,maxiter = 50 * vcount(g1),epsilon = 0,kkconst = vcount(g1))
  
  ## 可视化并输出
  par(font.main=4)
  
  plot(g1,vertex.frame.color=NA,vertex.label=NA,edge.width=2,
       vertex.size=5,edge.lty=1,edge.curved=F,margin=c(0,0,0,0),layout=sub_net_layout)
  
  name=gsub("\\.correlation.rb.graphml","",files[loop])
  title(main = paste0(name),', ',paste0('Nodes=',length(V(g1)$name),', ','Edges=',nrow(data.frame(as_edgelist(g1)))))
  
  #output
  #write_graph(g1, paste0(name,".coloredout.graphml"), format="graphml")
}

