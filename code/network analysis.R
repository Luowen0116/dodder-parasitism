net<-function(comun,tax1){
  b<-rcorr(as.matrix(comun),type="spearman");x<-b$r;y<-b$P
  x[abs(x)<0.6]<-0
  y<-p.adjust(y,method="BH");y[y>0.01]<-0  
  y[y<=0.01&y>0]<-1;z<-x*y;diag(z)<-0;g<-graph.adjacency(z,weighted=TRUE,mode="undirected");g1<-simplify(g)
  g1<-delete.vertices(g1,names(degree(g1)[degree(g1)==0]))
  p<-as.character(tax1[,2]);c<-as.character(tax1[,3]);o<-as.character(tax1[,4]);f<-as.character(tax1[,5]);G<-as.character(tax1[,6])
  V(g1)$phylum<-p;V(g1)$class<-c;V(g1)$order<-o;V(g1)$family<-f;V(g1)$genus<-G
  return(g1)
}
net_pro<-function(igraph){
  igraph.weight <- E(igraph)$weight
  num.edges <- length(E(igraph)) 
  num.vertices <- length(V(igraph))
  connectance <- edge_density(igraph,loops=FALSE)
  average.degree <- mean(igraph::degree(igraph))
  average.path.length <- average.path.length(igraph)
  edge.connectivity <- edge_connectivity(igraph)
  clustering.coefficient <- transitivity(igraph) 
  graph.density <- graph.density(igraph)
  fc<-cluster_fast_greedy(igraph,weights = NULL)
  modularity<-modularity(igraph,membership(fc))
  centralization.degree <- centralization.degree(igraph)$centralization
  centralization.betweenness <- centralization.betweenness(igraph)$centralization 
  centralization.closeness <- centralization.closeness(igraph)$centralization
  num.pos.edges<-sum(igraph.weight>0)
  num.neg.edges<-sum(igraph.weight<0)
  igraph.network.pro <- rbind(num.vertices,num.edges,num.pos.edges,num.neg.edges,connectance,average.degree,average.path.length,edge.connectivity,clustering.coefficient,graph.density,modularity,centralization.degree,centralization.betweenness,centralization.closeness)
  rownames(igraph.network.pro)<-c("num.vertices","num.edges","num.pos.edges","num.neg.edges","connectance","average.degree","average.path.length","edge.connectivity","clustering.coefficient","graph.density","modularity","centralization.degree","centralization.betweenness","centralization.closeness")
  colnames(igraph.network.pro)<- "value"
  igraph.network.pro
}