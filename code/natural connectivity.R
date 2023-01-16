
ncon<-function(data1,data2,data3,number){
  adj1 <- adjacency(data1)
  adj2 <- adjacency(data2)
  adj3 <- adjacency(data3)
  natural_connectivity1 <- nc(adj1)
  natural_connectivity2 <- nc(adj2)
  natural_connectivity3 <- nc(adj3)
  
  #转化??? igraph 邻接列表，计算节点平均度
  g1 <- graph_from_adjacency_matrix(as.matrix(adj1), mode = 'undirected', diag = FALSE)
  g2 <- graph_from_adjacency_matrix(as.matrix(adj2), mode = 'undirected', diag = FALSE)
  g3 <- graph_from_adjacency_matrix(as.matrix(adj3), mode = 'undirected', diag = FALSE)
  average_degree1 <- mean(degree(g1))
  average_degree2 <- mean(degree(g2))
  average_degree3 <- mean(degree(g3))
  
  #随机移除 number(不超过网络总节???) 个节点，并计算上??? 2种网络特???
  for (i in 1:number) {
    #在邻接矩阵中随机移除 i 个节???
    remove_node <- sample(1:nrow(adj1), i)
    adj1_remove <- adj1[-remove_node,-remove_node]
    remove_node <- sample(1:nrow(adj2), i)
    adj2_remove <- adj2[-remove_node,-remove_node]
    remove_node <- sample(1:nrow(adj3), i)
    adj3_remove <- adj3[-remove_node,-remove_node]
    #计算自然连通度
    natural_connectivity1 <- c(natural_connectivity1, nc(adj1_remove))
    natural_connectivity2 <- c(natural_connectivity2, nc(adj2_remove))
    natural_connectivity3 <- c(natural_connectivity3, nc(adj3_remove))
    #计算节点平均???
    g1 <- graph_from_adjacency_matrix(as.matrix(adj1_remove), mode = 'undirected', diag = FALSE)
    g2 <- graph_from_adjacency_matrix(as.matrix(adj2_remove), mode = 'undirected', diag = FALSE)
    g3 <- graph_from_adjacency_matrix(as.matrix(adj3_remove), mode = 'undirected', diag = FALSE)
    average_degree1 <- c(average_degree1, mean(degree(g1)))
    average_degree2 <- c(average_degree2, mean(degree(g2)))
    average_degree3 <- c(average_degree3, mean(degree(g3)))
  }
  #ggplot2 作图，拟合线
  dat <- data.frame(remove_node = rep(0:number, 6),
                    variable = c(rep(c(rep('natural_connectivity', number+1), rep('average_degree', number+1)), 3)),
                    values = c(natural_connectivity1, average_degree1, natural_connectivity2, average_degree2,
                               natural_connectivity3, average_degree3),
                    network = c(rep(c('data1','data2','data3'), each=(number+1)*2)))
  #write.csv(dat, 'dat.csv', row.names = FALSE, quote = FALSE)
  require(ggsci)
  p<-ggplot(dat, aes(remove_node, values, color = network)) +
    geom_smooth(se = FALSE,size=1.5) + #method = 'lm'，
    scale_color_aaas()+
    facet_wrap(~variable, ncol = 2, scale = 'free_y')+
    theme_bw()+
    theme(text = element_text(size=24),axis.text = element_text(color = "black"),legend.title = element_blank() )
  return(p)
}