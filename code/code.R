
#pairwise adonis test
library(vegan)
library(pairwiseAdonis)
g<-read.csv("group.csv",header = T,row=1);d<-read.csv("ASVs.csv",header=T,row=1)

res<-adonis2(d~compart*soil*damage, data=g);res
adonis2(d~damage, data=g);anosim(d, g$damage);mrpp(d,g$damage,distance = 'bray')
pairwise.adonis(x = d, factors = g$damage, sim.method = 'bray', perm = 999, p.adjust.m = 'fdr')


#sourcetracker analysis
source('SourceTracker.r')
otus <- read.table('ASVs.txt',sep='\t', header=T,row.names=1,check=F,skip=1,comment='');metadata<-read.csv('group.csv',row=1,header=T)

otus<-otus[rowSums(otus)>0,row.names(metadata)]
otus<-round(otus);otus <- t(as.matrix(otus));common.sample.ids <- intersect(rownames(metadata), rownames(otus))
otus <- otus[common.sample.ids,];metadata <- metadata[common.sample.ids,]
train.ix <- which(metadata$SourceSink=='source');test.ix <- which(metadata$SourceSink=='sink')
envs <- metadata$Env;alpha1 <- alpha2 <- 0.001;st <- sourcetracker(otus[train.ix,], envs[train.ix])
results <- predict(st,otus[test.ix,], alpha1=alpha1, alpha2=alpha2)
res1<-data.frame(results$proportions)


#randomforest analysis
library(randomForest)

group<-read.csv("group.csv",row=1,header = T)
comun<-read.csv("ASVs.csv",row=1,header=T)
tax<-read.csv("tax.csv",header=T,row=1);tax<-tax[row.names(comun),]

comun1<-cbind(comun,group$damage)
names(comun1)[length(comun1)]<-'group'
data <- sample(nrow(comun1), nrow(comun1)*0.7);train <- comun1[data, ];test <- comun1[-data, ]
rf <- randomForest(group ~., data = train, importance = TRUE, ntree = 5000, nPerm = 1000)

#taxa accurancy rate
pred<-predict(rf,newdata=test)  
table <- table(pred,test$group)  
sum(diag(table))/sum(table)  

#important taxa
data <- data.frame(importance(rf), check.names = FALSE)



#network construct
source('network analysis.R')
data<-read.csv("ASVs.csv",header=T,row=1);data<-data/rowSums(data)
tax<-read.csv("tax.csv",row=1,header=T)

g<-net(data,tax)
netpro<-net_pro(g)

#natural connectivity
source('natural connectivity.R')

data<-read.csv("ASVs.csv",header=T,row=1);data<-data/rowSums(data) 
group<-read.csv("group.csv",row=1,header=T)


g1<-group[group$damage=='un'&group$compart=='rz',];d1<-data[row.names(g1),];data1<-d1;N<-data1[,colMeans(data1)>0.00005]
g1<-group[group$damage=='m'&group$compart=='rz',];d1<-data[row.names(g1),];data1<-d1;S<-data1[,colMeans(data1)>0.00005]
g1<-group[group$damage=='s'&group$compart=='rz',];d1<-data[row.names(g1),];data1<-d1;I<-data1[,colMeans(data1)>0.00005]

p1<-ncon(N,S,I,1500)

#plspm analysis
library(plspm)

data<-read.csv("data.csv",row=1,header=T)

s      = c(0,0,0,0,0)
sp     = c(1,0,0,0,0)
rz     = c(1,1,0,0,0)
rsn    = c(1,1,1,0,0)
netrsn = c(1,1,1,1,0)
path_mat = rbind(s, sp, rz, rsn, netrsn)
blocks=list(1,2,3,4:6,8:10)
sat_pls = plspm(d,path_mat,blocks)
summary(sat_pls)
sat_pls$path_coefs

