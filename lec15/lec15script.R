# lec 15 class script
set.seed(123)
N = 100000; K = 2; P = 2; NK=N*K # set parameters to generate data
data_MC1 = rmvnorm(N,c(17,17),matrix(c(10,0,0,10),ncol=2)) # draw data group 1
data_MC2 = rmvnorm(N,c(10,10),matrix(c(10,9,9,10),ncol=2)) # draw data group 2
data_MC  = as.data.table(rbind(data_MC1,data_MC2)) # rbind the two datasets
data_MC[,group:=ifelse(.I/.N>0.5,2,1)]    # assign groups
setnames(data_MC,c("V1","V2"),c("x","y")) # rename variables
data_MC[c(1:3,(.N-2):.N)]

data_MC[,group_assign:=sample(1:K,K*N,replace=T)] # randomly assign initial groups
centroids = data_MC[,lapply(.SD, mean),by=group_assign,.SDcols=c("x","y")] 
setorder(centroids,group_assign); centroids # sort by assigned group and print

dat_mat = as.matrix(data_MC[,.(x,y)]) # store data as a matrix
Dist    = array(0,dim=c(NK,K))        # initialize distance matrix
## --- Define norm function for calculating distances
# p == 2 is the Euclidean distance/Pythogreon theorem. p == Inf is sup-norm
norm = function(x,p=2) if(p==Inf) max(abs(x)) else sum(abs(x)^p)^(1/p)
## --- For each group and observation, calculate distance to each centroid 
for(k in 1:K){
  cent_mat = centroids[rep(k,NK),.(x,y)]    # store centroids for group k
  cent_mat = as.matrix(cent_mat)            # guarantee cent_mat is a matrix
  Dist[,k] = apply(dat_mat-cent_mat,1,norm) # calculate Euclidean Distance
}
new_groups = apply(Dist,1,which.min) # new group is whichever has the smallest distance

## --- Continue the same process until no observations change groups
while(sum(data_MC$group_assign != new_groups)>0){
  data_MC[,group_assign:=new_groups] # assign new groups
  ## --- Calculate centroids
  centroids = data_MC[,lapply(.SD, mean),by=group_assign,.SDcols=c("x","y")] 
  centroids = centroids[order(group_assign)] # sort centroids by group
  for(k in 1:K){
    cent_mat = centroids[rep(k,NK),.(x,y)]    # store centroids for k
    cent_mat = as.matrix(cent_mat)            # make cent_mat a matrix
    Dist[,k] = apply(dat_mat-cent_mat,1,norm) # calculate Euclidean distance
  }
  new_groups = apply(Dist,1,which.min) # new group is whichever has smallest distance
}
data_MC[,group_assign:=new_groups] # assign final groups
centroids

kmeans_out = kmeans(data_MC[,.(x,y)],centers=2,nstart=10,iter.max=100)
kmeans_out$centers




