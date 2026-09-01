library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

Out_GroupClus<-function(ConditionIdx, rep, ngroups, 
                        nclus_group, reg_coeff, balance) {
  
  ##read the results in
  lg_lines<-readLines(paste0(getwd(),"/Simulation Study/LGResults/LGResults_c_", ConditionIdx, "_r_", rep, ".txt"),
                      encoding = "latin1")
  
  #find the starting line for the group clustering:
  GroupClus_start_idx<-grep("^ProbMeans", lg_lines)+6
  GroupClus_end_idx<-GroupClus_start_idx+ngroups-1
  GroupClus_lines<-lg_lines[GroupClus_start_idx:GroupClus_end_idx]
  
  #transforming into data frame:
  GroupClus_df<-read.table(
    text = GroupClus_lines,
    sep = "\t",
    header = F,
    stringsAsFactors = F,
    fill = T
  )
  
  GroupClus_df<-GroupClus_df[,2:(1+nclus_group)]
  colnames(GroupClus_df)<-paste0("GClass", seq(nclus_group))
  
  ##starting the permutation to locate the best matching clusters
  GroupClus_perm<-permn(nclus_group)
  
  #initialize empty vectors to hold results
  permutedClusterMat<-vector(mode="list", length = length(GroupClus_perm))
  permutedClusterVec<-vector(mode="list", length = length(GroupClus_perm))
  MisClassError<-numeric(length(GroupClus_perm))
  
  ##initialize the original clustering vectors for comparisons:
  if (balance=="balanced"){
    GperK<-rep(x=1:nclus_group, each=(ngroups/nclus_group))
  } else if (balance == "unbalanced"){
    largest<-ngroups*0.75
    smaller<-ngroups-largest
    GperK<-c(rep(x=1, times=largest), rep(2:nclus_group, each=(smaller/(nclus_group-1))))
  }
  
  ## to loop it:
  for (i in 1:length(GroupClus_perm)){
    
    permutedClusterMat[[i]]<-GroupClus_df[, GroupClus_perm[[i]]]
    
    ##make the largest value per row to 1 (by erasing all classification uncertainty)
    permutedClusterMat[[i]]<-t(apply(permutedClusterMat[[i]], 1, function(x) {
      as.integer(x==max(x))
    }))
    
    colnames(permutedClusterMat[[i]]) <- colnames(GroupClus_df)[GroupClus_perm[[i]]]
    
    ##turn the matrix to vector for clustering membership
    permutedClusterVec[[i]]<-max.col(permutedClusterMat[[i]])
    
    ##safe guard the confusion matrix
    permutedClusterVec[[i]]<-factor(x=permutedClusterVec[[i]], levels = 1:nclus_group)
    
    ##confusion matrix and misclassification errors
    conf_mat<-table(permutedClusterVec[[i]],GperK)
    MisClassError[i]<-1-(sum(diag(conf_mat))/sum(conf_mat))
    
  }
  
  GroupClusteringMat<-permutedClusterMat[[which.min(MisClassError)]]
  
  
  GroupClus_bestPerm<-GroupClus_perm[[which.min(MisClassError)]]
  
  GroupClus_bestVec <- factor(
    GroupClus_perm[[which.min(MisClassError)]][max.col(permutedClusterMat[[which.min(MisClassError)]])],
    levels = 1:nclus_group)
  
  return(list(GroupClustering=GroupClusteringMat,
              GroupClusPerm=GroupClus_bestPerm,
              GroupClusVec=GroupClus_bestVec))
  
}
