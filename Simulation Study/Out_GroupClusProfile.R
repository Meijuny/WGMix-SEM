library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

Out_GroupClusProfile<-function(ConditionIdx, rep,
                               IndClusPerm, GroupClusPerm,
                               nclus_group, nclus_ind){
  
  ##read the results in
  lg_lines<-readLines(paste0(getwd(),"/Simulation Study/LGResults/LGResults_c_", ConditionIdx, "_r_", rep, ".txt"),
                      encoding = "latin1")
  
  GroupClus_Profile_start_idx<-grep("^ProbMeans", lg_lines)-nclus_ind
  GroupClus_Profile_end_idx<-grep("^ProbMeans", lg_lines)-1
  GroupClus_Profile_lines<-lg_lines[GroupClus_Profile_start_idx:GroupClus_Profile_end_idx]
  
  ##turn it into data frame:
  GroupClus_Profile_df<-read.table(
    text = GroupClus_Profile_lines,
    sep = "\t",
    header = FALSE,
    stringsAsFactors = FALSE
  )
  
  
  ##extract the necessary columns to make it into a matrix:
  keep_cols<-seq(from=2, by=1, length.out=nclus_group) ##without SE (if SE is computed, we need to do by=2)
  GroupClus_Profile_mat<-as.matrix(GroupClus_Profile_df[,keep_cols])
  rownames(GroupClus_Profile_mat)<-paste0("EstClass", GroupClus_Profile_df[[1]])
  colnames(GroupClus_Profile_mat)<-paste0("GClass", seq_len(nclus_group))
  
  
  ##reorder based on the best permutation
  GroupClus_Profile_mat<-GroupClus_Profile_mat[IndClusPerm, GroupClusPerm]
  
  return(GroupClus_Profile_mat=GroupClus_Profile_mat)
  
}
