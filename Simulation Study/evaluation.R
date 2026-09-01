library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

evaluation<-function(ConditionIdx, rep,
                     current_beta_mat, current_GC_mat,
                     current_GC_vec,
                     current_GCProfile_mat, 
                     nclus_group, nclus_ind, balance, ngroups,
                     reg_coeff){
  
  ##check convergence:
  lg_lines<-readLines(paste0(getwd(),"/Simulation Study/LGResults/LGResults_c_", ConditionIdx, "_r_", rep, ".txt"),
                      encoding = "latin1")
  
  convergence<-!(any(grepl("WARNING: maximum number of iterations", lg_lines, fixed=T)))
  
  ##first focus on calculating RMSE for beta
  ##initialize the original beta matrix for comparison:
  if (nclus_ind==2){
    ori_beta_mat <- matrix(data = c(0, reg_coeff, reg_coeff, reg_coeff, 
                                    reg_coeff, 0, reg_coeff, reg_coeff),
                           nrow = 4,
                           ncol = nclus_ind,
                           dimnames = list(paste0("B", 1:4),
                                           paste0("OriClass", 1:nclus_ind)))
  } else if (nclus_ind==4){
    ori_beta_mat <- matrix(data = c(0, reg_coeff, reg_coeff, reg_coeff, 
                                    reg_coeff, 0, reg_coeff, reg_coeff,
                                    reg_coeff, reg_coeff, 0, reg_coeff,
                                    reg_coeff, reg_coeff, reg_coeff, 0),
                           nrow = 4,
                           ncol = nclus_ind,
                           dimnames = list(paste0("B", 1:4),
                                           paste0("OriClass", 1:nclus_ind)))
  }
  
  SquareError_mat<-(current_beta_mat-ori_beta_mat)^2
  
  RMSE_beta<-sqrt(rowMeans(SquareError_mat))
  RMSE_B1<-as.numeric(RMSE_beta["B1"])
  RMSE_B2<-as.numeric(RMSE_beta["B2"])
  RMSE_B3<-as.numeric(RMSE_beta["B3"])
  RMSE_B4<-as.numeric(RMSE_beta["B4"])
  
  ##next calculate the ARI using Andres's function:
  ##initialize the original cluster membership
  if (balance=="balanced"){
    GperK<-rep(x=1:nclus_group, each=(ngroups/nclus_group))
  } else if (balance == "unbalanced"){
    largest<-ngroups*0.75
    smaller<-ngroups-largest
    GperK<-c(rep(x=1, times=largest), rep(2:nclus_group, each=(smaller/(nclus_group-1))))
  }
  
  
  ARI<-adjrandindex(part1 = GperK, part2 = current_GC_vec)
  
  
  ##next calculate the relative bias of the group cluster profile:
  #initialize the original group-cluster profile:
  if((nclus_ind==4) & (nclus_group==2)){
    Ori_GCProfile<-matrix(data = c(0.8, 0.1,
                                   0.1, 0.1,
                                   0.05, 0.4,
                                   0.05, 0.4), 
                          nrow = nclus_ind, 
                          ncol = nclus_group,
                          byrow = T)
  } else if((nclus_ind==4) & (nclus_group==3)){
    Ori_GCProfile<-matrix(data = c(0.8, 0.1,0.25,
                                   0.1, 0.1,0.25,
                                   0.05, 0.4,0.25,
                                   0.05, 0.4,0.25), 
                          nrow = nclus_ind, 
                          ncol = nclus_group,
                          byrow = T)
  }
  
  SquareError_CGProfile<-(current_GCProfile_mat-Ori_GCProfile)^2
  RMSE_GCProfile<-sqrt(colMeans(SquareError_CGProfile))
  
  RMSE_GClass1<-as.numeric(RMSE_GCProfile[1]) ##take the original label
  RMSE_GClass2<-as.numeric(RMSE_GCProfile[2]) ##take the original label
  
  if(nclus_group==2){
    RMSE_GClass3<-NA
  } else if(nclus_group==3){
    RMSE_GClass3<-as.numeric(RMSE_GCProfile[3]) ##take the original label
  }
  
  return(
    data.frame(
      Condition      = ConditionIdx,
      Replication    = rep,
      Convergence    = convergence,
      RMSE_B1        = RMSE_B1,
      RMSE_B2        = RMSE_B2,
      RMSE_B3        = RMSE_B3,
      RMSE_B4        = RMSE_B4,
      ARI            = ARI,
      RMSE_GClass1   = RMSE_GClass1,
      RMSE_GClass2   = RMSE_GClass2,
      RMSE_GClass3   = RMSE_GClass3,
      stringsAsFactors = FALSE
    )
  )
}
