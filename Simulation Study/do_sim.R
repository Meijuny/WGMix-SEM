library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

do_sim<-function(design, ConditionIdx, TotalRep){
  
  reg_coeff<-design[design$ConditionIdx==ConditionIdx,"reg_coeff"]
  coeff_direct<-design[design$ConditionIdx==ConditionIdx,"coeff_direct"]
  ngroups<-design[design$ConditionIdx==ConditionIdx,"ngroups"]
  N_g<-design[design$ConditionIdx==ConditionIdx,"N_g"]
  nclus_ind<-design[design$ConditionIdx==ConditionIdx,"nclus_ind"]
  nclus_group<-design[design$ConditionIdx==ConditionIdx,"nclus_group"]
  balance<-design[design$ConditionIdx==ConditionIdx,"balance"]
  reliability<-design[design$ConditionIdx==ConditionIdx,"reliability"]
  NonInvG<-design[design$ConditionIdx==ConditionIdx,"NonInvG"]
  NonInvSize<-design[design$ConditionIdx==ConditionIdx,"NonInvSize"]
  
  model<-'
  ##measurement model:
  F1=~x1+x2+x3+x4+x5
  F2=~z1+z2+z3+z4+z5
  F3=~m1+m2+m3+m4+m5
  F4=~v1+v2+v3+v4+v5
  F5=~y1+y2+y3+y4+y5
  
  ##structural model:
  F5~F1+F2+F3+F4
  '
  
  RegParameters_i<-vector(mode = "list", length = TotalRep)
  GroupClustering_i<-vector(mode = "list", length = TotalRep)
  GroupClusProfile_i<-vector(mode = "list", length = TotalRep)
  
  eval_list<-vector("list", length = TotalRep)
  
  for(r in 1:TotalRep){
    
    print(paste("Condition", ConditionIdx, "Replication", r, "out of", TotalRep))
    
    set.seed(ConditionIdx*r)
    
    eval_list[[r]]<-tryCatch({SimData<-DataGeneration(model=model, reg_coeff = reg_coeff, coeff_direct = coeff_direct,
                                                      ngroups = ngroups, N_g = N_g, nclus_ind = nclus_ind, 
                                                      nclus_group = nclus_group,
                                                      balance = balance, 
                                                      reliability = reliability, NonInvG = NonInvG, NonInvSize = NonInvSize)
    
    withCallingHandlers(
      FactorScoreDFGeneration(data = SimData,
                              ConditionIdx = ConditionIdx,
                              Rep=r),
      warning = function(w){
        message("Warning in Condition ", ConditionIdx, ", Rep ", r, ": ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    
    RunLG_OutResult(ConditionIdx = ConditionIdx, rep = r, 
                    nclus_group = nclus_group, nclus_ind = nclus_ind)
    
    RegParameters_i[[r]]<-Out_RegCoeff(ConditionIdx = ConditionIdx, rep = r, 
                                       nclus_group = nclus_group, nclus_ind = nclus_ind,
                                       reg_coeff = reg_coeff)
    
    GroupClustering_i[[r]]<-Out_GroupClus(ConditionIdx = ConditionIdx, rep = r,
                                          ngroups = ngroups, nclus_group = nclus_group,
                                          reg_coeff = reg_coeff, balance = balance)
    
    
    GroupClusProfile_i[[r]]<-Out_GroupClusProfile(ConditionIdx = ConditionIdx, rep = r, 
                                                  IndClusPerm = RegParameters_i[[r]]$IndBestPerm,
                                                  GroupClusPerm = GroupClustering_i[[r]]$GroupClusPerm,
                                                  nclus_group = nclus_group, 
                                                  nclus_ind = nclus_ind)
    
    tmp_df<-evaluation(ConditionIdx = ConditionIdx, rep=r,
                       current_beta_mat = RegParameters_i[[r]]$beta_mat, 
                       current_GC_mat = GroupClustering_i[[r]]$GroupClustering,
                       current_GC_vec = GroupClustering_i[[r]]$GroupClusVec,
                       current_GCProfile_mat = GroupClusProfile_i[[r]],
                       nclus_group = nclus_group, nclus_ind = nclus_ind,
                       balance = balance, ngroups = ngroups,
                       reg_coeff = reg_coeff)
    
    tmp_df
    
    }, error = function(e){
      message("Error in Condition ", ConditionIdx, ", Rep ", r, ": ", e$message)
      # Return NA-filled row if any error occurs
      data.frame(
        Condition = i,
        Replication = r,
        Convergence = FALSE,
        RMSE_B1 = NA, RMSE_B2 = NA, RMSE_B3 = NA, RMSE_B4 = NA,
        ARI = NA,
        RMSE_GClass1 = NA, RMSE_GClass2 = NA, RMSE_GClass3 = NA, 
        stringsAsFactors = FALSE
      )
    })
  }
  list(
    RegParameters = RegParameters_i,
    GroupClustering = GroupClustering_i,
    GroupClusProfile = GroupClusProfile_i,
    evaluationTable = do.call(rbind, eval_list)
  )
  
}




####------------------------------------------------------------------------------
##testing the simulation

##simulation design
ngroups<-c(24,48)
nclus_group<-c(2,3)
balance<-"balanced"
nclus_ind<-4
N_g<-c(100,200,400,800)
reg_coeff<-c(0.3,0.4)
coeff_direct<-"same"
reliability<-c("low","high")
NonInvG<-0.75
NonInvSize<-0.4

design <- expand.grid(ngroups, nclus_group, balance, nclus_ind, N_g,
                      reg_coeff, coeff_direct, reliability, NonInvG, NonInvSize)
colnames(design) <- c("ngroups", "nclus_group", "balance", "nclus_ind", "N_g",
                      "reg_coeff", "coeff_direct", "reliability", "NonInvG", "NonInvSize")

design <- cbind(ConditionIdx = 1:nrow(design), design)

##source the necessary function in:
source("./Simulation Study/DataGeneration.R")
source("./Simulation Study/FactorScoreDF.R")
source("./Simulation Study/RunLG_OutResult.R")
source("./Simulation Study/Out_RegCoeff.R")
source("./Simulation Study/Out_GroupClus.R")
source("./Simulation Study/Out_GroupClusProfile.R")
source("./Simulation Study/ARI.R")
source("./Simulation Study/evaluation.R")
source("./Simulation Study/evaluation.R")


##create empty vector to store the results
RegParameters    <- vector("list", LastConditionIdx)
GroupClustering  <- vector("list", LastConditionIdx)
GroupClusProfile <- vector("list", LastConditionIdx)

##temporary create this for the test of only 2 conditions
RegParameters    <- vector("list", 2)
GroupClustering  <- vector("list", 2)
GroupClusProfile <- vector("list", 2)

evaluationTable <- data.frame()

##do the simulation:

StartTime<-Sys.time()

for (i in 59:60){
  
  res<-do_sim(design = design,
              ConditionIdx = i,
              TotalRep = 3)
  
  RegParameters[[i]]<-res$RegParameters
  GroupClustering[[i]]<-res$GroupClustering
  GroupClusProfile[[i]]<-res$GroupClusProfile
  
  evaluationTable<-rbind(evaluationTable, res$evaluationTable)
  
}

EndTime<-Sys.time()
EndTime-StartTime
