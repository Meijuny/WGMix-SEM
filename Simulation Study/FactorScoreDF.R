library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

FactorScoreDFGeneration<-function(data, ConditionIdx, Rep){
  F1.MM<-'
  F1=~x1+x2+x3+x4+x5
  '
  
  F1.fit<-cfa(model = F1.MM,
              data = data,
              group="group",
              group.equal="loadings",
              group.partial=c("F1=~x2","F1=~x3"))
  
  F1_factorScores<-lavPredict(F1.fit, se="standard", assemble = T)
  F1_factorScores<-F1_factorScores %>%
    mutate(id=row_number())
  
  F1_std<-attr(F1_factorScores, "se")
  F1_std<-unlist(F1_std, use.names=F)
  
  F1_std_df<-data.frame(group=lavInspect(F1.fit,"group.label"),
                        F1_std=F1_std)
  
  F1_factorScores<-merge(F1_factorScores, F1_std_df,
                         by.x = "group", by.y = "group") %>%
    arrange(id)
  
  ##measurement model and factor scores for F2:
  F2.MM<-'
  F2=~z1+z2+z3+z4+z5
  '
  
  F2.fit<-cfa(model = F2.MM,
              data = data,
              group="group",
              group.equal="loadings",
              group.partial=c("F2=~z2","F2=~z3"))
  
  F2_factorScores<-lavPredict(F2.fit, se="standard", assemble = T)
  F2_factorScores<-F2_factorScores %>%
    mutate(id=row_number())
  
  F2_std<-attr(F2_factorScores, "se")
  F2_std<-unlist(F2_std, use.names=F)
  
  F2_std_df<-data.frame(group=lavInspect(F2.fit,"group.label"),
                        F2_std=F2_std)
  
  F2_factorScores<-merge(F2_factorScores, F2_std_df,
                         by.x = "group", by.y = "group") %>%
    arrange(id)
  
  ##measurement model and factor scores for F3:
  F3.MM<-'
  F3=~m1+m2+m3+m4+m5
  '
  
  F3.fit<-cfa(model = F3.MM,
              data = data,
              group="group",
              group.equal="loadings",
              group.partial=c("F3=~m2","F3=~m3"))
  
  F3_factorScores<-lavPredict(F3.fit, se="standard", assemble = T)
  F3_factorScores<-F3_factorScores %>%
    mutate(id=row_number())
  
  F3_std<-attr(F3_factorScores, "se")
  F3_std<-unlist(F3_std, use.names=F)
  
  F3_std_df<-data.frame(group=lavInspect(F3.fit,"group.label"),
                        F3_std=F3_std)
  
  F3_factorScores<-merge(F3_factorScores, F3_std_df,
                         by.x = "group", by.y = "group") %>%
    arrange(id)
  
  ##measurement model and factor scores for F4:
  F4.MM<-'
  F4=~v1+v2+v3+v4+v5
  '
  
  F4.fit<-cfa(model = F4.MM,
              data = data,
              group="group",
              group.equal="loadings",
              group.partial=c("F4=~v2","F4=~v3"))
  
  F4_factorScores<-lavPredict(F4.fit, se="standard", assemble = T)
  F4_factorScores<-F4_factorScores %>%
    mutate(id=row_number())
  
  F4_std<-attr(F4_factorScores, "se")
  F4_std<-unlist(F4_std, use.names=F)
  
  F4_std_df<-data.frame(group=lavInspect(F4.fit,"group.label"),
                        F4_std=F4_std)
  
  F4_factorScores<-merge(F4_factorScores, F4_std_df,
                         by.x = "group", by.y = "group") %>%
    arrange(id)
  
  ##measurement model and factor scores for F5:
  F5.MM<-'
  F5=~y1+y2+y3+y4+y5
  '
  
  F5.fit<-cfa(model = F5.MM,
              data = data,
              group="group",
              group.equal="loadings",
              group.partial=c("F5=~y2","F5=~y3"))
  
  F5_factorScores<-lavPredict(F5.fit, se="standard", assemble = T)
  F5_factorScores<-F5_factorScores %>%
    mutate(id=row_number())
  
  F5_std<-attr(F5_factorScores, "se")
  F5_std<-unlist(F5_std, use.names=F)
  
  F5_std_df<-data.frame(group=lavInspect(F5.fit,"group.label"),
                        F5_std=F5_std)
  
  F5_factorScores<-merge(F5_factorScores, F5_std_df,
                         by.x = "group", by.y = "group") %>%
    arrange(id)
  
  
  ##putting everything together into the same FactorScoreDF:
  FactorScoreDF<-merge(F1_factorScores, F2_factorScores, 
                       by.x = "id", by.y = "id") %>% 
    dplyr::select(-group.y)
  
  FactorScoreDF<-merge(FactorScoreDF, F3_factorScores,
                       by.x = "id", by.y = "id") %>%
    dplyr::select(-group.x)
  
  FactorScoreDF<-merge(FactorScoreDF, F4_factorScores,
                       by.x = "id", by.y = "id") %>%
    dplyr::select(-group.y)
  
  FactorScoreDF<-merge(FactorScoreDF, F5_factorScores,
                       by.x = "id", by.y = "id") %>%
    dplyr::select(-group.x)
  
  FactorScoreDF<-FactorScoreDF %>%
    dplyr::select(id, group, 
                  F1, F1_std, F2, F2_std, F3, F3_std, F4, F4_std, F5, F5_std)
  
  write.csv(FactorScoreDF, 
            file = paste0("./Simulation Study/FactorScoreDF/AdditionalRep/FactorScoreDF_condition", ConditionIdx, "_Rep", Rep,".csv"),
            row.names = F)
  
  invisible(NULL)
  
}



##################################################################################################
################ Generate the Factor Score DF for the Windows Cluster to run Simulation ##########
##################################################################################################

##for Kristof
#
#First set up the simulation design
design <- expand.grid(ngroups = c(24, 48),
                      nclus_group = c(2, 3),
                      balance = "balanced",
                      nclus_ind = 4,
                      N_g = c(100, 200, 400, 800),
                      reg_coeff = c(0.3, 0.4),
                      coeff_direct = "same",
                      reliability = c("low", "high"),
                      NonInvG = 0.75,
                      NonInvSize = 0.4)
design <- cbind(ConditionIdx = seq_len(nrow(design)), design)

LastConditionIdx<-max(design$ConditionIdx)
TotalRep<-50

StartTime<-Sys.time()

for(ConditionIdx in 49:64) {
  
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
  
  for(r in 21:TotalRep){
    
    print(paste("Condition", ConditionIdx, "Replication", r, "out of", 50)) 
    
    set.seed(ConditionIdx*r)
    
    SimData<-DataGeneration(model=model, reg_coeff = reg_coeff, coeff_direct = coeff_direct,
                            ngroups = ngroups, N_g = N_g, nclus_ind = nclus_ind, 
                            nclus_group = nclus_group,
                            balance = balance, 
                            reliability = reliability, NonInvG = NonInvG, NonInvSize = NonInvSize)
    
    withCallingHandlers(
      FactorScoreDFGeneration(data=SimData, 
                              ConditionIdx=ConditionIdx, 
                              Rep=r),
      warning = function(w) {
        if (grepl("negative", conditionMessage(w), ignore.case = TRUE)) {
          message("warnings: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  
}


EndTime<-Sys.time()
StartTime-EndTime
