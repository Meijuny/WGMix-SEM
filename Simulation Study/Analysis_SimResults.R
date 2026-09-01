library(dplyr)
library(tidyr)
library(qwraps2)
library(flextable)
library(officer)
library(ggplot2)
library(patchwork)
library(effectsize)

AnalysisTable<-evaluationTable %>%
  rename(ConditionIdx=Condition) %>%
  left_join(design, by="ConditionIdx")

save(AnalysisTable, 
     file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/Preliminary_Simulation/Simulation Results/June HPC/AnalysisTable_complete.RData")

##load the results that include the newly converged cases after increasing the number of iterations
load("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/Preliminary_Simulation/Simulation Results/June HPC/AnalysisTableNEW.RData")


#######################################################################################
#################### Convergence ######################################################
#######################################################################################

##overall convergence rate
#mean(AnalysisTable$Convergence) #0.8882813
#
#after re-run the non-convergence
mean(AnalysisTableNEW$Convergence) ##0.9507813
#
#the proportion of non-convergence cases that can converge now after increasing max. iterations
#mean(NonCov_evaluationTable$Convergence) #0.5594406

##convergence rate for each level of each factor
factors<-c("ngroups","nclus_group","N_g","reg_coeff","reliability") 

list_results<-list() ##create empty list to store results

for(f in factors){
  tmp<-AnalysisTableNEW %>%
    group_by(.data[[f]]) %>%
    summarise(ConvergenceRate = formatC(mean(Convergence), digits = 2, format = "f"),
              .groups = "drop")
  
  tmp$Factor<-f
  
  colnames(tmp)[1]<-"Level"
  
  tmp$Level<-as.character(tmp$Level)
  
  tmp<-tmp[,c("Factor","Level","ConvergenceRate")]
  
  list_results[[f]]<-tmp
}

Conv_table<-bind_rows(list_results)

Conv_table<-Conv_table %>%
  mutate(Factor = ifelse(duplicated(Factor), "", Factor))

##make table for papers
ft<-flextable(Conv_table)

ft<-ft %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part="all") %>%
  valign(valign = "center", part = "all") %>%
  bold(j=1, bold=TRUE) %>%
  set_header_labels(
    Factor="Factor",
    Level="Level",
    ConvergenceRate="Convergence Rate"
  )


#######################################################################################
#################### Regression Parameter Recovery ####################################
#######################################################################################


#analyze only the ones that converge
Analysis_reg<-AnalysisTableNEW %>%
  filter(Convergence==TRUE)

##Table
factors<-c("ngroups","nclus_group","N_g","reg_coeff","reliability") 
rmse_vars<-c("RMSE_B1","RMSE_B2","RMSE_B3","RMSE_B4")

list_results<-list() ##create empty list to store results

for(f in factors){
  tmp<-Analysis_reg %>%
    group_by(.data[[f]]) %>%
    summarise(across(all_of(rmse_vars),
                     ~qwraps2::mean_sd(.x, denote_sd = "paren", digits = 3)),
              .groups = "drop")
  
  tmp$Factor<-f
  
  colnames(tmp)[1]<-"Level"
  
  tmp$Level<-as.character(tmp$Level)
  
  tmp<-tmp[,c("Factor","Level",rmse_vars)]
  
  list_results[[f]]<-tmp
}

Reg_table<-bind_rows(list_results)

##add the "total" row
total_row<-Analysis_reg %>%
  summarise(across(all_of(rmse_vars),
                   ~qwraps2::mean_sd(.x, denote_sd = "paren", digits = 3))) %>%
  mutate(Factor= "Total", Level= "") %>%
  dplyr::select(Factor, Level, all_of(rmse_vars))

Reg_table<-bind_rows(Reg_table, total_row)

Reg_table<-Reg_table %>%
  rename(
    B1=RMSE_B1,
    B2=RMSE_B2,
    B3=RMSE_B3,
    B4=RMSE_B4
  )

Reg_table<-Reg_table %>%
  mutate(Factor = ifelse(duplicated(Factor), "", Factor))

##make table for papers
ft<-flextable(Reg_table)

ft<-ft %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part="all") %>%
  valign(valign = "center", part = "all") %>%
  bold(j=1, bold=TRUE) %>%
  set_header_labels(
    Factor="Factor",
    Level="Level",
    B1="β1",
    B2="β2",
    B3="β3",
    B4="β4"
  )

##read it to an empty word document
#doc<-read_docx()
#doc<-body_add_flextable(doc, ft)
#print(doc, target = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/Publications/Second Paper/Parts/Tables.docx")

##plotting for interaction with only ngroups, N_g, reg_coeff, and reliability
Reg_interaction_summary<-Analysis_reg %>%
  group_by(ngroups,N_g, reg_coeff, reliability) %>% 
  summarise(RMSE_B1=mean(RMSE_B1),
            RMSE_B2=mean(RMSE_B2),
            RMSE_B3=mean(RMSE_B3),
            RMSE_B4=mean(RMSE_B4),
            .groups = "drop"
  )


Reg_interaction_summary_long<-pivot_longer(data = Reg_interaction_summary,
                                           cols = starts_with("RMSE_"),
                                           values_to = "RMSE",
                                           names_to = "RegPar") %>%
  mutate(RegPar = sub("RMSE_","",RegPar)) %>%
  mutate(reg_size=case_when(
    reg_coeff==0.3 ~ "β size of 0.3",
    reg_coeff==0.4 ~ "β size of 0.4"
  )) %>%
  mutate(NumOfGroups=case_when(
    ngroups==24 ~ "24 groups",
    ngroups==48 ~ "48 groups"
  ))


ggplot(Reg_interaction_summary_long, 
       aes(x=N_g, y = RMSE, color=factor(RegPar), shape=factor(RegPar), group=interaction(RegPar, reliability), linetype=reliability))+
  geom_point(size=2.5)+
  geom_line(linewidth=0.72)+
  facet_grid(rows = vars(NumOfGroups), cols = vars(reg_size))+
  scale_linetype_manual(values=c("low"="dashed", "high"="solid"))+
  labs(color="Beta",
       shape="Beta",
       linetype="reliability",
       x="within-group sample size",
       y="RMSE beta")+
  scale_y_continuous(limits = c(0,0.32), breaks = seq(0,0.32, by=0.05))+
  #geom_hline(yintercept = 0.1, linetype="dashed", color="blue", linewidth=0.75)+
  theme_bw()+
  theme(legend.position = "bottom")


#######################################################################################
#################### Group Cluster Recovery ###########################################
#######################################################################################

#analyze only the ones that converge
Analysis_GroupClus<-AnalysisTableNEW %>%
  filter(Convergence==TRUE) %>%
  mutate(PerfGClusRecovery=case_when(
    ARI==1~TRUE,
    ARI!=1~FALSE
  ))

##Table
factors<-c("ngroups","nclus_group","N_g","reg_coeff","reliability")
GroupClus_vars<-c("ARI","PerfGClusRecovery")


list_results<-list() ##create empty list to store results

for(f in factors){
  tmp<-Analysis_GroupClus %>%
    group_by(.data[[f]]) %>%
    summarise(across(all_of(GroupClus_vars),
                     ~qwraps2::mean_sd(.x, denote_sd = "paren", digits = 3)),
              .groups = "drop")
  
  tmp$Factor<-f
  
  colnames(tmp)[1]<-"Level"
  
  tmp$Level<-as.character(tmp$Level)
  
  tmp<-tmp[,c("Factor","Level",GroupClus_vars)]
  
  list_results[[f]]<-tmp
}

GroupClus_table<-bind_rows(list_results)

##add the "total" row
total_row<-Analysis_GroupClus %>%
  summarise(across(all_of(GroupClus_vars),
                   ~qwraps2::mean_sd(.x, denote_sd = "paren", digits = 3))) %>%
  mutate(Factor= "Total", Level= "") %>%
  dplyr::select(Factor, Level, all_of(GroupClus_vars))

GroupClus_table<-bind_rows(GroupClus_table, total_row)


GroupClus_table<-GroupClus_table %>%
  rename(
    ARI=ARI,
    CC=PerfGClusRecovery
  )

GroupClus_table<-GroupClus_table %>%
  mutate(Factor = ifelse(duplicated(Factor), "", Factor))

##make table for papers
ft<-flextable(GroupClus_table)

ft<-ft %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part="all") %>%
  valign(valign = "center", part = "all") %>%
  bold(j=1, bold=TRUE) %>%
  set_header_labels(
    Factor="Factor",
    Level="Level",
    ARI="ARI",
    CC="%CC"
  )

##read it to the working word document
#doc<-read_docx()
#doc<-body_add_flextable(doc, ft)
#print(doc, target = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/Publications/Second Paper/Parts/Tables.docx")

##plot: nclus_group (K), within-group sample size (N_g), and reg_coeff (β size) have the largest effect 
##plotting only interacting nclus_group (K), within-group sample size (N_g), and reg_coeff (β size)

Analysis_GroupClus<-AnalysisTableNEW %>%
  filter(Convergence==TRUE) %>%
  mutate(PerfGClusRecovery=case_when(
    ARI==1~TRUE,
    ARI!=1~FALSE
  ))

GroupClus_summary<-Analysis_GroupClus %>%
  group_by(nclus_group,N_g,reg_coeff) %>% 
  summarise(ARI=mean(ARI),
            CC=mean(PerfGClusRecovery),
            .groups = "drop"
  )

GroupClus_summary<-GroupClus_summary %>%
  mutate(GroupClusters=case_when(
    nclus_group==2 ~ "2 group-clusters",
    nclus_group==3 ~ "3 group-clusters"
  )) %>%
  mutate(reg_size=case_when(
    reg_coeff==0.3 ~ "β size of 0.3",
    reg_coeff==0.4 ~ "β size of 0.4"
  )) 

ARI_interact<-ggplot(GroupClus_summary, aes(x=N_g, y = ARI, color=factor(GroupClusters), shape=factor(GroupClusters)))+
  geom_point(size=2.5)+
  geom_line()+
  facet_grid(cols=vars(reg_size))+
  scale_y_continuous(limits = c(0,1))+
  #geom_hline(yintercept = 0.65, linetype="dashed", color="red")+
  #geom_hline(yintercept = 0.8, linetype="dashed", color="red")+
  labs(x="within-group sample size",
       y="ARI",
       color="group-clusters",
       shape="group-clusters")+
  theme_bw()+
  theme(legend.position = "bottom")



###############################################################################################
#################### Group Cluster Profile Recovery ###########################################
###############################################################################################

#analyze only the ones that converge
Analysis_GroupClusProfile<-AnalysisTableNEW %>%
  filter(Convergence==TRUE)

##Table
factors<-c("ngroups","nclus_group","N_g","reg_coeff","reliability") 
rmse_vars<-c("RMSE_GClass1","RMSE_GClass2","RMSE_GClass3")

list_results<-list() ##create empty list to store results

for(f in factors){
  tmp<-Analysis_GroupClusProfile %>%
    group_by(.data[[f]]) %>%
    summarise(
      across(all_of(rmse_vars),
             ~ {
               if(all(is.na(.x))){
                 NA_character_
               } else {
                 qwraps2::mean_sd(.x[!is.na(.x)], denote_sd = "paren", digits = 3)
               }
             }),
      .groups = "drop")
  
  tmp$Factor<-f
  
  colnames(tmp)[1]<-"Level"
  
  tmp$Level<-as.character(tmp$Level)
  
  tmp<-tmp[,c("Factor","Level",rmse_vars)]
  
  list_results[[f]]<-tmp
}

GroupClusProfile_table<-bind_rows(list_results)

##add the "total" row
total_row<-Analysis_GroupClusProfile %>%
  summarise(across(all_of(rmse_vars),
                   ~qwraps2::mean_sd(.x[!is.na(.x)], denote_sd = "paren", digits = 3))) %>%
  mutate(Factor= "Total", Level= "") %>%
  dplyr::select(Factor, Level, all_of(rmse_vars))

GroupClusProfile_table<-bind_rows(GroupClusProfile_table, total_row)


GroupClusProfile_table<-GroupClusProfile_table %>%
  rename(
    GClass1=RMSE_GClass1,
    GClass2=RMSE_GClass2,
    GClass3=RMSE_GClass3
  )

GroupClusProfile_table<-GroupClusProfile_table %>%
  mutate(Factor = ifelse(duplicated(Factor), "", Factor))

##make table for papers
ft<-flextable(GroupClusProfile_table)

ft<-ft %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part="all") %>%
  valign(valign = "center", part = "all") %>%
  bold(j=1, bold=TRUE) %>%
  set_header_labels(
    Factor="Factor",
    Level="Level",
    GClass1="group-cluster 1",
    GClass2="group-cluster 2",
    GClass3="group-cluster 3"
  )

##read it to an empty word document
#doc<-read_docx()
#doc<-body_add_flextable(doc, ft)
#print(doc, target = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/Publications/Second Paper/Parts/Tables.docx")

##plotting 
##interacting ngroups, N_g, reg_coeff, and reliability
Analysis_GroupClusProfile<-AnalysisTableNEW %>%
  filter(Convergence==TRUE)


GroupClusProfile_summary<-Analysis_GroupClusProfile %>%
  group_by(ngroups, N_g, reg_coeff, reliability) %>% 
  summarise(RMSE_GClass1=mean(RMSE_GClass1),
            RMSE_GClass2=mean(RMSE_GClass2),
            RMSE_GClass3=mean(RMSE_GClass3, na.rm=T),
            .groups = "drop"
  )

GroupClusProfile_summary_long<-pivot_longer(data = GroupClusProfile_summary,
                                            cols = starts_with("RMSE_"),
                                            values_to = "RMSE",
                                            names_to = "GClass") %>%
  mutate(GClass = sub("RMSE_","",GClass)) %>%
  mutate(reg_size=case_when(
    reg_coeff==0.3 ~ "β size of 0.3",
    reg_coeff==0.4 ~ "β size of 0.4"
  )) %>%
  mutate(GClusters=case_when(
    GClass=="GClass1"~"group-cluster 1",
    GClass=="GClass2"~"group-cluster 2",
    GClass=="GClass3"~"group-cluster 3"
  )) %>%
  mutate(NumOfGroup=case_when(
    ngroups==24~"24 groups",
    ngroups==48~"48 groups"
  ))


ggplot(GroupClusProfile_summary_long, 
       aes(x=N_g, y = RMSE, color=factor(GClusters), shape=factor(GClusters), group=interaction(GClusters, reliability), linetype=reliability))+
  geom_point(size=2.5, na.rm = T)+
  geom_line(linewidth=0.72, na.rm = T)+
  facet_grid(rows = vars(NumOfGroup), cols = vars(reg_size))+
  scale_linetype_manual(values=c("low"="dashed", "high"="solid"))+
  labs(color="group-clusters",
       shape="group-clusters",
       linetype="reliability",
       x="within-group sample size",
       y="RMSE of profile")+
  scale_y_continuous(limits = c(0,0.32), breaks = seq(0,0.32, by=0.05))+
  #geom_hline(yintercept = 0.1, linetype="dashed", color="blue", linewidth=0.75)+
  theme_bw()+
  theme(legend.position = "bottom")
