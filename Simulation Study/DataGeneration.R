library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

DataGeneration<-function(model, reg_coeff, coeff_direct,
                         ngroups, N_g, nclus_ind, nclus_group, 
                         balance,  
                         reliability,NonInvG, NonInvSize){
  
  ##first extract observed and latent variable names
  par_table<-lavaanify(model)
  lat_var<-lavNames(par_table, type = "lv")
  obs_var<-lavNames(par_table, type = "ov")
  m<-length(lat_var) ##number of latent variables
  p<-length(obs_var) ##number of observed items
  
  ##specify which one is exogenous, and endogenous 2 variable
  endog<-lat_var[(lat_var %in% par_table$lhs[par_table$op=="~"])]
  exog<-lat_var[(lat_var %in% par_table$rhs[par_table$op=="~"])]
  #
  ##(optional) reorder if necessary
  lat_var<-c(exog,endog)
  
  ##regression coefficients for each individual-level cluster
  B1<-numeric(nclus_ind)
  B2<-numeric(nclus_ind)
  B3<-numeric(nclus_ind)
  B4<-numeric(nclus_ind)
  
  #get regression parameters for each cluster
  if(coeff_direct=="same"){
    #cluster 1:
    B1[1]<-0
    B2[1]<-reg_coeff
    B3[1]<-reg_coeff
    B4[1]<-reg_coeff
    
    #cluster 2:
    B1[2]<-reg_coeff
    B2[2]<-0
    B3[2]<-reg_coeff
    B4[2]<-reg_coeff
    
    if (nclus_ind==4){
      #cluster 3:
      B1[3]<-reg_coeff
      B2[3]<-reg_coeff
      B3[3]<-0
      B4[3]<-reg_coeff
      
      #cluster 4:
      B1[4]<-reg_coeff
      B2[4]<-reg_coeff
      B3[4]<-reg_coeff
      B4[4]<-0
    }
  }
  
  if(coeff_direct=="opposite"){
    #cluster 1:
    B1[1]<-(-reg_coeff)
    B2[1]<-reg_coeff
    B3[1]<-reg_coeff
    B4[1]<-reg_coeff
    
    #cluster 2:
    B1[2]<-reg_coeff
    B2[2]<-(-reg_coeff)
    B3[2]<-reg_coeff
    B4[2]<-reg_coeff
    
    if (nclus_ind==4){
      #cluster 3:
      B1[3]<-reg_coeff
      B2[3]<-reg_coeff
      B3[3]<-(-reg_coeff)
      B4[3]<-reg_coeff
      
      #cluster 4:
      B1[4]<-reg_coeff
      B2[4]<-reg_coeff
      B3[4]<-reg_coeff
      B4[4]<-(-reg_coeff)
    }
  }
  
  ##set up the regression coefficient beta matrix for each individual-level cluster
  beta<-array(data = 0, dim = c(m,m,nclus_ind),dimnames = list(lat_var, lat_var))
  
  beta[endog,exog[1],]<-B1
  beta[endog,exog[2],]<-B2
  beta[endog,exog[3],]<-B3
  beta[endog,exog[4],]<-B4
  
  ##set up the regression coefficient array for all the group-individualCluster combination layers
  beta_GroupIndClus<-beta[,,rep(1:nclus_ind, times=ngroups)]
  
  #random sample from uniform distribution for the group-specific exogenous factor variances/covariances ψ_gk
  exog_var1<-rep(runif(n=ngroups, min = 0.75, max=1.25),each=nclus_ind)
  exog_var2<-rep(runif(n=ngroups, min = 0.75, max=1.25),each=nclus_ind)
  exog_var3<-rep(runif(n=ngroups, min = 0.75, max=1.25),each=nclus_ind)
  exog_var4<-rep(runif(n=ngroups, min = 0.75, max=1.25),each=nclus_ind)
  
  exog_cov12<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  exog_cov13<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  exog_cov14<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  exog_cov23<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  exog_cov24<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  exog_cov34<-rep(runif(n=ngroups, min=-0.3, max=0.3), each=nclus_ind)
  
  
  if (reg_coeff == 0.3){
    endog_var<-rep(runif(n=nclus_ind, min=0.65, max=0.75), times=ngroups)
  } else if (reg_coeff == 0.4) {
    endog_var<-rep(runif(n=nclus_ind, min=0.45, max=0.55), times=ngroups)
  }
  
  psi_gk<-array(data = diag(m), dim=c(m,m,ngroups*nclus_ind), dimnames = list(lat_var, lat_var))
  
  #variances of all factors
  psi_gk[exog[1],exog[1],]<-exog_var1
  psi_gk[exog[2],exog[2],]<-exog_var2
  psi_gk[exog[3],exog[3],]<-exog_var3
  psi_gk[exog[4],exog[4],]<-exog_var4
  psi_gk[endog, endog,]<-endog_var
  
  #covariances among the exogenous factors
  psi_gk[exog[1],exog[2],]<-psi_gk[exog[2],exog[1],]<-exog_cov12
  psi_gk[exog[1],exog[3],]<-psi_gk[exog[3],exog[1],]<-exog_cov13
  psi_gk[exog[1],exog[4],]<-psi_gk[exog[4],exog[1],]<-exog_cov14
  psi_gk[exog[2],exog[3],]<-psi_gk[exog[3],exog[2],]<-exog_cov23
  psi_gk[exog[2],exog[4],]<-psi_gk[exog[4],exog[2],]<-exog_cov24
  psi_gk[exog[3],exog[4],]<-psi_gk[exog[4],exog[3],]<-exog_cov34
  
  ##measurement model parameters Λ matrix:
  if(reliability=="high"){
    load<-0.6
  } else if (reliability == "low"){
    load<-0.4
  }
  
  loadings<-sqrt(load)
  
  ##random selecting groups to have non-invariant lambda matrix
  Lambda<-array(data = 0, dim=c(p,m,ngroups), dimnames=list(obs_var, lat_var))
  
  NonInvIdx<-sort(sample(x=1:ngroups, size=ngroups*NonInvG, replace = FALSE))
  
  ##Invariant Lambda matrix:
  LambdaInv<-matrix(data = rep(c(1,rep(loadings,times=4),rep(0,times=p)), times=m)[1:(p*m)],
                    nrow = p,
                    ncol = m)
  
  NonInvItems<-2
  
  for(g in 1:ngroups){
    if (!c(g %in% NonInvIdx)){
      Lambda[,,g]<-LambdaInv
    }
    if(g %in% NonInvIdx){
      
      
      NonInvariantLoadings <- sample(x = c(runif(100, min = (loadings - NonInvSize) - .1, max = (loadings - NonInvSize) + .1), 
                                           runif(100, min = (loadings + NonInvSize) - .1, max = (loadings + NonInvSize) + .1)),
                                     size = NonInvItems*5) ##5 for the basic model with 5 factors
      
      LambdaNonInv <- matrix(data = c(1, NonInvariantLoadings[1:NonInvItems], rep(loadings, (4 - NonInvItems)), rep(0, p),
                                      1, NonInvariantLoadings[(NonInvItems + 1):(NonInvItems*2)], rep(loadings, (4 - NonInvItems)), rep(0, p),
                                      1, NonInvariantLoadings[((NonInvItems*2) + 1):(NonInvItems*3)], rep(loadings, (4 - NonInvItems)), rep(0, p),
                                      1, NonInvariantLoadings[((NonInvItems*3) + 1):(NonInvItems*4)], rep(loadings, (4 - NonInvItems)), rep(0, p),
                                      1, NonInvariantLoadings[((NonInvItems*4) + 1):(NonInvItems*5)], rep(loadings, (4 - NonInvItems))),
                             nrow = p, ncol = m)
      
      Lambda[,,g]<-LambdaNonInv
    }
  }
  
  Lambda_GroupIndClus<-Lambda[,,rep(1:ngroups, each=nclus_ind)]
  
  #measurement model parameters θ for each group:
  Theta<-array(data = 0,
               dim = c(p,p,ngroups))
  
  for(g in 1:ngroups){
    Theta[,,g]<-diag(runif(n=p, min = ((1-load)-0.1), max = ((1-load)+0.1)))
  }
  
  #prepare the Theta for all 60 GroupIndClus layers
  Theta_GroupIndClus<-Theta[,,rep(1:ngroups,each=nclus_ind)]
  
  ##create sample covariance matrix Σ for each layer
  Sigma<-array(data = 0, dim = c(p,p,(ngroups*nclus_ind)))
  
  #identity matrix to fit the formula:
  I<-diag(m)
  
  for(i in 1:(ngroups*nclus_ind)){
    Sigma[,,i]<-Lambda_GroupIndClus[,,i] %*% solve(I-beta_GroupIndClus[,,i]) %*% psi_gk[,,i] %*% t(solve(I-beta_GroupIndClus[,,i])) %*% t(Lambda_GroupIndClus[,,i])+Theta_GroupIndClus[,,i]
  }
  
  ##first specify the number of individuals belonging to each ind-cluster (country-level cluster)
  ##probably not homogenous at the moment
  
  if ((nclus_ind==4) & (nclus_group==2)){
    CntryCluster<-list(
      c(N_g*0.8, N_g*0.1, N_g*0.05, N_g*0.05),
      c(N_g*0.1, N_g*0.1, N_g*0.4, N_g*0.4)
    )
  } else if((nclus_ind==4) & (nclus_group==3)){
    CntryCluster<-list(
      c(N_g*0.8, N_g*0.1, N_g*0.05, N_g*0.05),
      c(N_g*0.1, N_g*0.1, N_g*0.4, N_g*0.4),
      c(N_g*0.25, N_g*0.25, N_g*0.25, N_g*0.25)
    )
  }
  
  
  ##Country-level cluster membership
  
  if (balance=="balanced"){
    GperK<-rep(x=1:nclus_group, each=(ngroups/nclus_group))
  } else if (balance == "unbalanced"){
    largest<-ngroups*0.75
    smaller<-ngroups-largest
    GperK<-c(rep(x=1, times=largest), rep(2:nclus_group, each=(smaller/(nclus_group-1)))) ##potential problem here
  }
  
  ##sample data
  SimData<-list()
  
  for(g in 1:ngroups){
    
    WG_distribution<-CntryCluster[[GperK[g]]]
    
    for (k in 1:nclus_ind){
      n_k<-WG_distribution[k]
      
      idx<-(g-1)*nclus_ind+k
      
      if (n_k == 0){
        next
      }
      
      tmp<-mvrnorm(n=n_k,
                   mu=rep(0,p),
                   Sigma = Sigma[,,idx])
      
      SimData[[idx]]<-data.frame(group=g,
                                 IndClus=k,
                                 tmp)
    }
  }
  
  SimData<-do.call(rbind,SimData)
  
  colnames(SimData)[-(1:2)]<-obs_var
  
  return(SimData)
  
}
