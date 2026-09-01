library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

Out_RegCoeff<-function(ConditionIdx, rep, nclus_group, nclus_ind, reg_coeff){
  
  ##read the results in
  lg_lines<-readLines(paste0(getwd(),"/Simulation Study/LGResults/LGResults_c_", ConditionIdx, "_r_", rep, ".txt"),
                      encoding = "latin1")
  
  #the starting index for the rows we want to extract (at the end, we can add 1 to skip the empty line)
  #rp stands for regression parameters
  rp_start_idx<-grep("^Regression Parameters", lg_lines)+2+nclus_group+1
  
  #the ending index for the last row we want to extract 
  #(at the end, we need to deduct 2 lines including the empty line and the line after the empty line)
  rp_end_idx<-rp_start_idx+grep("^\\s*$", lg_lines[rp_start_idx:length(lg_lines)])[1]-2
  #
  ##extract the lines for regression coefficients
  rp_lines<-lg_lines[rp_start_idx:rp_end_idx]
  
  #make a data frame to take out the values we need before making the matrix
  rp_parsed <- do.call(
    rbind,
    lapply(rp_lines, function(x) {
      parts <- strsplit(x, "\t")[[1]]
      
      data.frame(
        predictor = parts[3],                        # F1, F2, F3, F4
        class     = as.integer(gsub("Class\\(|\\)", "", parts[5])),
        beta      = as.numeric(parts[6]),
        stringsAsFactors = FALSE
      )
    })
  )
  
  ##turn it into the matrix we want:
  predictors <- sort(unique(rp_parsed$predictor))
  classes <- seq_len(nclus_ind)
  
  beta_mat <- matrix(
    NA_real_,
    nrow = length(predictors),
    ncol = length(classes),
    dimnames = list(
      paste0("B", seq_along(predictors)),
      paste0("EstClass", classes)
    )
  )
  
  for (i in seq_len(nrow(rp_parsed))) {
    row_idx <- match(rp_parsed$predictor[i], predictors)
    col_idx <- rp_parsed$class[i]
    beta_mat[row_idx, col_idx] <- rp_parsed$beta[i]
  }
  
  ##all possible permutation index
  IndClus_perm<-permn(x=nclus_ind)
  
  ##initialize the empty vector to store the results:
  beta_mat_permuted<-vector(mode="list",length = length(IndClus_perm))
  #SquaredError_permuted<-numeric(length(IndClus_perm))
  RootMeanError_permuted<-numeric(length(IndClus_perm))
  
  
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
  
  ##loop it across all the permutation:
  
  for(p in 1:length(IndClus_perm)){
    
    beta_mat_permuted[[p]]<-beta_mat[,IndClus_perm[[p]]]
    #SquaredError_permuted[p]<-sum((ori_beta_mat-beta_mat_permuted[[p]])^2) ##might need to change to Mean Squared Error later
    RootMeanError_permuted[p]<-sqrt(mean((ori_beta_mat-beta_mat_permuted[[p]])^2))
    
  }
  
  ##find the permutation with the smallest Root Mean Squared Error
  #beta_mat_bestPerm<-beta_mat_permuted[[which.min(SquaredError_permuted)]]
  beta_mat_bestPerm<-beta_mat_permuted[[which.min(RootMeanError_permuted)]]
  
  #IndClus_bestPerm_sq<-IndClus_perm[[which.min(SquaredError_permuted)]]
  IndClus_bestPerm<-IndClus_perm[[which.min(RootMeanError_permuted)]]
  
  return(list(beta_mat=beta_mat_bestPerm,
              IndBestPerm=IndClus_bestPerm))
  
}
