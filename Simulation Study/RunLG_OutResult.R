library(lavaan)
library(MASS)
library(dplyr)
library(combinat)

RunLG_OutResult<-function(ConditionIdx, rep, nclus_group, nclus_ind){
  
  lg_syntax<-readLines(paste0(getwd(),"/Simulation Study/LGSyntax/template_lgs.lgs"))
  
  lg_syntax<-gsub(
    "^infile .*csv'.*$",
    paste0(
      "infile '", getwd(), "/Simulation Study/FactorScoreDF/FactorScoreDF_condition", ConditionIdx, "_Rep",
      rep,
      ".csv' delim = comma quote = double"
    ),
    lg_syntax
  )
  
  lg_syntax <- gsub(
    "GClass\\s+group\\s+nominal\\s+\\d+,",
    paste0("GClass  group nominal ", nclus_group, ","),
    lg_syntax
  )
  
  lg_syntax <- gsub(
    "Class\\s+nominal\\s+\\d+,",
    paste0("Class nominal ", nclus_ind, ","),
    lg_syntax
  )
  
  writeLines(lg_syntax,
             paste0(getwd(),"/Simulation Study/LGSyntax/LGSyntax_condition", ConditionIdx, "_Rep", rep, ".lgs"))
  
  ##specify the folder where latentGOLD is downloaded
  exe<-"C:/Program Files/LatentGOLD6.1/lg61.exe"
  
  ##specify the path and the file containing latentGOLD syntax
  inp<-paste0(getwd(),"/Simulation Study/LGSyntax/LGSyntax_condition", ConditionIdx, "_Rep", rep, ".lgs")
  
  ##specify the path and file that will print the output
  out<-paste0(getwd(),"/Simulation Study/LGResults/LGResults_c_", ConditionIdx, "_r_", rep, ".txt")
  
  exe <- normalizePath(exe, winslash = "/")
  inp <- normalizePath(inp, winslash = "/")
  out <- normalizePath(out, mustWork = FALSE, winslash = "/")
  command <- paste0(
    '"', exe, '"',
    ' "', inp, '"',
    " /b ", " /o ", '"', out, '"'
  )
  
  # call on LG to estimate the model and export output 
  system(command, wait = TRUE)
}
