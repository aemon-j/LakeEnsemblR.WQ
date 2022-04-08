#'Inputs values into a PCLake config file
#'@description
#'Inputs values into a PCLake config file
#'
#'@param file filepath; 
#'@param par_list list; parameter names without underscores and corresponding
#' value to enter
#'@param column character; column name to change in file. defaults to sSet1
#'@param verbose logical; print changed parameters to screen
#'
#'@examples
#'
#'@export

input_pclakeconfig <- function(file, par_list,
                               column = "sSet1", verbose = FALSE){
  
  if(!file.exists(file)){
    stop("Cannot find file ", file)
  }
  
  cnfg <- read.table(file,
                     sep = "\t",
                     header = TRUE,
                     fill = TRUE,
                     stringsAsFactors = FALSE)
  
  for(i in names(par_list)){
    ind <- which(cnfg[["sName"]] == paste0("_", i, "_"))
    
    if(length(ind) == 0L){
      stop("Could not find parameter ", i, " in ", file)
    }else if(length(ind) > 1L){
      stop("Parameter ", i, " found multiple times in ", file)
    }
    
    old_val <- cnfg[ind, column]
    cnfg[ind, column] <- par_list[[i]]
    
    if(verbose & !identical(old_val, par_list[[i]])){
      message("PCLake: replaced ", i, ": ", old_val, " by ", par_list[[i]])
    }
  }
  
  ind_colmax <- which(names(cnfg) == "Duflow")
  cnfg <- cnfg[, 1:ind_colmax]
  ind_minus1 <- which(names(cnfg) == "X.1")
  names(cnfg)[ind_minus1] <- "-1"
  names(cnfg) <- gsub("^X", "", names(cnfg))
  
  write.table(cnfg, file,
              sep = "\t", quote = FALSE, row.names = FALSE)
}
