#'Set value in model-specific config file
#'
#'Set value in model-specific config file based on dictionary path
#'
#'@param module character; 
#'@param process character; 
#'@param subprocess character; 
#'@param model character; options c("aed2", "selmaprotbas", "wet", "mylake", "pclake")
#'@param parameter character; 
#'@param dict data.frame; the LakeEnsemblR_WQ dictionary 
#'@examples
#'
#'@export

# Note: when we can actually build the package, the "dict"
#  argument can be removed so that the dictionary in the "data" folder is used

set_value_config <- function(module, process, subprocess, model,
                          parameter, dict){
  
  # Check if arguments are allowed
  chck_args <- sapply(c("module", "process", "subprocess", "model", "parameter"),
                     function(x) get(x) %in% dict[[x]])
  if(any(isFALSE(chck_args))){
    wrong_args <- c("module", "process",
                    "subprocess", "model", "parameter")[!chck_args]
    error_string <- unlist(lapply(wrong_args, function(x) paste0("\n", x, ": ", get(x))))
    
    stop("The following inputs are not found in the dictionary:",
         error_string)
  }
  
  row_dict <- dict[dict$module == module &
                   dict$process == process &
                   dict$subprocess == subprocess &
                   dict$model == model &
                   dict$parameter == parameter,]
  # Second argument check; see if all combinations are possible
  if(nrow(row_dict) == 0){
    stop("The parameter was not found in the dictionary for this combination of arguments.")
  }
  
  
  
}