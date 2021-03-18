#'Set value in model-specific config file
#'
#'Set value in model-specific config file based on dictionary path
#'
#'@param config_file character; path to LakeEnsemblR_WQ config file
#'@param module character; 
#'@param process character; 
#'@param subprocess character; 
#'@param model_coupled character; options one of "GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#'                                       "Simstrat-AED2", "MyLake", "PCLake"
#'@param parameter character;
#'@param value character or numeric; what value to enter 
#'@param dict data.frame; the LakeEnsemblR_WQ dictionary 
#'@param folder path; relative to what folder 
#'@param verbose boolean; print output to console
#'
#'@examples
#'
#'@importFrom configr read.config
#'@importFrom LakeEnsemblR input_yaml_multiple
#'@importFrom glmtools read_nml write_nml
#'
#'@export

# Note: when we can actually build the package, the "dict"
#  argument can be removed so that the dictionary in the "data" folder is used

# # Test
# folder = "."
# config_file = "LakeEnsemblR_WQ.yaml"
# module = "oxygen"
# process = "initial_conditions"
# subprocess = "initial_water"
# model_coupled = "GOTM-Selmaprotbas"
# parameter = "o2"
# value = 42
# verbose = TRUE

set_value_config <- function(config_file, module, process, subprocess, model_coupled,
                          parameter, value, dict, folder, verbose = FALSE){
  
  if(model_coupled == "GLM-AED2"){
    model <- "aed2"
  }else if(model_coupled == "GOTM-Selmaprotbas"){
    model <- "selmaprotbas"
  }else if(model_coupled == "GOTM-WET"){
    model <- "wet"
  }else if(model_coupled == "Simstrat-AED2"){
    model <- "aed2"
  }else if(model_coupled == "MyLake"){
    model <- "mylake"
  }else if(model_coupled == "PCLake"){
    model <- "pclake"
  }else{
    model <- model_coupled
  }
  
  # Check if arguments are allowed
  chck_args <- sapply(c("module", "process", "subprocess", "model", "parameter"),
                     function(x) get(x) %in% dict[[x]])
  if(!all(chck_args)){
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
  
  # phytoplankton and zooplankton are done very differently in most of the models
  if(module == "phytoplankton" | module == "zooplankton"){
    stop("Phytoplankton and zooplankton not yet implemented.")
  }
  
  # MyLake and PCLake not yet implemented
  if(model == "mylake" | model == "pclake"){
    stop("MyLake and PCLake not yet implemented.")
  }
  
  lst_config <- read.config(file.path(folder, config_file))
  model_config <- lst_config[["config_files"]][[model_coupled]]
  
  if(model_coupled == "GLM-AED2" | model_coupled == "Simstrat-AED2"){
    aed_config <- read_nml(file.path(folder, model_config))
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    if(length(path_parts) != 2L){
      stop("Path for AED2 parameter does not consist of two parts; needs ",
           "to be section/par_name")
    }
    
    aed_config[[path_parts[1]]][[path_parts[2]]] <- value * row_dict[1, "conversion"]
    write_nml(aed_config, file.path(folder, model_config))
    
  }else if(model_coupled == "GOTM-Selmaprotbas" | model_coupled == "GOTM-WET"){
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    names(path_parts) <- paste0("key", 1:length(path_parts))
    path_parts <- c(path_parts,
                    "value" = value * row_dict[1, "conversion"],
                    "file" = file.path(folder, model_config),
                    "verbose" = verbose)
    arglist <- split(path_parts, names(path_parts)) # Turn into named list
    do.call(input_yaml_multiple, args = arglist)
  }
  
}
