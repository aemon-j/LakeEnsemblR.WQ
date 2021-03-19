#'Set value in model-specific config file
#'
#'Set value in model-specific config file based on dictionary path
#'
#'@param config_file character; path to LakeEnsemblR_WQ config file
#'@param module character; 
#'@param group_name character; only for biological modules
#'@param group_position integer; only for biological modules
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
# module = "phytoplankton"
# group_name = "diatoms"
# process = "growth"
# subprocess = "maximum_growth_rates"
# model_coupled = "GOTM-Selmaprotbas"
# parameter = "r0"
# value = 1.5
# verbose = TRUE

set_value_config <- function(config_file, module, group_name = NULL, group_position = NULL,
                             process, subprocess, model_coupled, parameter, value, dict, folder,
                             verbose = FALSE){
  
  model <- strsplit(model_coupled, "-")[[1]]
  model <- tolower(model[length(model)])
  
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
  
  # MyLake and PCLake not yet implemented
  if(model == "mylake" | model == "pclake"){
    stop("MyLake and PCLake not yet implemented.")
  }
  
  lst_config <- read.config(file.path(folder, config_file))
  model_config <- lst_config[["config_files"]][[model_coupled]]
  
  if(model_coupled == "GLM-AED2" | model_coupled == "Simstrat-AED2"){
    # Different files for phytoplankton and zooplankton
    if(!(module %in% c("phytoplankton", "zooplankton"))){
      aed_config_path <- file.path(folder, model_config)
    }else if(module == "phytoplankton"){
      aed_config_path <- file.path(folder, dirname(model_config), "aed2_phyto_pars.nml")
    }else if(module == "zooplankton"){
      aed_config_path <- file.path(folder, dirname(model_config), "aed2_zoop_pars.nml")
    }
    aed_config <- read_nml(aed_config_path)
    
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    if(length(path_parts) != 2L){
      stop("Path for AED2 parameter does not consist of two parts; needs ",
           "to be section/par_name")
    }
    
    if(is.null(group_position)){
      group_position = 1L
    }
    
    if(is.numeric(value)){
      value <- value * row_dict[1, "conversion"]
    }
    
    aed_config[[path_parts[1]]][[path_parts[2]]][group_position] <- value
    write_nml(aed_config, aed_config_path)
    
  }else if(model_coupled == "GOTM-Selmaprotbas" | model_coupled == "GOTM-WET"){
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    path_parts[path_parts == "{group_name}"] <- group_name
    names(path_parts) <- paste0("key", 1:length(path_parts))
    
    path_parts <- c(path_parts,
                    "value" = value * row_dict[1, "conversion"],
                    "file" = file.path(folder, model_config),
                    "verbose" = verbose)
    arglist <- split(path_parts, names(path_parts)) # Turn into named list
    do.call(input_yaml_multiple, args = arglist)
  }
  
}
