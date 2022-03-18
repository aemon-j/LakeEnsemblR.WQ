#'Set value in model-specific config file
#'
#'Set value in model-specific config file based on dictionary path
#'
#'@param config_file character; path to LakeEnsemblR_WQ config file
#'@param module character; 
#'@param group_name character; only for biological modules
#'@param group_position integer; only for biological modules
#'@param domain character;
#'@param process character; 
#'@param subprocess character; 
#'@param model_coupled character; options one of "GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#'                                       "Simstrat-AED2", "MyLake", "PCLake"
#'@param parameter character;
#'@param value character or numeric; what value to enter 
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

# # Test
# folder = "."
# config_file = "LakeEnsemblR_WQ.yaml"
# module = "phytoplankton"
# domain = "water"
# group_name = "diatoms"
# process = "growth"
# subprocess = "growth_rates"
# model_coupled = "GOTM-Selmaprotbas"
# parameter = "r0"
# value = 1.5
# verbose = TRUE

set_value_config <- function(config_file, module, group_name = NULL, group_position = NULL,
                             domain, process, subprocess, model_coupled, parameter, value,
                             folder, verbose = FALSE){
  
  model <- strsplit(model_coupled, "-")[[1]]
  model <- tolower(model[length(model)])
  
  # Check if arguments are allowed
  chck_args <- sapply(c("module", "domain", "process", "subprocess", "model", "parameter"),
                     function(x) get(x) %in% LakeEnsemblR_WQ_dictionary[[x]])
  if(!all(chck_args)){
    wrong_args <- c("module", "process",
                    "subprocess", "model", "parameter")[!chck_args]
    error_string <- unlist(lapply(wrong_args, function(x) paste0("\n", x, ": ", get(x))))
    
    stop("The following inputs are not found in the dictionary:",
         error_string)
  }
  
  row_dict <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == module &
                                           LakeEnsemblR_WQ_dictionary$domain == domain &
                                           LakeEnsemblR_WQ_dictionary$process == process &
                                           LakeEnsemblR_WQ_dictionary$subprocess == subprocess &
                                           LakeEnsemblR_WQ_dictionary$model == model &
                                           LakeEnsemblR_WQ_dictionary$parameter == parameter,]
  # Second argument check; see if all combinations are possible
  if(nrow(row_dict) == 0){
    stop("The parameter was not found in the dictionary for this combination of arguments.")
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
    
    aed_config[[path_parts[1]]][[path_parts[2]]][group_position] <- value
    write_nml(aed_config, aed_config_path)
    
  }else if(model_coupled == "GOTM-Selmaprotbas" | model_coupled == "GOTM-WET"){
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    path_parts[path_parts == "{group_name}"] <- group_name
    names(path_parts) <- paste0("key", 1:length(path_parts))
    
    path_parts <- c(path_parts,
                    "value" = value,
                    "file" = file.path(folder, model_config),
                    "verbose" = verbose)
    arglist <- split(path_parts, names(path_parts)) # Turn into named list
    do.call(input_yaml_multiple, args = arglist)
  }else if(model_coupled == "MyLake"){
    if(!is.null(group_name)){
      if(group_position == 1L){
        # MyLake can only have one phytoplankton group
        # so skip if it's not the first group
        # Note: later we can add something in the input that
        # users could also use a different group than the first
        # for MyLake input? 
        # Name: mylake_config
        load(file.path(folder, model_config))
        
        path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
        if(length(path_parts) == 1L){
          
          mylake_config[[path_parts]] <- matrix(value,
                                                nrow = nrow(mylake_config[[path_parts]]),
                                                ncol = ncol(mylake_config[[path_parts]]))
          
        }else{
          # If length is 2, we have to find the index of the parameter
          # value in the ".names" vector
          names_par_list <- mylake_config[[paste0(path_parts[1], ".names")]]
          names_par_list <- sapply(names_par_list, "[[", 1)
          ind_par <- which(names_par_list == path_parts[2])
          
          mylake_config[[path_parts[1]]][ind_par] <- value
        }
        
        save(mylake_config, file = file.path(folder, model_config))
      }
    }
  }else if(model_coupled == "PCLake"){
    
    path_parts <- strsplit(row_dict[1, "path"], "/")[[1]]
    if(path_parts[1] == "parameters"){
      file_name <- "parameters.txt"
    }else if(path_parts[1] == "initialstates"){
      file_name <- "initialstates.txt"
    }else{
      stop("First entry PCLake parameter in dictionary should be ",
           "'parameters' or 'initialstates'")
    }
    
    file_name <- file.path(folder, dirname(model_config), file_name)
    
    pclake_config <- read.table(file_name,
                                sep = "\t",
                                header = TRUE,
                                fill = TRUE,
                                stringsAsFactors = FALSE)
    
    row_num <- which(pclake_config$sName == paste0("_",
                                                   path_parts[2],
                                                   "_"))
    
    if(length(row_num) == 0L){
      stop("Parameter ", parameter,
                                   " not found in PCLake config file")
    }
    
    pclake_config[row_num, "sSet1"] <- value
    
    # Ensure that table headers remain the same as in the original file
    inds <- sapply(c("X.1", "Open.water", "X_InclTran_",
                     "X_InclPrim_", "X_InclPhytS_", "X_InclBed_",
                     "X_InclWeb_", "X_InclMarsh_"),
                   grep, names(pclake_config))
    names(pclake_config) <- replace(names(pclake_config), inds,
                                    c("-1", "Open water", "_InclTran_",
                                      "_InclPrim_", "_InclPhytS_", "_InclBed_",
                                      "_InclWeb_", "_InclMarsh_"))
    
    write.table(pclake_config,
                file = file_name,
                row.names = FALSE,
                quote = FALSE,
                sep = "\t")
    
  }
}
