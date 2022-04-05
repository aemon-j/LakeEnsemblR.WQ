#'Export settings in LakeEnsemblR_WQ configuration file
#'
#'Export settings from configuration file to model-specific configuration files
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'@param verbose boolean; print changed parameters on screen
#'
#'@examples
#'
#'@importFrom configr read.config
#'
#'@export

export_config <- function(config_file, folder = ".", verbose = FALSE,
                          convert_from_lakeensemblr = TRUE,
                          ler_config_file = "LakeEnsemblR.yaml"){
  
  if(convert_from_lakeensemblr){
    # LakeEnsemblR::export_config has been run beforehand
    # Convert folders and activate wq settings
    convert_ler_to_lerwq(ler_config_file = ler_config_file,
                         lerwq_config_file = config_file,
                         folder = folder,
                         verbose = verbose)
  }
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file))
  
  modules <- names(lst_config)
  modules <- modules[!(modules %in% c("models", "config_files", "run_settings",
                                      "input", "output"))]
  
  # Set up the model-specific config files, with right amount
  # of groups for phytoplankton etc., and default values.
  set_up_configs(config_file, folder = folder)
  
  # Reads the nutrient inputs - through inflows and additional sources
  export_inputs(config_file, folder = folder, verbose = verbose,
                ler_config_file = ler_config_file)
  
  # Loop through the modules
  for(i in modules){
    
    if(!lst_config[[i]][["use"]]){
      disable_module(config_file = config_file, folder = folder,
                     module = i)
    }else{
      if(!(i %in% c("phytoplankton", "zooplankton", "fish"))){
        input_file_paths <- file.path(folder, lst_config[[i]][["par_file"]])
      }else{
        input_file_paths <- sapply(names(lst_config[[i]][["groups"]]), function(x){
          lst_config[[i]][["groups"]][[x]][["par_file"]]
        })
        input_file_paths <- file.path(folder, input_file_paths)
        names(input_file_paths) <- names(lst_config[[i]][["groups"]])
      }
      
      # Read the file(s)
      for(j in seq_len(length(input_file_paths))){
        input_file <- read.csv(input_file_paths[j], stringsAsFactors = FALSE)
        
        sapply(seq_len(nrow(input_file)), function (x){
          set_value_config(config_file = config_file,
                           module = i,
                           group_name = names(input_file_paths)[j],
                           group_position = j,
                           domain = input_file[x, "domain"],
                           process = input_file[x, "process"],
                           subprocess = input_file[x, "subprocess"],
                           model_coupled = input_file[x, "model_coupled"],
                           parameter = input_file[x, "parameter"],
                           value = input_file[x, "value"],
                           folder = folder,
                           verbose = verbose)
        })
        
      }
    }
  }
  
  set_coupling(config_file, folder = folder)
}
