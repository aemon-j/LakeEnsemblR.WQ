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

# # Test
# config_file = "LakeEnsemblR_WQ.yaml"
# folder = "."
# verbose = F

export_config <- function(config_file, folder = ".", verbose = FALSE){
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file)) 
  
  modules <- names(lst_config)
  modules <- modules[!(modules %in% c("models", "config_files", "bio-feedback",
                                      "output"))]
  
  # Loop through the modules
  for(i in modules){
    
    if(!lst_config[[i]][["use"]]){
      disable_module(config_file = config_file, folder = folder,
                     module = i)
    }else{
      # Important: in case the module is phytoplankton/zooplankton/fish
      #  things will be different! Multiple groups and different notation.
      #  For AED2, we'll need to write the names of the groups as well (pd%p_name)
      #  And group_name and group_position need to be passed into set_value_config
      
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
        
        warning("MyLake and PCLake not yet implemented.")
        input_file <- input_file[input_file$model_coupled != "MyLake" &
                                   input_file$model_coupled != "PCLake",]
        
        
        sapply(seq_len(nrow(input_file)), function (x){
          set_value_config(config_file = config_file,
                           module = i,
                           group_name = names(input_file_paths)[j],
                           group_position = j,
                           process = input_file[x, "process"],
                           subprocess = input_file[x, "subprocess"],
                           model_coupled = input_file[x, "model_coupled"],
                           parameter = input_file[x, "parameter"],
                           value = input_file[x, "value"],
                           folder = folder,
                           verbose = verbose)
        })
        
        # Not yet implemented; any values in the module that are not mentioned
        # in the input file, should be set to their default!
      }
    }
  }
}
