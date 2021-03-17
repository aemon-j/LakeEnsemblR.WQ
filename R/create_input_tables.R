#'Create input tables
#'
#'Create csv files from the LakeEnsemblR dictionary file, where the user can enter values 
#'for selected parameters. Running the function with default arguments will give all parameters. 
#'
#'@param config_file file path; read groups of phytoplankton, zooplankton, etc. from here
#'@param folder_out path; in what folder should the files be placed
#'@param modules character; "all" to include everything, vector otherwise
#'@param processes character; "all" to include everything, vector otherwise
#'@param subprocesses character; "all" to include everything, vector otherwise
#'@param models_coupled character; options "GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#'                                       "Simstrat-AED2", "MyLake", "PCLake"
#'@param parameters character; "all" to include everything, vector otherwise
#'@param dict data.frame; the LakeEnsemblR_WQ dictionary 
#'
#'@importFrom configr read.config
#'
#'@examples
#'
#'@export

# # Test
# config_file = "LakeEnsemblR_WQ.yaml"
# folder_out = "."
# modules = "oxygen"
# processes = c("water_atmosphere_exchange", "water_sediment_exchange")
# subprocesses = "all"
# models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#                    "Simstrat-AED2", "MyLake", "PCLake")
# parameters = "all"

# Note: when we can actually build the package, the "dict"
#  argument can be removed so that the dictionary in the "data" folder is used

create_input_tables <- function(config_file, folder_out = ".", modules = "all", processes = "all",
                                subprocesses = "all",
                                models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET", 
                                                   "Simstrat-AED2", "MyLake", "PCLake"),
                                parameters = "all", dict){
  
  lst_config <- read.config(config_file)
  
  # We'll need to make a function for this
  models <- models_coupled
  models[models %in% c("GLM-AED2", "Simstrat-AED2")] <- "aed2"
  models[models == "GOTM-Selmaprotbas"] <- "selmaprotbas"
  models[models == "GOTM-WET"] <- "wet"
  models[models == "MyLake"] <- "mylake"
  models[models == "PCLake"] <- "pclake"
  
  input_table <- dict
  
  if(!identical(processes, "all")){
    input_table <- input_table[input_table$module %in% modules,]
  }
  
  if(!identical(processes, "all")){
    input_table <- input_table[input_table$process %in% processes,]
  }
  
  if(!identical(subprocesses, "all")){
    input_table <- input_table[input_table$subprocess %in% subprocesses,]
  }
  
  input_table <- input_table[input_table$model %in% models,]
  
  if(!identical(parameters, "all")){
    input_table <- input_table[input_table$parameter %in% parameters,]
  }
  
  # Add a "value" column to the input table. Users can enter their values here
  input_table$value <- ""
  
  input_table <- input_table[, c(1:5, 8, 7, 12, 11),]
  
  # Write input tables
  for(i in unique(input_table$module)){
    # phytoplankton, zooplankton, and fish can have multiple groups
    if(i %in% c("phytoplankton", "zooplankton", "fish")){
      groups <- names(lst_config[[i]][["groups"]])
    }else{
      groups <- i
    }
    
    for(j in groups){
      write.csv(input_table[input_table$module == i, -1],
                paste0(folder_out, "/", j, ".csv"),
                row.names = FALSE)
    }
  }
}
