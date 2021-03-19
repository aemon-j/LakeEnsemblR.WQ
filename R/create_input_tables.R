#'Create input tables
#'
#'Create csv files from the LakeEnsemblR dictionary file, where the user can enter values 
#'for selected parameters. Running the function with default arguments will give all parameters. 
#'
#'@param folder path; where is the config_file located
#'@param config_file character; read groups of phytoplankton, zooplankton, etc. from here
#'@param folder_out path; in what folder should the files be placed
#'@param modules character; "all" to include everything, vector otherwise
#'@param processes character; "all" to include everything, vector otherwise
#'@param subprocesses character; "all" to include everything, vector otherwise
#'@param models_coupled character; options "GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#'                                       "Simstrat-AED2", "MyLake", "PCLake"
#'@param parameters character; "all" to include everything, vector otherwise
#'
#'@importFrom configr read.config
#'@importFrom plyr count
#'@importFrom stringr str_extract
#'
#'@examples
#'
#'@export

# # Test
# folder = "."
# config_file = "LakeEnsemblR_WQ.yaml"
# folder_out = "."
# modules = "oxygen"
# processes = c("water_atmosphere_exchange", "water_sediment_exchange")
# subprocesses = "all"
# models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#                    "Simstrat-AED2", "MyLake", "PCLake")
# parameters = "all"

create_input_tables <- function(folder = ".", config_file, folder_out = folder, modules = "all", processes = "all",
                                subprocesses = "all",
                                models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET", 
                                                   "Simstrat-AED2", "MyLake", "PCLake"),
                                parameters = "all"){
  
  lst_config <- read.config(file.path(folder, config_file))
  
  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function (x) tolower(x[length(x)]))
  names(wq_models) <- models_coupled
  
  input_table <- LakeEnsemblR_WQ_dictionary
  
  # Select modules
  if(!identical(modules, "all")){
    input_table <- input_table[input_table$module %in% modules,]
  }
  
  # Select processes
  if(!identical(processes, "all")){
    input_table <- input_table[input_table$process %in% processes,]
  }
  
  # Select subprocesses
  if(!identical(subprocesses, "all")){
    input_table <- input_table[input_table$subprocess %in% subprocesses,]
  }
  
  # Select models
  # One entry for every model
  # Note: This is long and hard-to-understand code for something rather simple.
  # If we can simplify this, or at least put it in a separate function, I think 
  # that'd be good.
  input_table <- input_table[input_table$model %in% wq_models,]
  
  counts <- count(wq_models)
  counts$x <- as.character(counts$x)
  
  input_table$dupl_freq <- sapply(input_table$model,
                                  function (x) counts$freq[counts$x == x])
  if(nrow(input_table) > 0){
    dupl_rows <- rep(1:nrow(input_table), times = input_table$dupl_freq)
    input_table <- input_table[dupl_rows,]
  }
  
  # It's necessary to turn the model column for the duplicated rows into
  # a unique name to match the right coupled model. 
  while(any(duplicated(wq_models))){
    wq_models[duplicated(wq_models)] <- paste0(wq_models[duplicated(wq_models)], ".1")
  }
  
  model_addition <- str_extract(row.names(input_table), "\\.[\\d*]")
  model_addition[is.na(model_addition)] <- ""
  input_table$model_coupled <- paste0(input_table$model, model_addition)
  input_table$model_coupled <- sapply(input_table$model_coupled,
                                      function (x) names(wq_models)[wq_models == x])
  
  # Select parameters
  if(!identical(parameters, "all")){
    input_table <- input_table[input_table$parameter %in% parameters,]
  }
  
  # Add a "value" column to the input table. Users can enter their values here
  if(nrow(input_table) > 0){
    input_table$value <- ""
  }else{
    input_table$value <- as.character()
  }
  
  input_table <- input_table[, c(1:3, 13, 5, 8, 7, 14, 11),]
  
  # Write input tables
  for(i in modules){
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
