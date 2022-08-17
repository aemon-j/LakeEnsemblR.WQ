#'Create input tables
#'
#'Create csv files from the LakeEnsemblR dictionary file, where the user can enter values
#'for selected parameters. Running the function with default arguments will print all
#'empty input files, whereas adding the argument "all" will print all parameters.
#'
#'@param folder path; where is the config_file located
#'@param config_file character; read groups of phytoplankton, zooplankton, etc. from here
#'@param folder_out path; in what folder should the files be placed
#'@param input character vector; for what parameters do you want to fill in values
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
# input = c("oxygen/initial_conditions",
#           "phytoplankton/growth/maximum_growth_rates")
# models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
#                    "Simstrat-AED2", "MyLake", "PCLake")

create_input_tables <- function(folder = ".", config_file, folder_out = folder, input = NULL,
                                models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET",
                                                   "Simstrat-AED2", "MyLake", "PCLake")){

  if (!file.exists(folder_out)) {
    cat("Creating new folder for input tables")
    dir.create(file.path(dirname(folder), folder_out))
  }

  lst_config <- read.config(file.path(folder, config_file))

  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function (x) tolower(x[length(x)]))
  names(wq_models) <- models_coupled

  input_table <- LakeEnsemblR_WQ_dictionary

  # Double rows for models that occur multiple times
  # Note: This is long and hard-to-understand code for something rather simple.
  # If we can simplify this, or at least put it in a separate function, I think
  # that'd be good.
  counts <- count(wq_models)
  counts$x <- as.character(counts$x)

  input_table$dupl_freq <- sapply(input_table$model,
                                  function (x) counts$freq[counts$x == x])
  input_table <- input_table[which(input_table$dupl_freq >0),]
  dupl_rows <- rep(1:nrow(input_table), times = input_table$dupl_freq)
  input_table <- input_table[dupl_rows,]

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

  # Select variables for input
  input_table$include <- FALSE

  # Loop through the "input" argument, and set corresponding values
  # to TRUE
  if(any(input == "all")){
    input_table$include <- TRUE
  }else{
    for(i in input){
      input_parts <- strsplit(i, "/")[[1]]

      # Note: currently not checking if the parameter occurs at all
      condition <- rep(TRUE, nrow(input_table))
      condition[input_table$module != input_parts[1]] <- FALSE
      if(length(input_parts) > 1L) condition[input_table$domain != input_parts[2]] <- FALSE
      if(length(input_parts) > 2L) condition[input_table$process != input_parts[3]] <- FALSE
      if(length(input_parts) > 3L) condition[input_table$subprocess != input_parts[4]] <- FALSE
      if(length(input_parts) > 4L) condition[input_table$model_coupled != input_parts[5]] <- FALSE
      if(length(input_parts) > 5L) condition[input_table$parameter != input_parts[6]] <- FALSE
      if(length(input_parts) > 6L){
        stop("Your input ", i, " was longer than six levels.",
             " You can only provide information up to parameter level.")
      }

      input_table$include[condition] <- TRUE
    }
  }

  input_table <- input_table[input_table$include,]

  # Only the models specified by the user input
  input_table <- input_table[input_table$model_coupled %in% models_coupled,]

  # Add a "value" column to the input table. Users can enter their values here
  if(nrow(input_table) > 0){
    input_table$value <- ""
  }else{
    input_table$value <- as.character()
  }

  input_table <- input_table[, c("module", "domain", "process", "subprocess", "model_coupled",
                                 "parameter", "default", "unit", "value", "note"),]

  # Write input tables
  # All modules that are set to use == TRUE
  modules <- names(lst_config)[!(names(lst_config) %in% c("models",
                                                          "config_files",
                                                          "run_settings",
                                                          "input",
                                                          "output"))]
  modules <- sapply(modules, function (x) if(lst_config[[x]][["use"]]) x)
  modules <- unlist(modules)

  for(i in modules){
    # phytoplankton, zooplankton, and fish can have multiple groups
    if(i %in% c("phytoplankton", "zooplankton", "fish", "macrophytes",
                "zoobenthos", "pathogens")){
      groups <- names(lst_config[[i]][["groups"]])
    }else{
      groups <- i
    }
    
    file.to.write <- input_table[input_table$module == i, -1]
    
    for(j in groups){
      
      if (any(names(lst_config[[i]]) == 'initial')){
        label <- i
        key <- names(lst_config[[i]][['initial']])
        
        for (p in key){
          idx <- which(file.to.write$subprocess == p)
          file.to.write$value[idx] = as.numeric(lst_config[[i]][['initial']][p])
          
          file.to.write$value <- as.double(file.to.write$value)
        }
        
      }

      file.to.write$value <- as.double(file.to.write$value)
      write.csv(file.to.write,
                paste0(folder_out, "/", j, ".csv"),
                row.names = FALSE)
    }
  }
}
