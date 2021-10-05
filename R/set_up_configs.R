#'Sets up the model-specific configuration files
#'
#'Sets up the model-specific config files with the right groups
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'
#'@examples
#'
#'@importFrom configr read.config write.config
#'@importFrom glmtools write_nml
#'
#'@export

# Note; this will set up files without comments
# If we add a "comment" column to the dictionary,
# we could include comments

# # Test
# config_file = "LakeEnsemblR_WQ.yaml"
# folder = "."

set_up_configs <- function(config_file, folder = "."){
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file)) 
  
  models_coupled <- lst_config[["models"]]
  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function (x) tolower(x[length(x)]))
  
  for(i in seq_len(length(models_coupled))){
    
    if(models_coupled[i] == "GOTM-Selmaprotbas"){
      dict <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$model == "selmaprotbas",]
      
      lst <- list()
      
      # Biogeochemistry
      lst[["instances"]][["selmaprotbas"]] <- list(model = "selmaprotbas/selmaprotbas",
                                                   parameters = list(),
                                                   initialization = list())
      dict_biogeochem <- dict[!(dict$module %in% c("phytoplankton", "zooplankton")),]
      
      for(j in seq_len(nrow(dict_biogeochem))){
        path <- strsplit(dict_biogeochem[j, "path"], "/")[[1]]
        lst[["instances"]][[path[1]]][[path[2]]][[path[3]]] <- as.numeric(dict_biogeochem[j, "default"])
      }
      
      # Phyto- & zooplankton
      for(j in c("phytoplankton", "zooplankton")){
        if(lst_config[[j]][["use"]]){
          
          dict_biology <- dict[dict$module == j,]
          groups <- names(lst_config[[j]][["groups"]])
          
          for(k in groups){
            lst[["instances"]][[k]] <- list(model = paste0("selmaprotbas/", j),
                                            parameters = list(),
                                            initialization = list(),
                                            coupling = list())
            for(l in seq_len(nrow(dict_biology))){
              path <- strsplit(dict_biology[l, "path"], "/")[[1]]
              path[path == "{group_name}"] <- k
              lst[["instances"]][[path[1]]][[path[2]]][[path[3]]] <- as.numeric(dict_biology[l, "default"])
            }
          }
        }
      }
      
      filename <- lst_config[["config_files"]][[models_coupled[i]]]
      write.config(lst, file.path(folder, filename), write.type = "yaml")
    }
    
    if(models_coupled[i] == "GOTM-WET"){
      
      dict <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$model == "wet",]
      
      lst <- list()
      
      # Biogeochemistry
      lst[["instances"]][["abiotic_water"]] <- list(long_name = "abiotic_process_in_water",
                                                    model = "wet/abiotic_water",
                                                    parameters = list(),
                                                    initialization = list())
      lst[["instances"]][["abiotic_sediment"]] <- list(long_name = "abiotic_process_in_sediment",
                                                       model = "wet/abiotic_sediment",
                                                       parameters = list(),
                                                       initialization = list(),
                                                       coupling = list())
      lst[["instances"]][["burial"]] <- list(long_name = "burial",
                                             model = "wet/burial",
                                             parameters = list(),
                                             coupling = list())
      lst[["instances"]][["resus_sed"]] <- list(long_name = "resuspension_sedimentation",
                                                model = "wet/resus_sed",
                                                parameters = list(),
                                                coupling = list())
      
      
      
      dict_biogeochem <- dict[!(dict$module %in% c("phytoplankton", "zooplankton",
                                                   "fish", "macrophytes", "zoobenthos")),]
      
      for(j in seq_len(nrow(dict_biogeochem))){
        path <- strsplit(dict_biogeochem[j, "path"], "/")[[1]]
        lst[["instances"]][[path[1]]][[path[2]]][[path[3]]] <- as.numeric(dict_biogeochem[j, "default"])
      }
      
      # Phytoplankton, zooplankton, fish, macrophytes, zoobenthos
      for(j in c("phytoplankton", "zooplankton", "fish", "macrophytes", "zoobenthos")){
        if(lst_config[[j]][["use"]]){
          
          dict_biology <- dict[dict$module == j,]
          groups <- names(lst_config[[j]][["groups"]])
          
          for(k in groups){
            mod_name <- ifelse(j == "fish", "fish_mod", j)
            lst[["instances"]][[k]] <- list(long_name = k,
                                            model = paste0("wet/", mod_name),
                                            parameters = list(),
                                            initialization = list(),
                                            coupling = list())
            for(l in seq_len(nrow(dict_biology))){
              path <- strsplit(dict_biology[l, "path"], "/")[[1]]
              path[path == "{group_name}"] <- k
              lst[["instances"]][[path[1]]][[path[2]]][[path[3]]] <- as.numeric(dict_biology[l, "default"])
            }
          }
        }
      }
      filename <- lst_config[["config_files"]][[models_coupled[i]]]
      write.config(lst, file.path(folder, filename), write.type = "yaml")
    }
    
    if(models_coupled[i] == "GLM-AED2" | models_coupled[i] == "Simstrat-AED2"){
      
      dict <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$model == "aed2",]
      
      lst <- list()
      class(lst) <- "nml"
      
      ## Biogeochemistry
      # Set models
      lst[["aed2_models"]] <- list(models = c("'aed2_sedflux'", "'aed2_oxygen'", "'aed2_carbon'",
                                              "'aed2_silica'", "'aed2_nitrogen'",
                                              "'aed2_phosphorus'", "'aed2_organic_matter'"))
      lst[["aed2_sedflux"]] <- list(sedflux_model = "Constant")
      
      lst[["aed2_sed_constant"]] <- list()
      
      lst[["aed2_oxygen"]] <- list()
      
      lst[["aed2_carbon"]] <- list()
      
      lst[["aed2_silica"]] <- list()
      
      lst[["aed2_nitrogen"]] <- list()
      
      lst[["aed2_phosphorus"]] <- list()
      
      lst[["aed2_organic_matter"]] <- list()
      
      dict_biogeochem <- dict[!(dict$module %in% c("phytoplankton", "zooplankton",
                                                   "fish", "macrophytes", "zoobenthos",
                                                   "pathogens")),]
      
      for(j in seq_len(nrow(dict_biogeochem))){
        path <- strsplit(dict_biogeochem[j, "path"], "/")[[1]]
        lst[[path[1]]][[path[2]]] <- as.numeric(dict_biogeochem[j, "default"])
      }
      
      # Phytoplankton
      if(lst_config[["phytoplankton"]][["use"]]){
        
        lst[["aed2_models"]][["models"]] <- c(lst[["aed2_models"]][["models"]],
                                              "'aed2_phytoplankton'")
        
        num_phyto_groups <- length(names(lst_config[["phytoplankton"]][["groups"]]))
        lst[["aed2_phytoplankton"]] <- list(num_phytos = length(seq_len(num_phyto_groups)),
                                            the_phytos = seq_len(num_phyto_groups))
        
        lst_phyto <- list()
        class(lst_phyto) <- "nml"
        
        dict_phyto <- dict[dict$module == "phytoplankton",]
        
        groups <- names(lst_config[["phytoplankton"]][["groups"]])
        
        for(j in seq_len(nrow(dict_phyto))){
          path <- strsplit(dict_phyto[j, "path"], "/")[[1]]
          values <- rep(NA, length(groups))
          for(k in seq_len(length(groups))){
            values[k] <- as.numeric(dict_phyto[j, "default"])
          }
          lst_phyto[[path[1]]][[path[2]]] <- values
        }
        
        filename <- file.path(dirname(lst_config[["config_files"]][[models_coupled[i]]]),
                              "aed2_phyto_pars.nml")
        write_nml(lst_phyto, file.path(folder, filename))
      }
      
      # Zooplankton
      if(lst_config[["zooplankton"]][["use"]]){
        
        lst[["aed2_models"]][["models"]] <- c(lst[["aed2_models"]][["models"]],
                                              "'aed2_zooplankton'")
        
        num_zoop_groups <- length(names(lst_config[["zooplankton"]][["groups"]]))
        lst[["aed2_zooplankton"]] <- list(num_zoops = length(seq_len(num_zoop_groups)),
                                          the_zoops = seq_len(num_zoop_groups))
        
        lst_zoop <- list()
        class(lst_zoop) <- "nml"
        
        dict_zoop <- dict[dict$module == "zooplankton",]
        
        groups <- names(lst_config[["zooplankton"]][["groups"]])
        
        for(j in seq_len(nrow(dict_zoop))){
          path <- strsplit(dict_zoop[j, "path"], "/")[[1]]
          values <- rep(NA, length(groups))
          for(k in seq_len(length(groups))){
            values[k] <- as.numeric(dict_zoop[j, "default"])
          }
          lst_zoop[[path[1]]][[path[2]]] <- values
        }
        
        filename <- file.path(dirname(lst_config[["config_files"]][[models_coupled[i]]]),
                              "aed2_zoop_pars.nml")
        write_nml(lst_zoop, file.path(folder, filename))
      }
      
      filename <- lst_config[["config_files"]][[models_coupled[i]]]
      write_nml(lst, file.path(folder, filename))
    }
    
    # For MyLake no setup is needed because the WQ config file
    # is the same as the physics config file. 
    
    if(models_coupled[i] == "PCLake"){
      # For PCLake, we don't generate a file from scratch, as you
      # can't switch off modules as in AED or the FABM-models, and
      # then we'd have to save all the unit, remark, long_name, 
      # short_name information as well. We copy a template from the
      # inst folder instead. 
      
      template_initial <- system.file("extdata/pclake_initialstates.txt",
                                      package = "LakeEnsemblR.WQ")
      template_pars <- system.file("extdata/pclake_parameters.txt",
                                   package = "LakeEnsemblR.WQ")
      
      
      pclake_dir <- dirname(lst_config[["config_files"]][[models_coupled[i]]])
      file.copy(template_initial,
                file.path(folder, pclake_dir, "initialstates.txt"),
                overwrite = TRUE)
      file.copy(template_pars,
                file.path(folder, pclake_dir, "parameters.txt"),
                overwrite = TRUE)
    }
  }
}
