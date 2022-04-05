#'Reads and sets nutrient inputs for all models
#'
#'Handles nutrient concentrations in inflows and other sources
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'@param ler_config_file character; name of LakeEnsemblR config file
#'@param verbose boolean; print changed parameters on screen
#'
#'@examples
#'
#'@importFrom configr read.config
#'@importFrom glmtools read_nml
#'@importFrom gotmtools get_yaml_value
#'@importFrom LakeEnsemblR format_inflow get_json_value
#'
#'@export

export_inputs <- function(config_file, folder = ".",
                          ler_config_file = "LakeEnsemblR.yaml",
                          verbose = FALSE){
  # Fix time zone
  original_tz <- Sys.getenv("TZ")
  on.exit({
    Sys.setenv(TZ = original_tz)
  })
  Sys.setenv(TZ = "UTC")
  
  # Read config files as a list
  lst_config <- read.config(file.path(folder, config_file))
  lst_config_ler <- read.config(file.path(folder, ler_config_file))
  
  models_coupled <- lst_config[["models"]]
  
  ##### Nutrients in inflows -----
  nutrients_inflows_zero <- FALSE
  input_file <- NULL
  if(!lst_config_ler[["inflows"]][["use"]]){
    nutrients_inflows_zero <- TRUE
  }else{
    # See if user input is in a different file
    if(!is.null(lst_config[["input"]][["inflows"]])){
      input_file <- lst_config[["input"]][["inflows"]]
    }else{
      df_inflow <- read.csv(file.path(folder,
                                      lst_config_ler[["inflows"]][["file"]]))
      if(any(grepl("wq_", names(df_inflow)))){
        input_file <- lst_config_ler[["inflows"]][["file"]]
      }else{
        nutrients_inflows_zero <- TRUE
      }
    }
  }
  
  if(verbose & nutrients_inflows_zero){
    message("export_inputs did not detect any nutrient inputs in inflows.")
    # Next step would be to either create a df with
    # all required inputs 0?
  }
  
  if(!nutrients_inflows_zero){
    df_inflow <- read.csv(file.path(folder, input_file))
    num_inflows <- lst_config_ler[["inflows"]][["number_inflows"]]
  }else{
    return("No nutrient inputs detected")
    # For now, just stop the function. Later we could add more elaborate
    # code for this
  }
  
  # Check header names (see helpers.R)
  chk_names_nutr_flow(names(df_inflow))
  # Note: LER might need an update to not crash on headers that have "wq_"?
  
  # Standardise inflow headers; add _1 if there is only one inflow
  if(!any(grepl("_\\d+$", names(df_inflow)))){
    names(df_inflow) <- paste0(names(df_inflow), "_1")
    names(df_inflow)[1L] <- "datetime"
  }
  
  # For every model: write in correct model config file and write file
  for(i in models_coupled){
    model_name_parsed <- strsplit(i, "-")[[1L]]
    phys_model <- model_name_parsed[1L]
    wq_model <- tolower(model_name_parsed[length(model_name_parsed)])
    
    if(phys_model == "GOTM"){
      gotmyaml <- read.config(file.path(folder, i, "gotm.yaml"))
      name_file <- "LERWQ_inflow_chem.dat"
      
      # Loop over the df_inflow columns to write the required inputs to
      # gotm.yaml
      # Important; this does not remove nutrient inputs that were in gotm.yaml,
      # but that are not present in df_inflow!
      for(j in seq_len(ncol(df_inflow))){
        if(!grepl("wq_", names(df_inflow)[j])) next
        inflow_num_ind <- regexec("_(\\d+)$", names(df_inflow)[j])
        inflow_num <- substr(names(df_inflow)[j],
                             start = inflow_num_ind[[1]][2],
                             stop = inflow_num_ind[[1]][2] +
                               attr(inflow_num_ind[[1]], "match.length")[2] -
                               1)
        inflow_name <- names(gotmyaml[["streams"]])[as.numeric(inflow_num)]
        var_name <- gsub("_\\d+$", "", names(df_inflow)[j])
        
        cnfg_name <- wq_var_dic[wq_var_dic$standard_name == var_name,
                                wq_model]
        if(length(cnfg_name) == 0L){
          stop("Nutrient input not understood for model ", i, "!")
        }
        
        # Important: this assumes that first inflows are listed, then outflows
        # This is done by LER, but for a custom gotm.yaml file it could go wrong
        gotmyaml[["streams"]][[inflow_name]][[cnfg_name]] <- list(method = 2L,
                                                                  constant_value = 0.0,
                                                                  file = name_file,
                                                                  column = j - 1L)
      }
      
      # Writing gotm.yaml file, see helpers.R
      lerwq_write_yaml_file(gotmyaml, file.path(folder, i, "gotm.yaml"),
                            is_gotm_yaml = TRUE)
      
      # Write inflow chem file
      cols_to_use <- names(df_inflow)[grepl("wq_", names(df_inflow)) |
                                        names(df_inflow) == "datetime"]
      df_inflow_gotm <- df_inflow[, cols_to_use]
      names(df_inflow_gotm)[1] <- "!datetime"
      write.table(df_inflow_gotm, file.path(folder, i, name_file),
                  row.names = FALSE, quote = FALSE, sep = "\t")
      
    }else if(phys_model == "GLM"){
      # GLM: add nutrient concentrations to the inflow file.
      # Needs to be the same frequency as the inflow file
      
      glm_nml <- read_nml(file.path(folder, i,
                                    basename(lst_config_ler[["config_files"]][["GLM"]])))
      
      if(verbose){
        message("If the datetimes of discharge and nutrienst are not the ",
                "same, this will lead to errors for GLM. LER.WQ does not ",
                "yet interpolate these values.")
      }
      
      for(j in seq_len(ncol(df_inflow))){
        if(!grepl("wq_", names(df_inflow)[j])) next
        inflow_num_ind <- regexec("_(\\d+)$", names(df_inflow)[j])
        inflow_num <- substr(names(df_inflow)[j],
                             start = inflow_num_ind[[1]][2],
                             stop = inflow_num_ind[[1]][2] +
                               attr(inflow_num_ind[[1]], "match.length")[2] -
                               1)
        var_name <- gsub("_\\d+$", "", names(df_inflow)[j])
        cnfg_name <- wq_var_dic[wq_var_dic$standard_name == var_name,
                                wq_model]
        if(length(cnfg_name) == 0L){
          stop("Nutrient input not understood for model ", i, "!")
        }
        
        # Add to inflow_vars if needed
        if(!grepl(cnfg_name, glm_nml[["inflow"]][["inflow_vars"]])){
          glm_nml[["inflow"]][["inflow_vars"]] <- paste0(glm_nml[["inflow"]][["inflow_vars"]],
                                                         ",", cnfg_name)
          glm_nml[["inflow"]][["inflow_varnum"]] <- glm_nml[["inflow"]][["inflow_varnum"]] +
            1L
        }
        
        # Write values to inflow file
        inf_file <- strsplit(glm_nml[["inflow"]][["inflow_fl"]], ",")[[1]][as.numeric(inflow_num)]
        
        df_inf_file_glm <- read.csv(file.path(folder, i, inf_file))
        names(df_inf_file_glm)[1] <- "datetime"
        if(cnfg_name %in% names(df_inf_file_glm)){
          df_inflow_tmp <- merge(df_inflow[, c("datetime", names(df_inflow)[j])],
                                 df_inf_file_glm[, "datetime", drop = FALSE],
                                 by = "datetime", all.y = TRUE)
          df_inf_file_glm[[names(df_inflow)[j]]] <- df_inflow_tmp[[names(df_inflow)[j]]]
        }else{
          df_inf_file_glm <- merge(df_inf_file_glm, df_inflow[, c("datetime", names(df_inflow)[j])],
                                   by = "datetime", all.x = TRUE)
        }
        names(df_inf_file_glm)[1] <- "Time"
        names(df_inf_file_glm)[names(df_inf_file_glm) == names(df_inflow)[j]] <- cnfg_name
        write.csv(df_inf_file_glm, file.path(folder, i, inf_file),
                  row.names = FALSE, quote = FALSE)
      }
      
      write_nml(glm_nml, file.path(folder, i,
                                   basename(lst_config_ler[["config_files"]][["GLM"]])))
      
    }else if(phys_model == "Simstrat"){
      # Not sure, but the LakeZurich example suggests that the 
      # times and number of columns need to be the same as discharge
      # We use LakeEnsemblR:::format_flow_simstrat()
      # This function was not originally designed for wq variables,
      # but it can be used for them nonetheless. 
      
      # The flow_file needs to be generated
      unique_wq_vars <- gsub("_\\d+$", "", names(df_inflow))
      unique_wq_vars <- unique(unique_wq_vars[unique_wq_vars != "datetime"])
      
      # Flow_metersCubedPerSecond also needs to be provided, in case averaging
      # over multiple inflows is needed (after scaling)
      ### Import data
      inflow_file <- lst_config_ler[["inflows"]][["file"]]
      inflow <- read.csv(file.path(folder, inflow_file))
      inflow[, 1] <- as.POSIXct(inflow[, 1])
      start_date <- lst_config_ler[["time"]][["start"]]
      # Stop date
      stop_date <- lst_config_ler[["time"]][["stop"]]
      inflow_start <- which(inflow$datetime == as.POSIXct(start_date))
      inflow_stop <- which(inflow$datetime == as.POSIXct(stop_date))
      inflow <- inflow[inflow_start:inflow_stop, ]
      
      ### Naming conventions standard input
      LakeEnsemblR:::chk_names_flow(inflow, num_inflows, inflow_file)
      
      if(!is.null(lst_config_ler$scaling_factors$Simstrat$inflow)){
        scale_param_tmp <- lst_config_ler$scaling_factors$Simstrat$inflow
      }else{
        if(!is.null(lst_config_ler$scaling_factors$all$inflow)){
          scale_param_tmp <- lst_config_ler$scaling_factors$all$inflow
        }else{
          scale_param_tmp <- rep(1, num_inflows)
        }
      }
      inflow_tmp <- LakeEnsemblR:::scale_flow(inflow, num_inflows,
                                              scale_param_tmp)
      df_inflow_sim <- df_inflow
      df_inflow_sim$datetime <- as.POSIXct(df_inflow_sim$datetime)
      df_inflow_sim <- merge(df_inflow_sim,
                             inflow_tmp[, "datetime", drop = FALSE],
                             by = "datetime", all.y = TRUE)
      
      for(j in unique_wq_vars){
        # Use the format_inflow function of LER 
        # Note: This is a rather hacky solution, but it avoid that we have
        # to rewrite the LakeEnsemblR format_inflow and format_flow_simstrat
        # functions. I name the column of the wq var "Salinity_practicalSalinityUnits"
        # just to ensure that the columns get averaged according to the discharge.
        
        inflow_ls <- list()
        for(k in 1:num_inflows){
          inflow_ls[[paste0("inflow_", k)]] <-
            data.frame(datetime = as.POSIXct(df_inflow_sim$datetime),
                       Flow_metersCubedPerSecond = inflow_tmp[[paste0("Flow_metersCubedPerSecond_", k)]],
                       Salinity_practicalSalinityUnits = df_inflow_sim[[paste0(j, "_", k)]])
        }
        
        sim_inflow <- format_inflow(inflow = inflow_ls, model = "Simstrat",
                                    config_file = file.path(folder, ler_config_file))
        
        # Prepare other arguments for the format_flow_simstrat functions
        sim_par <- file.path(folder, lst_config_ler[["config_files"]][["Simstrat"]])
        lvl_inflows <- rep(-1, num_inflows) # 2022-03-31: this is fixed in LER
        
        qin_file <- LakeEnsemblR:::format_flow_simstrat(flow_file = sim_inflow,
                                                        levels = lvl_inflows,
                                                        surf_flow = (lvl_inflows == -1),
                                                        in_or_out = "inflow",
                                                        type = "salt",
                                                        sim_par = sim_par)
        
        # Correcting headers and writing
        col_header <- wq_var_dic[wq_var_dic$standard_name == j, wq_model]
        col_unit <- wq_var_dic[wq_var_dic$standard_name == j, "unit"]
        qin_file[1L] <- paste0("Time [d]\t", col_header, " [", col_unit,"]")
        writeLines(qin_file, file.path(folder, i, paste0(col_header, "_inflow.dat")))
      }
      
    }
    
    # At this point, PCLake and MyLake are not yet included in this function
    
  }
  
  
  # Then handle additional sources, such as constants for atmo input
}