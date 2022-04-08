#'Sets PCLake physics settings
#'
#'A LakeEnsemblR::export_config function for the PCLake physics
#' settings are based on the LakeEnsemblR config file
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'@param ler_config_file character; name of LakeEnsemblR config file
#'@param verbose boolean; print changed parameters on screen
#'
#'@examples
#'
#'@importFrom configr read.config
#'@importFrom gotmtools get_yaml_value
#'@importFrom lubridate day year
#'
#'@export

export_pclake_physics <- function(config_file, folder = ".",
                               ler_config_file = "LakeEnsemblR.yaml",
                               verbose = FALSE){
  
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # Fix time zone
  original_tz <- Sys.getenv("TZ")
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
    Sys.setenv(TZ = original_tz)
  })
  
  Sys.setenv(TZ = "UTC")
  
  # Check if config file exists
  if(!file.exists(config_file)){
    stop(config_file, " does not exist.")
  }
  if(!file.exists(ler_config_file)){
    stop(ler_config_file, " does not exist.")
  }
  
  # Read config files as a list
  lst_config <- read.config(file.path(folder, config_file))
  lst_config_ler <- read.config(file.path(folder, ler_config_file))
  
  pcl_pars_path <- lst_config[["config_files"]][["PCLake"]]
  pcl_init_path <- gsub("parameters", "initialstates", pcl_pars_path)
  pclake_pars <- read.table(pcl_pars_path,
                            sep = "\t",
                            header = TRUE,
                            fill = TRUE,
                            stringsAsFactors = FALSE)
  pclake_pars <- pclake_pars[!is.na(pclake_pars$lId),]
  pclake_init <- read.table(pcl_init_path,
                            sep = "\t",
                            header = TRUE,
                            fill = TRUE,
                            stringsAsFactors = FALSE)
  pclake_init <- pclake_init[!is.na(pclake_init$lId),]
  cols_to_keep <- !grepl("v\\d\\d|code.versie", names(pclake_init))
  pclake_init <- pclake_init[, cols_to_keep]
  cols_to_keep <- colSums(is.na(pclake_init)) < nrow(pclake_init)
  pclake_init <- pclake_init[, cols_to_keep]
  
  ### export_dirs
  # not necessary, because set_up_configs does this
  
  ### export_time
  start_date <- as.POSIXct(get_yaml_value(ler_config_file, "time", "start"))
  stop_date <- as.POSIXct(get_yaml_value(ler_config_file, "time", "stop"))
  # timestep in PCLake seems to be fixed to 1 day
  
  inp_lst_pclake <- list(DayStart = day(start_date),
                         StartTime = day(start_date),
                         EndTime = as.numeric(difftime(stop_date, start_date, units = "days")),
                         YearZero = year(start_date))
  pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  
  ### export_location
  lat <- get_yaml_value(ler_config_file, "location", "latitude")
  max_depth <- get_yaml_value(ler_config_file, "location", "depth")
  init_depth <- get_yaml_value(ler_config_file, "location", "init_depth")
  
  # Read in hypsograph data
  hyp_file <- get_yaml_value(ler_config_file, "location", "hypsograph")
  if(!file.exists(hyp_file)){
    stop(hyp_file, " does not exist. Check filepath in ", ler_config_file)
  }
  hyp <- read.csv(hyp_file)
  
  # Calculate fetch, assuming a circular lake
  surface_depth <- hyp$Depth_meter == 0.0
  fetch <- round(2 * sqrt(hyp[surface_depth, "Area_meterSquared"] / pi))
  
  # Also activate epi/hypo part
  inp_lst_pclake <- list(InitDepth = 1,
                         InitMixDepth = 0, # Otherwise we need to get a time series of MLD
                         InclStrat = 1,
                         InclLat = 0, # swr is required input for LER, so not needed?
                         calcMixDepth = 1,
                         calcQEv = 1,
                         mDepthW = max(hyp$Depth_meter),
                         cDepthWInit0 = init_depth,
                         cFetch = fetch,
                         cLAT = lat)
  pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  
  ### export_output_settings
  # Select what variables to output
  # So far, this is hard-coded (and it includes biogeochemical variables)
  inp_lst_pclake <- list(sMixDepthW0 = 1,
                         sNH4WHyp0 = 1,
                         sNO3WHyp0 = 1,
                         sPO4WHyp0 = 1,
                         sSiO2WHyp0 = 1,
                         sO2WHyp0 = 1,
                         sDDiatWHyp0 = 1,
                         sDGrenWHyp0 = 1,
                         sDBlueWHyp0 = 1,
                         sNH4WEpi0 = 1,
                         sNO3WEpi0 = 1,
                         sPO4WEpi0 = 1,
                         sSiO2WEpi0 = 1,
                         sO2WEpi0 = 1,
                         sDDiatWEpi0 = 1,
                         sDGrenWEpi0 = 1,
                         sDBlueWEpi0 = 1)
  pclake_init <- set_pclake_r(pclake_init, inp_lst_pclake,
                              column = "iReport", verbose = verbose)
  
  ### export_meteo
  meteo_file <- get_yaml_value(file = ler_config_file, label = "meteo", key = "file")
  met_var_dic <- LakeEnsemblR::met_var_dic
  # Check if file exists
  if(!file.exists(meteo_file)){
    stop(meteo_file, " does not exist. Check filepath in ", config_file)
  }
  
  met <- read.csv(file.path(folder, meteo_file), stringsAsFactors = FALSE)
  met[, 1] <- as.POSIXct(met[, 1])
  met <- met[met$datetime >= start_date & met$datetime <= stop_date, ]
  met[, 1] <- as.numeric(difftime(met[, 1], start_date, units = "days"))
  
  l_names <- as.list(met_var_dic$standard_name)
  names(l_names) <- met_var_dic$short_name
  
  # Calculate wind speed from vectors
  if(all(c(l_names$u10, l_names$v10) %in% names(met)) &
     !(l_names$wind_speed %in% names(met))){
    met[[l_names$wind_speed]] <- sqrt(met[[l_names$u10]]^2 + met[[l_names$v10]]^2)
  }
  
  chck_met <- sapply(list(colnames(met)),
                     function(x) x %in% met_var_dic$standard_name)
  if(any(!chck_met)) {
    stop(paste0("Colnames of meteo file are not in standard notation!\n",
                "Colnames: ", paste0(colnames(met)[!chck_met], collapse = ", "),
                ifelse(sum(!chck_met)>1, " are", " is")," wrong.\n",
                "They should be one of: \n", paste0(met_var_dic$standard_name,
                                                    collapse = "\n")))
  }
  
  # Scaling: assume that scaling factors are listed in "all", as
  # PCLake would probably not work in ler_config_file. Still, this could
  # become a section in the LER.WQ config file
  scale_params <- LakeEnsemblR:::create_scaling_factors(ler_config_file,
                                                        "PCLake",
                                                        folder)
  par_nams <- names(scale_params)
  
  if("wind_speed" %in% par_nams){
    met[[l_names$wind_speed]] <- met[[l_names$wind_speed]] * scale_params$wind_speed
  }
  
  if("swr" %in% par_nams){
    met[[l_names$swr]] <- met[[l_names$swr]] * scale_params$swr
  }
  
  # Write files and set values in pars file
  # 1. Light (swr)
  df_pclake_tmp <- data.frame(dTime = met[["datetime"]],
                              dValue = met[[l_names$swr]],
                              "TMP" = -1)
  names(df_pclake_tmp)[3] <- "-1"
  write.table(df_pclake_tmp, file.path(dirname(pcl_pars_path),
                                       "mLout.txt"),
              sep = "\t", quote = FALSE, row.names = FALSE)
  
  # 2. Wind
  df_pclake_tmp <- data.frame(dTime = met[["datetime"]],
                              dValue = met[[l_names$wind_speed]],
                              "TMP" = -1)
  names(df_pclake_tmp)[3] <- "-1"
  write.table(df_pclake_tmp, file.path(dirname(pcl_pars_path),
                                       "mVWind.txt"),
              sep = "\t", quote = FALSE, row.names = FALSE)
  
  # 3. Precip
  if(any(c(l_names$precip, l_names$precip_h) %in% names(met))){
    meas_precip_setting <- 1
    
    if(l_names$precip_h %in% names(met)){
      met[[l_names$precip]] <- met[[l_names$precip_h]] * 24
    }
    
    df_pclake_tmp <- data.frame(dTime = met[["datetime"]],
                                dValue = met[[l_names$precip]],
                                "TMP" = -1)
    names(df_pclake_tmp)[3] <- "-1"
    write.table(df_pclake_tmp, file.path(dirname(pcl_pars_path),
                                         "mQPrec.txt"),
                sep = "\t", quote = FALSE, row.names = FALSE)
  }else{
    meas_precip_setting <- 0
    # Code specifies that precip is assumed to be 0 if not provided
  }
  
  # cVWind, cLDayAve etc. not needed, because wind and swr are required input
  inp_lst_pclake <- list(ReadLOut = 1,
                         mLOut = 1,
                         ReadVWind = 1,
                         mVWind = 1,
                         ReadQPrec = meas_precip_setting,
                         mQPrec = meas_precip_setting)
  pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  
  ### export_init_cond
  # PCLake does not need an initial wtemp condition, as wtemp is prescribed
  
  ### export_extinction
  k_w <- get_yaml_value(ler_config_file, "light", "Kw")
  if(!is.numeric(k_w)){
    k_w_file <- read.csv(file.path(folder, k_w))
    k_w_file$datetime <- as.POSIXct(k_w_file$datetime)
    k_w <- LakeEnsemblR::time_average(k_w_file,
                              start = start_date,
                              end = stop_date,
                              n = 1000)
  }
  
  inp_lst_pclake <- list(cExtWat = k_w)
  pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  
  ### export_flow
  use_inflows <- lst_config_ler[["inflows"]][["use"]]
  if("outflows" %in% names(lst_config_ler)){
    use_outflows <- lst_config_ler[["outflows"]][["use"]]
  }else{
    use_outflows <- FALSE
  }
  
  # Because PCLake is not depth-specific, all inflows and outflows
  # can be summed. They can differ for Epi and Hyp, but they are
  # assumed to be epilimnetic inflows and outflows. 
  if(use_inflows){
    inflow <- read.csv(get_yaml_value(file = ler_config_file, label = "inflows", key = "file"))
    inflow[, 1] <- as.POSIXct(inflow[, 1])
    inflow <- inflow[inflow$datetime >= start_date &
                       inflow$datetime <= stop_date, ]
    
    num_inflows <- get_yaml_value(ler_config_file, "inflows", "number_inflows")
    # Get scaling parameters
    if(!is.null(lst_config_ler$scaling_factors$all$inflow)){
      scale_param_inf <- get_yaml_value(file = ler_config_file, "all", "inflow")
    }else{
      scale_param_inf <- rep(1, num_inflows)
    }
    if(!is.null(lst_config_ler$scaling_factors$PCLake$inflow)){
      scale_param_tmp <- lst_config_ler$scaling_factors$PCLake$inflow
    }else{
      scale_param_tmp <- scale_param_inf
    }
    inflow_tmp <- LakeEnsemblR:::scale_flow(inflow,
                                            num_inflows,
                                            scale_param_tmp)
    
    cols_to_sum <- grep("Flow_", names(inflow_tmp))
    
    pclake_inflow <- data.frame(datetime = inflow_tmp[["datetime"]],
                                Flow = rowSums(inflow_tmp[, cols_to_sum,
                                                          drop = FALSE]))
    pclake_inflow[, 1] <- as.numeric(difftime(pclake_inflow[, 1], start_date,
                                              units = "days"))
    
    # Conversion from m3/s to mm/d - assume surface area of depth = 0 in hyp
    surf_area <- hyp[surface_depth, "Area_meterSquared"]
    pclake_inflow[, 2] <- pclake_inflow[, 2] / surf_area * 86400 * 1000
    
    # Write
    df_pclake_tmp <- data.frame(dTime = pclake_inflow[["datetime"]],
                                dValue = pclake_inflow[["Flow"]],
                                "TMP" = -1)
    names(df_pclake_tmp)[3] <- "-1"
    write.table(df_pclake_tmp, file.path(dirname(pcl_pars_path),
                                         "mQInEpi.txt"),
                sep = "\t", quote = FALSE, row.names = FALSE)
    
    inp_lst_pclake <- list(ReadQIn = 1,
                           mQInEpi = 1)
    pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
    
  }else{
    # i.e.: if use_inflows == FALSE
    inp_lst_pclake <- list(ReadQIn = 0,
                           mQInEpi = 0)
    pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
    
  }
  
  if(use_outflows){
    outflow <- read.csv(get_yaml_value(file = ler_config_file, label = "outflows", key = "file"))
    outflow[, 1] <- as.POSIXct(outflow[, 1])
    outflow <- outflow[outflow$datetime >= start_date &
                         outflow$datetime <= stop_date, ]
    
    num_outflows <- get_yaml_value(ler_config_file, "inflows", "number_outflows")
    # Get scaling parameters
    if(!is.null(lst_config_ler$scaling_factors$all$outflow)){
      scale_param_inf <- get_yaml_value(file = ler_config_file, "all", "outflow")
    }else{
      scale_param_inf <- rep(1, num_outflows)
    }
    if(!is.null(lst_config_ler$scaling_factors$PCLake$outflow)){
      scale_param_tmp <- lst_config_ler$scaling_factors$PCLake$outflow
    }else{
      scale_param_tmp <- scale_param_inf
    }
    outflow_tmp <- LakeEnsemblR:::scale_flow(outflow,
                                             num_outflows,
                                             scale_param_tmp)
    
    cols_to_sum <- grep("Flow_", names(outflow_tmp))
    
    pclake_outflow <- data.frame(datetime = outflow_tmp[["datetime"]],
                                 Flow = rowSums(outflow_tmp[, cols_to_sum,
                                                            drop = FALSE]))
    pclake_outflow[, 1] <- as.numeric(difftime(pclake_outflow[, 1], start_date,
                                              units = "days"))
    
    # Conversion from m3/s to mm/d - assume surface area of depth = 0 in hyp
    surf_area <- hyp[surface_depth, "Area_meterSquared"]
    pclake_outflow[, 2] <- pclake_outflow[, 2] / surf_area * 86400 * 1000
    
    # Write
    df_pclake_tmp <- data.frame(dTime = pclake_outflow[["datetime"]],
                                dValue = pclake_outflow[["Flow"]],
                                "TMP" = -1)
    names(df_pclake_tmp)[3] <- "-1"
    write.table(df_pclake_tmp, file.path(dirname(pcl_pars_path),
                                         "mQOutEpi.txt"),
                sep = "\t", quote = FALSE, row.names = FALSE)
    
    inp_lst_pclake <- list(ReadQOut = 1,
                           mQOutEpi = 1)
    pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  }else{
    # i.e.: if use_outflows == FALSE
    inp_lst_pclake <- list(ReadQOut = 0,
                           mQOutEpi = 0)
    pclake_pars <- set_pclake_r(pclake_pars, inp_lst_pclake, verbose = verbose)
  }
  
  ### export_model_parameters
  # Not used yet; will require a PCLake-devoted section in config_file
  
  ### Write the PCLake config files
  # Remove all columns that are not necessary, and correct column names
  ind_colmax <- which(names(pclake_pars) == "Duflow")
  pclake_pars <- pclake_pars[, 1:ind_colmax]
  ind_minus1 <- which(names(pclake_pars) == "X.1")
  names(pclake_pars)[ind_minus1] <- "-1"
  names(pclake_pars) <- gsub("^X", "", names(pclake_pars))
  
  ind_minus1 <- which(names(pclake_init) == "X.1")
  names(pclake_init)[ind_minus1] <- "-1"
  
  write.table(pclake_pars, pcl_pars_path,
              sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(pclake_init, pcl_init_path,
              sep = "\t", quote = FALSE, row.names = FALSE)
}

# Note: PCLake has the option to use prescribed aquatic physical inputs, e.g. 
# water temperature or wlvl. We could add an option to use an
# earlier LakeEnsemblR run for this. So far, this has not been done.

#'Sets a value in a PCLake par data.frame
#'@description
#'Sets a value in a PCLake parameter or initial states file
#' that has been read into R as a data.frame
#'
#'@param file data.frame; 
#'@param par_list list; parameter names without underscores and corresponding
#' value to enter
#'@param column character; column name to change in file. defaults to sSet1
#'@param verbose logical; print changed parameters to screen
#'
#'@examples
#'

set_pclake_r <- function(file, par_list,
                         column = "sSet1", verbose = FALSE){
  
  for(i in names(par_list)){
    ind <- which(file[["sName"]] == paste0("_", i, "_"))
    
    if(length(ind) == 0L){
      stop("Could not find parameter ", i, " in pclake par file!")
    }else if(length(ind) > 1L){
      stop("Parameter ", i, " found multiple times in pclake par file!")
    }
    
    old_val <- file[ind, column]
    file[ind, column] <- par_list[[i]]
    
    if(verbose & !identical(old_val, par_list[[i]])){
      message("PCLake: replaced ", i, ": ", old_val, " by ", par_list[[i]])
    }
  }
  
  return(file)
}
