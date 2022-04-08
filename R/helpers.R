#' @title Adds an AED2 section to the Simstrat config file
#'
#' @description Checks for existence of and then adds a AED2Config section
#'  in the Simstrat configuration file (JSON format). Takes into account
#'  information in LER.WQ config file, e.g. on shading. 
#'
#' @param folder path; to the location of the config files
#' @param simstrat_par character; name of the Simstrat config file
#' @param verbose logical; whether to show messages
#' @param settings_section list; corresponding section from LER.WQ config
#' 
#' @importFrom LakeEnsemblR get_yaml_multiple input_json
#' 
#' @examples
#' 
#' @noRd
add_aed2_section_simstrat <- function(folder = ".",
                                      simstrat_par = "simstrat.par",
                                      verbose = TRUE,
                                      settings_section = NULL){
  # This function will interpret a commented-out AED2Config as present
  # and not create a new section. 
  
  # configr was not able to read sim_par. Non-conformity to the Simstrat-
  # format as present in e.g. SimstratR, might lead to errors. This is not a
  # json-parser.
  sim_par <- readLines(file.path(folder, simstrat_par))
  
  if(is.null(settings_section)){
    stop("settings_section must be provided to add aed2 section to Simstrat")
  }
  shading <- ifelse(settings_section[["bio-shading"]], 1, 0)
  benthic <- ifelse(settings_section[["bottom_everywhere"]], 1, 0)
  
  aed_section_present <- any(grepl("AED2Config", sim_par))
  if(aed_section_present){
    input_json(file.path(folder, simstrat_par), label = "AED2Config",
               key = "BioshadeFeedback", value = shading)
    input_json(file.path(folder, simstrat_par), label = "AED2Config",
               key = "BenthicMode", value = benthic)
    
    return()
  }
  
  # Grab settings and information, to be used in writing the aed2config section
  num_spaces <- attr(regexpr("\\s+", sim_par[2]), "match.length")
  s1 <- paste0(rep(" ", num_spaces), collapse = "")
  s2 <- paste0(rep(" ", num_spaces * 2), collapse = "")
  folder_simstrat <- dirname(simstrat_par)
  aed_nml <- "aed2.nml"
  
  
  ### Create the AED2Config section
  aed2config <- c(paste0(s1, "\"AED2Config\" : {"),
                  paste0(s2, "\"AED2ConfigFile\" :  \"", folder_simstrat,
                         "/", aed_nml, "\","),
                  paste0(s2, "\"PathAED2initial\" :  \"", folder_simstrat,
                         "/\","),
                  paste0(s2, "\"PathAED2inflow\" :  \"", folder_simstrat,
                         "/\","),
                  paste0(s2, "\"ParticleMobility\" : 0,"),
                  paste0(s2, "\"BioshadeFeedback\" : ", shading, ","),
                  paste0(s2, "\"BackgroundExtinction\" : 0.2,"),
                  paste0(s2, "\"BenthicMode\" : ", benthic,","),
                  paste0(s2, "\"OutputDiagnosticVars\" : false,"),
                  paste0(s1, "},"))
  
  ### Add AED2Config after ModelConfig
  ind_modelconfig <- grep("ModelConfig", sim_par)
  for(i in ind_modelconfig:length(sim_par)){
    if(grepl("},", sim_par[i])){
      ind_modelconfig_end <- i
      break
    }
    if(i == length(sim_par)){
      stop("Could not find end of ModelConfig section in sim_par!")
    }
  }
  
  ### Write file
  writeLines(text = c(sim_par[1:ind_modelconfig_end],
                      aed2config,
                      sim_par[(ind_modelconfig_end + 1):length(sim_par)]),
             con = file.path(folder, simstrat_par))
  
  
}

#' @title Modifies FABM section in gotm.yaml
#'
#' @description Activates WQ settings in the gotm.yaml file,
#'  and adds a numerics section if not present. 
#'
#' @param folder path; to the location of the config files
#' @param gotmyaml character; name of the Simstrat config file
#' @param verbose logical; whether to show messages
#' @param settings_section list; corresponding section from LER.WQ config
#' 
#' @importFrom LakeEnsemblR get_yaml_multiple input_yaml_multiple
#' 
#' @examples
#' 
#' @noRd
add_fabm_settings_gotm <- function(folder = ".",
                                   gotmyaml = "gotm.yaml",
                                   verbose = TRUE,
                                   settings_section = NULL){
  
  bottom <- tolower(as.character(settings_section[["bottom_everywhere"]]))
  shading <- tolower(as.character(settings_section[["bio-shading"]]))
  split <- settings_section[["split_factor"]]
  repair <- tolower(as.character(settings_section[["repair_state"]]))
  
  ode_method <- settings_section[["ode_method"]]
  valid_ode <- c("Euler", "RK2", "RK4", "Pat1", "PatRK2", "PatRK4", "ModPat1",
                 "ModPatRK2", "ModPatRK4", "ExtModPat1", "ExtModPatRK2")
  
  if(!(ode_method %in% valid_ode)){
    stop(ode_method, " is not a valid entry for GOTM!")
  }else{
    ode_num <- which(valid_ode == ode_method)
  }
  
  numerics_section_present <- tryCatch(get_yaml_multiple(file.path(folder,
                                                                   gotmyaml),
                                                         key1 = "fabm",
                                                         key2 = "numerics",
                                                         key3 = "ode_method"),
                                       error = function(e){FALSE})
  
  if(isFALSE(numerics_section_present)){
    # configr can read the yaml file, but here readLines is used to
    # conserve comments if present.
    yml <- readLines(file.path(folder, gotmyaml))
    
    num_spaces <- attr(regexpr("\\s+", yml[3]), "match.length")
    s1 <- paste0(rep(" ", num_spaces), collapse = "")
    s2 <- paste0(rep(" ", num_spaces * 2), collapse = "")
    
    numerics_section <- c(paste0(s1, "numerics:"),
                          paste0(s2, "ode_method: 1"),
                          paste0(s2, "split_factor: 1"))
    
    # Add after repair_state line
    ind_repairstate <- grep("repair_state:", yml)
    
    writeLines(text = c(yml[1:ind_repairstate],
                        numerics_section,
                        yml[(ind_repairstate + 1):length(yml)]),
               con = file.path(folder, gotmyaml))
  }
  
  # Now enter the values
  input_yaml_multiple(file.path(folder, gotmyaml),
                      bottom,
                      key1 = "fabm", key2 = "feedbacks",
                      key3 = "bottom_everywhere", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      shading,
                      key1 = "fabm", key2 = "feedbacks",
                      key3 = "shade", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      repair,
                      key1 = "fabm", key2 = "repair_state", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      ode_num,
                      key1 = "fabm", key2 = "numerics",
                      key3 = "ode_method", verbose = verbose)
  input_yaml_multiple(file.path(folder, gotmyaml),
                      split,
                      key1 = "fabm", key2 = "numerics",
                      key3 = "split_factor", verbose = verbose)
}


#' @title Get the phytoplankton group to be used in MyLake
#'
#' @description MyLake only uses one phytoplankton group, so it is needed
#'  to determine one of the groups used in the config_file to be the group
#'  used in MyLake. By default the 1st group, if nothing is specified.  
#'
#' @param config_file character; name of the config file
#' @param module character; name of the module
#' @param folder path; to the location of the config file
#' 
#' @importFrom configr read.config
#' 
#' @examples
#' 
#' @noRd

get_mylake_group <- function(config_file, module, folder = "."){
  
  if(module != "phytoplankton"){
    stop("The get_mylake_group function only works for phytoplankton!")
  }
  
  lst_config <- read.config(file.path(folder, config_file))
  if(!lst_config[[module]][["use"]]){
    return("")
  }
  
  groups <- names(lst_config[[module]][["groups"]])
  
  # See if a group has been specified with "mylake_group: true"
  use_mylake <- lapply(lst_config[[module]][["groups"]],
                              "[[",
                              "mylake_group")
  use_mylake <- sapply(use_mylake, function(x) ifelse(is.null(x), FALSE, x))
  
  if(class(use_mylake) != "logical"){
    stop("An entry of mylake_group in the config_file is not 'true' or 'false'")
  }
  
  if(sum(use_mylake) > 1L){
    stop("Multiple phytoplankton groups are marked to be used in MyLake!")
  }else if(sum(use_mylake) == 0L){
    return(groups[1L])
  }else{
    return(groups[use_mylake])
  }
}


#' @title Get the groups to be used in PCLake
#'
#' @description PCLake has a fixed number of groups for phytoplankton,
#'  zooplankton, macrophytes, and fish. Therefore it is needed to determine
#'  which groups in the config_file belong to which PCLake group. 
#'  This can either be specified in the config_file, or this function
#'  tries to deduce it from the group names.  
#'
#' @param config_file character; name of the config file
#' @param module character; name of the module
#' @param folder path; to the location of the config file
#' @param auto_recognisition logical; in absence of user input, try to
#'  identify groups by their names?
#' 
#' @importFrom configr read.config
#' 
#' @examples
#' 
#' @noRd

get_pclake_groups <- function(config_file, module, folder = ".",
                              auto_recognisition = TRUE){
  
  lst_config <- read.config(file.path(folder, config_file))
  if(!lst_config[[module]][["use"]]){
    return("")
  }
  
  groups <- names(lst_config[[module]][["groups"]])
  
  # See if groups have been specified with "pclake_group"
  pclake_groups <- lapply(lst_config[[module]][["groups"]],
                       "[[",
                       "pclake_group")
  pclake_groups <- sapply(pclake_groups, function(x) ifelse(is.null(x),
                                                            "", x))
  pclake_groups <- tolower(pclake_groups)
  
  # Define what standard_groups PCLake uses and the pattern to search for them
  # If there's only one group, no pattern is needed
  if(module == "phytoplankton"){
    standard_groups <- c(Blue = "cyano|blue",
                         Gren = "(green|gren|chloro)^blue", # No "blue", to avoid detecting "bluegreen"
                         Diat = "diat")
  }else if(module == "zooplankton"){
    standard_groups <- "Zoo"
  }else if(module == "zoobenthos"){
    standard_groups <- "Bent"
  }else if(module == "fish"){
    standard_groups <- c(FiAd = "ad|benthiv",
                         FiJv = "jv|juv",
                         Pisc = "pisc|pred")
  }else if(module == "macrophytes"){
    standard_groups <- c(Veg = "plant|phyt",
                         Phra = "phrag|reed")
  }
  
  group_division <- rep(as.character(NA), length(groups))
  names(group_division) <- groups
  
  for(i in seq_len(length(pclake_groups))){
    rgx <- sapply(standard_groups, function(x) regexpr(x, pclake_groups[i]))
    if(sum(rgx > 0L) > 1L){
      stop("pclake_group user input identified same group multiple times. ",
           "Maximum one group of ", paste(names(standard_groups),
                                          collapse = ", "))
    }else if(sum(rgx > 0L) == 1L){
      if(!is.na(group_division[i])){
        stop(names(group_division)[i], " is identified double by pclake_group",
             " user input")
      }
      group_division[i] <- names(rgx)[rgx > 0L]
    }else if(pclake_groups[i] == "true" & length(standard_groups) == 1L){
      group_division[i] <- names(rgx)
    }
  }
  
  # Now loop over group_division again to recognise names
  if(auto_recognisition){
    if(length(standard_groups) == 1L & all(is.na(group_division))){
      # If there is only one group, just take the first group
      message("Autorecognition PCLake: identifying ",
              names(group_division)[1L], " as ",
              standard_groups, ".")
      group_division[1L] <- standard_groups
      
    }else{
      for(i in seq_len(length(group_division))){
        if(!is.na(group_division[i])) next
        
        rgx <- sapply(standard_groups,
                      function(x) regexpr(x, names(group_division)[i]))
        # Instead of throwing an error, the first hit is used
        # e.g. if someone makes groups diatoms1 and diatoms2, diatoms1 is used
        ind <- which(rgx > 0L)[1L]
        if(!is.na(ind)){
          message("Autorecognition PCLake: identifying ",
                  names(group_division)[i], " as ",
                  names(rgx)[ind], ".")
          group_division[i] <- names(rgx)[ind]
        }
      }
    }
  }
  
  return(group_division)
}

#' check naming convention for inflow nutrients
#'@description
#'check if the header in in files follow the naming convention
#'
#' @name chk_names_nutr_flow
#' @param headers vector of column headers
#' @noRd
chk_names_nutr_flow <- function(headers){
  
  # remove numbers if multiple in/outflows are there
  headers <- gsub("_\\d+$", "", headers)
  
  allowed_names <- c("datetime", wq_var_dic$standard_name)
  if(isTRUE(requireNamespace("LakeEnsemblR", quietly = TRUE))){
    ler_dic_names <- LakeEnsemblR::lake_var_dic$standard_name
    ler_dic_names <- ler_dic_names[!(ler_dic_names %in% c("Ice_Thickness_meter",
                                                          "Density_kiloGramPerCubedMeter",
                                                          "Water_Level_meter"))]
    allowed_names <- c(allowed_names, ler_dic_names)
  }
  
  # test if names are right
  chck_flow <- sapply(headers, function(x) x %in% allowed_names)
  if(any(!chck_flow)){
    stop("The following headers of the inflow nutrients files are not correct: ",
         headers[!chck_flow], "! They should be one of:\n",
         paste(allowed_names, collapse = "\n"))
  }
}

#'write yaml file in list-format
#'@description
#'write yaml file in GOTM yaml format
#'
#' @name lerwq_write_yaml_file
#' @param yml list; yaml file in list format, as read by configr
#' @param filepath character; path to file location
#' @param is_gotm_yaml logical; if unspecified, it try to detect gotm.yaml
#' @noRd
lerwq_write_yaml_file <- function(yml, filepath, is_gotm_yaml = NULL){
  # Method is very cumbersome, hence the separate function
  
  write.config(yml,
               filepath,
               write.type = "yaml",
               indent = 3L,
               handlers = list(logical = function(x){
                 result = ifelse(x, "true", "false")
                 class(result) = "verbatim"
                 return(result)
               },
               NULL = function(x){
                 result = ""
                 class(result) = "verbatim"
                 return(result)
               }))
  
  # Only for gotm.yaml:
  # The function writes two spaces between "-" and "source", and this should be one
  # GOTM will crash if this doesn't happen
  if(is.null(is_gotm_yaml)){
    if(all(c("title", "location", "time") %in% names(yml))){
      is_gotm_yaml <- TRUE
    }else{
      is_gotm_yaml <- FALSE
    }
  }
  
  if(is_gotm_yaml){
    yml_txt <- readLines(con = filepath)
    the_lines <- grep("-  source:", yml_txt)
    
    for(i in the_lines){
      yml_txt[i] <- gsub("-  source:", "- source:",
                         yml_txt[i])
    }
    
    writeLines(yml_txt, con = filepath)
  }
}


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
