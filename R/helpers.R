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
