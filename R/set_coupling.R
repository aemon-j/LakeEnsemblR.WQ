#'Set coupling settings for each model
#'
#'Sets any coupling settings that are needed to run a model
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'
#'@examples
#'
#'@importFrom configr read.config write.config
#'@importFrom glmtools read_nml write_nml
#'
#'@export


set_coupling <- function(config_file, folder){
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file)) 
  
  models_coupled <- lst_config[["models"]]
  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function (x) tolower(x[length(x)]))
  
  for(i in seq_len(length(models_coupled))){
    
    if(wq_models[i] == "selmaprotbas"){
      wq_config <- read.config(file.path(folder,
                                         lst_config[["config_files"]][[models_coupled[i]]]))
      
      # Loop through the names and check to what model they correspond
      for(j in names(wq_config[["instances"]])){
        selmaprotbas_model <- wq_config[["instances"]][[j]][["model"]]
        
        if(selmaprotbas_model == "selmaprotbas/phytoplankton"){
          coupling <- list(aa = "selmaprotbas/aa",
                           nn = "selmaprotbas/nn",
                           o2 = "selmaprotbas/o2",
                           po = "selmaprotbas/po",
                           si = "selmaprotbas/si",
                           dd_c = "selmaprotbas/dd_c",
                           dd_p = "selmaprotbas/dd_p",
                           dd_n = "selmaprotbas/dd_n",
                           dd_si = "selmaprotbas/dd_si")
          
          # Also coupling to fluff if sedrate > 0
          if(!is.null(wq_config[["instances"]][[j]][["parameters"]][["sed_rate"]])){
            if(wq_config[["instances"]][[j]][["parameters"]][["sed_rate"]] > 0){
              
              coupling[["fl_c"]] <- "selmaprotbas/fl_c"
              coupling[["fl_p"]] <- "selmaprotbas/fl_p"
              coupling[["fl_n"]] <- "selmaprotbas/fl_n"
              coupling[["fl_si"]] <- "selmaprotbas/fl_si"
            }
          }
        }else if(selmaprotbas_model == "selmaprotbas/zooplankton"){
          coupling <- list(aa = "selmaprotbas/aa",
                           o2 = "selmaprotbas/o2",
                           po = "selmaprotbas/po",
                           si = "selmaprotbas/si",
                           dd_c = "selmaprotbas/dd_c",
                           dd_p = "selmaprotbas/dd_p",
                           dd_n = "selmaprotbas/dd_n",
                           dd_si = "selmaprotbas/dd_si")
          
          # Coupling to prey. Not implemented yet. Should be
          # specified in the input. Default predation on all phytoplankton?
          # Also setting prey nutrient ratios
        }
        
        if(exists("coupling")){
          wq_config[["instances"]][[j]][["coupling"]] <- coupling
          rm(coupling)
        }
        
      }
      
      write.config(wq_config,
                   file.path(folder,
                             lst_config[["config_files"]][[models_coupled[i]]]),
                   write.type = "yaml")
      
    }else if(wq_models[i] == "wet"){
      wq_config <- read.config(file.path(folder,
                                         lst_config[["config_files"]][[models_coupled[i]]]))
      
      # Loop through the names and check to what model they correspond
      for(j in names(wq_config[["instances"]])){
        wet_model <- wq_config[["instances"]][[j]][["model"]]
        
        if(wet_model == "wet/abiotic_sediment"){
          coupling <- list(oxygen_pool_water = "abiotic_water/sO2W",
                           water_column_NH4 = "abiotic_water/sNH4W" ,
                           water_column_NO3 = "abiotic_water/sNO3W",
                           water_column_PO4 = "abiotic_water/sPO4W",
                           water_column_DDOM = "abiotic_water/sDDOMW",
                           water_column_NDOM = "abiotic_water/sNDOMW",
                           water_column_PDOM = "abiotic_water/sPDOMW",
                           water_column_SiO2 = "abiotic_water/sSiO2W")
        }else if(wet_model == "wet/burial"){
          coupling <- list(ammonium_pool_in_sediment = "abiotic_sediment/sNH4S",
                           nitrate_pool_in_sediment = "abiotic_sediment/sNO3S",
                           phosphate_pool_in_sediment = "abiotic_sediment/sPO4S",
                           adsorbed_phosphorus_in_sediment = "abiotic_sediment/sPAIMS",
                           inorg_pool_in_sediment = "abiotic_sediment/sDIMS",
                           POM_DW_in_sediment = "abiotic_sediment/sDPOMS",
                           POM_N_in_sediment = "abiotic_sediment/sNPOMS",
                           POM_P_in_sediment = "abiotic_sediment/sPPOMS",
                           particulate_Si_in_sediment = "abiotic_sediment/sSiPaS",
                           humus_DW_in_sediment = "abiotic_sediment/sDHumS",
                           humus_N_in_sediment = "abiotic_sediment/sNHumS",
                           humus_P_in_sediment = "abiotic_sediment/sPHumS",
                           POM_abiotic_update = "abiotic_sediment/tDAbioPOMS",
                           POM_resus_sed = "resus_sed/tDAbioPOMS",
                           humus_abiotic_update = "abiotic_sediment/tDAbioHumS",
                           IM_abiotic_update = "resus_sed/tDAbioIMS",
                           bPorS = "abiotic_sediment/bPorS",
                           cDepthS = "abiotic_sediment/cDepthS")
        }else if(wet_model == "wet/resus_sed"){
          coupling <- list(inorg_pool_in_sediment = "abiotic_sediment/sDIMS",
                           POM_DW_in_sediment = "abiotic_sediment/sDPOMS",
                           POM_N_in_sediment = "abiotic_sediment/sNPOMS",
                           POM_P_in_sediment = "abiotic_sediment/sPPOMS",
                           PO4_in_sediment = "abiotic_sediment/sPO4S",
                           adsorbed_phosphorus_in_sediment = "abiotic_sediment/sPAIMS",
                           NH4_in_sediment = "abiotic_sediment/sNH4S",
                           NO3_in_sediment = "abiotic_sediment/sNO3S",
                           particulate_Si_in_sediment = "abiotic_sediment/sSiPaS",
                           particulate_Si_in_water = "abiotic_water/sSiPaW",
                           NH4_in_water = "abiotic_water/sNH4W",
                           NO3_in_water = "abiotic_water/sNO3W",
                           PO4_in_water = "abiotic_water/sPO4W",
                           inorg_pool_in_water = "abiotic_water/sDIMW",
                           adsorbed_phosphorus_in_water = "abiotic_water/sPAIMW",
                           POM_DW_in_water = "abiotic_water/sDPOMW",
                           POM_N_in_water = "abiotic_water/sNPOMW",
                           POM_P_in_water = "abiotic_water/sPPOMW",
                           bPorS = "abiotic_sediment/bPorS")
        }else if(wet_model == "wet/macrophytes"){
          coupling <- list(ammonium_pool_water = "abiotic_water/sNH4W",
                           nitrate_pool_water = "abiotic_water/sNO3W",
                           phosphate_pool_water = "abiotic_water/sPO4W",
                           oxygen_pool_water = "abiotic_water/sO2W",
                           POM_DW_pool_water = "abiotic_water/sDPOMW",
                           POM_N_pool_water = "abiotic_water/sPPOMW",
                           POM_P_pool_water = "abiotic_water/sNPOMW",
                           ammonium_pool_sediment = "abiotic_sediment/sNH4S",
                           nitrate_pool_sediment = "abiotic_sediment/sNO3S",
                           phosphate_pool_sediment = "abiotic_sediment/sPO4S",
                           POM_DW_pool_sediment = "abiotic_sediment/sDPOMS",
                           POM_N_pool_sediment = "abiotic_sediment/sNPOMS",
                           POM_P_pool_sediment = "abiotic_sediment/sPPOMS",
                           DOM_DW_pool_water = "abiotic_water/sDDOMW",
                           DOM_N_pool_water = "abiotic_water/sNDOMW",
                           DOM_P_pool_water = "abiotic_water/sPDOMW",
                           DOM_DW_pool_sediment = "abiotic_sediment/sDDOMS",
                           DOM_N_pool_sediment = "abiotic_sediment/sNDOMS",
                           DOM_P_pool_sediment = "abiotic_sediment/sPDOMS",
                           oxic_layer_fraction = "abiotic_sediment/afOxySed",
                           bPorS = "abiotic_sediment/bPorS",
                           cDepthS = "abiotic_sediment/cDepthS")
        }else if(wet_model == "wet/phytoplankton"){
          coupling <- list(PO4_pool_water = "abiotic_water/sPO4W",
                           NH4_pool_water = "abiotic_water/sNH4W",
                           NO3_pool_water = "abiotic_water/sNO3W",
                           oxygen_pool_water = "abiotic_water/sO2W",
                           POM_DW_pool_water = "abiotic_water/sDPOMW",
                           POM_N_pool_water = "abiotic_water/sPPOMW",
                           POM_P_pool_water = "abiotic_water/sNPOMW",
                           DOM_DW_pool_water = "abiotic_water/sDDOMW",
                           DOM_N_pool_water = "abiotic_water/sNDOMW",
                           DOM_P_pool_water = "abiotic_water/sPDOMW",
                           PO4_pool_sediment = "abiotic_sediment/sPO4S",
                           NO3_pool_sediment = "abiotic_sediment/sNO3S",
                           NH4_pool_sediment = "abiotic_sediment/sNH4S",
                           POM_DW_pool_sediment = "abiotic_sediment/sDPOMS",
                           POM_N_pool_sediment = "abiotic_sediment/sNPOMS",
                           POM_P_pool_sediment = "abiotic_sediment/sPPOMS",
                           DOM_DW_pool_sediment = "abiotic_sediment/sDDOMS",
                           DOM_N_pool_sediment = "abiotic_sediment/sNDOMS",
                           DOM_P_pool_sediment = "abiotic_sediment/sPDOMS",
                           SiPa_pool_water = "abiotic_water/sSiPaW",
                           SiO2_pool_water = "abiotic_water/sSiO2W",
                           SiPa_pool_sediment = "abiotic_sediment/sSiPaS",
                           SiO2_pool_sediment = "abiotic_sediment/sSiO2S",
                           base_resuspension_rate = "resus_sed/tDResusDead",
                           aFunTauTmSet = "resus_sed/aFunTauTmSet")
        }else if(wet_model == "wet/zooplankton"){
          coupling <- list(POM_DW_pool_water = "abiotic_water/sDPOMW",
                           POM_N_pool_water = "abiotic_water/sPPOMW",
                           POM_P_pool_water = "abiotic_water/sNPOMW",
                           SiPa_pool_water = "abiotic_water/sSiPaW",
                           NH4_pool_water = "abiotic_water/sNH4W",
                           NO3_pool_water = "abiotic_water/sNO3W",
                           PO4_pool_water = "abiotic_water/sPO4W",
                           DOM_DW_pool_water = "abiotic_water/sDDOMW",
                           DOM_N_pool_water = "abiotic_water/sNDOMW",
                           DOM_P_pool_water = "abiotic_water/sPDOMW",
                           SiO2_pool_water = "abiotic_water/sSiO2W",
                           oxygen_pool_water = "abiotic_water/sO2W")
          
          # Prey not yet implemented
        }else if(wet_model == "wet/zoobenthos"){
          coupling <- list(POM_DW_pool_sediment = "abiotic_sediment/sDPOMS",
                           POM_P_pool_sediment = "abiotic_sediment/sPPOMS",
                           POM_N_pool_sediment = "abiotic_sediment/sNPOMS",
                           SiPa_pool_sediment = "abiotic_sediment/sSiPaS",
                           NH4_pool_sediment = "abiotic_sediment/sNH4S",
                           NO3_pool_sediment = "abiotic_sediment/sNO3S",
                           PO4_pool_sediment = "abiotic_sediment/sPO4S",
                           DOM_DW_pool_sediment = "abiotic_sediment/sDDOMS",
                           DOM_N_pool_sediment = "abiotic_sediment/sNDOMS",
                           DOM_P_pool_sediment = "abiotic_sediment/sPDOMS",
                           SiO2_pool_sediment = "abiotic_sediment/sSiO2S")
          
          # Prey not yet implemented
        }else if(wet_model == "wet/fish_mod"){
          
          coupling <- list(POM_DW_pool_water = "abiotic_water/sDPOMW",
                           POM_N_pool_water = "abiotic_water/sPPOMW",
                           POM_P_pool_water = "abiotic_water/sNPOMW",
                           NH4_pool_water = "abiotic_water/sNH4W",
                           PO4_pool_water = "abiotic_water/sPO4W",
                           DOM_DW_pool_water = "abiotic_water/sDDOMW",
                           DOM_N_pool_water = "abiotic_water/sNDOMW",
                           DOM_P_pool_water = "abiotic_water/sPDOMW",
                           oxygen_pool_water = "abiotic_water/sO2W")
          
          # Predation and life stages not yet implemented
        }
        if(exists("coupling")){
          wq_config[["instances"]][[j]][["coupling"]] <- coupling
          rm(coupling)
        }
      }
      
      write.config(wq_config,
                   file.path(folder,
                             lst_config[["config_files"]][[models_coupled[i]]]),
                   write.type = "yaml")
    }else if(wq_models[i] == "aed2"){
      
      wq_config <- read_nml(file.path(folder,
                                      lst_config[["config_files"]][[models_coupled[i]]]))
      
      # Loop through the names and check to what model they correspond
      for(j in names(wq_config)){
        
        if(j == "aed2_carbon"){
          wq_config[[j]]["Fsed_dic_variable"] <- "SDF_Fsed_dic"
          wq_config[[j]]["methane_reactant_variable"] <- "OXY_oxy"
        }else if(j == "aed2_silica"){
          wq_config[[j]]["silica_reactant_variable"] <- "OXY_oxy"
          wq_config[[j]]["Fsed_rsi_variable"] <- "SDF_Fsed_rsi"
        }else if(j == "aed2_nitrogen"){
          wq_config[[j]]["nitrif_reactant_variable"] <- "OXY_oxy"
          wq_config[[j]]["denit_product_variable"] <- ""
        }else if(j == "aed2_phosphorus"){
          wq_config[[j]]["phosphorus_reactant_variable"] <- "OXY_oxy"
          wq_config[[j]]["Fsed_frp_variable"] <- ""
          wq_config[[j]]["po4sorption_target_variable"] <- ""
        }else if(j == "aed2_organic_matter"){
          wq_config[[j]]["don_miner_product_variable"] <- "NIT_amm"
          wq_config[[j]]["dop_miner_product_variable"] <- "PHS_frp"
          wq_config[[j]]["doc_miner_reactant_variable"] <- "OXY_oxy"
          wq_config[[j]]["doc_miner_product_variable"] <- "CAR_dic"
        }else if(j == "aed2_phytoplankton"){
          wq_config[[j]]["p_excretion_target_variable"] <- "OGM_dop"
          wq_config[[j]]["n_excretion_target_variable"] <- "OGM_don"
          wq_config[[j]]["c_excretion_target_variable"] <- "OGM_doc"
          wq_config[[j]]["si_excretion_target_variable"] <- ""
          wq_config[[j]]["p_mortality_target_variable"] <- "OGM_pop"
          wq_config[[j]]["n_mortality_target_variable"] <- "OGM_pon"
          wq_config[[j]]["c_mortality_target_variable"] <- "OGM_poc"
          wq_config[[j]]["si_mortality_target_variable"] <- ""
          wq_config[[j]]["p1_uptake_target_variable"] <- "PHS_frp"
          wq_config[[j]]["n1_uptake_target_variable"] <- "NIT_nit"
          wq_config[[j]]["n2_uptake_target_variable"] <- "NIT_amm"
          wq_config[[j]]["si_uptake_target_variable"] <- "SIL_rsi"
          wq_config[[j]]["do_uptake_target_variable"] <- "OXY_oxy"
          wq_config[[j]]["c_uptake_target_variable"] <- "CAR_dic"
        }else if(j == "aed2_phytoplankton"){
          wq_config[[j]]["dn_target_variable"] <- "OGM_don"
          wq_config[[j]]["pn_target_variable"] <- "OGM_pon"
          wq_config[[j]]["dp_target_variable"] <- "OGM_dop"
          wq_config[[j]]["pp_target_variable"] <- "OGM_pop"
          wq_config[[j]]["dc_target_variable"] <- "OGM_doc"
          wq_config[[j]]["pc_target_variable"] <- "OGM_poc"
        }
         
      }
      
      write_nml(wq_config, file.path(folder,
                                     lst_config[["config_files"]][[models_coupled[i]]]))
      
    }
    
    # MyLake and PCLake don't require coupling, because they do not work with
    # modules. However, in case LER.WQ is run without phytoplankton, we could
    # use this function to "shut down" (some) phytoplankton (groups), e.g.
    # by setting growth rates to 0. Could also be done in the disable_module
    # function. 
  }
}
