#'Disables a module
#'
#'Make sure that a certain module is not used in all models
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'@param module character; name of the module to disable
#'
#'@examples
#'
#'@importFrom configr read.config
#'
#'@export

# # Test
# config_file = "LakeEnsemblR_WQ.yaml"
# folder = "."
# module = "oxygen"

disable_module <- function(config_file, folder = ".", module){
  warning("Modules can not yet be disabled.")
}
