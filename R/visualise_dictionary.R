#'Visualise LakeEnsemblR.WQ dictionary
#'
#'Visualise the file structure of the dictionary
#'
#'@param print_console boolean; print result to console
#'@param save_as_table boolean; save result as a txt file?
#'                     If yes, use folder and filename arguments
#'@param folder path; path where to write table
#'@param filename character; file name of table
#'@param module boolean; include module in visualisation?
#'@param process boolean; include process in visualisation?
#'@param subprocess boolean; include subprocess in visualisation?
#'@param model boolean; include model in visualisation?
#'@param parameter boolean; include parameter in visualisation?
#'
#'@examples
#'
#'@import data.tree
#'
#'@export

visualise_dictionary <- function(print_console = TRUE, save_as_table = FALSE, folder = ".",
                                 filename = "dictionary.txt", module = TRUE, process = TRUE,
                                 subprocess = TRUE, model = FALSE, parameter = FALSE){
  
  dict <- LakeEnsemblR_WQ_dictionary
  
  dict$pathString <- "dictionary"
  # Note: "pathString" (camelCase) is not in line with the coding style used in LakeEnsemblR_WQ,
  # but it should not be changed, as this seems to be hard-coded in the data.tree package!!
  
  if(module){
    dict$pathString <- paste(dict$pathString, dict$module, sep = "/")
  }
  
  if(process){
    dict$pathString <- paste(dict$pathString, dict$process, sep = "/")
  }
  
  if(subprocess){
    dict$pathString <- paste(dict$pathString, dict$subprocess, sep = "/")
  }
  
  if(model){
    dict$pathString <- paste(dict$pathString, dict$model, sep = "/")
  }
  
  if(parameter){
    dict$pathString <- paste(dict$pathString, dict$parameter, sep = "/")
  }
  
  dict <- data.tree::as.Node(dict)
  
  if(print_console){
    print(dict)
  }
  
  if(save_as_table){
    write.table(dict,
                file.path(folder, filename),
                row.names = FALSE,
                quote = FALSE,
                col.names = FALSE)
  }
  
}

# Create alias for American-English spelling

#' @export
#' @rdname visualise_dictionary
visualize_dictionary <- visualise_dictionary
