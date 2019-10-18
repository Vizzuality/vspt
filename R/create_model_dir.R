#' Create a directory for modelling
#'
#' @param path The path where the directory should be created
#'
#' @return
#' Nothing, side effect is a directory is create with arg_list.yaml
#' and pipeline.R, and a directory for adding bioclimatic rasters.
#' @export
#'
#' @examples
create_model_dir <- function(path){
  dir.create(path)
  dir.create(file.path(path, 'bioclimatic'))
  file.copy(
    system.file('ext_data', 'arg_list.yaml', package = 'vspt'),
    path)
  file.copy(
    system.file('ext_data', 'pipeline.R', package = 'vspt'),
    path)
  print(paste0("Created spp modelling directory at ", path))
}
