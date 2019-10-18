#' Project biomod ensemble model onto biovar stack
#'
#' @param biomod_ensemble_model_list List of calibrated biomod2 ensemble models
#' @param bimod_model_projection_list List of raster::stacks representing the model projections on biovariables
#'
#' @return
#' @export
#'
#' @examples
create_biomod_ensemble_model_projection_list <-
  function(biomod_ensemble_model_list,
           biomod_model_projection_list) {
    return(
      parallel::mcmapply(
        function(x, y) {
          #x <- biomod_ensemble_model_list[[1]]
          #y <- biomod_model_projection_list$historical[[1]]
          out <-
            biomod2::BIOMOD_EnsembleForecasting(
              EM.output =  x,
              projection.output = y,
              binary.meth = "ROC",
              filtered.meth = NULL
            )

          return(out)
        },
        biomod_ensemble_model_list,
        biomod_model_projection_list,
        mc.cores = 4
      )
    )
  }
