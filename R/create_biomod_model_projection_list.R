#' Project biomod2 model onto biovariable stack
#'
#'
#' @param biomod_model_list List of calibrated biomod2 models
#' @param biovar_stack A raster::stack representing the biovariables for the projection
#' @param proj_name A charchter name to label the output rasters
#'
#' @return
#' A character path to a biomod2::BIOMOD.projection.out saved as .RDA
#' @export
#'
#' @examples
create_biomod_model_projection_list <- function(biomod_model_list, biovar_stack, proj_name='historical'){
  return(
    parallel::mclapply(biomod_model_list,
           function(x){
             #x <- biomod_model_list[[1]]
             #biovar_stack <- bvso[[arg_list$region]]$historical$pc_masked
             #proj_name='historical'
             out <-
               biomod2::BIOMOD_Projection(
                 modeling.output = x,
                 selected.models = "all",
                 new.env = raster::stack(biovar_stack),
                 xy.new.env = NULL,
                 proj.name = proj_name,
                 binary.meth = 'ROC',
                 filtered.meth = NULL,
                 build.clamping.mask = T,
                 compress = T,
                 keep.in.memory = F
                 #,output.format = '.Rdata',
                 #do.stack = T
                 )
             return(out)

           }
           , mc.cores = 4 # reduce this if getting errors!
           )
  )
}
