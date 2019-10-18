#' Create biomod model list
#'
#' @param biomod_data_package_list List of biomod2 data package objects
#' @param biomod_model_options Biomod2 model options object
#'
#' @return
#' List of biomod2 model objects
#' @export
#'
#' @examples
create_biomod_model_list <- function(biomod_data_package_list, biomod_model_options){
  spp_list <- names(biomod_data_package_list)
  return(
    parallel::mcmapply(
         function(x, y){
           #x = biomod_data_package_list[[1]]
           #y = spp_list[[1]]
           spp_name <- y
           pa_strategy <- "nopa"
           try(pa_strategy <- x@PA.strategy)
           return(
             biomod2::BIOMOD_Modeling(data =  x,
                                      models = c('CTA', 'RF','GLM'),
                                      models.options = biomod_model_options,
                                      DataSplit = 70,
                                      NbRunEval = 10,
                                      Yweights = NULL,
                                      VarImport = 3,
                                      models.eval.meth = c('KAPPA', 'TSS','ROC'),
                                      SaveObj = T,
                                      rescal.all.models = T,
                                      do.full.models = T,
                                      modeling.id = paste(spp_name, pa_strategy, as.character(format(Sys.time(), '%y%m%d')), sep="_"))

           )
         }, biomod_data_package_list, spp_list
         , mc.cores = 4)
  )
}
