#' Create spp presence-absence raster(s)
#'
#' @param spp_occurence_sf_list List of sf::sfc objects with spp occurence
#' @param r Raster object to use as base for adding presence-absence
#' @param land_mask sf::sf polygon land mask
#'
#' @return
#' A list of raster::raster objects representing presence (1) and absence (0) of a species, and NA values for no data.
#' @export
#'
#' @examples
create_spp_occurence_raster_list <- function(spp_occurence_sfc_list, r, land_mask){
  spp_name_list <- names(spp_occurence_sfc_list)
  return(
    parallel::mcmapply(
         function(x, y){
           #land_mask <- land_poly
           #r <- bvso[[arg_list$region]]$historical$pc_masked
           #x <- spp_occurence_sfc_list[[7]]
           #y <- names(spp_occurence_sfc_list)[7]
           out <- raster::mask(
               raster::rasterize(x = sf::as_Spatial(x), y = r, field = 1, background=0)
               , land_mask)
            names(out) <- y
           return(out)
         }, spp_occurence_sfc_list, spp_name_list, mc.cores = parallel::detectCores() - 1)
  )
}
