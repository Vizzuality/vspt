#' Create ISEA-3-HEXAGON_grid_from_polygon
#'
#' Create a hexagon grid covering the polygon object at given resolution.
#' Set `intersect_polygon` = T for a grid with only geometries that touch the
#' polygon object, else a grid covering the bounding box of the object is
#' returned.
#'
#' @param polygon_object A sf polygon object
#' @param grid_res The desired grid resolution for the hex grid
#' @param set_seed Give a string used to fix random seed for uuids
#' @param intersect_polygon Boolean, intersect the grid with polygon object
#'
#' @return
#' A sf::sf object where each feature is a hexagon with a unique uuid, area [m2], and perimeter [m].
#' @export
#'
#' @examples
#' bv <- create_biovar_stack(
#' region = 'biovar_Sweden',
#' biovar_list = c("biovar04", "biovar06", "biovar16"),
#' time_period = "biovar_historical",
#' path = "inst/ext_data/bioclimatic"
#' )
#' g <- create_grid_from_poly(bv$land_poly, grid_res=11, set_seed=999)
#' if(require(tmap)){
#' tmap_mode("view")
#' tm_shape(g)+ tm_fill("area_m2", palette = sf::sf.colors(3), alpha=0.7, colorNA=NULL,
#' legend.show = T)}else{plot(g)}
create_grid_from_poly <- function(polygon_object, grid_res, set_seed=F, intersect_polygon=T){

  # Convert text seed to integer
  if(!is.null(set_seed) | isTRUE(set_seed)){
    letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}
    set_seed <- sum(unname(sapply(set_seed, letter2num)))
    }
  # Convert to temporary shape file
  # required as dggs only accepts shape files?
  f <- file.path(tempdir(), "poly.shp")
  sf::st_write(polygon_object, f, delete_dsn=TRUE)

  # Create ISEA-3-HEXAGON grid definition
  dggs <- dggridR::dgconstruct(res=grid_res, metric=TRUE, resround='down', show_info = T)

  # Apply to polygon and convert to sf object
  hg <- sf::st_as_sf(dggridR::dgshptogrid(dggs=dggs, shpfname=f, frame=F))

  if(intersect_polygon){
    hg <- hg[lengths(sf::st_intersects(hg, polygon_object)) > 0,]
  }

  # Add random uuids to each feature
  if(set_seed){set.seed(set_seed)}
  hg$uuid <- apply(hg, 1, FUN=function(x){return(uuid::UUIDgenerate(FALSE))})

  # Add area for each feature
  hg$area_m2 <- as.numeric(sf::st_area(hg$geometry))

  # Add area for each feature
  hgls <- sf::st_cast(hg$geometry, "LINESTRING" )
  hg$perimeter_m <- as.numeric(sf::st_length(hgls))

  return(hg)
}
