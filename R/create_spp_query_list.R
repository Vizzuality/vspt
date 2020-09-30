#' Create list of GBIF queries
#'
#' @param spp_list Species names
#' @param bb A sf::st_box object to limit search
#' @param creds gbif credentials
#' @param start_year Start year  to limit search
#' @param end_year End year to limit search
#'
#' @return
#' List of rgbif::occ_download_prep objects named by spp_list
#' @export
#'
#' @examples
#spp_list <- c('Pinus mugo')
#bb <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(bvso$biovar_Sweden$historical$raw)))
#create_spp_query_list(spp_list, bb, creds, start_year=1986, end_year=2020)
create_spp_query_list <- function(spp_list, bb, creds, start_year=1986, end_year=2020){
  keys <- lapply(spp_list, rgbif::name_backbone, rank='species')
  match_type <- sapply(keys, function(x) x$matchType)
  keys <- keys[match_type != 'NONE']
  nms <- sapply(keys, function(x) x$canonicalName)

  # c_list <- lapply(keys, function(k){
  #   rgbif::occ_count(
  #     taxonKey = k$usageKey
  #     , georeferenced = T
  #     , basisOfRecord = "HUMAN_OBSERVATION,OBSERVATION,MACHINE_OBSERVATION"
  #     , datasetKey = NULL
  #     , date = NULL
  #     , typeStatus = NULL
  #     , country = NULL
  #     , year = NULL
  #     , from = start_year
  #     , to = end_year
  #     , type = "countries"
  #     , publishingCountry = "US"
  #     , protocol = NULL
  #     , curlopts = list())
  # })
  #c_list[[1]]$SWEDEN
  #c_list[[1]]$DENMARK
  #c_list[[1]]$NORWAY

  # Prepare decimal lat long
  decimalLatitude = c(bb['ymin'], bb['ymax'])
  decimalLongitude = c(bb['xmin'], bb['xmax'])

   q_list <- lapply(keys, function(k){
     print(k$canonicalName)
     rgbif::occ_download_prep(
    rgbif::pred("taxonKey", k$usageKey),
    rgbif::pred_in("basisOfRecord", c("HUMAN_OBSERVATION","OBSERVATION","MACHINE_OBSERVATION","PRESERVED_SPECIMEN","MATERIAL_SAMPLE")),
    rgbif::pred("hasCoordinate",TRUE),
    rgbif::pred_gte("decimalLatitude", decimalLatitude[1]),
    rgbif::pred_lte("decimalLatitude", decimalLatitude[2]),
    rgbif::pred_gte("decimalLongitude", decimalLongitude[1]),
    rgbif::pred_lte("decimalLongitude", decimalLongitude[2]),
    #paste("geometry =", geom),
    rgbif::pred("hasGeospatialIssue",FALSE),
    rgbif::pred_gte("year", start_year),
    rgbif::pred_lte("year", end_year),
    email =creds$email,
    pwd =creds$pwd,
    user =creds$user,
    format="SIMPLE_CSV"
    )
   })
   names(q_list) <- nms
   return(q_list)
}
